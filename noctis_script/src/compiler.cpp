// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/compiler.hpp>
#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/optimizer.hpp>

#include <sstream>
#include <format>
#include <iomanip>
#include <print>

namespace NCSC
{

template <typename T>
static T getInstrOperand(const std::vector<Byte>& bc, size_t& i) {
    T val = readWord<T>(bc, i);
    i += sizeof(T);
    return val;
}

std::string Compiler::disassemble(const std::vector<Byte>& bc) {
    std::ostringstream oss;

    for (size_t i = 0; i < bc.size();) {
        size_t offset = i;
        Byte op = bc[i++];

        Instruction instr = static_cast<Instruction>(op);
        auto it = INSTR_INFO.find(instr);
        if (it == INSTR_INFO.end()) {
            oss << std::setw(4) << offset << ": UNKNOWN INSTR\n";
            continue;
        }

        const auto& info = it->second;
        oss << std::setw(4) << offset << ": " << info.first;

        if (instr == Instruction::PUSH) {
            oss << " ";

            size_t size = 0;
            Value val = Value::fromBytes(bc, i, size);
            
            oss << val.operator std::string();
            
            i += size;
        } else if (info.second > 0) {
            oss << " ";
            switch (info.second) {
                case 2: oss << getInstrOperand<Word>(bc, i); break;
                case 4: oss << getInstrOperand<DWord>(bc, i); break;
                case 8: oss << getInstrOperand<QWord>(bc, i); break;
                default: oss << "(invalid size)";
            }
        }

        oss << "\n";
    }

    return oss.str();
}

std::unique_ptr<Script> Compiler::compileScript(std::shared_ptr<ScriptSource> source) {
    src_ = source;

    std::vector<Token> tokens = Lexer(src_).tokenizeAll();
    Parser parser(tokens, src_);

    ASTNode root = parser.parseAll();
    if (parser.hasErrors()) {
        std::vector<Error> parserErrors = parser.getErrors();
        compileErrors_.insert(compileErrors_.end(), parserErrors.begin(), parserErrors.end()); 
    
        return nullptr;
    }

    return compileScript(root);
}

std::unique_ptr<Script> Compiler::compileScript(const ASTNode &root) {
    auto script = std::make_unique<Script>();
    script->ctx = ctx_;

    currScript_ = script.get();

    for (const auto &node : root.children()) {
        switch (node.type()) {
            case ASTNodeType::FUNCTION:
                enterNewScope();
                compileFunction(node);
                scopes_.clear();

                break;

            case ASTNodeType::VARIABLE_DECLARATION: {
                compileVariableDeclaration(node, true);

                emit(Instruction::RETVOID);

                finalizeBc(tempCompiledBytecode_);

                GlobalVar *gv = currScript_->getGlobalVariable(currScript_->numGlobalVariables - 1); 
                gv->bytecode = tempCompiledBytecode_;
                gv->requiredStackSize = computeRequiredStackSize(tempCompiledBytecode_);

                tempCompiledBytecode_.clear();

                break;
            }

            case ASTNodeType::OBJECT:
                compileObject(node);
                break;
            
            default:
                break;
        }
    }

    currScript_ = nullptr;
    return script;
}

void Compiler::enterNewScope() {
    scopes_.push_back(Scope{});
    currScope_ = &scopes_.back();
    if (scopes_.size() > 1)
        currScope_->parent = &scopes_[scopes_.size() - 2];
    nextScopeIdx_++;
}

void Compiler::exitScope() {
    nextScopeIdx_--;
    if (nextScopeIdx_ == 0)
        currScope_ = nullptr;
    else {
        currScope_ = &scopes_[nextScopeIdx_ - 1];
    }
}

void Compiler::finalizeBc(std::vector<Byte> &bc) {
#if NCSC_ALWAYS_OPTIMIZE
    Optimizer::optimize(tempCompiledBytecode_);
#endif

    resolveJumps(bc);
}


void Compiler::createCompileError(const ErrInfo &info, const ASTNode &node) {
    Error err(info, src_);
    err.setLocation(node.line, node.col, node.colEnd);
    compileErrors_.push_back(err);
}

void Compiler::emit(Byte byte) {
    tempCompiledBytecode_.push_back(byte);
}

void Compiler::emit(Instruction instr) {
    emit(static_cast<Byte>(instr));
}

void Compiler::emit(const std::vector<Byte> &bytecode) {
    for (size_t i = 0; i < bytecode.size(); i++) 
        emit(bytecode[i]);
}

void Compiler::emit(Word dw) {
    std::vector<Byte> bytes {
        static_cast<Byte>(dw & 0xFF),
        static_cast<Byte>((dw >> 8) & 0xFF),
    };
    emit(bytes);
}

void Compiler::emit(DWord dw) {
    std::vector<Byte> bytes {
        static_cast<Byte>(dw & 0xFF),
        static_cast<Byte>((dw >> 8) & 0xFF),
        static_cast<Byte>((dw >> 16) & 0xFF),
        static_cast<Byte>((dw >> 24) & 0xFF),
    };
    emit(bytes);
}

void Compiler::emit(QWord qw) {
    std::vector<Byte> bytes;
    bytes.resize(sizeof(QWord));
    makeBytes(qw, bytes, 0);
    emit(bytes);
}

void Compiler::patchBytecode(size_t location, Instruction instr, const std::vector<Byte> &operandBytes) {
    std::vector<Byte> tempBytes;
    tempBytes.reserve(1 + operandBytes.size());
    tempBytes.push_back(static_cast<Byte>(instr));
    tempBytes.insert(tempBytes.end(), operandBytes.begin(), operandBytes.end());

    tempCompiledBytecode_.insert(
        tempCompiledBytecode_.begin() + location, 
        tempBytes.begin(), 
        tempBytes.end());
}

void Compiler::compileFunction(const ASTNode &funcDecl, bool method) {
    assert(funcDecl.type() == ASTNodeType::FUNCTION);

    const auto &firstChild = funcDecl.child(0);
    size_t reTyNodeIdx = 0;
    // Access modifier for a member variable
    if (method && firstChild.type() == ASTNodeType::TOKEN)
        reTyNodeIdx = 1;

    std::string funcDeclName = funcDecl.child(reTyNodeIdx + 1).token()->val;
    if ((method && currObject_->getMethod(funcDeclName)) || currScript_->getFunction(funcDeclName)) {
        createCompileError(FUNC_ALREADY_EXISTS.format(funcDeclName), funcDecl.child(1));
        return;
    }

    if (method) {
        Method me;
        me.isPublic = firstChild.token() && 
                      firstChild.token()->type == TokenType::PUBLIC_KWD;
        
        me.numParams = 1;

        Variable thisVar;
        thisVar.name = "this";
        thisVar.type = currObject_->type;
        currScope_->addLocalVar(thisVar);

        currFunction_ = &me;
    } else {
        ScriptFunction fun;
        fun.numParams = 0;

        currFunction_ = &fun;
    }
    
    ASTNode retType = funcDecl.child(reTyNodeIdx); 
    if (retType.type() == ASTNodeType::DATA_TYPE)
        currFunction_->returnTy = valueTypeFromTok(*retType.token());
    else
        currFunction_->returnTy = ValueType::VOID;
    
    currFunction_->name = funcDeclName;

    ASTNode paramsNode = funcDecl.child(reTyNodeIdx + 2);
    currFunction_->numParams += paramsNode.numChildren() / 2;
    currFunction_->numLocals = currFunction_->numParams;
    for (int i = 0; i < paramsNode.numChildren(); i++) {
        ValueType ty = valueTypeFromTok(*paramsNode.child(i).token());
        
        // Arguments are treated as local variables
        Variable v;
        v.name = paramsNode.child(++i).token()->val;
        v.type = ty;
        currScope_->addLocalVar(v);
        currFunction_->paramTypes.push_back(ty);   
    }

    const ASTNode& statementBlock = funcDecl.lastChild();
    compileStatementBlock(statementBlock);

    currFunction_->numLocals += computeMaxLocals(&scopes_.back()) - currFunction_->numParams;

    if (!hasErrors()) {
        // For void functions, add a return at the end if none currently exists
        if (currFunction_->returnTy == ValueType::VOID && !currScope_->hasReturned) {
            emit(Instruction::RETVOID);
        }
        // For non void functions, error if there is no return at the end
        else if (currFunction_->returnTy != ValueType::VOID && !currScope_->hasReturned) {
            createCompileError(FUNCTION_SHOULD_RET_VAL.format(currFunction_->name), statementBlock.lastChild());
            return;
        }

        finalizeBc(tempCompiledBytecode_);

        currFunction_->requiredStackSize = computeRequiredStackSize(tempCompiledBytecode_);
        currFunction_->bytecode = tempCompiledBytecode_;
        tempCompiledBytecode_.clear();
    }

    if (method) 
        currObject_->addMethod(*static_cast<Method *>(currFunction_));
    else
        currScript_->addFunction(*currFunction_);

    currFunction_ = nullptr;
}

size_t Compiler::computeRequiredStackSize(std::vector<Byte> &bc) {
    size_t maxSize = 0;
    size_t currSize = 0;
    for (size_t i = 0; i < bc.size();) {
        Instruction instr = static_cast<Instruction>(bc[i]);

        switch (instr) {
            case Instruction::LOADLOCAL: 
            case Instruction::LOADLOCAL_REF: 
            case Instruction::LOADGLOBAL: 
            case Instruction::LOADGLOBAL_REF: 
            case Instruction::PUSH: 
            case Instruction::DUP: 
                maxSize = std::max(maxSize, ++currSize);
                break;
            
            case Instruction::ADD:
            case Instruction::SUB:
            case Instruction::MUL:
            case Instruction::DIV:
            case Instruction::CMPST:
            case Instruction::CMPSE:
            case Instruction::CMPGT:
            case Instruction::CMPGE:
            case Instruction::CMPEQ:
            case Instruction::CMPNE:
            case Instruction::SETREF:
                currSize -= 2;
                break;

            case Instruction::CALLSCRFUN: {
                DWord idx = readWord<DWord>(bc, i + 1);
                const ScriptFunction *scrFun = currScript_->getFunction(idx);
                currSize -= scrFun->numParams;

                break;
            }

            case Instruction::CLGLBLCPPFUN: {
                DWord idx = readWord<DWord>(bc, i + 1);
                const auto *fun = ctx_->getGlobalFunction(idx);
                currSize -= fun->numParams;

                break;
            }

            default: break;
        }

        i += getInstructionSize(bc, i);
    }

    return maxSize;
}

size_t Compiler::computeMaxLocals(const Scope *scope) {
    if (scope == nullptr)
        return 0;
    return std::max(scope->localVariables.size(), computeMaxLocals(scope->parent));
}

void Compiler::resolveJumps(std::vector<Byte> &bc) {
    std::unordered_map<QWord, size_t> jmpLocations;

    for (QWord i = 0; i < tempCompiledBytecode_.size();) {
        Instruction instr = static_cast<Instruction>(bc[i]);
        
        switch (instr) {
            case Instruction::JMP:
            case Instruction::JMPFALSE: {
                QWord jmpLabelNum = readWord<QWord>(bc, i + sizeof(Instruction));
                jmpLocations.emplace(jmpLabelNum, i);

                i += sizeof(QWord) + sizeof(Instruction);
                break;
            }

            case Instruction::LABEL: {
                QWord labelNum = readWord<QWord>(bc, i + sizeof(Instruction));
                size_t jmpIdx = jmpLocations.at(labelNum);
                Instruction jmpTy = static_cast<Instruction>(readWord<Byte>(bc, jmpIdx));
                size_t operandIdx = jmpIdx + sizeof(Instruction);

                std::vector<Byte> operandBytes(sizeof(QWord), 0);
                makeBytes(i, operandBytes, 0);

                // Patch the jump to point to the current idx 
                std::copy(operandBytes.begin(), operandBytes.end(), bc.begin() + operandIdx);
                // Remove the LABEL instruction
                constexpr size_t instrSize = sizeof(Instruction) + sizeof(QWord);
                bc.erase(bc.begin() + i, bc.begin() + i + instrSize);
                break;
            }

            default:
                i += getInstructionSize(bc, i);
                break;
        }
    }
}

Compiler::SymbolSearchRes Compiler::searchSymbol(const std::string &name, ScriptObject *obj) {
    // Local variable
    if (currScope_) {
        DWord varIdx = currScope_->getLocalVarIdx(name);
        if (varIdx != NCSC_INVALID_IDX) {
            const Variable *var = currScope_->getLocalVar(varIdx); 
            return SymbolSearchRes{
                .var = var,
                .idx = varIdx,
                .foundType = var->type,
                .ty = SymbolSearchRes::LOCAL_VAR,
            };
        }
    }

    // Search for members or methods in the object
    if (obj) {        
        DWord memberIdx = obj->getMemberIdx(name);
        if (memberIdx != NCSC_INVALID_IDX) {
            const Variable *var = obj->getMember(memberIdx);
            return SymbolSearchRes{
                .var = var,
                .idx = memberIdx,
                .foundType = var->type,
                .ty = SymbolSearchRes::MEMBER_VAR,
            };
        }

        DWord methodIdx = obj->getMethodIdx(name);
        if (methodIdx != NCSC_INVALID_IDX) {
            const Function *method = obj->getMethod(methodIdx);
            return SymbolSearchRes{
                .fun = method,
                .idx = methodIdx,
                .foundType = method->returnTy,
                .ty = SymbolSearchRes::METHOD,
            };
        } 
    }

    // Script object or function
    if (currScript_) {
        DWord funIdx = currScript_->getFunctionIdx(name);
        if (funIdx != NCSC_INVALID_IDX) {
            const Function *fun = currScript_->getFunction(funIdx);
            return SymbolSearchRes{
                .fun = fun,
                .idx = funIdx,
                .foundType = fun->returnTy,
                .ty = SymbolSearchRes::FUNCTION,
            };
        }

        DWord scriptObjIdx = currScript_->getObjectIdx(name);
        if (scriptObjIdx != NCSC_INVALID_IDX)
            return SymbolSearchRes{
                .obj = currScript_->getObject(funIdx),
                .idx = scriptObjIdx,
                .ty = SymbolSearchRes::OBJECT,
            };
    }

    if (currObject_) {        
        DWord memberIdx = currObject_->getMemberIdx(name);
        if (memberIdx != NCSC_INVALID_IDX) {
            const Variable *var = currObject_->getMember(memberIdx);
            return SymbolSearchRes{
                .var = var,
                .idx = memberIdx,
                .foundType = var->type,
                .ty = SymbolSearchRes::MEMBER_VAR,
            };
        }

        DWord methodIdx = currObject_->getMethodIdx(name);
        if (methodIdx != NCSC_INVALID_IDX) {
            const Function *method = currObject_->getMethod(methodIdx);
            return SymbolSearchRes{
                .fun = method,
                .idx = methodIdx,
                .foundType = method->returnTy,
                .ty = SymbolSearchRes::METHOD,
            };
        } 
    }

    DWord cppFunIdx = ctx_->getGlobalFunctionIdx(name);
    if (cppFunIdx != NCSC_INVALID_IDX) {
        const Function *cppFun = ctx_->getGlobalFunction(cppFunIdx); 
        return SymbolSearchRes{
            .fun = cppFun,
            .idx = cppFunIdx,
            .foundType = cppFun->returnTy,
            .ty = SymbolSearchRes::FUNCTION,
        };
    }
        
    return SymbolSearchRes{ .ty = SymbolSearchRes::INVALID };
}

void Compiler::compileObject(const ASTNode &obj) {
    assert(obj.type() == ASTNodeType::OBJECT);

    ScriptObject scriptObj;
    scriptObj.name = obj.child(0).token()->val;
    scriptObj.type = makeObjectType(currScript_->getObjectCount());

    // Member init method
    Method membInit;
    membInit.name = "<membInit>";
    membInit.returnTy = ValueType::VOID;
    // 'this'
    membInit.numLocals = 1;
    membInit.paramTypes = { scriptObj.type };

    scriptObj.addMethod(membInit);   

    currObject_ = &scriptObj;

    for (const auto &child : obj.children()) {
        switch (child.type()) {
            case ASTNodeType::VARIABLE_DECLARATION: {
                // Compile variable declaration as a member variable
                compileVariableDeclaration(child, false, true);

                resolveJumps(tempCompiledBytecode_);

                auto &membInitBc = scriptObj.getMethod(0)->bytecode; 
                membInitBc.insert(membInitBc.end(), tempCompiledBytecode_.begin(), tempCompiledBytecode_.end());
                tempCompiledBytecode_.clear();

                break;
            }

            case ASTNodeType::FUNCTION:
                enterNewScope();
                compileFunction(child, true);
                scopes_.clear();

                break;
        }
    }

    // Optimize the member init method's bytecode
    Method &membInitt = *scriptObj.getMethod(0);

    finalizeBc(membInitt.bytecode);
    membInitt.requiredStackSize = computeRequiredStackSize(membInitt.bytecode);

    currScript_->addObject(scriptObj);
}

void Compiler::compileVariableDeclaration(const ASTNode &varDecl, bool global, bool member) {
    assert(varDecl.type() == ASTNodeType::VARIABLE_DECLARATION);

    const auto &firstChild = varDecl.child(0);
    size_t varTyNodeIdx = 0;
    // Access modifier for a member variable
    if (member && firstChild.type() == ASTNodeType::TOKEN)
        varTyNodeIdx = 1;

    const ASTNode &assignmentNode = varDecl.child(varTyNodeIdx + 1); 
    const auto &varNameNode = assignmentNode.child(0).child(0);

    if (varNameNode.numChildren() > 1 || varNameNode.numChildren() == 0) {
        createCompileError(EXPECTED_AN_ID, varNameNode);
        return;
    }

    const ASTNode &varNameVal = varNameNode.child(0);
    std::string varName = varNameVal.token()->val;
    ValueType varType = valueTypeFromTok(*varDecl.child(varTyNodeIdx).token());

    if (global) {
        GlobalVar gv;
        gv.name = varName;
        gv.type = varType;

        currScript_->addGlobalVariable(gv);
        currScript_->numGlobalVariables++;
    } else if (member) {
        MemberVariable mv;
        mv.name = varName;
        mv.type = varType;
        mv.isPublic = firstChild.token() && 
                      firstChild.token()->type == TokenType::PUBLIC_KWD;

        currObject_->addMember(mv);
        currObject_->numMembers++;
    } else {
        Variable v;
        v.name = varName;
        v.type = varType;

        currScope_->addLocalVar(v);
    }

    compileAssignment(assignmentNode, varType);
}

void Compiler::compileExpression(const ASTNode &expr, ValueType expectedType) {
    assert(expr.type() == ASTNodeType::EXPRESSION);

    if (expr.numChildren() == 1 && expr.child(0).type() == ASTNodeType::EXPRESSION_TERM) {
        compileExpressionTerm(expr.child(0), expectedType);
        return;
    }

    ASTNode rootOp = expr.child(0);
    // Comparison ops will always be at the top of the binary tree
    // so if any of those is present, the expression returns a boolean
    TokenType rootOpTy = rootOp.token()->type;
    bool isComparisonOp = rootOpTy == TokenType::STRICTLY_SMALLER
       || rootOpTy == TokenType::SMALLER_EQUAL
       || rootOpTy == TokenType::STRICTLY_BIGGER
       || rootOpTy == TokenType::BIGGER_EQUAL
       || rootOpTy == TokenType::DOUBLE_EQUAL
       || rootOpTy == TokenType::NOT_EQUAL;
    
    if (isComparisonOp) {
        if (expectedType != ValueType::BOOL) {
            createCompileError(EXPECTED_TYPE_INSTEAD_GOT.format(
                valueTypeToString(expectedType), 
                valueTypeToString(ValueType::BOOL)), expr);
            return;
        }
        else
            // The rest of the expression can be of any type 
            expectedType = ValueType::INVALID;
    }

    recursivelyCompileExpression(rootOp, expectedType);
}

void Compiler::recursivelyCompileExpression(const ASTNode &exprChild, ValueType expectedType) {
    if (exprChild.type() == ASTNodeType::BINOP) {
        recursivelyCompileExpression(exprChild.child(1), expectedType);
        recursivelyCompileExpression(exprChild.child(0), expectedType);
        compileOperator(exprChild);
    } else if (exprChild.type() == ASTNodeType::EXPRESSION_TERM) {
        compileExpressionTerm(exprChild, expectedType);
    }
}

void Compiler::compileOperator(const ASTNode &binop) {
    assert(binop.type() == ASTNodeType::BINOP);

    switch (binop.token()->type) {
        case TokenType::PLUS:             emit(Instruction::ADD);   return;
        case TokenType::MINUS:            emit(Instruction::SUB);   return;
        case TokenType::STAR:             emit(Instruction::MUL);   return;
        case TokenType::SLASH:            emit(Instruction::DIV);   return;

        case TokenType::STRICTLY_SMALLER: emit(Instruction::CMPST); return;
        case TokenType::SMALLER_EQUAL:    emit(Instruction::CMPSE); return;
        case TokenType::STRICTLY_BIGGER:  emit(Instruction::CMPGT); return;
        case TokenType::BIGGER_EQUAL:     emit(Instruction::CMPGE); return;
        case TokenType::DOUBLE_EQUAL:     emit(Instruction::CMPEQ); return;
        case TokenType::NOT_EQUAL:        emit(Instruction::CMPNE); return;

        default:                          emit(Instruction::NOOP);  return;
    }
}

void Compiler::compileConstantPush(const ASTNode &constant, ValueType expectedType) {
    assert(constant.type() == ASTNodeType::CONSTANT);

    TokenType constTokTy = constant.token()->type; 
    const std::string &constTokVal = constant.token()->val;
    if (constTokTy == TokenType::FLOAT_CONSTANT) {
        switch(expectedType) {
            case ValueType::FLOAT32: {
                float32_t val = std::stof(constTokVal);
                std::vector<Byte> bytes(getValueSize(val), 0);
                makeValueBytes(std::bit_cast<uint32_t>(val), ValueType::FLOAT32, bytes, 0);
                emit(Instruction::PUSH);
                emit(bytes);
                break;
            }
            case ValueType::INVALID:
            case ValueType::FLOAT64: {
                float64_t val = std::stod(constTokVal);
                std::vector<Byte> bytes(getValueSize(val), 0);
                makeValueBytes(std::bit_cast<uint64_t>(val), ValueType::FLOAT64, bytes, 0);
                emit(Instruction::PUSH);
                emit(bytes);
                break;
            }
            default:
                createCompileError(EXPECTED_NON_FLOATING_POINT.format(constTokVal), constant);
                return;
        }
    } else if (constTokTy == TokenType::INT_CONSTANT) {
        switch(expectedType) {
            // Bools are ints
            case ValueType::BOOL:    emitIntConstant<int8_t>(constTokVal, constant, ValueType::BOOL);     break;
            
            case ValueType::INT8:    emitIntConstant<int8_t>(constTokVal, constant, ValueType::INT8);     break;
            case ValueType::INT16:   emitIntConstant<int16_t>(constTokVal, constant, ValueType::INT16);   break;
            // If a float is expected for the expression, we can safely emit an int
            // and the VM will be able to use it as a float
            case ValueType::FLOAT32:
            case ValueType::INT32:   emitIntConstant<int32_t>(constTokVal, constant, ValueType::INT32);   break;
            
            case ValueType::FLOAT64:
            case ValueType::INVALID:
            case ValueType::INT64:   emitIntConstant<int64_t>(constTokVal, constant, ValueType::INT64);   break;

            case ValueType::UINT8:   emitIntConstant<uint8_t>(constTokVal, constant, ValueType::UINT8);   break;
            case ValueType::UINT16:  emitIntConstant<uint16_t>(constTokVal, constant, ValueType::UINT16); break;
            case ValueType::UINT32:  emitIntConstant<uint32_t>(constTokVal, constant, ValueType::UINT32); break;
            case ValueType::UINT64:  emitIntConstant<uint64_t>(constTokVal, constant, ValueType::UINT64); break;

            default: assert(0 && "Wtf");
        }
    } else if (constTokTy == TokenType::TRUE_KWD || constTokTy == TokenType::FALSE_KWD) {
        if (expectedType != ValueType::BOOL) {
            createCompileError(CANT_PROMOTE_TY_TO.format( 
                valueTypeToString(expectedType), 
                valueTypeToString(ValueType::BOOL)), constant);
            return;
        }
        
        if (constTokTy == TokenType::TRUE_KWD)
            emitIntConstant<int8_t>("1", constant, ValueType::BOOL);
        else 
            emitIntConstant<int8_t>("0", constant, ValueType::BOOL);
    }
}

void Compiler::compileExpressionTerm(const ASTNode &exprTerm, ValueType expectedType) {
    assert(exprTerm.type() == ASTNodeType::EXPRESSION_TERM);

    size_t exprValueIdx = SIZE_MAX;
    for (size_t i = 0; i < exprTerm.numChildren(); i++) {
        if (exprTerm.child(i).type() == ASTNodeType::EXPRESSION_VALUE) {
            exprValueIdx = i;
            break;
        }
    }

    if (exprValueIdx == SIZE_MAX)
        // Parsing error
        return;

    const auto &exprValue = exprTerm.child(exprValueIdx);

    // Pre-ops and post-ops will push the required value on the stack
    // If an expression term has none (constant, ...), pushing the value is necessary
    bool hasValOnStack = false;

    size_t childIdx = 0;
    for (const auto &child : exprTerm.children()) {
        if (child.type() == ASTNodeType::EXPRESSION_PREOP) {
            compileExpressionPreOp(child, exprValue, expectedType);
            childIdx++;
            hasValOnStack = true;
        } else 
            break;
    }

    if (exprTerm.child(childIdx).type() != ASTNodeType::EXPRESSION_VALUE)
        // Error while parsing
        return;
    childIdx++;

    for (;;) {
        if (childIdx >= exprTerm.numChildren())
            break;
        else {
            compileExpressionPostOp(exprTerm.child(childIdx), exprValue, expectedType);
            childIdx++;
            hasValOnStack = true;
        }
    }

    if (!hasValOnStack)
        compileExpressionValue(exprValue, expectedType);
    else if (hasValOnStack && clearMask(expectedType, ValueType::REF_MASK) == ValueType::VOID)
        emit(Instruction::POP);
}

void Compiler::compileStatementBlock(const ASTNode &stmtBlock) {
    assert(stmtBlock.type() == ASTNodeType::STATEMENT_BLOCK);

    for (const auto &stmt : stmtBlock.children()) {
        if (currScope_->hasReturned) break;

        switch (stmt.type()) {
            case ASTNodeType::IF_STATEMENT:
                compileIfStatement(stmt);
                break;
            case ASTNodeType::VARIABLE_DECLARATION:
                compileVariableDeclaration(stmt, false);
                break;
            case ASTNodeType::ASSIGNMENT:
                compileAssignment(stmt);
                break;
            case ASTNodeType::RETURN_STMT:
                compileReturn(stmt);
                break;
            default: 
                emit(Instruction::NOOP);
                break;
        }
    }
}

void Compiler::compileFunctionCall(const ASTNode &funCall, bool shouldReturnVal) {
    assert(funCall.type() == ASTNodeType::FUNCTION_CALL);

    const auto &funNameNode = funCall.child(0);
    std::string funName = funNameNode.token()->val;
    if (isScriptFunction(funName)) {
        DWord idx = currScript_->getFunctionIdx(funName);
        const Function *fun = currScript_->getFunction(idx);

        if (shouldReturnVal && fun->returnTy == ValueType::VOID) {
            createCompileError(FUNCTION_HAS_VOID_RET_TY.format(funName), funNameNode);
            return;
        }

        ASTNode argsNode = funCall.child(1);
        compileArguments(argsNode, fun);

        emit(Instruction::CALLSCRFUN);
        emit(idx);
    }
    // Try finding it in globally registered functions
    else {
        DWord idx = ctx_->getGlobalFunctionIdx(funName);
        if (idx == NCSC_INVALID_IDX) {
            createCompileError(CANT_FIND_FUNCTION_NAMED.format(funName), funNameNode);
            return;
        }

        const Function *fun = ctx_->getGlobalFunction(idx);

        if (shouldReturnVal && fun->returnTy == ValueType::VOID) {
            createCompileError(FUNCTION_HAS_VOID_RET_TY.format(funName), funNameNode);
            return;
        }

        ASTNode argsNode = funCall.child(1);
        compileArguments(argsNode, fun);

        emit(Instruction::CLGLBLCPPFUN);
        emit(idx);
    }
}

void Compiler::compileReturn(const ASTNode &ret) {
    assert(ret.type() == ASTNodeType::RETURN_STMT);

    ValueType funRetTy = currFunction_->returnTy;
    if (ret.hasChildren()) {
        ASTNode expr = ret.child(0);
        if (funRetTy == ValueType::VOID) {
            createCompileError(FUNCTION_SHOULDNT_RET_VAL.format(currFunction_->name), ret);
            return;
        }

        compileExpression(expr, funRetTy);
        emit(Instruction::RET);
    } else {
        if (funRetTy != ValueType::VOID) {
            createCompileError(FUNCTION_SHOULD_RET_VAL.format(currFunction_->name), ret);
            return;
        }

        emit(Instruction::RETVOID);
    }

    currScope_->hasReturned = true;
}

void Compiler::compileVariableAccess(const ASTNode &varAccess, ValueType expectedType) {
    assert(varAccess.type() == ASTNodeType::IDENTIFIER);

    bool wantRef = hasMask(expectedType, ValueType::REF_MASK);
    expectedType = clearMask(expectedType, ValueType::REF_MASK);

    std::string varAccessName = varAccess.token()->val;
    SymbolSearchRes sres = searchSymbol(varAccessName);

    if (sres.ty == SymbolSearchRes::INVALID) {
        createCompileError(CANT_FIND_VAR_NAMED.format(varAccess.token()->val), varAccess);
        return;
    }

    if (sres.ty != SymbolSearchRes::LOCAL_VAR 
     && sres.ty != SymbolSearchRes::GLOBAL_VAR 
     && sres.ty != SymbolSearchRes::MEMBER_VAR) {
        createCompileError(S_IS_NOT_A_VAR.format(varAccessName), varAccess);
        return;
    }

    DWord idx = sres.idx;
    ValueType varType = sres.var->type;
    // If expected type is INVALID, the expression this variable access is in can be of any type
    // so type checking is useless
    if (expectedType != ValueType::INVALID && !canPromoteType(varType, expectedType)) {
        createCompileError(CANT_PROMOTE_TY_TO.format(valueTypeToString(varType), valueTypeToString(expectedType)), varAccess);
        return;
    }

    if (sres.ty == SymbolSearchRes::LOCAL_VAR)       
        emit(wantRef ? Instruction::LOADLOCAL_REF : Instruction::LOADLOCAL);
    else if (sres.ty == SymbolSearchRes::GLOBAL_VAR) 
        emit(wantRef ? Instruction::LOADGLOBAL_REF : Instruction::LOADGLOBAL);
    else if (sres.ty == SymbolSearchRes::MEMBER_VAR) {
        emit(Instruction::LOADLOCAL);
        emit((DWord)0); // 'this'

        emit(wantRef ? Instruction::LOADMEMBER_REF : Instruction::LOADMEMBER);
    }
    else return;

    emit(idx);
}

bool Compiler::compileArguments(const ASTNode &argsNode, const Function *fun) {
    assert(argsNode.type() == ASTNodeType::ARGUMENT_LIST);

    size_t argsNum = argsNode.numChildren();
    if (argsNum != fun->numParams) {
        createCompileError(EXPECTED_NUM_ARGS_INSTEAD_GOT.format(fun->numParams, fun->name, argsNum), argsNode);
        return false;
    }

    if (argsNum > 0) {
        for (int i = 0; i < argsNum; i++)
            compileExpression(argsNode.child(i), fun->paramTypes[i]);
    }

    return true;
}

void Compiler::compileIfStatement(const ASTNode &ifStmt, int nestedCount) {
    assert(ifStmt.type() == ASTNodeType::IF_STATEMENT);

    bool hasElse = ifStmt.numChildren() > 2;

    // No expected type
    compileExpression(ifStmt.child(0), ValueType::BOOL);
    
    QWord ifLabelNum = tmpLabelNum_++; 
    emit(Instruction::JMPFALSE);
    emit(ifLabelNum);

    enterNewScope();
    compileStatementBlock(ifStmt.child(1));
    exitScope();

    if (hasElse) {
        emit(Instruction::JMP);
        QWord elseLabelNum = tmpLabelNum_++; 
        emit(elseLabelNum);

        // Make the if's JMPFALSE jumps over the else's JUMP
        emit(Instruction::LABEL);
        emit(ifLabelNum);
        
        const ASTNode &elseBrChild = ifStmt.child(2).child(0);
        if (elseBrChild.type() == ASTNodeType::IF_STATEMENT) {
            compileIfStatement(elseBrChild, nestedCount + 1);
        } else if (elseBrChild.type() == ASTNodeType::STATEMENT_BLOCK) {
            enterNewScope();
            compileStatementBlock(elseBrChild);
            exitScope();
        }

        emit(Instruction::LABEL);
        emit(elseLabelNum);
    } else {
        emit(Instruction::LABEL);
        emit(ifLabelNum);
    }
}

void Compiler::compileJmpBcPatch(size_t patchLoc, Instruction jmpInstr, size_t jmpLoc) {
    std::vector<Byte> operandBytes(sizeof(QWord), 0);
    makeBytes(static_cast<QWord>(jmpLoc), operandBytes, 0);
    patchBytecode(patchLoc, jmpInstr, operandBytes);
}

void Compiler::compileAssignment(const ASTNode &assignment, ValueType expectedType) {
    assert(assignment.type() == ASTNodeType::ASSIGNMENT);

    if (assignment.numChildren() == 0)
        return;

    // A simple statement like a function call
    if (assignment.numChildren() == 1) {
        compileExpressionTerm(assignment.child(0), ValueType::VOID);
        return;
    }

    const ASTNode &varTerm = assignment.child(0);

    // Load a reference to the variable
    compileExpressionTerm(varTerm, setMask(expectedType, ValueType::REF_MASK));

    // Compile expression
    compileExpression(assignment.child(2), expectedType);
    
    TokenType opTokTy = assignment.child(1).token()->type;
    // If its an =, the variable doesn't need to be on the stack for the expression
    if (opTokTy != TokenType::EQUAL) {
        // We need two references on the stack, one for getting 
        // the value, one for setting it
        emit(Instruction::DUP);
        emit(Instruction::LOADREF);
    }

    switch (opTokTy) {
        case TokenType::PLUS_EQUAL:  emit(Instruction::ADD); break;
        case TokenType::MINUS_EQUAL: emit(Instruction::SUB); break;
        case TokenType::STAR_EQUAL:  emit(Instruction::MUL); break;
        case TokenType::SLASH_EQUAL: emit(Instruction::DIV); break;
    }

    emit(Instruction::SETREF);
}

#define CHECK_OPERAND_IS_MODIFIABLE_AND_NUMERIC()                                                       \
    auto sres = searchSymbol(operand.child(0).token()->val);                                      \
    if (sres.ty != SymbolSearchRes::LOCAL_VAR && sres.ty != SymbolSearchRes::GLOBAL_VAR                 \
        && !hasMask(sres.foundType, ValueType::REF_MASK)) {                                             \
        createCompileError(EXP_MODIFIABLE_VALUE, operand);                                              \
        return;                                                                                         \
    }                                                                                                   \
    if (!isNumeric(clearMask(sres.foundType, ValueType::REF_MASK))) {                                   \
        createCompileError(EXPECTED_NUMERIC_TYPE.format(valueTypeToString(sres.foundType)), operand);   \
        return;                                                                                         \
    }                                                                                                   \

void Compiler::compileExpressionPreOp(const ASTNode &preOp, const ASTNode &operand, ValueType expectedTy) {
    assert(preOp.type() == ASTNodeType::EXPRESSION_PREOP);

    TokenType preopTy = preOp.token()->type;
    switch (preopTy) {
        case TokenType::PLUS_PLUS: {
            CHECK_OPERAND_IS_MODIFIABLE_AND_NUMERIC()

            compileExpressionValue(operand, setMask(expectedTy, ValueType::REF_MASK));
            // One reference for getting the value, one for setting it
            emit(Instruction::DUP);
            // Get the value of the operand
            emit(Instruction::LOADREF);
            // Increment it
            emit(Instruction::INC);
            // Set the value of the operand
            emit(Instruction::SETREF);

            // For a pre-inc, the updated value should be on the stack at the end of the operation
            compileExpressionValue(operand, expectedTy);

            break;
        }
        case TokenType::MINUS_MINUS: {
            CHECK_OPERAND_IS_MODIFIABLE_AND_NUMERIC()

            compileExpressionValue(operand, setMask(expectedTy, ValueType::REF_MASK));
            
            emit(Instruction::DUP);
            emit(Instruction::LOADREF);
            emit(Instruction::DEC);
            emit(Instruction::SETREF);
            // Same reason as the pre-inc
            compileExpressionValue(operand, expectedTy);

            break;
        }

        case TokenType::NOT: {
            if (expectedTy != ValueType::BOOL) {
                createCompileError(EXPECTED_A_BOOLEAN.format(valueTypeToString(expectedTy)), operand);
                return;
            }

            compileExpressionValue(operand, expectedTy);
            emit(Instruction::NOT);

            break;
        }

        default: break;
    }
}

void Compiler::compileExpressionPostOp(const ASTNode &postOp, const ASTNode &operand, ValueType expectedTy) {
    assert(postOp.type() == ASTNodeType::EXPRESSION_POSTOP);

    TokenType postOpTy = postOp.token()->type;
    switch (postOpTy) {
        case TokenType::PLUS_PLUS: {
            CHECK_OPERAND_IS_MODIFIABLE_AND_NUMERIC()

            // Here the last value on the stack is the value before the increment
            compileExpressionValue(operand, expectedTy);
            compileExpressionValue(operand, setMask(expectedTy, ValueType::REF_MASK));
            emit(Instruction::DUP);
            emit(Instruction::LOADREF);
            emit(Instruction::INC);
            emit(Instruction::SETREF);

            break;
        }
        case TokenType::MINUS_MINUS: {
            CHECK_OPERAND_IS_MODIFIABLE_AND_NUMERIC()

            compileExpressionValue(operand, expectedTy);
            compileExpressionValue(operand, setMask(expectedTy, ValueType::REF_MASK));
            emit(Instruction::DUP);
            emit(Instruction::LOADREF);
            emit(Instruction::DEC);
            emit(Instruction::SETREF);

            break;
        }

        // Member access
        case TokenType::ID: {
            compileExpressionValue(operand, expectedTy);
            break;
        }
        
        default: break;
    }
}
#undef CHECK_OPERAND_IS_MODIFIABLE_AND_NUMERIC
    
void Compiler::compileExpressionValue(const ASTNode &exprVal, ValueType expectedTy) {
    assert(exprVal.type() == ASTNodeType::EXPRESSION_VALUE);

    ASTNode firstChild = exprVal.child(0);

    bool isConst = firstChild.type() == ASTNodeType::CONSTANT;
    bool isFnCall = firstChild.type() == ASTNodeType::FUNCTION_CALL;
    bool wantRef = hasMask(expectedTy, ValueType::REF_MASK);

    if (isConst && wantRef) {
        createCompileError(EXP_MODIFIABLE_VALUE, firstChild);
        return;
    }

    if (isConst)
        compileConstantPush(firstChild, expectedTy);
    else if (isFnCall)
        // Expect a return only if the expected type isn't void
        compileFunctionCall(firstChild, clearMask(expectedTy, ValueType::REF_MASK) != ValueType::VOID);
    else if (firstChild.type() == ASTNodeType::IDENTIFIER)
        compileVariableAccess(firstChild, expectedTy);
    else if (firstChild.type() == ASTNodeType::CONSTRUCT_CALL)
        true; // TODO
}

} // namespace NCSC
