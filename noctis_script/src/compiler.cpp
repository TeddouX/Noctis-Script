// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/compiler.hpp>
#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>
#include <ncsc/optimizer.hpp>

#include <sstream>
#include <format>
#include <iomanip>
// #include <print>

namespace NCSC
{

template <typename T>
static constexpr T getInstrOperand(const std::vector<Byte>& bc, size_t& i) {
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

    ScriptNode root = parser.parseAll();
    if (parser.hasErrors()) {
        std::vector<Error> parserErrors = parser.getErrors();
        compileErrors_.insert(compileErrors_.end(), parserErrors.begin(), parserErrors.end()); 
    
        return nullptr;
    }

    return compileScript(root);
}

std::unique_ptr<Script> Compiler::compileScript(const ScriptNode &root) {
    auto script = std::make_unique<Script>();
    script->ctx = ctx_;

    currScript_ = script.get();

    for (const auto &node : root.getAllChildren()) {
        if (node.getType() == ScriptNodeType::FUNCTION) {
            enterNewScope();
            compileFunction(node);
            exitScope();

            scopes_.clear();
        }
        else if (node.getType() == ScriptNodeType::VARIABLE_DECLARATION) {
            enterNewScope();
            compileVariableDeclaration(node, true);
            exitScope();

#if NCSC_ALWAYS_OPTIMIZE
            Optimizer optimizer(tempCompiledBytecode_);
            tempCompiledBytecode_ = optimizer.optimizeAll();
#endif

            resolveJumps();

            GlobalVar *gv = currScript_->getGlobalVar(currScript_->numGlobalVariables - 1); 
            gv->bytecode = tempCompiledBytecode_;
            gv->requiredStackSize = computeRequiredStackSize();

            tempCompiledBytecode_.clear();
            scopes_.clear();
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

void Compiler::createCompileError(const ErrInfo &info, const ScriptNode &node) {
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

void Compiler::compileFunction(const ScriptNode &funcDecl) {
    assert(funcDecl.getType() == ScriptNodeType::FUNCTION);

    std::string funcDeclName = funcDecl.getChild(1).getToken()->val;
    if (currScript_->getFunction(funcDeclName)) {
        createCompileError(FUNC_ALREADY_EXISTS.format(funcDeclName), funcDecl.getChild(1));
        return;
    }

    auto fun = std::make_shared<ScriptFunction>();
    currFunction_ = fun.get();
    
    ScriptNode retType = funcDecl.getChild(0); 
    if (retType.getType() == ScriptNodeType::DATA_TYPE)
        currFunction_->returnTy = valueTypeFromTok(*retType.getToken());
    else
        currFunction_->returnTy = ValueType::VOID;
    
    currFunction_->name = funcDeclName;

    ScriptNode paramsNode = funcDecl.getChild(2);
    currFunction_->numParams = paramsNode.getNumChildren() / 2;
    currFunction_->numLocals = currFunction_->numParams;
    for (int i = 0; i < paramsNode.getNumChildren(); i++) {
        ValueType ty = valueTypeFromTok(*paramsNode.getChild(i).getToken());
        
        // Arguments are treated as local variables
        currScope_->addLocalVar(paramsNode.getChild(++i).getToken()->val, ty);
        currFunction_->paramTypes.push_back(ty);   
    }

    const ScriptNode& statementBlock = funcDecl.getLastChild();
    compileStatementBlock(statementBlock);

    currFunction_->numLocals += computeMaxLocals(&scopes_.back());

    if (!hasErrors()) {
        // For void functions, add a return at the end if none currently exists
        if (currFunction_->returnTy == ValueType::VOID && !currScope_->hasReturned) {
            emit(Instruction::RETVOID);
        }
        // For non void functions, error if there is no return at the end
        else if (currFunction_->returnTy != ValueType::VOID && !currScope_->hasReturned) {
            createCompileError(FUNCTION_SHOULD_RET_VAL.format(currFunction_->name), statementBlock.getLastChild());
            return;
        }

#if NCSC_ALWAYS_OPTIMIZE
        Optimizer optimizer(tempCompiledBytecode_);
        tempCompiledBytecode_ = optimizer.optimizeAll();
#endif

        resolveJumps();

        currFunction_->requiredStackSize = computeRequiredStackSize();
        currFunction_->bytecode = tempCompiledBytecode_;
        tempCompiledBytecode_.clear();
    }

    currScript_->addFunction(*fun);

    currFunction_ = nullptr;
}

size_t Compiler::computeRequiredStackSize() {
    size_t maxSize = 0;
    size_t currSize = 0;
    for (size_t i = 0; i < tempCompiledBytecode_.size();) {
        Instruction instr = static_cast<Instruction>(tempCompiledBytecode_[i]);

        switch (instr) {
            case Instruction::LOADLOCAL: 
            case Instruction::LOADGLOBAL: 
            case Instruction::LOADOBJ: 
            case Instruction::PUSH: 
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
                currSize -= 2;
                break;
                
            case Instruction::STORELOCAL:
            case Instruction::STOREGLOBAL:
            case Instruction::SETOBJ:
                currSize--;
                break;

            case Instruction::CALLSCRFUN: {
                size_t idx = readWord<DWord>(tempCompiledBytecode_, i + 1);
                const ScriptFunction *scrFun = currScript_->getFunction(idx);
                currSize -= scrFun->numParams;

                break;
            }

            case Instruction::CLGLBLCPPFUN: {
                size_t idx = readWord<DWord>(tempCompiledBytecode_, i + 1);
                const auto *fun = ctx_->getGlobalFunction(idx);
                currSize -= fun->numParams;

                break;
            }

            default: break;
        }

        i += getInstructionSize(tempCompiledBytecode_, i);
    }

    return maxSize;
}

size_t Compiler::computeMaxLocals(const Scope *scope) {
    if (scope == nullptr)
        return 0;
    return std::max(scope->localVariables.size(), computeMaxLocals(scope->parent));
}

void Compiler::resolveJumps() {
    std::unordered_map<QWord, size_t> jmpLocations;

    for (QWord i = 0; i < tempCompiledBytecode_.size();) {
        Instruction instr = static_cast<Instruction>(tempCompiledBytecode_[i]);
        
        switch (instr) {
            case Instruction::JMP:
            case Instruction::JMPFALSE: {
                QWord jmpLabelNum = readWord<QWord>(tempCompiledBytecode_, i + sizeof(Instruction));
                jmpLocations.emplace(jmpLabelNum, i);

                i += sizeof(QWord) + sizeof(Instruction);
                break;
            }

            case Instruction::LABEL: {
                QWord labelNum = readWord<QWord>(tempCompiledBytecode_, i + sizeof(Instruction));
                size_t jmpIdx = jmpLocations.at(labelNum);
                Instruction jmpTy = static_cast<Instruction>(readWord<Byte>(tempCompiledBytecode_, jmpIdx));
                size_t operandIdx = jmpIdx + sizeof(Instruction);

                std::vector<Byte> operandBytes(sizeof(QWord), 0);
                makeBytes(i, operandBytes, 0);

                // Patch the jump to point to the current idx 
                std::copy(operandBytes.begin(), operandBytes.end(), tempCompiledBytecode_.begin() + operandIdx);
                // Remove the LABEL instruction
                constexpr size_t instrSize = sizeof(Instruction) + sizeof(QWord);
                tempCompiledBytecode_.erase(
                    tempCompiledBytecode_.begin() + i, 
                    tempCompiledBytecode_.begin() + i + instrSize);
                break;
            }

            default:
                i += getInstructionSize(tempCompiledBytecode_, i);
                break;
        }
    }
}

void Compiler::compileVariableDeclaration(const ScriptNode &varDecl, bool global) {
    assert(varDecl.getType() == ScriptNodeType::VARIABLE_DECLARATION);

    const ScriptNode &assignmentNode = varDecl.getChild(1); 
    const ScriptNode &firstExprTermVal = assignmentNode.getChild(0).getChild(0);
    if (firstExprTermVal.getType() != ScriptNodeType::IDENTIFIER) {
        createCompileError(EXPECTED_AN_ID, firstExprTermVal);
        return;
    }

    std::string varName = firstExprTermVal.getToken()->val;
    ValueType varType = valueTypeFromTok(*varDecl.getChild(0).getToken());
    if (global) {
        GlobalVar gv{
            .name = varName,
            .bytecode = {},
            .requiredStackSize = 0,
            .type = varType,
        };

        currScript_->addGlovalVar(gv);
    } else {
        currScope_->addLocalVar(varName, varType);
        // currFunction_->numLocals++;
    }

    compileAssignment(assignmentNode, varType);
    
    if (global) {
        // So the VM is able to stop
        emit(Instruction::RETVOID);
    }
}

void Compiler::compileExpression(const ScriptNode &expr, ValueType expectedType) {
    assert(expr.getType() == ScriptNodeType::EXPRESSION);

    if (expr.getNumChildren() == 1 && expr.getChild(0).getType() == ScriptNodeType::EXPRESSION_TERM) {
        compileExpressionTerm(expr.getChild(0), expectedType, false);
        return;
    }

    ScriptNode rootOp = expr.getChild(0);
    // Comparison ops will always be at the top of the binary tree
    // so if any of those is present, the expression returns a boolean
    TokenType rootOpTy = rootOp.getToken()->type;
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

void Compiler::recursivelyCompileExpression(const ScriptNode &exprChild, ValueType expectedType) {
    if (exprChild.getType() == ScriptNodeType::OP) {
        recursivelyCompileExpression(exprChild.getChild(0), expectedType);
        recursivelyCompileExpression(exprChild.getChild(1), expectedType);
        compileOperator(exprChild);
    } else if (exprChild.getType() == ScriptNodeType::EXPRESSION_TERM) {
        compileExpressionTerm(exprChild, expectedType, false);
    }
}

void Compiler::compileOperator(const ScriptNode &binop) {
    assert(binop.getType() == ScriptNodeType::OP);

    switch (binop.getToken()->type) {
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

void Compiler::compileConstantPush(const ScriptNode &constant, ValueType expectedType) {
    assert(constant.getType() == ScriptNodeType::CONSTANT);

    TokenType constTokTy = constant.getToken()->type; 
    const std::string &constTokVal = constant.getToken()->val;
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

void Compiler::compileExpressionTerm(const ScriptNode &exprTerm, ValueType expectedType, bool shouldBeModifiable) {
    assert(exprTerm.getType() == ScriptNodeType::EXPRESSION_TERM);

    if (exprTerm.getNumChildren() == 1) {
        ScriptNode firstChild = exprTerm.getChild(0);

        bool isConst = firstChild.getType() == ScriptNodeType::CONSTANT;
        bool isFnCall = firstChild.getType() == ScriptNodeType::FUNCTION_CALL;

        if ((isConst || isFnCall) && shouldBeModifiable) {
            createCompileError(TERM_SHOULD_BE_MODIFIABLE, firstChild);
            return;
        }

        if (isConst)
            compileConstantPush(firstChild, expectedType);
        else if (isFnCall)
            // Expect a return only if the expected type isn't void
            compileFunctionCall(firstChild, expectedType != ValueType::VOID);
        else if (firstChild.getType() == ScriptNodeType::IDENTIFIER)
            compileVariableAccess(firstChild, expectedType);
    }
}

void Compiler::compileStatementBlock(const ScriptNode &stmtBlock) {
    assert(stmtBlock.getType() == ScriptNodeType::STATEMENT_BLOCK);

    for (const auto &stmt : stmtBlock.getAllChildren()) {
        if (currScope_->hasReturned) break;

        switch (stmt.getType()) {
            case ScriptNodeType::IF_STATEMENT:
                compileIfStatement(stmt);
                break;
            case ScriptNodeType::VARIABLE_DECLARATION:
                compileVariableDeclaration(stmt, false);
                break;
            case ScriptNodeType::SIMPLE_STATEMENT:
                if (stmt.hasChildren())
                    compileAssignment(stmt.getChild(0), ValueType::INVALID);
                break;
            case ScriptNodeType::RETURN_STMT:
                compileReturn(stmt);
                break;
            default: 
                emit(Instruction::NOOP);
                break;
        }
    }
}

void Compiler::compileFunctionCall(const ScriptNode &funCall, bool shouldReturnVal) {
    assert(funCall.getType() == ScriptNodeType::FUNCTION_CALL);

    const auto &funNameNode = funCall.getChild(0);
    std::string funName = funNameNode.getToken()->val;
    if (isScriptFunction(funName)) {
        DWord idx = currScript_->getFunctionIdx(funName);
        const IFunction *fun = currScript_->getFunction(idx);

        if (shouldReturnVal && fun->returnTy == ValueType::VOID) {
            createCompileError(FUNCTION_HAS_VOID_RET_TY.format(funName), funNameNode);
            return;
        }

        ScriptNode argsNode = funCall.getChild(1);
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

        const IFunction *fun = ctx_->getGlobalFunction(idx);

        if (shouldReturnVal && fun->returnTy == ValueType::VOID) {
            createCompileError(FUNCTION_HAS_VOID_RET_TY.format(funName), funNameNode);
            return;
        }

        ScriptNode argsNode = funCall.getChild(1);
        compileArguments(argsNode, fun);

        emit(Instruction::CLGLBLCPPFUN);
        emit(idx);
    }
}

void Compiler::compileReturn(const ScriptNode &ret) {
    assert(ret.getType() == ScriptNodeType::RETURN_STMT);

    ValueType funRetTy = currFunction_->returnTy;
    if (ret.hasChildren()) {
        ScriptNode expr = ret.getChild(0);
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

void Compiler::compileVariableAccess(const ScriptNode &varAccess, ValueType expectedType) {
    assert(varAccess.getType() == ScriptNodeType::IDENTIFIER);

    std::string varAccessName = varAccess.getToken()->val;
    Scope::Var *var = currScope_->getLocalVar(varAccessName); 
    if (var) {
        DWord idx = currScope_->getLocalVarIdx(varAccessName);
        ValueType varType = var->type;
        // If expected type is INVALID, the expression this variable access' in can be of any type
        // so type checking is useless
        if (expectedType != ValueType::INVALID && !canPromoteType(varType, expectedType)) {
            createCompileError(CANT_PROMOTE_TY_TO.format(valueTypeToString(varType), valueTypeToString(expectedType)), varAccess);
            return;
        }
 
        emit(Instruction::LOADLOCAL);
        emit(idx);
    } else {
        // Search in globals
        if (!currScript_->hasGlobalVar(varAccessName)) {
            createCompileError(CANT_FIND_VAR_NAMED.format(varAccess.getToken()->val), varAccess);
            return;
        }
        
        DWord globalIdx = currScript_->getGlobalVarIdx(varAccess.getToken()->val);
        ValueType varType = currScript_->getGlobalVar(globalIdx)->type; 
        if (expectedType != ValueType::INVALID && !canPromoteType(varType, expectedType)) {
            createCompileError(CANT_PROMOTE_TY_TO.format(valueTypeToString(varType), valueTypeToString(expectedType)), varAccess);
            return;
        }

        emit(Instruction::LOADGLOBAL);
        emit(globalIdx);
    }
}

bool Compiler::compileArguments(const ScriptNode &argsNode, const IFunction *fun) {
    assert(argsNode.getType() == ScriptNodeType::ARGUMENT_LIST);

    size_t argsNum = argsNode.getNumChildren();
    if (argsNum != fun->numParams) {
        createCompileError(EXPECTED_NUM_ARGS_INSTEAD_GOT.format(fun->numParams, fun->name, argsNum), argsNode);
        return false;
    }

    if (argsNum > 0) {
        for (int i = 0; i < argsNum; i++)
            compileExpression(argsNode.getChild(i), fun->paramTypes[i]);
    }

    return true;
}

void Compiler::compileIfStatement(const ScriptNode &ifStmt, int nestedCount) {
    assert(ifStmt.getType() == ScriptNodeType::IF_STATEMENT);

    constexpr size_t instrSize = sizeof(Instruction) + sizeof(QWord);
    bool hasElse = ifStmt.getNumChildren() > 2;

    // No expected type
    compileExpression(ifStmt.getChild(0), ValueType::BOOL);
    emit(Instruction::JMPFALSE);
    emit(++tmpLabelNum_);

    enterNewScope();
    compileStatementBlock(ifStmt.getChild(1));
    exitScope();

    size_t jmpFalseOperand = getLastByteInsertedLoc() + instrSize + 1;
    // Jump over all the if's block's jumps
    if (hasElse) jmpFalseOperand += instrSize * nestedCount;
    
    emit(Instruction::LABEL);
    emit(tmpLabelNum_);
    
    if (hasElse) {
        emit(Instruction::JMP);
        emit(++tmpLabelNum_);
        
        const ScriptNode &elseBrChild = ifStmt.getChild(2).getChild(0);
        if (elseBrChild.getType() == ScriptNodeType::IF_STATEMENT) {
            compileIfStatement(elseBrChild, nestedCount + 1);
        } else if (elseBrChild.getType() == ScriptNodeType::STATEMENT_BLOCK) {
            enterNewScope();
            compileStatementBlock(elseBrChild);
            exitScope();
        }

        emit(Instruction::LABEL);
        emit(tmpLabelNum_);
    }
}

void Compiler::compileJmpBcPatch(size_t patchLoc, Instruction jmpInstr, size_t jmpLoc) {
    std::vector<Byte> operandBytes(sizeof(QWord), 0);
    makeBytes(static_cast<QWord>(jmpLoc), operandBytes, 0);
    patchBytecode(patchLoc, jmpInstr, operandBytes);
}

void Compiler::compileAssignment(const ScriptNode &assignment, ValueType expectedType) {
    assert(assignment.getType() == ScriptNodeType::ASSIGNMENT);

    // A simple statement like a function call
    if (assignment.getNumChildren() == 1) {
        compileExpressionTerm(assignment.getChild(0), ValueType::VOID);
        return;
    }
    
    const ScriptNode &exprTerm = assignment.getChild(0); 

    TokenType opTokTy = assignment.getChild(1).getToken()->type;
    // If its an =, the variable doesn't need to be on the stack for the expression
    if (opTokTy != TokenType::EQUAL)
        compileExpressionTerm(exprTerm, expectedType, true);
    
    compileExpression(assignment.getChild(2), getExpressionTermType(exprTerm));

    // Compile it a second time to set its value
    compileExpressionTerm(exprTerm, expectedType, true);
    // Only needed in the object register, not on the stack
    emit(Instruction::POP);

    switch (opTokTy) {
        case TokenType::PLUS_EQUAL:  emit(Instruction::ADD); break;
        case TokenType::MINUS_EQUAL: emit(Instruction::SUB); break;
        case TokenType::STAR_EQUAL:  emit(Instruction::MUL); break;
        case TokenType::SLASH_EQUAL: emit(Instruction::DIV); break;
    }

    emit(Instruction::SETOBJ);
    // The object is in the object register and we don't need it on the stack
    // TOOD: Handle objects that only need to be loaded in the register differently
}

ValueType Compiler::getExpressionTermType(const ScriptNode &exprTerm) {
    assert(exprTerm.getType() == ScriptNodeType::EXPRESSION_TERM);
    
    const ScriptNode &firstChild = exprTerm.getChild(0);
    if (firstChild.getType() == ScriptNodeType::IDENTIFIER) {
        std::string name = firstChild.getToken()->val;
        Scope::Var *localVar = currScope_->getLocalVar(name); 
        if (localVar) {
            return localVar->type;
        } else if (currScript_->hasGlobalVar(name)) {
            return currScript_->getGlobalVar(currScript_->getGlobalVarIdx(name))->type;
        }
    } 
    else if (firstChild.getType() == ScriptNodeType::CONSTANT) {
        return firstChild.getToken()->type == TokenType::INT_CONSTANT ? ValueType::INT64 : ValueType::FLOAT64;
    }

    return ValueType{};
}

} // namespace NCSC
