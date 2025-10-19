#include <ncsc/compiler.hpp>
#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>

#include <sstream>
#include <format>
#include <iomanip>

namespace NCSC
{

template <typename T>
static constexpr T getInstrOperand(const std::vector<Byte>& bc, size_t& i) {
    T val = readWord<T>(bc.data(), i);
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
            Value val = Value::fromBytes(bc.data(), i, size);
            
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

std::unique_ptr<Script> Compiler::compileScript(const std::string &code) {
    std::vector<Token> tokens = Lexer(code).tokenizeAll();
    Parser parser(tokens);
    
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
    currScript_ = script.get();

    for (auto &node : root.children) {
        if (node.type == ScriptNodeType::FUNCTION)
            compileFunction(node);
        else if (node.type == ScriptNodeType::VARIABLE_DECLARATION) {
            expectedExpressionType_ = valueTypeFromTok(*node.children[0].token);
            compileVariableDeclaration(node, true);
            // So the VM is able to stop
            emit(Instruction::RETVOID);

            GlobalVar gv {
                .name = node.children[1].token->val,
                .bytecode = tempCompiledBytecode_,
                .requiredStackSize = computeMaxStackSize(node),
                .type = expectedExpressionType_,
            };

            currScript_->addGlovalVar(gv);
            tempCompiledBytecode_.clear();
        }
    }

    currScript_ = nullptr;
    return script;
}

void Compiler::error(const std::string &mess, const ScriptNode &node) {
    compileErrors_.push_back(Error(mess, node));
}

void Compiler::emit(Byte byte) {
    tempCompiledBytecode_.push_back(byte);
}

void Compiler::emit(Instruction instr) {
    emit(static_cast<Byte>(instr));
}

void Compiler::emit(Byte *bytecode, size_t size) {
    for (size_t i = 0; i < size; i++) 
        emit(bytecode[i]);
}

void Compiler::emit(Word dw) {
    Byte bytes[] {
        static_cast<Byte>(dw & 0xFF),
        static_cast<Byte>((dw >> 8) & 0xFF),
    };
    emit(bytes, sizeof(bytes));
}

void Compiler::emit(DWord dw) {
    Byte bytes[] {
        static_cast<Byte>(dw & 0xFF),
        static_cast<Byte>((dw >> 8) & 0xFF),
        static_cast<Byte>((dw >> 16) & 0xFF),
        static_cast<Byte>((dw >> 24) & 0xFF),
    };
    emit(bytes, sizeof(bytes));
}

void Compiler::compileFunction(const ScriptNode &funcDecl) {
    assert(funcDecl.type == ScriptNodeType::FUNCTION);

    auto fun = std::make_shared<Function>();
    currFunction_ = fun.get();
    
    ScriptNode retType = funcDecl.children[0]; 
    if (retType.type == ScriptNodeType::DATA_TYPE)
        currFunction_->returnType = valueTypeFromTok(*retType.token);
    else
        currFunction_->returnType = ValueType::VOID;
    
    currFunction_->name = funcDecl.children[1].token->val;
    currFunction_->requiredStackSize = computeMaxStackSize(funcDecl);

    ScriptNode paramsNode = funcDecl.children[2];
    currFunction_->numParams = paramsNode.children.size() / 2;
    currFunction_->numLocals = currFunction_->numParams;
    for (int i = 0; i < paramsNode.children.size(); i++) {
        ValueType ty = valueTypeFromTok(*paramsNode.children[i].token);
        
        // Arguments are treated as local variables
        localVariables_.push_back(LocalVar(paramsNode.children[++i].token->val, ty));
        currFunction_->paramTypes.push_back(ty);   
    }

    const ScriptNode& statementBlock = funcDecl.children.back();
    for (auto &node : statementBlock.children) {
        switch (node.type) {
            case ScriptNodeType::VARIABLE_DECLARATION:
                compileVariableDeclaration(node, false);
                break;
            case ScriptNodeType::SIMPLE_STATEMENT:
                compileSimpleStatement(node);
                break;
            default:
                emit(Instruction::NOOP);
                break;
        }
    }

    if (!hasErrors()) {
        // For void functions, add a return at the end if none currently exists
        if (currFunction_->returnType == ValueType::VOID 
         && tempCompiledBytecode_.back() != static_cast<Byte>(Instruction::RETVOID)) 
        {
            emit(Instruction::RETVOID);
        }
        // For non void functions, error if there is no return at the end
        else if (currFunction_->returnType == ValueType::VOID 
              && tempCompiledBytecode_.back() != static_cast<Byte>(Instruction::RET)) 
        {
            error(std::format(FUNCTION_SHOULD_RET_VAL, currFunction_->name), funcDecl.children.back());
            return;
        }

        currFunction_->bytecode = tempCompiledBytecode_;
        tempCompiledBytecode_.clear();
    }

    currScript_->addFunction(*fun);

    currFunction_ = nullptr;
    localVariables_.clear();
}

size_t Compiler::computeMaxStackSize(const ScriptNode &node) {
    switch (node.type) {
        case ScriptNodeType::FUNCTION: {
            const ScriptNode &stmtBlock = node.children.back();
            assert(stmtBlock.type == ScriptNodeType::STATEMENT_BLOCK);
            size_t requiredStackSize = 0;
            for (const auto &stmt : stmtBlock.children) {
                requiredStackSize = std::max(requiredStackSize, computeMaxStackSize(stmt));
            }

            return requiredStackSize;
        }

        case ScriptNodeType::VARIABLE_DECLARATION: {
            if (node.children.size() == 2)
                return 1;
            else
                // Compute expression size
                return computeMaxStackSize(node.children[2]);   
        }

        case ScriptNodeType::EXPRESSION:
        case ScriptNodeType::EXPRESSION_TERM: {
            return computeMaxStackSize(node.children[0]);
        }
    
        case ScriptNodeType::BINOP: {
            size_t lhs = computeMaxStackSize(node.children[0]);
            size_t rhs = computeMaxStackSize(node.children[1]);

            // Result occupies one slot
            return std::max(lhs, rhs + 1);
        }

        case ScriptNodeType::SIMPLE_STATEMENT:
        case ScriptNodeType::RETURN: {
            if (node.hasChildren())
                return computeMaxStackSize(node.children[0]);
            return 0;
        }
        
        case ScriptNodeType::FUNCTION_CALL: {
            size_t requiredStackSize = 0;
            for (const auto &expr : node.children[1].children) {
                requiredStackSize += computeMaxStackSize(expr);
            }
            return requiredStackSize;
        }
            

        case ScriptNodeType::CONSTANT:
        // Variable access
        case ScriptNodeType::IDENTIFIER:
            return 1;
    
        default:
            return 0;
    }
}

void Compiler::compileVariableDeclaration(const ScriptNode &varDecl, bool global) {
    assert(varDecl.type == ScriptNodeType::VARIABLE_DECLARATION);

    ValueType varType = valueTypeFromTok(*varDecl.children[0].token);
    if (varDecl.children.size() == 2) {
        if (isPrimitive(varType)) {
            // Get default value
        } else {
            // do whatever
        }
    } else {
        expectedExpressionType_ = varType;

        // Compile the expression
        // The result will be stored on the top of the stack
        compileExpression(varDecl.children[2]);
    }

    if (global) {
        emit(Instruction::STOREGLOBAL);
        emit(currScript_->numGlobalVariables);

        currScript_->numGlobalVariables++;
    } else {
        emit(Instruction::STORELOCAL); 
        emit(currFunction_->numLocals);

        currFunction_->numLocals++;
        localVariables_.push_back(LocalVar(varDecl.children[1].token->val, varType));
    }
}

void Compiler::compileExpression(const ScriptNode &expr) {
    assert(expr.type == ScriptNodeType::EXPRESSION);

    if (expr.children.size() == 1 && expr.children[0].type == ScriptNodeType::EXPRESSION_TERM) {
        compileExpressionTerm(expr.children[0]);
        return;
    }

    ScriptNode rootOp = expr.children[0]; 
    recursivelyCompileExpression(rootOp);
}

void Compiler::recursivelyCompileExpression(const ScriptNode &exprChild) {
    if (exprChild.type == ScriptNodeType::BINOP) {
        recursivelyCompileExpression(exprChild.children[0]);
        recursivelyCompileExpression(exprChild.children[1]);
        compileOperator(exprChild);
    } else if (exprChild.type == ScriptNodeType::EXPRESSION_TERM) {
        compileExpressionTerm(exprChild);
    }
}

void Compiler::compileOperator(const ScriptNode &binop) {
    assert(binop.type == ScriptNodeType::BINOP);

    switch (binop.token->type) {
        case TokenType::PLUS:  emit(Instruction::ADD);  return;
        case TokenType::MINUS: emit(Instruction::SUB);  return;
        case TokenType::STAR:  emit(Instruction::MUL);  return;
        case TokenType::SLASH: emit(Instruction::DIV);  return;
        default:               emit(Instruction::NOOP); return;
    }
}

void Compiler::compileConstantPush(const ScriptNode &constant) {
    assert(constant.type == ScriptNodeType::CONSTANT);

    TokenType constTokTy = constant.token->type; 
    const std::string &constTokVal = constant.token->val;
    if (constTokTy == TokenType::FLOAT_CONSTANT) {
        switch(expectedExpressionType_) {
            case ValueType::FLOAT32: {
                float32_t val = std::stof(constTokVal);
                Byte bytes[getValueSize(val)];
                makeValueBytes(std::bit_cast<uint32_t>(val), ValueType::FLOAT32, bytes, 0);
                emit(Instruction::PUSH);
                emit(bytes, sizeof(bytes));
                break;
            }
            case ValueType::FLOAT64: {
                float64_t val = std::stod(constTokVal);
                Byte bytes[getValueSize(val)];
                makeValueBytes(std::bit_cast<uint64_t>(val), ValueType::FLOAT64, bytes, 0);
                emit(Instruction::PUSH);
                emit(bytes, sizeof(bytes));
                break;
            }
            default:
                error(std::format(EXPECTED_NON_FLOATING_POINT, constTokVal), constant);
                return;
        }
    } else if (constTokTy == TokenType::INT_CONSTANT) {
        switch(expectedExpressionType_) {
            // Bools are ints
            case ValueType::BOOL:   emitIntConstant<int8_t>(constTokVal, constant, ValueType::BOOL);   break;
            
            case ValueType::INT8:   emitIntConstant<int8_t>(constTokVal, constant, ValueType::INT8);   break;
            case ValueType::INT16:  emitIntConstant<int16_t>(constTokVal, constant, ValueType::INT16); break;
            case ValueType::INT32:  emitIntConstant<int32_t>(constTokVal, constant, ValueType::INT32); break;
            case ValueType::INT64:  emitIntConstant<int64_t>(constTokVal, constant, ValueType::INT64); break;

            case ValueType::UINT8:  emitIntConstant<uint8_t>(constTokVal, constant, ValueType::UINT8);   break;
            case ValueType::UINT16: emitIntConstant<uint16_t>(constTokVal, constant, ValueType::UINT16); break;
            case ValueType::UINT32: emitIntConstant<uint32_t>(constTokVal, constant, ValueType::UINT32); break;
            case ValueType::UINT64: emitIntConstant<uint64_t>(constTokVal, constant, ValueType::UINT64); break;
            
            // If a float is expected for the expression, we can safely emit an int
            // and the VM will be able to use it as a float
            case ValueType::FLOAT32: emitIntConstant<int32_t>(constTokVal, constant, ValueType::INT32); break;
            case ValueType::FLOAT64: emitIntConstant<int64_t>(constTokVal, constant, ValueType::INT64); break;

            default: assert(0 && "Wtf");
        }
    } else if (constTokTy == TokenType::TRUE_KWD || constTokTy == TokenType::FALSE_KWD) {
        if (expectedExpressionType_ != ValueType::BOOL) {
            error(std::format(CANT_PROMOTE_TY_TO, 
                VTYPE_NAMES.at(expectedExpressionType_), 
                VTYPE_NAMES.at(ValueType::BOOL)), constant);
            return;
        }
        
        if (constTokTy == TokenType::TRUE_KWD)
            emitIntConstant<int8_t>("1", constant, ValueType::BOOL);
        else 
            emitIntConstant<int8_t>("0", constant, ValueType::BOOL);
    }
}

void Compiler::compileExpressionTerm(const ScriptNode &exprTerm) {
    assert(exprTerm.type == ScriptNodeType::EXPRESSION_TERM);

    if (exprTerm.children.size() == 1) {
        ScriptNode firstChild = exprTerm.children[0];
        if (firstChild.type == ScriptNodeType::CONSTANT)
            compileConstantPush(firstChild);
        else if (firstChild.type == ScriptNodeType::FUNCTION_CALL)
            compileFunctionCall(firstChild, true);
        else if (firstChild.type == ScriptNodeType::IDENTIFIER)
            compileVariableAccess(firstChild);
    }
}

void Compiler::compileSimpleStatement(const ScriptNode &simpleStmt) {
    assert(simpleStmt.type == ScriptNodeType::SIMPLE_STATEMENT);

    auto stmt = simpleStmt.children[0]; 
    if (stmt.type == ScriptNodeType::FUNCTION_CALL)
        compileFunctionCall(stmt, false);
    else if (stmt.type == ScriptNodeType::RETURN)
        compileReturn(stmt);
}

void Compiler::compileFunctionCall(const ScriptNode &funCall, bool shouldReturnVal) {
    assert(funCall.type == ScriptNodeType::FUNCTION_CALL);

    const auto &funNameNode = funCall.children[0];
    std::string funName = funNameNode.token->val;
    if (isScriptFunction(funName)) {
        DWord idx = currScript_->getFunctionIdx(funName);
        const Function *fun = currScript_->getFunction(idx);

        if (shouldReturnVal && fun->returnType == ValueType::VOID) {
            error(std::format(FUNCTION_HAS_VOID_RET_TY, funName), funNameNode);
            return;
        }

        ScriptNode argsNode = funCall.children[1];
        if (argsNode.hasChildren()) {
            size_t argsNum = argsNode.children.size();
            if (argsNum != fun->numParams) {
                error(std::format(EXPECTED_NUM_ARGS_INSTEAD_GOT, fun->numParams, fun->name, argsNum), funNameNode);
                return;
            }

            for (int i = 0; i < argsNum; i++) {
                expectedExpressionType_ = fun->paramTypes[i];
                compileExpression(argsNode.children[i]);
            }
        }

        emit(Instruction::CALLSCRFUN);
        emit(idx);
    }
    else {
        error(std::format(CANT_FIND_FUNCTION_NAMED, funName), funNameNode);
        return;
    }
}

void Compiler::compileReturn(const ScriptNode &ret) {
    assert(ret.type == ScriptNodeType::RETURN);

    ScriptNode expr = ret.children[0];
    ValueType funRetTy = currFunction_->returnType;
    if (expr.children.empty()) {
        if (funRetTy != ValueType::VOID) {
            error(std::format(FUNCTION_SHOULD_RET_VAL, currFunction_->name), ret);
            return;
        }

        emit(Instruction::RETVOID);
    } else {
        if (funRetTy == ValueType::VOID) {
            error(std::format(FUNCTION_SHOULDNT_RET_VAL, currFunction_->name), ret);
            return;
        }

        expectedExpressionType_ = funRetTy;

        compileExpression(expr);
        emit(Instruction::RET);
    }
}

void Compiler::compileVariableAccess(const ScriptNode &varAccess) {
    assert(varAccess.type == ScriptNodeType::IDENTIFIER);

    bool found = false;
    Word idx = 0;
    for (Word i = 0; i < localVariables_.size(); i++) {
        const std::string &varName = localVariables_[i].name;

        if (varName == varAccess.token->val) {
            found = true;
            idx = i;
        }
    }

    if (found) {
        ValueType varType = localVariables_[idx].type;
        if (varType != expectedExpressionType_)
            compileDifferentValueTypePush(varType, expectedExpressionType_, varAccess);
 
        emit(Instruction::LOADLOCAL);
        emit(idx);
    } else {
        // Search in globals
        DWord globalIdx = currScript_->getGlobalVarIdx(varAccess.token->val);
        if (globalIdx == NCSC_INVALID_IDX)
            error(std::format(CANT_FIND_VAR_NAMED, varAccess.token->val), varAccess);
        
        ValueType varType = currScript_->getGlobalVar(globalIdx)->type; 
        if (varType != expectedExpressionType_)
            compileDifferentValueTypePush(varType, expectedExpressionType_, varAccess);

        emit(Instruction::LOADGLOBAL);
        emit(globalIdx);
    }
}

void Compiler::compileDifferentValueTypePush(ValueType from, ValueType to, const ScriptNode &node) {
    assert(from != to);

    if (!canPromoteType(from, to)) {
        error(std::format(CANT_PROMOTE_TY_TO, VTYPE_NAMES.at(from), VTYPE_NAMES.at(to)), node);
        return;
    }

    emit(Instruction::TYCAST);
    emit(static_cast<DWord>(expectedExpressionType_));
}


} // namespace NCSC
