#include <ncsc/compiler.hpp>
#include <ncsc/type_info.hpp>
#include <sstream>
#include <format>
#include <iomanip>

namespace NCSC
{

static constexpr void intToBytes(int64_t val, Byte bytes[]) {
    for (int i = 0; i < 8; ++i)
        bytes[i] = (val >> (i * 8)) & 0xFF;
}

static constexpr void floatToBytes(float64_t val, Byte bytes[]) {
    uint64_t bits = std::bit_cast<uint64_t>(val);;
    intToBytes(bits, bytes);
}

template <typename T>
static constexpr T fetchOperand(const std::vector<Byte>& bc, size_t& i) {
    T val = readWord<T>(bc.data(), i);
    i += sizeof(T);
    return val;
}

std::string Compiler::disassemble(const std::vector<Byte>& bc) {
    std::ostringstream oss;

    for (size_t i = 0; i < bc.size();) {
        size_t offset = i;
        Byte op = bc[i++];

        auto it = INSTR_INFO.find(static_cast<Instruction>(op));
        if (it == INSTR_INFO.end()) {
            oss << std::setw(4) << offset << ": UNKNOWN INSTR\n";
            continue;
        }

        const auto& info = it->second;
        oss << std::setw(4) << offset << ": " << info.first;

        if (info.second > 0) {
            oss << " ";
            switch (info.second) {
                case 2: oss << fetchOperand<uint16_t>(bc, i); break;
                case 4: oss << fetchOperand<uint32_t>(bc, i); break;
                case 8: oss << fetchOperand<uint64_t>(bc, i); break;
                default: oss << "(invalid size)";
            }
        }

        oss << "\n";
    }

    return oss.str();
}

std::unique_ptr<Script> Compiler::compileScript(const ScriptNode &root) {
    auto script = std::make_unique<Script>(); 
    currScript_ = script.get();

    for (auto &node : root.children) {
        if (node.type == ScriptNodeType::FUNCTION)
            compileFunction(node);
        else if (node.type == ScriptNodeType::VARIABLE_DECLARATION) {
            compileVariableDeclaration(node, true);
            // So the VM is able to stop
            emit(Instruction::RETVOID);

            GlobalVar gv {
                .name = node.children[1].token->val,
                .bytecode = tempCompiledBytecode_,
                .requiredStackSize = computeMaxStackSize(node),
                .type = lastTypeOnStack_,
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
    
    ScriptNode retType = funcDecl.children[0]; 
    if (retType.type == ScriptNodeType::DATA_TYPE)
        fun->returnType = TypeInfo(retType.token->type);
    
    currFunction_ = fun.get();
    currFunction_->name = funcDecl.children[1].token->val;
    currFunction_->requiredStackSize = computeMaxStackSize(funcDecl);

    ScriptNode paramsNode = funcDecl.children[2];
    currFunction_->numParams = paramsNode.children.size() / 2;
    currFunction_->numLocals = currFunction_->numParams;
    for (int i = 0; i < paramsNode.children.size(); i++) {
        TypeInfo ty(paramsNode.children[i].token->type);
        
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
        if (currFunction_->returnType.isVoid() && tempCompiledBytecode_.back() != static_cast<Byte>(Instruction::RETVOID))
            emit(Instruction::RETVOID);
        // For non void functions, error if there is no return at the end
        else if (currFunction_->returnType.isVoid() && tempCompiledBytecode_.back() != static_cast<Byte>(Instruction::RET)) {
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

    TypeInfo varType(varDecl.children[0].token->type);
    if (varDecl.children.size() == 2) {
        if (varType.isPrimitive()) {
            Byte bytes[8]{0};
            varType.getDefaultValue(bytes, sizeof(bytes));

            if (varType.isInt())        emit(Instruction::PUSHINT);
            else if (varType.isFloat()) emit(Instruction::PUSHFLOAT);

            emit(bytes, sizeof(bytes));
        } else {
            // do whatever
        }
    } else {
        // Compile the expression
        // The result will be stored on the top of the stack
        compileExpression(varDecl.children[2]);

        if (lastTypeOnStack_ != varType) {
            error(std::format(EXPECTED_TYPE_INSTEAD_GOT, varType.getStrRepr(), lastTypeOnStack_.getStrRepr()), varDecl.children[0]);
            return;
        }
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
    TypeInfo exprType;

    recursivelyCompileExpression(rootOp.children[0]);
    exprType = lastTypeOnStack_;

    recursivelyCompileExpression(rootOp.children[1]);
    // The float type overrides everything
    if (exprType == TypeInfo::FLOAT)
        lastTypeOnStack_ = TypeInfo::FLOAT;
    
    compileOperator(rootOp);
}

void Compiler::recursivelyCompileExpression(const ScriptNode &exprChild) {
    if (exprChild.type == ScriptNodeType::BINOP) {
    TypeInfo exprType;
        recursivelyCompileExpression(exprChild.children[0]);
        exprType = lastTypeOnStack_;
        recursivelyCompileExpression(exprChild.children[1]);
        if (exprType == TypeInfo::FLOAT)
            lastTypeOnStack_ = TypeInfo::FLOAT;
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

    Byte bytes[8]{0};
    if (constant.token->type == TokenType::FLOAT_CONSTANT) {
        float64_t val = std::stod(constant.token->val);
        lastTypeOnStack_ = TypeInfo::fromLiteral(val);

        floatToBytes(val, bytes);
        emit(Instruction::PUSHFLOAT);
    } else if (constant.token->type == TokenType::INT_CONSTANT) {
        int val = std::stoi(constant.token->val);
        lastTypeOnStack_ = TypeInfo::fromLiteral(val);
        
        intToBytes(val, bytes);
        emit(Instruction::PUSHINT);
    } else
        assert(0 && "TODO");
    
    emit(bytes, sizeof(bytes));
    
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

        if (shouldReturnVal && fun->returnType.isVoid()) {
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
                compileExpression(argsNode.children[i]);

                // Compare parameter type with given argument type
                const TypeInfo &paramType = fun->paramTypes[i]; 
                if (paramType != lastTypeOnStack_) {
                    error(std::format(EXPECTED_TYPE_INSTEAD_GOT, paramType.getStrRepr(), lastTypeOnStack_.getStrRepr()), funNameNode);
                    return;
                }
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
    if (expr.children.empty()) {
        if (!currFunction_->returnType.isVoid()) {
            error(std::format(FUNCTION_SHOULD_RET_VAL, currFunction_->name), ret);
            return;
        }

        emit(Instruction::RETVOID);
    } else {
        if (currFunction_->returnType.isVoid()) {
            error(std::format(FUNCTION_SHOULDNT_RET_VAL, currFunction_->name), ret);
            return;
        }

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

    if (!found) {
        DWord idx = currScript_->getGlobalVarIdx(varAccess.token->val);
        if (idx == NCSC_INVALID_IDX)
            error(std::format(CANT_FIND_VAR_NAMED, varAccess.token->val), varAccess);

        emit(Instruction::LOADGLOBAL);
        emit(idx);
        lastTypeOnStack_ = currScript_->getGlobalVar(idx)->type;

        return;
    } else {
        emit(Instruction::LOADLOCAL);
        emit(idx);
        lastTypeOnStack_ = localVariables_[idx].type;
    }

}


} // namespace NCSC
