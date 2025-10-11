#include <ncsc/compiler.hpp>
#include <ncsc/type_info.hpp>
#include <iostream>

namespace NCSC
{

static void intToBytes(int64_t val, Byte bytes[]) {
    for (int i = 0; i < 8; ++i)
        bytes[i] = (val >> (i * 8)) & 0xFF;
}

static void floatToBytes(double val, Byte bytes[]) {
    uint64_t bits;
    assert(sizeof(bits) == sizeof(val));
    std::memcpy(&bits, &val, sizeof(bits));
    intToBytes(bits, bytes);
}

std::shared_ptr<Script> Compiler::compileScript(const ScriptNode &root) {
    auto script = std::make_shared<Script>(); 

    currScript_ = script.get();

    for (auto &node : root.children) {
        if (node.type == ScriptNodeType::FUNCTION)
            compileFunction(node);
    }

    currScript_ = nullptr;

    return script;
}

void Compiler::emit(Byte *bytecode, size_t size) {
    for (size_t i = 0; i < size; i++) 
        emit(bytecode[i]);
}

void Compiler::emitWord(Word dw) {
    Byte bytes[] {
        static_cast<Byte>(dw & 0xFF),
        static_cast<Byte>((dw >> 8) & 0xFF)
    };
    emit(bytes, sizeof(bytes));
}

void Compiler::compileFunction(const ScriptNode &funcDecl) {
    assert(funcDecl.type == ScriptNodeType::FUNCTION);

    auto fun = std::make_shared<Function>();
    fun->requiredStackSize = computeMaxStackSize(funcDecl);

    currFunction_ = fun.get();
    currFunction_->name = funcDecl.children[1].token->val;

    const ScriptNode& statementBlock = funcDecl.children.back();
    for (auto &node : statementBlock.children) {
        switch (node.type) {
            case ScriptNodeType::VARIABLE_DECLARATION:
                compileVariableDeclaration(node);
        }
    }

    // Any function should return
    emit(Instruction::RET);

    currScript_->addFunction(*fun);

    currFunction_ = nullptr;
}

size_t Compiler::computeMaxStackSize(const ScriptNode &node) {
    switch (node.type) {
        case ScriptNodeType::FUNCTION: {
            size_t argsSize = 0;
            // Possibly doesn't have any arguments
            if (node.children[2].type == ScriptNodeType::ARGUMENT_LIST) {
                // Arguments' values should be by pairs (1 type 1 identifier)
                const ScriptNode &args = node.children[2];
                assert(args.children.size() % 2 == 0);
                argsSize = args.children.size() / 2;
            }
            
            const ScriptNode &stmtBlock = node.children.back();
            assert(stmtBlock.type == ScriptNodeType::STATEMENT_BLOCK);
            size_t requiredStackSize = 0;
            for (const auto &stmt : stmtBlock.children) {
                requiredStackSize = std::max(requiredStackSize, computeMaxStackSize(stmt));
            }

            return argsSize + requiredStackSize;
        }

        case ScriptNodeType::VARIABLE_DECLARATION: {
            // int i;
            if (node.children.size() == 2)
                // PUSH 1
                // STORELOCAL n
                return 1;
            else
                // Compute expression size
                return computeMaxStackSize(node.children[2]);   
        }

        case ScriptNodeType::EXPRESSION: {
            return computeMaxStackSize(node.children[0]);
        }
    
        case ScriptNodeType::BINOP: {
            size_t lhs = computeMaxStackSize(node.children[0]);
            size_t rhs = computeMaxStackSize(node.children[1]);

            // Result occupies one slot
            return std::max(lhs, rhs + 1);
        }

        case ScriptNodeType::EXPRESSION_TERM: {
            // Temp
            return 1;
        }
    
        default:
            return 0;
    }
}

void Compiler::compileVariableDeclaration(const ScriptNode &varDecl) {
    assert(varDecl.type == ScriptNodeType::VARIABLE_DECLARATION);

    if (varDecl.children.size() == 2) {
        TypeInfo type(varDecl.children[0]);
        if (type.isPrimitive()) {
            Byte bytes[8]{0};
            type.getDefaultValue(bytes, sizeof(bytes));

            if (type.isInt())        emit(Instruction::PUSHINT);
            else if (type.isFloat()) emit(Instruction::PUSHFLOAT);

            emit(bytes, sizeof(bytes));
        } else {
            // do whatever
        }
    } else
        // Compile the expression
        // The result will be stored on the top of the stack
        compileExpression(varDecl.children[2]);

    emit(Instruction::STORELOCAL); 
    emitWord(currFunction_->numLocals);

    currFunction_->numLocals++;
}

void Compiler::compileExpression(const ScriptNode &expr) {
    assert(expr.type == ScriptNodeType::EXPRESSION);

    if (expr.children.size() == 1 && expr.children[0].type == ScriptNodeType::EXPRESSION_TERM) {
        compileConstantPush(expr.children[0]);
        return;
    }

    ScriptNode rootOp = expr.children[0]; 
    // first go to the left and then the right
    recursivelyCompileExpression(rootOp.children[0]);
    recursivelyCompileExpression(rootOp.children[1]);
    compileOperator(rootOp);
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

    TypeInfo type(*constant.token);
    if (type.isPrimitive()) {
        Byte bytes[8]{0};

        if (type.isInt()) {
            int val = std::stoi(constant.token->val);
            intToBytes(val, bytes);
            emit(Instruction::PUSHINT);
        } else if (type.isFloat()) {
            double val = std::stod(constant.token->val);
            floatToBytes(val, bytes);
            emit(Instruction::PUSHFLOAT);
        }

        emit(bytes, sizeof(bytes));
    } else
        assert(0 && "TODO");
}

void Compiler::compileExpressionTerm(const ScriptNode &exprTerm) {
    assert(exprTerm.type == ScriptNodeType::EXPRESSION_TERM);

    if (exprTerm.children.size() == 1 && exprTerm.children[0].type == ScriptNodeType::CONSTANT)
        compileConstantPush(exprTerm.children[0]);
}

} // namespace NCSC
