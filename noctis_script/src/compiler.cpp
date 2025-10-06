#include <ncsc/compiler.hpp>
#include <ncsc/type_info.hpp>

namespace NCSC
{

static void intToBytes(int64_t val, Byte bytes[]) {
    for (int i = 0; i < 8; ++i)
        bytes[i] = (val >> (i * 8)) & 0xFF;
}

static void floatToBytes(double val, Byte bytes[]) {
    uint64_t bits;
    static_assert(sizeof(bits) == sizeof(val));
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

void Compiler::emitInt16(int16_t i) {
    Byte bytes[] {
        i & 0xFF,
        (i >> 8) & 0xFF
    };
    emit(bytes, sizeof(bytes));
}


void Compiler::compileFunction(const ScriptNode &funcDecl) {
    assert(funcDecl.type == ScriptNodeType::FUNCTION);

    auto fun = std::make_shared<Function>();
    fun->requiredStackSize = computeMaxStackSize(funcDecl);

    currFunction_ = fun.get();

    for (auto &node : funcDecl.children) {
        switch (node.type) {
            case ScriptNodeType::VARIABLE_DECLARATION:
                compileVariableDeclaration(node);
        }
    }

    currScript_->addFunction(*fun);

    currFunction_ = nullptr;
}

size_t Compiler::computeMaxStackSize(const ScriptNode &node) {
    assert(node.type == ScriptNodeType::FUNCTION);

    switch (node.type) {
        case ScriptNodeType::FUNCTION: {
            const ScriptNode &args = node.children[2];
            // Arguments' values should be by pairs (1 type 1 identifier)
            assert(args.type == ScriptNodeType::ARGUMENT_LIST && args.children.size() % 2 == 0);
            size_t argsSize = args.children.size() / 2;
            
            const ScriptNode &stmtBlock = node.children[3];
            assert(stmtBlock.type == ScriptNodeType::STATEMENT_BLOCK);
            size_t requiredStackBlock = 0;
            for (const auto &stmt : stmtBlock.children) {
                requiredStackBlock = std::max(requiredStackBlock, computeMaxStackSize(stmt));
            }

            return argsSize + requiredStackBlock;
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
            // An expression should always have an odd number of children
            assert(node.children.size() % 2 != 0);

            if (node.children.size() == 1)
                return computeMaxStackSize(node.children[0]);

            // Start with first operand
            size_t maxDepth = computeMaxStackSize(node.children[0]);
            size_t currentDepth = maxDepth;

            // Process operator-operand pairs
            for (size_t i = 1; i < node.children.size(); i += 2) {
                const ScriptNode& rhs = node.children[i + 1];

                size_t rhsDepth = computeMaxStackSize(rhs);
                // While evaluating the part on the right, the
                // left part is already on the stack so it can be ingnored
                maxDepth = std::max(maxDepth, currentDepth + rhsDepth);

                // Operator pops two values and pushes one
                // POP POP PUSH -> -1 net stack change
                currentDepth--;
            }

            return maxDepth;
        }
    
        case ScriptNodeType::EXPRESSION_TERM: {
            // Temp
            return 1;
        }
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
            emit(Instruction::STORELOCAL); emitInt16(currFunction_->numLocals);
        } else {
            // do whatever
        }
    } else {

    }

    currFunction_->numLocals++;
}

        // if (type.isPrimitive()) {
        //     Byte bytes[8]{0};

        //     if (type.isInt()) {
        //         int val = std::stoi(varDecl.children[0].token->val);
        //         intToBytes(val, bytes);
        //     } else if (type.isFloat()) {
        //         double val = std::stod(varDecl.children[0].token->val);
        //         floatToBytes(val, bytes);
        //     }

        //     emit(bytes, sizeof(bytes));
        // } else {

        // }

} // namespace NCSC
