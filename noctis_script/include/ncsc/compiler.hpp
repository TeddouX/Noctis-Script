#pragma once
#include "function.hpp"
#include "script.hpp"
#include "ncsc.hpp"

#include <memory>

#define MAKE_INT_WORDS(i) (int64_t)i & 0xFFFF, ((int64_t)i >> 16) & 0xFFFF, ((int64_t)i >> 32) & 0xFFFF, ((int64_t)i >> 48) & 0xFFFF
#define MAKE_FLOAT_WORDS(i) (double)i & 0xFFFF, ((double)i >> 16) & 0xFFFF, ((double)i >> 32) & 0xFFFF, ((double)i >> 48) & 0xFFFF

namespace NCSC
{

class NCSC_API Compiler {
public:
    Compiler() = default;

    std::shared_ptr<Script> compileScript(const ScriptNode &root);

private:
    Script   *currScript_;
    Function *currFunction_;

    // currFunction = function and at the end set it to nullptr to indicate that it has finished compilation
    void compileFunction(const ScriptNode &funcDecl);

    // Computes required stack size for a node
    size_t computeMaxStackSize(const ScriptNode &node);

    // Add a byte to the bytecode of the current function
    void emit(Byte bytecode) { currFunction_->bytecode.push_back(bytecode); }
    void emit(Byte *bytecode, size_t size);
    void emitInt16(int16_t i);

    // Add an instruction to the bytecode of the current function
    void emit(Instruction instr) { currFunction_->bytecode.push_back(static_cast<Byte>(instr)); }

    void compileVariableDeclaration(const ScriptNode &varDecl);
    void compileConstantPush(const ScriptNode &constant);
    void compileOperator(const ScriptNode &op);
    void compileExpression(const ScriptNode &expr);
    void recursivelyCompileExpression(const ScriptNode &exprChild);
    void compileExpressionTerm(const ScriptNode &exprTerm);
};

} // namespace NCSC
