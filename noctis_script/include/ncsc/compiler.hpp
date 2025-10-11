#pragma once
#include "function.hpp"
#include "script.hpp"
#include "ncsc.hpp"

#include <memory>

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
    void emitWord(Word dw);

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
