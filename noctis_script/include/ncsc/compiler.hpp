#pragma once
#include "function.hpp"
#include "script.hpp"
#include "ncsc.hpp"
#include "error.hpp"

#include <memory>

namespace NCSC
{

class NCSC_API Compiler {
public:
    Compiler() = default;

    std::unique_ptr<Script> compileScript(const ScriptNode &root);

    bool hasErrors() { return !compileErrors_.empty(); }
    const std::vector<Error> &getErrors() { return compileErrors_; }

private:
    Script   *currScript_;
    Function *currFunction_;

    std::vector<Error> compileErrors_;

    struct LocalVar {
        std::string name;
        TypeInfo type;
    };
    std::vector<LocalVar> localVariables_;

    TypeInfo lastTypeOnStack_;

    void error(const std::string &mess, const ScriptNode &node);

    // currFunction = function and at the end set it to nullptr to indicate that it has finished compilation
    void compileFunction(const ScriptNode &funcDecl);

    bool isScriptFunction(const std::string &name) { return currScript_->getFunction(name) != nullptr; }

    // Computes required stack size for a node
    size_t computeMaxStackSize(const ScriptNode &node);

    // Add a byte to the bytecode of the current function
    void emit(Byte bytecode) { currFunction_->bytecode.push_back(bytecode); }
    void emit(Byte *bytecode, size_t size);
    void emit(Word w);
    void emit(DWord dw);

    // Add an instruction to the bytecode of the current function
    void emit(Instruction instr) { currFunction_->bytecode.push_back(static_cast<Byte>(instr)); }

    void compileVariableDeclaration(const ScriptNode &varDecl, bool global);
    void compileConstantPush(const ScriptNode &constant);
    void compileOperator(const ScriptNode &op);
    void compileExpression(const ScriptNode &expr);
    void recursivelyCompileExpression(const ScriptNode &exprChild);
    void compileExpressionTerm(const ScriptNode &exprTerm);
    void compileSimpleStatement(const ScriptNode &simpleStmt);
    void compileFunctionCall(const ScriptNode &funCall, bool shouldReturnVal);
    void compileReturn(const ScriptNode &ret);
    void compileVariableAccess(const ScriptNode &varAccess);

    inline static constexpr std::string_view CANT_FIND_FUNCTION_NAMED      = "Error: Can't find function named '{}'";
    inline static constexpr std::string_view CANT_FIND_VAR_NAMED           = "Error: Can't find variable named '{}'";
    inline static constexpr std::string_view FUNCTION_HAS_VOID_RET_TY      = "Error: Function '{}' has a void return type, but it is still being used in an expression";
    inline static constexpr std::string_view FUNCTION_SHOULD_RET_VAL       = "Error: Function '{}' should return a value";
    inline static constexpr std::string_view FUNCTION_SHOULDNT_RET_VAL     = "Error: Function '{}' has a return type of void, so it shouldn't return anything";
    inline static constexpr std::string_view EXPECTED_TYPE_INSTEAD_GOT     = "Error: Expected type '{}', instead got '{}'";
    inline static constexpr std::string_view EXPECTED_NUM_ARGS_INSTEAD_GOT = "Error: Expected {} arguments for function {} instead got {}";
};

} // namespace NCSC
