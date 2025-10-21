// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "script.hpp"
#include "ncsc.hpp"
#include "error.hpp"
#include "value.hpp"
#include "script_context.hpp"
#include "script_function.hpp"
#include "instructions.hpp"

#include <memory>
#include <format>

namespace NCSC
{

class NCSC_API Compiler {
public:
    Compiler(std::shared_ptr<ScriptContext> ctx, std::shared_ptr<ScriptSource> src = nullptr)
        : ctx_(ctx), src_(src) {};

    static std::string disassemble(const std::vector<Byte> &bc);

    std::unique_ptr<Script> compileScript(std::shared_ptr<ScriptSource> source);
    std::unique_ptr<Script> compileScript(const ScriptNode &root);

    bool hasErrors() { return !compileErrors_.empty(); }
    const std::vector<Error> &getErrors() { return compileErrors_; }

private:
    std::shared_ptr<ScriptContext> ctx_;
    std::shared_ptr<ScriptSource> src_;
    Script *currScript_;
    ScriptFunction *currFunction_;

    std::vector<Byte> tempCompiledBytecode_;

    std::vector<Error> compileErrors_;

    struct LocalVar {
        std::string name;
        ValueType type;
    };
    std::vector<LocalVar> localVariables_;

    ValueType expectedExpressionType_;

    void createCompileError(const ErrInfo &info, const ScriptNode &node);

    // currFunction = function and at the end set it to nullptr to indicate that it has finished compilation
    void compileFunction(const ScriptNode &funcDecl);

    bool isScriptFunction(const std::string &name) const { return currScript_->getFunction(name) != nullptr; }

    // Computes required stack size for a node
    size_t computeMaxStackSize(const ScriptNode &node);

    // Add a byte to the bytecode of the current function
    void emit(Byte bytecode);
    void emit(Byte *bytecode, size_t size);
    void emit(Word w);
    void emit(DWord dw);

    // Add an instruction to the bytecode of the current function
    void emit(Instruction instr);

    template <typename T>
    requires std::is_integral_v<T> 
    void emitIntConstant(const std::string &valStr, const ScriptNode &constant, ValueType vtype) {
        using IntermediateTy_ = std::conditional_t<std::is_signed_v<T>, int64_t, uint64_t>;
        IntermediateTy_ intermediate{};

        if constexpr (std::is_signed_v<IntermediateTy_>)
            intermediate = std::strtoll(valStr.c_str(), nullptr, 0);
        else
            intermediate = std::strtoull(valStr.c_str(), nullptr, 0);

        if (intermediate < std::numeric_limits<T>::min()) {
            createCompileError(NUMBER_IS_TOO_SMALL_FOR_TY.format(valStr, valueTypeToString(vtype)), constant);
            return;
        } else if (intermediate > std::numeric_limits<T>::max()) {
            createCompileError(NUMBER_IS_TOO_BIG_FOR_TY.format(valStr, valueTypeToString(vtype)), constant);
            return;
        }

        T val = static_cast<T>(intermediate);
        
        constexpr size_t bytesSize = getValueSize(T{}); 
        Byte bytes[bytesSize];
        makeValueBytes(val, vtype, bytes, 0);
        emit(Instruction::PUSH);
        emit(bytes, bytesSize);
    }

    bool hasLocalVariable(const std::string &name) const;

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
    bool compileArguments(const ScriptNode &argsNode, const IFunction *fun);

    inline static ErrInfo CANT_FIND_FUNCTION_NAMED      { "Compilation error", "C", 0,  "Can't find function named '{}'" };
    inline static ErrInfo CANT_FIND_VAR_NAMED           { "Compilation error", "C", 1,  "Can't find variable named '{}'" };
    inline static ErrInfo FUNCTION_HAS_VOID_RET_TY      { "Compilation error", "C", 2,  "Function '{}' has a void return type, but it is still being used in an expression" };
    inline static ErrInfo FUNCTION_SHOULD_RET_VAL       { "Compilation error", "C", 3,  "Function '{}' should return a value" };
    inline static ErrInfo FUNCTION_SHOULDNT_RET_VAL     { "Compilation error", "C", 4,  "Function '{}' has a return type of void, so it shouldn't return anything" };
    inline static ErrInfo EXPECTED_TYPE_INSTEAD_GOT     { "Compilation error", "C", 5,  "Expected type '{}', instead got '{}'" };
    inline static ErrInfo EXPECTED_NUM_ARGS_INSTEAD_GOT { "Compilation error", "C", 6,  "Expected {} arguments for function {} instead got {}" };
    inline static ErrInfo EXPECTED_NON_FLOATING_POINT   { "Compilation error", "C", 7,  "Unexpected floating point number '{}'" };
    inline static ErrInfo CANT_PROMOTE_TY_TO            { "Compilation error", "C", 8,  "Unable to convert type {} to {}" };
    inline static ErrInfo NUMBER_IS_TOO_BIG_FOR_TY      { "Compilation error", "C", 9,  "Number '{}' is too big for an {}" };
    inline static ErrInfo NUMBER_IS_TOO_SMALL_FOR_TY    { "Compilation error", "C", 10, "Number '{}' is too small for an {}" };
    inline static ErrInfo VAR_ALREADY_EXISTS            { "Compilation error", "C", 11, "Another variable with name '{}' already exists" };
    inline static ErrInfo FUNC_ALREADY_EXISTS           { "Compilation error", "C", 12, "Another function with name '{}' already exists" };
};

} // namespace NCSC
