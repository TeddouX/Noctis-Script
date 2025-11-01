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
#include "scope.hpp"

#include <memory>
#include <deque>

namespace NCSC
{

class NCSC_API Compiler {
public:
    Compiler(std::shared_ptr<ScriptContext> ctx, std::shared_ptr<ScriptSource> src = nullptr)
        : ctx_(ctx), src_(src) {};

    static std::string disassemble(const std::vector<Byte> &bc);

    std::unique_ptr<Script> compileScript(std::shared_ptr<ScriptSource> source);
    std::unique_ptr<Script> compileScript(const ASTNode &root);

    bool hasErrors() { return !compileErrors_.empty(); }
    const std::vector<Error> &getErrors() { return compileErrors_; }

private:
    std::shared_ptr<ScriptContext> ctx_;
    std::shared_ptr<ScriptSource> src_;
    Script *currScript_;
    ScriptFunction *currFunction_;
    ScriptObject *currObject_;

    std::vector<Byte> tempCompiledBytecode_;

    std::vector<Error> compileErrors_;

    std::deque<Scope> scopes_;
    size_t nextScopeIdx_ = 0;
    Scope *currScope_ = nullptr;

    QWord tmpLabelNum_ = 0;

    void enterNewScope();
    void exitScope();

    void finalizeBc(std::vector<Byte> &bc);

    void createCompileError(const ErrInfo &info, const ASTNode &node);

    bool isScriptFunction(const std::string &name) const { return currScript_->getFunction(name) != nullptr; }

    size_t computeRequiredStackSize(std::vector<Byte> &bc);
    // Computes maximum number of local variables of the scope
    size_t computeMaxLocals(const Scope *scope);

    void resolveJumps(std::vector<Byte> &bc);

    // Add a byte to the bytecode of the current function
    void emit(Byte bytecode);
    void emit(const std::vector<Byte> &bytecode);
    void emit(Word w);
    void emit(DWord dw);
    void emit(QWord qw);

    // Add an instruction to the bytecode of the current function
    void emit(Instruction instr);

    // Patch bytecode
    void patchBytecode(size_t location, Instruction instr, const std::vector<Byte> &operandBytes);

    size_t getLastByteInsertedLoc() const { return tempCompiledBytecode_.size() - 1; }

    template <typename T>
    requires std::is_integral_v<T> 
    void emitIntConstant(const std::string &valStr, const ASTNode &constant, ValueType vtype) {
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
        std::vector<Byte> bytes(bytesSize, 0);
        makeValueBytes(val, vtype, bytes, 0);
        emit(Instruction::PUSH);
        emit(bytes);
    }

    struct SymbolSearchRes {
        union {
            const ScriptObject *obj;
            const Function *fun;
            const Variable *var;
        };
        DWord idx = NCSC_INVALID_IDX;
        ValueType foundType = ValueType::INVALID;

        enum {
            INVALID,
            FUNCTION,
            METHOD,
            GLOBAL_VAR,
            LOCAL_VAR,
            OBJECT,
            MEMBER_VAR,
        } ty;
    };
    SymbolSearchRes searchSymbol(const std::string &name, ScriptObject *obj = nullptr);

    void compileFunction(const ASTNode &funcDecl, bool method = false);
    void compileObject(const ASTNode &obj);
    void compileVariableDeclaration(const ASTNode &varDecl, bool global = false, bool member = false);
    void compileConstantPush(const ASTNode &constant, ValueType expectedType);
    void compileOperator(const ASTNode &op);
    void compileExpression(const ASTNode &expr, ValueType expectedType = ValueType::INVALID);
    void recursivelyCompileExpression(const ASTNode &exprChild, ValueType expectedType);
    // shouldBeModifiable: true if it should be, false if you don't care
    void compileExpressionTerm(const ASTNode &exprTerm, ValueType expectedType = ValueType::INVALID);
    void compileStatementBlock(const ASTNode &stmtBlock);
    void compileFunctionCall(const ASTNode &funCall, bool shouldReturnVal);
    void compileReturn(const ASTNode &ret);
    void compileVariableAccess(const ASTNode &varAccess, ValueType expectedType = ValueType::INVALID);
    bool compileArguments(const ASTNode &argsNode, const Function *fun);
    void compileIfStatement(const ASTNode &ifStmt, int nestedCount = 1);
    void compileJmpBcPatch(size_t patchLoc, Instruction jmpInstr, size_t jmpLoc);
    void compileAssignment(const ASTNode &assignment, ValueType expectedType = ValueType::INVALID);
    void compileExpressionPreOp(const ASTNode &preOp, const ASTNode &operand, ValueType expectedTy);
    void compileExpressionValue(const ASTNode &exprVal, ValueType expectedTy);
    void compileExpressionPostOp(const ASTNode &postOp, const ASTNode &operand, ValueType expectedTy);

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
    inline static ErrInfo EXP_MODIFIABLE_VALUE          { "Compilation error", "C", 13, "Expected a modifiable value" };
    inline static ErrInfo EXPECTED_AN_ID                { "Compilation error", "C", 14, "Expected an identifier" };
    inline static ErrInfo EXPECTED_NUMERIC_TYPE         { "Compilation error", "C", 15, "Expected a numeric type, instead got {}" };
    inline static ErrInfo EXPECTED_A_BOOLEAN            { "Compilation error", "C", 16, "Expected a boolean, instead got {}" };
    inline static ErrInfo S_IS_NOT_A_VAR                { "Compilation error", "C", 17, "{} is not a variable" };
};

} // namespace NCSC
