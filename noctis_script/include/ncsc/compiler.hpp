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
    explicit Compiler(std::shared_ptr<ScriptContext> ctx, std::shared_ptr<ScriptSource> src = nullptr)
        : ctx_(ctx), src_(src) {};

    static std::string disassemble(const std::vector<Byte> &bc);

    std::unique_ptr<Script> compileScript(std::shared_ptr<ScriptSource> source);
    std::unique_ptr<Script> compileScript(const ASTNode &root);

    bool hasErrors() { return !compileErrors_.empty(); }
    const std::vector<Error> &getErrors() const { return compileErrors_; }

    void setSource(const std::shared_ptr<ScriptSource> &src) { src_ = src; }

private:
    std::shared_ptr<ScriptContext> ctx_;
    std::shared_ptr<ScriptSource> src_;
    Script *currScript_ = nullptr;
    ScriptFunction *currFunction_ = nullptr;
    ScriptObject *currObject_ = nullptr;

    std::vector<Byte> tempCompiledBytecode_;

    std::vector<Error> compileErrors_;

    std::deque<Scope> scopes_;
    size_t nextScopeIdx_ = 0;
    Scope *currScope_ = nullptr;

    QWord tmpLabelNum_ = 0;

    void enterNewScope();
    void exitScope();
    // Clear scopes and set currScope_ to nullptr
    void resetScopes();

    void finalizeBc(std::vector<Byte> &bc);

    void createCompileError(const ErrInfo &info, const ASTNode &node);

    size_t computeRequiredStackSize(const std::vector<Byte> &bc);
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

    template <typename T>
    requires(std::is_integral_v<T>) 
    void emitIntConstant(const std::string &valStr, const ASTNode &constant, ValueType vtype) {
        using IntermediateTy_ = std::conditional_t<std::is_signed_v<T>, int64_t, uint64_t>;
        IntermediateTy_ intermediate{};

        if constexpr (std::is_signed_v<IntermediateTy_>)
            intermediate = std::strtoll(valStr.c_str(), nullptr, 0);
        else {
            // Number is negative
            if (valStr[0] == '-') {
                createCompileError(NUMBER_IS_TOO_SMALL_FOR_TY.format(ctx_->getTypeName(vtype)), constant);
                return;
            }
            
            intermediate = std::strtoull(valStr.c_str(), nullptr, 0);
        }

        if (intermediate < std::numeric_limits<T>::min()) {
            createCompileError(NUMBER_IS_TOO_SMALL_FOR_TY.format(ctx_->getTypeName(vtype)), constant);
            return;
        } else if (intermediate > std::numeric_limits<T>::max()) {
            createCompileError(NUMBER_IS_TOO_BIG_FOR_TY.format(ctx_->getTypeName(vtype)), constant);
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
            Object   *obj = nullptr;
            Function *fun;
            Variable *var;
        };

        DWord idx = INVALID_IDX;
        ValueType foundType = ValueType::INVALID;

        enum {
            INVALID,
            GLOBAL_VAR,
            LOCAL_VAR,
            
            FUNCTION,
            METHOD,
            OBJECT,
            MEMBER_VAR,

            CPP_FUNCTION,
            CPP_METHOD,
            CPP_OBJECT,
        } ty = SymbolSearchRes::INVALID;
    };
    SymbolSearchRes searchSymbol(const std::string &name, Object *obj = nullptr);

    ValueType valueTypeFromASTNode(const ASTNode &typeNode);

    void compileFunction(const ASTNode &funcDecl, bool method = false);
    void compileObject(const ASTNode &obj);
    void compileVariableDeclaration(const ASTNode &varDecl, bool global = false, bool member = false);
    void compileConstantPush(const ASTNode &constant, ValueType expectedType);
    void compileOperator(const ASTNode &op, ValueType expectedType);
    void compileExpression(const ASTNode &expr, ValueType expectedType = ValueType::VOID);
    void recursivelyCompileExpression(const ASTNode &exprChild, ValueType expectedType, bool shouldBeAssignable = false);
    void compileExpressionTerm(const ASTNode &exprTerm, ValueType expectedType = ValueType::VOID, bool shouldBeAssignable = false);
    void compileStatementBlock(const ASTNode &stmtBlock);
    void compileFunctionCall(const ASTNode &funCall, ValueType expectedType);
    void compileReturn(const ASTNode &ret);
    void compileVariableAccess(const ASTNode &varAccess, ValueType expectedType = ValueType::VOID);
    bool compileArguments(const ASTNode &argsNode, const Function *fun, bool isMethod = false);
    void compileIfStatement(const ASTNode &ifStmt, int nestedCount = 1);
    void compileJmpBcPatch(size_t patchLoc, Instruction jmpInstr, size_t jmpLoc);
    void compileAssignment(const ASTNode &assignment);
    void compileExpressionValue(const ASTNode &exprVal, ValueType expectedTy, bool shouldBeAssignable);
    void compileConstructCall(const ASTNode &constructCall, ValueType expectedTy);
    // Won't create compile errors, if storing into a member variable, the member should already have been compiled
    void compileStore(const ASTNode &varNode);

    // Naively try to find the type of an expression term
    // Won't create a compileError if one is encountered
    ValueType getExpressionTermType(const ASTNode &exprTerm);

    Object *getValueTypeAsObject(ValueType type, bool &isScriptObj, DWord &idx);

    inline static ErrInfo CANT_FIND_FUNCTION_NAMED      { "Compilation error", "C", 0,  "Can't find function named '{}'" };
    inline static ErrInfo CANT_FIND_VAR_NAMED           { "Compilation error", "C", 1,  "Can't find variable named '{}'" };
    inline static ErrInfo FUNCTION_HAS_VOID_RET_TY      { "Compilation error", "C", 2,  "Function '{}' has a void return type, but it is still being used in an expression" };
    inline static ErrInfo FUNCTION_SHOULD_RET_VAL       { "Compilation error", "C", 3,  "Function '{}' should return a value" };
    inline static ErrInfo FUNCTION_SHOULDNT_RET_VAL     { "Compilation error", "C", 4,  "Function '{}' has a return type of void, so it shouldn't return anything" };
    inline static ErrInfo EXPECTED_TYPE_INSTEAD_GOT     { "Compilation error", "C", 5,  "Expected type '{}', instead got '{}'" };
    inline static ErrInfo EXPECTED_NUM_ARGS_INSTEAD_GOT { "Compilation error", "C", 6,  "Expected {} arguments, instead got {}" };
    // UNUSED
    inline static ErrInfo EXPECTED_NON_FLOATING_POINT   { "Compilation error", "C", 7,  "Unexpected floating point number '{}'" };
    // UNUSED
    inline static ErrInfo CANT_PROMOTE_TY_TO            { "Compilation error", "C", 8,  "Unable to convert type {} to {}" };
    inline static ErrInfo NUMBER_IS_TOO_BIG_FOR_TY      { "Compilation error", "C", 9,  "Number is too big for an {}" };
    inline static ErrInfo NUMBER_IS_TOO_SMALL_FOR_TY    { "Compilation error", "C", 10, "Number is too small for an {}" };
    inline static ErrInfo EXP_MODIFIABLE_VALUE          { "Compilation error", "C", 13, "Expected a modifiable value" };
    inline static ErrInfo EXPECTED_AN_ID                { "Compilation error", "C", 14, "Expected an identifier" };
    inline static ErrInfo EXPECTED_NUMERIC_TYPE         { "Compilation error", "C", 15, "Expected a numeric type, instead got {}" };
    inline static ErrInfo EXPECTED_A_BOOLEAN            { "Compilation error", "C", 16, "Expected a boolean, instead got {}" };
    inline static ErrInfo NOT_A_VAR                     { "Compilation error", "C", 17, "Not a variable" };
    inline static ErrInfo NOT_A_TYPE                    { "Compilation error", "C", 18, "Not a type" };
    inline static ErrInfo NOT_A_FUNCTION                { "Compilation error", "C", 19, "Not a function" };
    inline static ErrInfo NOT_AN_OBJ                    { "Compilation error", "C", 20, "Not a constructible object" };
    inline static ErrInfo CONSTRUCTOR_SHOULDNT_RET      { "Compilation error", "C", 21, "The compiler shouldn't return" };
    inline static ErrInfo SYMBOL_ALREADY_EXISTS         { "Compilation error", "C", 22, "This symbol was already defined somewhere else" };
    inline static ErrInfo TY_IS_NOT_AN_OBJECT           { "Compilation error", "C", 23, "Type {} is not an object" };
    inline static ErrInfo NOT_A_MEMBER                  { "Compilation error", "C", 24, "Not a member variable of {}" };
    inline static ErrInfo INACESSIBLE_BC_NOT_PUB        { "Compilation error", "C", 25, "Inaccessible because it isn't marked as public" };
    inline static ErrInfo NOT_A_METHOD                  { "Compilation error", "C", 26, "Not a method of {}" };
    inline static ErrInfo DIV_ALWAYS_RETS_A_F64         { "Compilation error", "C", 27, "Division always results in a 'Float64', which can't be converted to '{}'" };
    inline static ErrInfo NOT_A_CTOR                    { "Compilation error", "C", 28, "Not a constructor of {}" };
    inline static ErrInfo EXPECTED_ASSIGNABLE_TERM      { "Compilation error", "C", 29, "Expected an assignable term" };
};

} // namespace NCSC
