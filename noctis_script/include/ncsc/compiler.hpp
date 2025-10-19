#pragma once
#include "function.hpp"
#include "script.hpp"
#include "ncsc.hpp"
#include "error.hpp"
#include "value.hpp"

#include <memory>
#include <format>

namespace NCSC
{

class NCSC_API Compiler {
public:
    Compiler() = default;

    static std::string disassemble(const std::vector<Byte> &bc);

    std::unique_ptr<Script> compileScript(const ScriptNode &root);

    bool hasErrors() { return !compileErrors_.empty(); }
    const std::vector<Error> &getErrors() { return compileErrors_; }

private:
    Script   *currScript_;
    Function *currFunction_;

    std::vector<Byte> tempCompiledBytecode_;

    std::vector<Error> compileErrors_;

    struct LocalVar {
        std::string name;
        ValueType type;
    };
    std::vector<LocalVar> localVariables_;

    ValueType expectedExpressionType_;

    void error(const std::string &mess, const ScriptNode &node);

    // currFunction = function and at the end set it to nullptr to indicate that it has finished compilation
    void compileFunction(const ScriptNode &funcDecl);

    bool isScriptFunction(const std::string &name) { return currScript_->getFunction(name) != nullptr; }

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
            error(std::format(NUMBER_IS_TOO_SMALL_FOR_TY, valStr, VTYPE_NAMES.at(vtype)), constant);
            return;
        } else if (intermediate > std::numeric_limits<T>::max()) {
            error(std::format(NUMBER_IS_TOO_BIG_FOR_TY, valStr, VTYPE_NAMES.at(vtype)), constant);
            return;
        }

        T val = static_cast<T>(intermediate);
        
        constexpr size_t bytesSize = getValueSize(T{}); 
        Byte bytes[bytesSize];
        makeValueBytes(val, vtype, bytes, 0);
        emit(Instruction::PUSH);
        emit(bytes, bytesSize);
    }

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
    void compileDifferentValueTypePush(ValueType from, ValueType to, const ScriptNode &node);

    inline static constexpr std::string_view CANT_FIND_FUNCTION_NAMED      = "Error: Can't find function named '{}'";
    inline static constexpr std::string_view CANT_FIND_VAR_NAMED           = "Error: Can't find variable named '{}'";
    inline static constexpr std::string_view FUNCTION_HAS_VOID_RET_TY      = "Error: Function '{}' has a void return type, but it is still being used in an expression";
    inline static constexpr std::string_view FUNCTION_SHOULD_RET_VAL       = "Error: Function '{}' should return a value";
    inline static constexpr std::string_view FUNCTION_SHOULDNT_RET_VAL     = "Error: Function '{}' has a return type of void, so it shouldn't return anything";
    inline static constexpr std::string_view EXPECTED_TYPE_INSTEAD_GOT     = "Error: Expected type '{}', instead got '{}'";
    inline static constexpr std::string_view EXPECTED_NUM_ARGS_INSTEAD_GOT = "Error: Expected {} arguments for function {} instead got {}";
    inline static constexpr std::string_view EXPECTED_NON_FLOATING_POINT   = "Error: Unexpected floating point number '{}'";
    inline static constexpr std::string_view CANT_PROMOTE_TY_TO            = "Error: Unable to convert type {} to {}";
    inline static constexpr std::string_view NUMBER_IS_TOO_BIG_FOR_TY      = "Error: Number '{}' is too big for an {}";
    inline static constexpr std::string_view NUMBER_IS_TOO_SMALL_FOR_TY    = "Error: Number '{}' is too small for an {}";
};

} // namespace NCSC
