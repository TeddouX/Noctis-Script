// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "function.hpp"
#include "script.hpp"
#include "value.hpp"

#include <memory>
#include <deque>
#include <typeinfo>
#include <print>

namespace NCSC
{

struct CallFrame {
    const std::vector<Byte> *bytecode;
    size_t bp = 0;
    QWord  ip = 0;
    size_t sp = 0;
    size_t stackSize = 0;
};

class NCSC_API VM {
public:
    explicit VM(std::shared_ptr<Script> script);

    bool computeGlobals();

    void prepareFunction(const ScriptFunction *fun);
    // Returns false if there was an error during execution
    bool execute();
    std::string getStackStrRepr() const;

    const std::string &getLastError() { return lastError_; }

    // Returns true on success
    template <typename... Args>
    bool setArguments(Args ...args) {
        hasError_ = false;

        if (!currFun_) {
            error(std::string(NO_FUN_PREPD));
            return false;
        }

        size_t argIdx = 0;
        // Unfold args
        return (setArgument(args, argIdx) | ...);
    }

    // Returns false on failure
    template <typename T>
    bool getFunctionReturn(T &val) {
        if (!executionFinished_) {
            error(std::string(NO_SCRIPT_EXECUTED));
            return false;
        }
        else if (currFun_->returnTy == ValueType::VOID) {
            error(std::format(FUN_HAS_VOID_RET_TY_SO_NO_VAL, currFun_->name));
            return false;
        }
        else if (hasError_)
            return false;

        ValueType givenTy = valueTypeFromLiteral(T{});
        Value top = pop();

        if (!canPromoteType(top.ty, givenTy)) {
            error(std::format(PASSED_TY_DONT_MATCH_W_PASSED_TY, currFun_->name));
            return false;
        }

        val = top.castTo<T>();
        return true;
    }

private:
    std::shared_ptr<Script> script_;
    const std::shared_ptr<ScriptContext> &ctx_; // script_'s ctx

    const ScriptFunction *currFun_ = nullptr;
    
    bool executionFinished_ = false;

    // Stack
    std::deque<Value> stack_;
    // In this implementation, the stack pointer points at the 
    // next free spot on the stack
    size_t sp_ = 0;
    std::vector<CallFrame> callStack_;

    // Computed when the vm instance is created
    std::vector<Value> globalVariables_;

    bool hasError_ = false;
    std::string lastError_;

    Value pop();
    void push(const Value &val);

    void error(const std::string &mess);

    void executeNext();

    void prepareScriptFunction(const ScriptFunction *fun);

    template <typename T>
    bool setArgument(const T &arg, size_t &idx) {
        // Stop parsing arguments if an error was encountered
        if (hasError_)
            return false;
        
        if (idx + 1 > currFun_->numParams) {
            error(std::format(TOO_MANY_ARGS, idx + 1, currFun_->name, currFun_->numParams));
            return false;
        }

        ValueType givenTy = valueTypeFromLiteral(arg);
        ValueType paramType = currFun_->paramTypes[idx];

        if (!canPromoteType(givenTy, paramType)) {
            error(std::format(ARG_DONT_MATCH_WITH_PARAM, idx, script_->ctx->getTypeName(paramType)));
            return false;
        }

        Value val{ .ty = givenTy };
        val.setProperty(arg, givenTy);

        stack_[idx] = val;
        idx++;

        return true;
    }

    // Preparation errors
    inline static constexpr std::string_view NO_FUN_PREPD                     = "Preparation error P0: No function prepared";
    inline static constexpr std::string_view TOO_MANY_ARGS                    = "Preparation error P1: Too many arguments ({} or more) for function {} ({})";
    inline static constexpr std::string_view ARG_DONT_MATCH_WITH_PARAM        = "Preparation error P3: Argument at index {} can't be converted to match parameter ({})";
    inline static constexpr std::string_view NO_SCRIPT_ATTACHED               = "Preparation error P4: No script attached to the VM";
    inline static constexpr std::string_view NO_SCRIPT_EXECUTED               = "Preparation error P5: No script executed";
    inline static constexpr std::string_view FUN_HAS_VOID_RET_TY_SO_NO_VAL    = "Preparation error P6: Function {} has void return type, so there is no return value";
    inline static constexpr std::string_view PASSED_TY_DONT_MATCH_W_PASSED_TY = "Preparation error P7: Passed type can't converted to {}'s return type";

    // Execution errors
    inline static constexpr std::string_view UNSAFE_CAST                        = "Execution error E0: Unsafe cast {} to {}";
    inline static constexpr std::string_view STACK_UNDERFLOW_EMPTY              = "Execution error E1: Stack underflow (empty stack)";
    inline static constexpr std::string_view STACK_UNDERFLOW_STACK_FRAME        = "Execution error E2: Stack underflow (below current frame)";
    inline static constexpr std::string_view STACK_OVERFLOW                     = "Execution error E3: Stack overflow";
    inline static constexpr std::string_view TRIED_ACCESSING_VAL_OF_INVALID_REF = "Execution error E4: Tried accessing the value of an invalid reference";
    inline static constexpr std::string_view INVALID_OR_CORRUPTED_BC            = "Execution error E5: Encounterd invalid or corrupted bytecode";
    inline static constexpr std::string_view CANT_INC_OR_DEC_NON_NUM            = "Execution error E6: Can't increment or decrement non numeric type";
    inline static constexpr std::string_view CANT_INVERT_NON_BOOLEAN            = "Execution error E7: Can't invert a non boolean type";
    inline static constexpr std::string_view TRYED_SETTING_VAL_OF_INVALID_REF   = "Execution error E8: Tried setting the value of an invalid reference";
};
 
} // namespace NCSC

