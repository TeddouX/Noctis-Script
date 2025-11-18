// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "function.hpp"
#include "script.hpp"
#include "value.hpp"
#include "garbage_collector.hpp"
#include "error.hpp"

#include <memory>
#include <deque>
#include <typeinfo>
#include <print>

namespace NCSC
{

struct CallFrame {
    const Bytecode *bytecode;
    union {
        const ScriptFunction *func = nullptr;
        const GlobalVar *gv;
    };
    bool isFunction = true;

    size_t bp = 0;
    DWord numLocals = 0;

    QWord  ip = 0;
    
    size_t sp = 0;
    
    size_t stackSize = 0;
};

class NCSC_API VM {
public:
    explicit VM(std::shared_ptr<Script> script, std::shared_ptr<GarbageCollector> gc = nullptr);

    bool computeGlobals();

    void prepareFunction(const ScriptFunction *fun);
    // Returns false if there was an error during execution
    bool execute();
    std::string getStackStrRepr() const;

    const Error &getLastError() { return lastError_; }

    // Returns true on success
    template <typename... Args>
    bool setArguments(Args... args) {
        hasError_ = false;

        if (!currFun_) {
            error(NO_FUN_PREPD, nullptr, 0);
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
            error(NO_SCRIPT_EXECUTED, nullptr, 0);
            return false;
        }
        else if (currFun_->returnTy == ValueType::VOID) {
            error(FUN_HAS_VOID_RET_TY_SO_NO_VAL.format(currFun_->name), nullptr, 0);
            return false;
        }
        else if (hasError_)
            return false;

        ValueType givenTy = valueTypeFromLiteral(T{});
        Value top = pop();

        if (!canPromoteType(top.ty, givenTy)) {
            error(PASSED_TY_DONT_MATCH_W_PASSED_TY.format(currFun_->name), nullptr, 0);
            return false;
        }

        val = top.castTo<T>();
        return true;
    }

private:
    inline static GarbageCollectorConfig DEFAULT_GC_CONF_ {
        .gcStartThreshold = 6,
        .gcStartThresholhGrowthFactor = 2.f,
        .majorGCTreshold = 3,
        .majorGCThresholdGrowthFactor = 2.f,
    };

    std::shared_ptr<Script> script_;
    const std::shared_ptr<ScriptContext> &ctx_; // script_'s ctx
    std::shared_ptr<GarbageCollector> garbageCollector_;

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
    Error lastError_;

    Value pop();
    void push(const Value &val);

    void error(const ErrInfo &errInfo, const Bytecode *bc, size_t ip);

    void executeNext();

    void prepareScriptFunction(const ScriptFunction *fun);
    Value makeReference(Value &val);

    template <typename T>
    bool setArgument(const T &arg, size_t &idx) {
        // Stop parsing arguments if an error was encountered
        if (hasError_)
            return false;
        
        if (idx + 1 > currFun_->numParams) {
            error(TOO_MANY_ARGS.format(idx + 1, currFun_->name, currFun_->numParams), nullptr, 0);
            return false;
        }

        ValueType givenTy = valueTypeFromLiteral(arg);
        ValueType paramType = currFun_->paramTypes[idx];

        if (!canPromoteType(givenTy, paramType)) {
            error(ARG_DONT_MATCH_WITH_PARAM.format(idx, script_->ctx->getTypeName(paramType)), nullptr, 0);
            return false;
        }

        Value val{ .ty = givenTy };
        val.setProperty(&arg, givenTy);

        stack_[idx] = val;
        idx++;

        return true;
    }

    // Preparation errors
    inline static ErrInfo NO_FUN_PREPD                     = { "Preparation error", "P", 0, "No function prepared" };
    inline static ErrInfo TOO_MANY_ARGS                    = { "Preparation error", "P", 1, "Too many arguments ({} or more) for function {} ({})" };
    inline static ErrInfo ARG_DONT_MATCH_WITH_PARAM        = { "Preparation error", "P", 3, "Argument at index {} can't be converted to match parameter ({})" };
    inline static ErrInfo NO_SCRIPT_ATTACHED               = { "Preparation error", "P", 4, "No script attached to the VM" };
    inline static ErrInfo NO_SCRIPT_EXECUTED               = { "Preparation error", "P", 5, "No script executed" };
    inline static ErrInfo FUN_HAS_VOID_RET_TY_SO_NO_VAL    = { "Preparation error", "P", 6, "Function {} has void return type, so there is no return value" };
    inline static ErrInfo PASSED_TY_DONT_MATCH_W_PASSED_TY = { "Preparation error", "P", 7, "Passed type can't converted to {}'s return type" };

    // Execution errors
    inline static ErrInfo BC_ENDED_WOUT_RET = { "Execution error", "E", 0, "Bytecode ended without returning (in {})" };
    inline static ErrInfo TRIED_ACCESING_MEMB_OF_INV_OBJ = { "Execution error", "E", 1, "Accessing a member of an invalid object (in {})" };
    inline static ErrInfo TRIED_ACCESSING_MEMB_OF_NULL = { "Execution error", "E", 2, "Accessing a member of an object that is null (in {})" };
    inline static ErrInfo CANT_INC_OR_DEC_NOM_NUM = { "Execution error", "E", 3, "Can't increment or decrement a non numeric or uninitialized value (in {})" };
    inline static ErrInfo CANT_INVERT_NON_BOOLEAN = { "Execution error", "E", 4, "Can't invert a non boolean or uninitialized value (in {})" };
    inline static ErrInfo INVALID_OR_CORRUPTED_BC = { "Execution error", "E", 5, "Encountered invalid or corrupted bytecode (in {})" };
    inline static ErrInfo STACK_UNDERFLOW_EMPTY = { "Execution error", "E", 6, "Stack underflow (empty stack) (in {})" };
    inline static ErrInfo STACK_UNDERFLOW_STACK_FRAME = { "Execution error", "E", 7, "Stack underflow (below current frame) (in {})" };
    inline static ErrInfo STACK_OVERFLOW = { "Execution error", "E", 8, "Stack overflow (in {})" };
    inline static ErrInfo TRIED_CALLING_METHTOD_OF_NULL = { "Execution error", "E", 9, "Calling a method of an object that is null (in {})" };
    
};
 
} // namespace NCSC

