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
    const std::string *name;
    
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

    const std::string &getLastError() { return lastError_; }

    // Returns true on success
    template <typename... Args>
    bool setArguments(Args... args) {
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
    std::string lastError_;

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
    inline static ErrInfo BC_ENDED_WOUT_RET = { "Execution error", "E", 0, "Bytecode ended without returning" };
    
};
 
} // namespace NCSC

