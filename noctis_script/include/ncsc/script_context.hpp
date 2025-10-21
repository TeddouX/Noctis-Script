// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <string>
#include <vector>
#include <functional>
#include <memory>

#include "ncsc.hpp"
#include "value.hpp"
#include "function.hpp"

namespace NCSC
{

struct GlobalCPPFunctionRepr : public IFunction {
    std::function<Value (const std::vector<Value>&)> registryFun;
};

// Context bridges the script and CPP
class NCSC_API ScriptContext {
public:
    static std::shared_ptr<ScriptContext> create();

    template <typename RetTy_, typename... FuncArgs_>
    void registerGlobalFunction(const std::string &name, RetTy_(*fun)(FuncArgs_...)) {
        GlobalCPPFunctionRepr glblFun{};
        glblFun.name = name;
        glblFun.paramTypes = { valueTypeFromCPPType<FuncArgs_>()... }; 
        glblFun.returnTy = valueTypeFromCPPType<RetTy_>();
        glblFun.numParams = sizeof...(FuncArgs_);
        glblFun.registryFun = [this, fun](const std::vector<Value> &args) {
            return this->registryFunImpl(fun, args, std::index_sequence_for<FuncArgs_...>{});
        };

        globalCPPFunctions_.push_back(glblFun);
    }

    const GlobalCPPFunctionRepr *getGlobalFunction(DWord idx) const;
    const GlobalCPPFunctionRepr *getGlobalFunction(const std::string &name) const;
    DWord getGlobalFunctionIdx(const std::string &name) const;
    
    Value callGlobalFunction(DWord idx, const std::vector<Value> &args);

private:
    std::vector<GlobalCPPFunctionRepr> globalCPPFunctions_;
    
    // Force the user to create a ptr to the class 
    // instead of a regular instance
    ScriptContext() = default;

    // Holy cpp gibberish
    
    template <typename RetTy_, typename... FuncArgs_, size_t... I>
    Value registryFunImpl(RetTy_(*fun)(FuncArgs_...), const std::vector<Value> &args, std::index_sequence<I...>) {
        if constexpr (std::is_void_v<RetTy_>) {
            // Unsafe casting but will do for now
            // TODO: Compare types before casting
            fun(args[I].castTo<FuncArgs_>()...);
            return {};
        } 
        else {
            RetTy_ ret = fun(args[I].castTo<FuncArgs_>()...);
            return Value::fromLiteral(ret);
        }
    } 
};

} // namespace NCSC
