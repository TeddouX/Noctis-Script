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

struct GlobalCPPFunctionRepr : public Function {
    std::function<Value (const std::vector<Value>&)> registryFun;
};

struct CPPObject : public Object {

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

    GlobalCPPFunctionRepr *getGlobalFunction(DWord idx);
    GlobalCPPFunctionRepr *getGlobalFunction(const std::string &name);
    DWord getGlobalFunctionIdx(const std::string &name) const;
    
    Value callGlobalFunction(DWord idx, const std::vector<Value> &args);

    std::string getTypeName(ValueType ty) const;

private:
    friend class Compiler;

    std::vector<GlobalCPPFunctionRepr> globalCPPFunctions_;
    std::unordered_map<ValueType, std::string> typeNames_;
    
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

    void addTypeName(ValueType ty, const std::string &name) { typeNames_.emplace(ty, name); }
};

} // namespace NCSC
