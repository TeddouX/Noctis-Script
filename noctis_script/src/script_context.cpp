// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/script_context.hpp>

namespace NCSC
{
 
std::shared_ptr<ScriptContext> ScriptContext::create() {
    return std::shared_ptr<ScriptContext>(new ScriptContext());
}

const GlobalCPPFunctionRepr *ScriptContext::getGlobalFunction(DWord idx) const {
    if (idx > globalCPPFunctions_.size())
        return nullptr;
    return &globalCPPFunctions_[idx];
}

const GlobalCPPFunctionRepr *ScriptContext::getGlobalFunction(const std::string &name) const {
    for (const auto &fun : globalCPPFunctions_)
        if (fun.name == name)
            return &fun;
    return nullptr;
}

DWord ScriptContext::getGlobalFunctionIdx(const std::string &name) const {
    for (int i = 0; i < globalCPPFunctions_.size(); i++)
        if (globalCPPFunctions_[i].name == name)
            return i;
    return -1;
}

Value ScriptContext::callGlobalFunction(DWord idx, const std::vector<Value> &args) {
    GlobalCPPFunctionRepr &globalFun = globalCPPFunctions_[idx];
    return globalFun.registryFun(args);
}

} // namespace NCSC
