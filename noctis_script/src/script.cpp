// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/script.hpp>

namespace NCSC
{
    
const ScriptFunction *Script::getFunction(const std::string &name) const {
    for (const auto &fun : functions_)
        if (fun.name == name)
            return &fun;
    return nullptr;
}

const ScriptFunction *Script::getFunction(DWord idx) const {
    if (idx > functions_.size())
        return nullptr;
    return &functions_[idx];
}

DWord Script::getFunctionIdx(const std::string &name) const {
    for (int i = 0; i < functions_.size(); i++)
        if (functions_[i].name == name)
            return i;

    // DWord is unsigned, this will underflow
    // compare with NCSC_INVALID_IDX
    return -1;
}

const GlobalVar *Script::getGlobalVar(DWord idx) const {
    if (idx > globalVars_.size())
        return nullptr;
    return &globalVars_[idx];
}

DWord Script::getGlobalVarIdx(const std::string &name) const {
    for (int i = 0; i < globalVars_.size(); i++)
        if (globalVars_[i].name == name)
            return i;
    return -1;
}


} // namespace NCSC
