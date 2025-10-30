// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/scope.hpp>

namespace NCSC
{

Variable *Scope::getLocalVar(const std::string &name) {
    for (auto &var : localVariables) {
        if (var.name == name)   
            return &var;
    }
    if (parent)
        return parent->getLocalVar(name);
    return nullptr;
}

DWord Scope::getLocalVarIdx(const std::string &name) {
    for (DWord i = 0; i < localVariables.size(); i++) {
        if (localVariables[i].name == name) {
            if (parent)
                return parent->localVariables.size() + i;
            return i;
        }
    }
    if (parent)
        return parent->getLocalVarIdx(name);
    return -1;
}

Variable *Scope::getLocalVar(size_t idx) {
    if (idx >= localVariables.size()) {
        if (!parent)
            return nullptr;
        return parent->getLocalVar(idx - localVariables.size());
    }

    return &localVariables[idx];
}

} // namespace NCSC
