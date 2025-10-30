// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"
#include "value_type.hpp"
#include "variable.hpp"

#include <string>
#include <vector>

namespace NCSC
{
    
struct NCSC_API Scope {
    Scope *parent = nullptr;
    bool hasReturned = false;

    std::vector<Variable> localVariables;

    void addLocalVar(const Variable &var) { localVariables.push_back(var); }
    bool hasLocalVar(const std::string &name) { return getLocalVar(name) != nullptr; }
    Variable *getLocalVar(const std::string &name);
    Variable *getLocalVar(size_t idx);
    DWord getLocalVarIdx(const std::string &name);
};

} // namespace NCSC
