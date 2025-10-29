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

    void addLocalVar(const std::string &name, ValueType ty);
    bool hasLocalVar(const std::string &name);
    Variable *getLocalVar(const std::string &name);
    DWord     getLocalVarIdx(const std::string &name);
    Variable *getLocalVar(size_t idx);
};

} // namespace NCSC
