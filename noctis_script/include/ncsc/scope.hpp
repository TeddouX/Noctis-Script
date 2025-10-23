// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"
#include "value_type.hpp"

#include <string>
#include <vector>

namespace NCSC
{
    
struct NCSC_API Scope {
    Scope *parent = nullptr;
    bool hasReturned = false;

    struct Var {
        std::string name;
        ValueType type;
    };
    std::vector<Var> localVariables;

    void addLocalVar(const std::string &name, ValueType ty);
    bool hasLocalVar(const std::string &name);
    Var *getLocalVar(const std::string &name);
    DWord getLocalVarIdx(const std::string &name);
    Var *getLocalVar(size_t idx);
};

} // namespace NCSC
