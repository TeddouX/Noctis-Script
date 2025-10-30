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

    GETTERS_SETTERS_FOR_NAMED_VECTOR(LocalVar, localVariables, Variable)
};

} // namespace NCSC
