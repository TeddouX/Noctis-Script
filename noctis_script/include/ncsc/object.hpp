// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "variable.hpp"
#include "function.hpp"
#include "named_utils.hpp"

namespace NCSC
{
    
struct Member : public virtual Variable {
    bool isPublic = false;
};

struct Method : public virtual Function {
    bool isPublic = false;
};

struct Object {
    ValueType type = ValueType::INVALID;
    std::string name;

    virtual ~Object() = default;
};

} // namespace NCSC
