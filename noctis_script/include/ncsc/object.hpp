// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "variable.hpp"
#include "function.hpp"

namespace NCSC
{
    
struct Member : public virtual Variable {
    bool isPublic = false;
};

struct Method : public virtual Function {
    bool isPublic = false;
};

struct Object {
    std::string name;
    std::vector<Method> methods;
    std::vector<Member> members;

    virtual ~Object() = default;
};

} // namespace NCSC
