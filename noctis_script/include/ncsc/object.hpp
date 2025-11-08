// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"
#include "function.hpp"
#include "variable.hpp"

namespace NCSC
{

struct MemberVariable : public Variable {
    bool isPublic = false;
};

struct Object {
    std::vector<MemberVariable> members;
};

} // namespace NCSC
