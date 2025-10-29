// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"
#include "script_function.hpp"
#include "variable.hpp"

namespace NCSC
{
    
struct MemberVariable : public Variable {
    bool isPublic = false;
};

struct ScriptObject {
    std::string name;
    std::vector<ScriptFunction> functions;
    std::vector<MemberVariable> members;
    DWord numMembers;
};

} // namespace NCSC
