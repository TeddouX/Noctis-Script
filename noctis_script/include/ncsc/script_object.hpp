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

struct Method : public ScriptFunction {
    bool isPublic = false;
};

class ScriptObject {
public:
    ScriptObject() = default;
    
    DWord numMembers;
    std::string name;
    ValueType type;

    Method *getConstructor();
    DWord   getConstructorIdx() const;

    GETTERS_SETTERS_FOR_NAMED_VECTOR(Method, methods_, Method)
    GETTERS_SETTERS_FOR_NAMED_VECTOR(Member, members_, MemberVariable)

private:
    std::vector<Method> methods_;
    std::vector<MemberVariable> members_;
};

} // namespace NCSC
