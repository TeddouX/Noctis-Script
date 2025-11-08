// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"
#include "object.hpp"

namespace NCSC
{

struct ScriptMethod : public ScriptFunction {
    bool isPublic = false;
};

class ScriptObject : private Object {
public:
    ScriptObject() = default;
    
    DWord numMembers = 0;
    std::string name;
    ValueType type = ValueType::INVALID;

    ScriptMethod *getConstructor();
    DWord   getConstructorIdx() const;

    GETTERS_SETTERS_FOR_NAMED_VECTOR(Method, methods_, ScriptMethod)
    GETTERS_SETTERS_FOR_NAMED_VECTOR(Member, members, MemberVariable)

private:
    std::vector<ScriptMethod> methods_;
};

} // namespace NCSC
