// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"
#include "named_utils.hpp"
#include "object.hpp"
#include "script_function.hpp"
#include "variable.hpp"

#include <type_traits>

namespace NCSC
{

struct ScriptMethod : public Method, public ScriptFunction {};

class ScriptObject : public Object {
public:
    ScriptObject() = default;
    
    DWord numMembers = 0;

    ScriptMethod *getConstructor();
    DWord         getConstructorIdx() const;

    NCSC_GETTERS_SETTERS_FOR_NAMED_VECTOR(Method, methods_, ScriptMethod)
    NCSC_GETTERS_SETTERS_FOR_NAMED_VECTOR(Member, members_, Member)

private:
    std::vector<Member> members_;
    std::vector<ScriptMethod> methods_;
};

} // namespace NCSC
