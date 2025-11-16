// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "script_node.hpp"
#include "script_function.hpp"
#include "script_context.hpp"
#include "script_object.hpp"
#include "named_utils.hpp"
#include "variable.hpp"

#include <vector>
#include <memory>

namespace NCSC
{
    
struct GlobalVar : public Variable {
    // Global's bytecode should get ran before executing any functions
    Bytecode bytecode;
    size_t requiredStackSize = 0;
};

class NCSC_API Script {
public:
    Script() = default;

    std::shared_ptr<ScriptContext> ctx;
    DWord numGlobalVariables = 0;
    
    NCSC_GETTERS_SETTERS_FOR_NAMED_VECTOR(Function, functions_, ScriptFunction)
    NCSC_GETTERS_SETTERS_FOR_NAMED_VECTOR(GlobalVariable, globalVars_, GlobalVar)
    NCSC_GETTERS_SETTERS_FOR_NAMED_VECTOR(Object, objects_, ScriptObject)

private:
    std::vector<ScriptFunction> functions_;
    std::vector<GlobalVar>      globalVars_;
    std::vector<ScriptObject>   objects_;
};

} // namespace NCSC
