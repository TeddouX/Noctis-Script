// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <vector>
#include <string>

#include "ncsc.hpp"
#include "value_type.hpp"
#include "function.hpp"
#include "bytecode.hpp"

namespace NCSC
{
    
struct ScriptFunction : public virtual Function {
    Bytecode bytecode;
    // Locals + Parameters
    DWord numLocals = 0;
    
    // Required stack size to run the function in the VM
    // Not counting locals
    size_t requiredStackSize = 0;
};

} // namespace NCSC
