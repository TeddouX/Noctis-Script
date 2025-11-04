// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <vector>
#include <string>

#include "ncsc.hpp"
#include "value_type.hpp"
#include "function.hpp"

namespace NCSC
{
    
struct ScriptFunction : public Function {
    std::vector<Byte> bytecode;
    // Locals + Parameters
    DWord  numLocals = 0;
    
    // Required stack size to run the function in the VM
    // not counting locals
    size_t requiredStackSize = 0;
};

} // namespace NCSC
