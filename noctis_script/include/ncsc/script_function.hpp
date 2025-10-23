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
    
struct ScriptFunction : public IFunction {
    std::vector<Byte> bytecode;
    // std::vector<Value> constants;

    DWord  numLocals = 0;
    size_t requiredStackSize = 0;
};

} // namespace NCSC
