// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <string>
#include "value_type.hpp"

namespace NCSC
{
    
struct Variable {
    std::string name;
    ValueType type;

    virtual ~Variable() = default;
};

} // namespace NCSC
