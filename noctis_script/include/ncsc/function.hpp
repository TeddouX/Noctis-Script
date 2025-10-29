// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"
#include "value_type.hpp"

#include <string>
#include <vector>

namespace NCSC
{

struct Function {
    std::string name;
    std::vector<ValueType> paramTypes;
    ValueType returnTy;
    size_t numParams;
};

} // namespace NCSC
