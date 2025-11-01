// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "ncsc.hpp"
#include "instructions.hpp"

#include <functional>
#include <array>

namespace NCSC
{

class NCSC_API Optimizer {
public:
    static void optimize(std::vector<Byte> &bc);

private:
    static bool constantFolding(std::vector<Byte> &bc, size_t &idx);

    struct Rule {
        std::function<bool (std::vector<Byte>&, size_t&)> rule;
    };

    static inline const std::array<Rule, 1> rules_ = {
        Rule{ constantFolding }
    };
};

} // namespace NCSC
