#pragma once
#include "ncsc.hpp"
#include "instructions.hpp"

#include <functional>
#include <array>

namespace NCSC
{

struct OptimizationRule {
    std::function<bool (std::vector<Byte>&, size_t&)> rule;
};

class NCSC_API Optimizer {
public:
    Optimizer(const std::vector<Byte> &bc)
        : bc_(bc) {}

    std::vector<Byte> optimizeAll();

private:
    const std::vector<Byte> &bc_;

    static bool constantFolding(std::vector<Byte> &bc, size_t &idx);
    static bool collapseLoadPopSetObjToStoreLocal(std::vector<Byte> &bc, size_t &idx);

    static inline const std::array<OptimizationRule, 2> rules_ = {
        OptimizationRule{ constantFolding }, OptimizationRule{ collapseLoadPopSetObjToStoreLocal }
    };
};

} // namespace NCSC
