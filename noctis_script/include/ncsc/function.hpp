#pragma once
#include "ncsc.hpp"
#include "value_type.hpp"

#include <string>
#include <vector>

namespace NCSC
{

struct IFunction {
    std::string name;
    std::vector<ValueType> paramTypes;
    ValueType returnTy;
    size_t numParams;
};

} // namespace NCSC
