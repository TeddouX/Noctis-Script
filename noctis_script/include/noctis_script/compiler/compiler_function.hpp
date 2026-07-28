#pragma once
#include <string>
#include <vector>

#include "value_type.hpp"

namespace NCSC::CompilerData
{
    
struct Function
{
    std::string name;
    std::vector<ValueType> param_types;
    ValueType return_ty;
};

} // namespace NCSC::CompilerData
