#pragma once
#include <string>

#include "value_type.hpp"


namespace NCSC::CompilerData
{
    
struct Variable
{
    std::string name;
    ValueType type;
};

} // namespace NCSC::CompilerData
