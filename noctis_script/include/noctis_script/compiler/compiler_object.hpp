#pragma once
#include <vector>

#include "compiler_variable.hpp"
#include "compiler_function.hpp"


namespace NCSC::CompilerData
{

enum class AccessModifier
{
    PUBLIC,
    PRIVATE
};

struct MemberVariable : public Variable
{
    AccessModifier access_mod;
};

struct Method : public Function
{
    AccessModifier access_mod;  
};

struct Object
{
    std::string name;
    std::vector<MemberVariable> member_variables;
    std::vector<Method> methods;
};

} // namespace NCSC::CompilerData
