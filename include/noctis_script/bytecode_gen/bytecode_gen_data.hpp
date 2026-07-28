#pragma once
#include <vector>
#include <string>

#include "value_type.hpp"


namespace NCSC::Internal
{
    
enum class AccessModifier
{
    PUBLIC,
    PRIVATE
};

struct Variable
{
    std::string name;
    ValueType type;
};

struct Function
{
    std::string name;
    std::vector<ValueType> param_types;
    ValueType return_ty;
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

struct Scope
{
    Scope *parent = nullptr;

    std::vector<Variable> local_variables;

    auto get_local_var_index(const std::string &name) const -> index_word_t;
    auto get_local_var(index_word_t idx) const -> const Variable *;
};

} // namespace NCSC::Internal
