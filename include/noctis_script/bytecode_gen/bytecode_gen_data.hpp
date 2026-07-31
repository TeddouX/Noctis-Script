#pragma once
#include <vector>
#include <string>
#include <unordered_map>

#include "value_type.hpp"
#include "bytecode.hpp"


namespace NCSC::Internal
{
    
enum class AccessModifier
{
    PUBLIC,
    PRIVATE
};

struct Variable
{
    std::string     name;
    GenValueType    type;
    Location        defined_at;
};

struct GlobalVariable : public Variable
{
    Bytecode bytecode;
};

struct Function
{
    std::string             name;
    std::vector<Variable>   params;
    GenValueType            return_type;
    Location                defined_at;

    bool is_method = false;
    AccessModifier access_mod;

    Bytecode                bytecode;
};

struct MemberVariable : public Variable
{
    AccessModifier access_mod;
};

struct Object
{
    std::string                                     name;
    GenValueType                                    type;
    std::vector<MemberVariable>                     member_variables;
    std::unordered_map<std::string, std::size_t>    method_offsets;
    Location                                        defined_at;
};

struct Scope
{
    Scope *parent = nullptr;

    std::vector<Variable> local_variables;

    auto get_local_var_index(const std::string &name) const -> isize_t;
    auto get_local_var(index_word_t idx) const -> const Variable *;
};

} // namespace NCSC::Internal
