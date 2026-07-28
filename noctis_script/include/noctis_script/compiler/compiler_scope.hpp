#pragma once 
#include <vector>
#include <string>

#include "value_type.hpp"
#include "compiler_variable.hpp"
#include "../ncsc.hpp"


namespace NCSC::CompilerData
{

struct Scope
{
    Scope *parent = nullptr;

    std::vector<Variable> local_variables;

    auto get_local_var_index(const std::string &name) const -> index_word_t;
    auto get_local_var(index_word_t idx) const -> const Variable *;
};

} // namespace NCSC::CompilerData
