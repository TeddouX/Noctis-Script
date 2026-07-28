#include "bytecode_gen/bytecode_gen_data.hpp"


namespace NCSC::Internal
{
    
auto Scope::get_local_var_index(const std::string &name) const -> index_word_t
{
    for (std::size_t i = 0; i < local_variables.size(); i++)
    {
        const Variable &var = local_variables[i];
        if (var.name == name)
            return i;
    }

    if (parent)
        return parent->get_local_var_index(name);

    return INVALID_INDEX;
}

auto Scope::get_local_var(index_word_t idx) const -> const Variable *
{
    if (idx >= local_variables.size())
    {
        if (not parent)
            return nullptr;

        return parent->get_local_var(idx);
    }

    return &local_variables[idx];
}


} // namespace NCSC::Internal
