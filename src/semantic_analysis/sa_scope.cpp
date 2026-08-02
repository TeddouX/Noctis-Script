#include "semantic_analysis/sa_scope.hpp"

namespace NCSC::SemanticAnalysis
{
    
auto Scope::set_parent(PtrRef<Scope> parent) -> void
{
    parent_ = parent;

    if (not parent_)
        return;
    
    decl_indices_ = parent_->decl_indices_;
}

auto Scope::get_parent() const -> const PtrRef<Scope> &
{
    return parent_;
}

auto Scope::add_declaration(const std::string &name, DeclData &data) -> isize_t
{
    data.idx = decl_indices_.increase(data.decl_type);
    declaration_data_.emplace(name, data);

    return data.idx;
}

auto Scope::get_declaration(const std::string &name) -> DeclData *
{
    auto it = declaration_data_.find(name);
    if (it == declaration_data_.end())
    {
        if (parent_)
            return parent_->get_declaration(name);
        else
            return nullptr;
    }
    
    return &it->second;
}

} // namespace NCSC::SemanticAnalysis
