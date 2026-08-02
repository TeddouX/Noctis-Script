#include "semantic_analysis/sa_scope.hpp"

#include "semantic_analysis/module_data.hpp"


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

// Local declarations
// Parent declarations
// Using alias
// Using module
auto Scope::get_declaration(const std::string &name) const -> std::vector<std::pair<PtrRef<ModuleData>, const DeclData *>>
{
    auto local_decl = declaration_data_.find(name);
    if (local_decl != declaration_data_.end())
        return { { nullptr, &local_decl->second } };

    if (parent_)
    {
        auto parent_decl = parent_->get_declaration(name);
        if (not parent_decl.empty())
            return parent_decl;
    }

    auto using_alias = using_aliases.find(name);
    if (using_alias != using_aliases.end())
        return { { nullptr, &using_alias->second } };

    std::vector<std::pair<PtrRef<ModuleData>, const DeclData *>> candidates;
    for (auto module : used_modules)
    {
        if (auto exported_decl = module->find_exported_symbol(name))
            candidates.push_back({ module, exported_decl });
    }

    return candidates;

}

} // namespace NCSC::SemanticAnalysis
