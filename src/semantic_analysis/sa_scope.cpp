#include "semantic_analysis/sa_scope.hpp"

namespace NCSC::SemanticAnalysis
{
    
auto Scope::set_parent(std::shared_ptr<Scope> parent) -> void
{
    parent_ = parent;

    if (not parent_)
        return;
    
    var_idx_ = parent_->var_idx_;
    func_idx_ = parent_->func_idx_;
    obj_idx_ = parent_->obj_idx_;
}

auto Scope::get_parent() const -> const std::shared_ptr<Scope> &
{
    return parent_;
}

auto Scope::add_declaration(const std::string &name, std::shared_ptr<DeclData> data) -> void
{
    switch (data->type)
    {
        case DeclData::Type::FUNCTION:
            data->idx = func_idx_++;
            break;
        
        case DeclData::Type::OBJECT:
            data->idx = obj_idx_++;
            break;
        
        case DeclData::Type::VARIABLE:
            data->idx = var_idx_++;
            break;
    }

    declaration_data_.emplace(name, data);
}

auto Scope::get_declaration(const std::string &name) const -> const DeclData *
{
    auto it = declaration_data_.find(name);
    if (it == declaration_data_.end())
    {
        if (parent_)
            return parent_->get_declaration(name);
        else
            return nullptr;
    }
    
    return it->second;
}

} // namespace NCSC::SemanticAnalysis
