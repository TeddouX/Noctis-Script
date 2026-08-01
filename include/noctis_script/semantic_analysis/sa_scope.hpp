#pragma once
#include <memory>
#include <unordered_map>
#include <string>

#include "sa_decl_data.hpp"


namespace NCSC::SemanticAnalysis
{
    
class Scope
{
public:
    Scope() = default;

    auto set_parent(std::shared_ptr<Scope> parent) -> void;
    auto get_parent() const -> const std::shared_ptr<Scope> &;

    auto add_declaration(const std::string &name, DeclData data) -> void;
    auto get_declaration(const std::string &name) const -> const DeclData *;

private:
    std::shared_ptr<Scope>                      parent_;
    std::unordered_map<std::string, DeclData>   declaration_data_;

    std::size_t                                 var_idx_{};
    std::size_t                                 func_idx_{};
    std::size_t                                 obj_idx_{};
};
    
} // namespace NCSC::SemanticAnalysis
