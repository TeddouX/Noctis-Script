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
    Scope(const DeclIndices &decl_indices)
        : decl_indices_{decl_indices}
    {}

    auto set_parent(PtrRef<Scope> parent) -> void;
    auto get_parent() const -> const PtrRef<Scope> &;

    auto add_declaration(const std::string &name, DeclData &data) -> isize_t;
    auto get_declaration(const std::string &name) -> DeclData *;

private:
    PtrRef<Scope>                               parent_;
    std::unordered_map<std::string, DeclData>   declaration_data_;

    DeclIndices                                 decl_indices_;             
};
} // namespace NCSC::SemanticAnalysis
