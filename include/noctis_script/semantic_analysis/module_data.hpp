#pragma once
#include <unordered_map>
#include <unordered_set>

#include "../parsing/ast_node.hpp"
#include "../parsing/scoped_path.hpp"
#include "sa_scope.hpp"
#include "sa_value_type.hpp"


namespace NCSC::SemanticAnalysis
{
    
struct ModuleData
{
    bool                                is_module = false;
    Parsing::ScopedPath                 path;
    TypeErased<ASTNode>                 root_node; 
    PtrRef<Scope>                       root_scope;
    
    std::unordered_map<
        Parsing::ScopedPath,
        ValueType>                      type_table;

    std::vector<PtrRef<ModuleData>>     imported_modules;
    std::vector<DeclData>               exported_symbols;

    auto find_exported_symbol(const std::string &symbol_name) const -> const DeclData *;
    auto find_imported_symbol(const Parsing::ScopedPath &symbol_path) const -> const DeclData *;

    auto find_imported_module(const Parsing::ScopedPath &module_path) const -> PtrRef<ModuleData>;

    auto search_local_type(const Parsing::ScopedPath &type_path) const -> ValueType;
    auto search_type(const Parsing::ScopedPath &type_path) const -> ValueType;
};

} // namespace NCSC::SemanticAnalysis
