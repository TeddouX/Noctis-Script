#pragma once
#include <unordered_map>

#include "../parsing/ast_node.hpp"
#include "../parsing/scoped_path.hpp"
#include "sa_scope.hpp"
#include "sa_value_type.hpp"


namespace NCSC::SemanticAnalysis
{
    
struct ModuleData
{
    Parsing::ScopedPath                     path;
    TypeErased<ASTNode>                     root_node; 
    PtrRef<Scope>                           root_scope;
    
    std::unordered_map<
        Parsing::ScopedPath,
        SemanticAnalysis::ValueType>        type_table;

    std::vector<PtrRef<ModuleData>>         imported_modules;
    std::vector<SemanticAnalysis::DeclData> exported_symbols;
};

} // namespace NCSC::SemanticAnalysis
