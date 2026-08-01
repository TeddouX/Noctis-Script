#pragma once
#include "../ast_node.hpp"
#include "../../semantic_analysis/sa_value_type.hpp"


namespace NCSC::Parsing
{

class VarDeclASTNode : public ASTNode
{
public:
    explicit VarDeclASTNode(ASTNodeType type)
        : ASTNode{type}
    {}

    SemanticAnalysis::ValueType var_type;
    std::string                 var_name;
};

} // namespace NCSC::Parsing