#pragma once
#include "../ast_node.hpp"
#include "../../semantic_analysis/sa_value_type.hpp"


namespace NCSC::Parsing
{

class VarDeclASTNode : public ASTNode
{
public:
    VarDeclASTNode()
        : ASTNode{ASTNodeType::VARIABLE_DECLARATION}
    {}

    bool                        parser_is_member = false;

    SemanticAnalysis::ValueType var_type;
    std::string                 name;
};

} // namespace NCSC::Parsing