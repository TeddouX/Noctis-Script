#pragma once
#include "../ast_node.hpp"
#include "../../semantic_analysis/sa_value_type.hpp"


namespace NCSC::Parsing
{

class ObjDeclASTNode : public ASTNode
{
public:
    explicit ObjDeclASTNode(ASTNodeType type)
        : ASTNode{type}
    {}

    SemanticAnalysis::ValueType obj_type;
    std::string                 obj_name;
};

} // namespace NCSC::Parsing