#pragma once
#include "ast_node.hpp"
#include "../semantic_analysis/sa_value_type.hpp"


namespace NCSC { class SemanticAnalyzer; }


namespace NCSC::Parsing
{

class ObjDeclASTNode : public ASTNode
{
public:
    explicit ObjDeclASTNode(ASTNodeType type)
        : ASTNode{type}
    {}

    SemanticAnalysis::ValueType type;
    std::string                 name;
};

} // namespace NCSC::Parsing