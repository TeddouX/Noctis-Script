#pragma once
#include "ast_node.hpp"
#include "../semantic_analysis/sa_value_type.hpp"


namespace NCSC { class SemanticAnalyzer; }


namespace NCSC::Parsing
{

class FuncDeclASTNode : public ASTNode
{
public:
    explicit FuncDeclASTNode(ASTNodeType type)
        : ASTNode{type}
    {}
    
    std::vector<std::pair<std::string, SemanticAnalysis::ValueType>>    params;
    SemanticAnalysis::ValueType                                         return_type;
    std::string                                                         name;
};

} // namespace NCSC::Parsing
