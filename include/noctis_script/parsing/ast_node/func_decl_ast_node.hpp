#pragma once
#include "../ast_node.hpp"
#include "../../semantic_analysis/sa_value_type.hpp"


namespace NCSC::Parsing
{

class FuncDeclASTNode : public ASTNode
{
public:
    explicit FuncDeclASTNode(ASTNodeType type)
        : ASTNode{type}
    {}
    
    std::vector<std::pair<std::string, SemanticAnalysis::ValueType>>    func_params;
    SemanticAnalysis::ValueType                                         func_return_type;
    std::string                                                         func_name;
};

} // namespace NCSC::Parsing
