#pragma once
#include "ast_node.hpp"


namespace NCSC
{
    
class FunctionDeclASTNode
{
public:
    explicit FunctionDeclASTNode(ASTNodeType type)
    {
        ASTNode::ASTNode(type);
    }

    
private:
    friend class SemanticAnalyzer;


};

} // namespace NCSC
