#pragma once
#include "../ast_node.hpp"
#include "../../semantic_analysis/sa_value_type.hpp"


namespace NCSC::Parsing
{

class UsingStmtASTNode : public ASTNode
{
public:
    UsingStmtASTNode()
        : ASTNode{ASTNodeType::USING_STMT}
    {}

    bool is_using_module = false;
};

} // namespace NCSC::Parsing
