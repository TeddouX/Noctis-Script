#pragma once
#include "../ast_node.hpp"
#include "../scoped_path.hpp"
#include "../../semantic_analysis/sa_value_type.hpp"


namespace NCSC::Parsing
{

class ScopedIdentifierASTNode : public ASTNode
{
public:
    explicit ScopedIdentifierASTNode(ASTNodeType type)
        : ASTNode{type}
    {}

    ScopedPath path;
};

} // namespace NCSC::Parsing