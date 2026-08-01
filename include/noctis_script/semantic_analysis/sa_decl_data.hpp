#pragma once
#include "../parsing/ast_node.hpp"
#include "sa_value_type.hpp"

namespace NCSC::SemanticAnalysis
{
    
struct DeclData
{
    virtual ~DeclData() = default;

    enum class Type {
        FUNCTION,
        VARIABLE,
        OBJECT,
    } type;

    std::shared_ptr<ASTNode> decl_node;
    bool is_error = false;
    isize_t idx = -1;
};
    
} // namespace NCSC::SemanticAnalysis