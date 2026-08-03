#pragma once
#include "../parsing/ast_node.hpp"
#include "sa_value_type.hpp"
#include "sa_decl_type.hpp"

namespace NCSC::SemanticAnalysis
{

enum class AccessModifier
{
    PUBLIC,
    PRIVATE,
};

struct DeclData
{
    virtual ~DeclData() = default;

    DeclarationType decl_type;
    std::string     name;
    ValueType       type;
    PtrRef<ASTNode> decl_node;
    bool            is_error = false;
    AccessModifier  access_mod = AccessModifier::PRIVATE;
    isize_t         idx = -1;
};
    
} // namespace NCSC::SemanticAnalysis