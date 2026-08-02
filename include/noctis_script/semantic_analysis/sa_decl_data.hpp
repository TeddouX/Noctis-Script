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
    } decl_type;

    std::string     name;
    ValueType       type;
    PtrRef<ASTNode> decl_node;
    bool            is_error = false;
    isize_t         idx = -1;
};

struct DeclIndices
{
    std::size_t obj_idx{0};
    std::size_t func_idx{0};
    std::size_t var_idx{0};

    constexpr DeclIndices(
        std::size_t var_idx = 0, 
        std::size_t func_idx = 0,
        std::size_t obj_idx = 0)
        : obj_idx{obj_idx}
        , func_idx{func_idx}
        , var_idx{var_idx}
    {}

    constexpr auto increase(DeclData::Type decl_type) -> usize_t
    {
        switch (decl_type)
        {
            case DeclData::Type::OBJECT:
                return obj_idx++;
            
            case DeclData::Type::FUNCTION:
                return func_idx++;
            
            case DeclData::Type::VARIABLE:
                return var_idx++;
        }
        
        return -1;
    }
};
    
} // namespace NCSC::SemanticAnalysis