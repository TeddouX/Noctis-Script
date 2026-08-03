#pragma once
#include <cstdint>

#include "sa_decl_type.hpp"
#include "../ncsc.hpp"



namespace NCSC::SemanticAnalysis
{
    
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

    constexpr auto increase(DeclarationType decl_type) -> usize_t
    {
        switch (decl_type)
        {
            case DeclarationType::OBJECT:
                return obj_idx++;
            
            case DeclarationType::FUNCTION:
                return func_idx++;
            
            case DeclarationType::VARIABLE:
                return var_idx++;
        }
        
        return -1;
    }
};

} // namespace NCSC::SemanticAnalysis
