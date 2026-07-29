#pragma once
#include "../ncsc.hpp"

namespace NCSC
{
    
using builtin_type_size_t = byte_t;

enum class BuiltinType : builtin_type_size_t 
{
    OBJ_NULL,
    VOID,
    
    INT8,
    INT16,
    INT32,
    INT64,

    UINT8,
    UINT16,
    UINT32,
    UINT64,
    
    FLOAT32,
    FLOAT64,
    
    BOOL,
};

} // namespace NCSC
