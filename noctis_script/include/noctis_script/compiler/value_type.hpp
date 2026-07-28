#pragma once
#include "../ncsc.hpp"


namespace NCSC
{

using value_type_size_t = dword_t;

constexpr std::size_t VALUE_TYPE_SIZE_BITS = sizeof(value_type_size_t) * 8;

enum class ValueType : value_type_size_t 
{
    INVALID,

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

    REF_MASK     = (value_type_size_t)1 << VALUE_TYPE_SIZE_BITS - 2,
    OBJ_MASK     = (value_type_size_t)1 << VALUE_TYPE_SIZE_BITS - 1,
};

} // namespace NCSC
