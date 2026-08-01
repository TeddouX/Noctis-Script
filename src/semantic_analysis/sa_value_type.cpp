#include "semantic_analysis/sa_value_type.hpp"


namespace NCSC::SemanticAnalysis
{

auto vtype_get_rank(ValueType vtype) -> bool
{
    switch (vtype) 
    {
        case ValueType::BOOL:    return 0;
        case ValueType::INT8:    return 1;
        case ValueType::UINT8:   return 2;
        case ValueType::INT16:   return 3;
        case ValueType::UINT16:  return 4;
        case ValueType::INT32:   return 5;
        case ValueType::UINT32:  return 6;
        case ValueType::INT64:   return 7;
        case ValueType::UINT64:  return 8;
        case ValueType::FLOAT32: return 9;
        case ValueType::FLOAT64: return 10;
        default:                 return -1;
    }
}

auto vtype_has_mask(ValueType in, ValueType mask) -> bool 
{
    return (value_type_size_t)in & (value_type_size_t)mask;
}

auto vtype_clear_mask(ValueType in, ValueType mask) -> ValueType
{
    return static_cast<ValueType>((value_type_size_t)in & ~(value_type_size_t)mask);
}

auto vtype_set_mask(ValueType in, ValueType mask) -> ValueType
{
    return static_cast<ValueType>((value_type_size_t)in | (value_type_size_t)mask);
}

auto make_object_vtype(dword_t objIdx) -> ValueType
{
    return vtype_set_mask(static_cast<ValueType>(objIdx), ValueType::OBJ_MASK);
}

auto vtype_remove_const_ref(ValueType in) -> ValueType
{
    return vtype_clear_mask(
        vtype_clear_mask(in, ValueType::REF_MASK), 
        ValueType::CONST_MASK
    );
}

bool vtype_is_float(ValueType ty) 
{
    return ty == ValueType::FLOAT32 || ty == ValueType::FLOAT64;
}

bool vtype_is_int(ValueType ty) 
{
    return ty == ValueType::INT8   or
           ty == ValueType::INT16  or
           ty == ValueType::INT32  or
           ty == ValueType::INT64  or
           ty == ValueType::UINT8  or
           ty == ValueType::UINT16 or
           ty == ValueType::UINT32 or
           ty == ValueType::UINT64;
}

auto vtype_is_unsigned_int(ValueType type) -> bool
{
    return type == ValueType::UINT8  or
           type == ValueType::UINT16 or
           type == ValueType::UINT32 or
           type == ValueType::UINT64;
}

bool vtype_is_numeric(ValueType ty) 
{
    return vtype_is_int(ty) || vtype_is_float(ty);
}

auto vtype_is_const(ValueType type) -> bool
{
    return vtype_has_mask(type, ValueType::CONST_MASK);
}

auto vtype_is_object(ValueType type) -> bool
{
    return vtype_has_mask(type, ValueType::OBJ_MASK);
}

} // namespace NCSC::SemanticAnalysis
