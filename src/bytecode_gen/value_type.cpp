#include "bytecode_gen/value_type.hpp"


namespace NCSC
{

// For builtin types
// PUSH 10000000 256u8

// std::string type_name = "<class std::math::vec3>"

// ValueType:
// is_object: bool (byte)
// if is_object == true:
//      typename: string (null terminated)
// else:
//      type: byte
// Example:
//      - 00000001 <class std::math::vec3>\0
//      - 00000000 00000010

auto get_vtype_rank(GenValueType vtype) -> bool
{
    switch (vtype) 
    {
        case GenValueType::BOOL:    return 0;
        case GenValueType::INT8:    return 1;
        case GenValueType::UINT8:   return 2;
        case GenValueType::INT16:   return 3;
        case GenValueType::UINT16:  return 4;
        case GenValueType::INT32:   return 5;
        case GenValueType::UINT32:  return 6;
        case GenValueType::INT64:   return 7;
        case GenValueType::UINT64:  return 8;
        case GenValueType::FLOAT32: return 9;
        case GenValueType::FLOAT64: return 10;
        default:                 return -1;
    }
}

auto vtype_has_mask(GenValueType in, GenValueType mask) -> bool 
{
    return (value_type_size_t)in & (value_type_size_t)mask;
}

auto vtype_clear_mask(GenValueType in, GenValueType mask) -> GenValueType
{
    return static_cast<GenValueType>((value_type_size_t)in & ~(value_type_size_t)mask);
}

auto vtype_set_mask(GenValueType in, GenValueType mask) -> GenValueType
{
    return static_cast<GenValueType>((value_type_size_t)in | (value_type_size_t)mask);
}

auto make_object_vtype(dword_t objIdx) -> GenValueType
{
    return vtype_set_mask(static_cast<GenValueType>(objIdx), GenValueType::OBJ_MASK);
}

auto vtype_remove_const_ref(GenValueType in) -> GenValueType
{
    return vtype_clear_mask(
        vtype_clear_mask(in, GenValueType::REF_MASK), 
        GenValueType::CONST_MASK
    );
}

} // namespace NCSC
