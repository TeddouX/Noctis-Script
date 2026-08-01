#pragma once
#include <unordered_map>
#include <string>

#include "../ncsc.hpp"


namespace NCSC::SemanticAnalysis
{

using value_type_size_t = dword_t;

constexpr std::size_t VALUE_TYPE_SIZE_BITS = sizeof(value_type_size_t) * 8;

enum class ValueType : value_type_size_t 
{
    ERROR_TYPE,

    VOID,
    
    NULL_OBJ,
    
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
    
    ANY,

    CONST_MASK  = (value_type_size_t)1 << VALUE_TYPE_SIZE_BITS - 3,
    REF_MASK    = (value_type_size_t)1 << VALUE_TYPE_SIZE_BITS - 2,
    OBJ_MASK    = (value_type_size_t)1 << VALUE_TYPE_SIZE_BITS - 1,
};

auto vtype_get_rank(ValueType type) -> bool;
auto vtype_has_mask(ValueType in, ValueType mask) -> bool;
auto vtype_clear_mask(ValueType in, ValueType mask) -> ValueType;
auto vtype_set_mask(ValueType in, ValueType mask) -> ValueType;
auto vtype_remove_const_ref(ValueType in) -> ValueType;

auto vtype_is_int(ValueType type) -> bool;
auto vtype_is_unsigned_int(ValueType type) -> bool;
auto vtype_is_float(ValueType type) -> bool;
auto vtype_is_numeric(ValueType type) -> bool;

auto vtype_is_const(ValueType type) -> bool;
auto vtype_is_object(ValueType type) -> bool;

auto make_object_vtype(dword_t objIdx) -> ValueType;

const std::unordered_map<ValueType, std::string> BUILTIN_VALUE_TYPES_NAMES = 
{
    { ValueType::ERROR_TYPE, "error_type"},
    { ValueType::VOID,       "void"      },
    { ValueType::INT8,       "int8"      },
    { ValueType::INT16,      "int16"     },
    { ValueType::INT32,      "int32"     },
    { ValueType::INT64,      "int64"     },
    { ValueType::UINT8,      "uint8"     },
    { ValueType::UINT16,     "uint16"    },
    { ValueType::UINT32,     "uint32"    },
    { ValueType::UINT64,     "uint64"    },
    { ValueType::FLOAT32,    "float32"   },
    { ValueType::FLOAT64,    "float64"   },
    { ValueType::BOOL,       "bool"      },
};
    
} // namespace NCSC::SemanticAnalysis
