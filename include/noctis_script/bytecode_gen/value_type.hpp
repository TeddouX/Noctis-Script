#pragma once
#include <unordered_map>
#include <string>

#include "../ncsc.hpp"


namespace NCSC
{

using value_type_size_t = dword_t;

constexpr std::size_t VALUE_TYPE_SIZE_BITS = sizeof(value_type_size_t) * 8;

enum class GenValueType : value_type_size_t 
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
    NUMERIC, // Only used by the compiler

    CONST_MASK  = (value_type_size_t)1 << VALUE_TYPE_SIZE_BITS - 3,
    REF_MASK    = (value_type_size_t)1 << VALUE_TYPE_SIZE_BITS - 2,
    OBJ_MASK    = (value_type_size_t)1 << VALUE_TYPE_SIZE_BITS - 1,
};

auto get_vtype_rank(GenValueType vtype) -> bool;
auto vtype_has_mask(GenValueType in, GenValueType mask) -> bool;
auto vtype_clear_mask(GenValueType in, GenValueType mask) -> GenValueType;
auto vtype_set_mask(GenValueType in, GenValueType mask) -> GenValueType;
auto vtype_remove_const_ref(GenValueType in) -> GenValueType;

auto make_object_vtype(dword_t objIdx) -> GenValueType;

const std::unordered_map<GenValueType, std::string> BUILTIN_VALUE_TYPES_NAMES = 
{
    { GenValueType::INVALID,   "invalid"   },
    { GenValueType::VOID,      "void"      },
    { GenValueType::INT8,      "int8"      },
    { GenValueType::INT16,     "int16"     },
    { GenValueType::INT32,     "int32"     },
    { GenValueType::INT64,     "int64"     },
    { GenValueType::UINT8,     "uint8"     },
    { GenValueType::UINT16,    "uint16"    },
    { GenValueType::UINT32,    "uint32"    },
    { GenValueType::UINT64,    "uint64"    },
    { GenValueType::FLOAT32,   "float32"   },
    { GenValueType::FLOAT64,   "float64"   },
    { GenValueType::BOOL,      "bool"      },
    { GenValueType::NUMERIC,   "numeric"   },
};

} // namespace NCSC
