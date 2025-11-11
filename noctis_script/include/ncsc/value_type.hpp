// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <unordered_map>
#include "ncsc.hpp"
#include "token.hpp"

namespace NCSC
{

typedef DWord VTypeWord;

enum class ValueType : VTypeWord {
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

    REF_MASK     = (VTypeWord)1 << 29,
    OBJ_MASK     = (VTypeWord)1 << 30,
    CPP_OBJ_MASK = (VTypeWord)1 << 31,
};

const std::unordered_map<ValueType, std::string> BUILTIN_VTYPES_NAMES = {
    { ValueType::VOID,    "void"    },
    { ValueType::INT8,    "Int8"    },
    { ValueType::INT16,   "Int16"   },
    { ValueType::INT32,   "Int32"   },
    { ValueType::INT64,   "Int64"   },
    { ValueType::UINT8,   "UInt8"   },
    { ValueType::UINT16,  "UInt16"  },
    { ValueType::UINT32,  "UInt32"  },
    { ValueType::UINT64,  "UInt64"  },
    { ValueType::FLOAT32, "Float32" },
    { ValueType::FLOAT64, "Float64" },
    { ValueType::BOOL,    "Bool"    },
};

template <typename T>
inline ValueType valueTypeFromCPPType() {
    if constexpr (std::is_same_v<T, int8_t>)         return ValueType::INT8;
    else if constexpr (std::is_same_v<T, int16_t>)   return ValueType::INT16;
    else if constexpr (std::is_same_v<T, int32_t>)   return ValueType::INT32;
    else if constexpr (std::is_same_v<T, int64_t>)   return ValueType::INT64;
    else if constexpr (std::is_same_v<T, uint8_t>)   return ValueType::UINT8;
    else if constexpr (std::is_same_v<T, uint16_t>)  return ValueType::UINT16;
    else if constexpr (std::is_same_v<T, uint32_t>)  return ValueType::UINT16;
    else if constexpr (std::is_same_v<T, uint64_t>)  return ValueType::UINT16;
    else if constexpr (std::is_same_v<T, float32_t>) return ValueType::FLOAT32;
    else if constexpr (std::is_same_v<T, float64_t>) return ValueType::FLOAT64;
    else if constexpr (std::is_same_v<T, bool>)      return ValueType::BOOL;
    else if constexpr (std::is_void_v<T>)            return ValueType::VOID;
    else                                             return ValueType::INVALID;
}

template <typename T>
inline ValueType valueTypeFromLiteral(const T&) {
    return valueTypeFromCPPType<T>();
}

bool NCSC_API isInt(ValueType ty);
bool NCSC_API isFloat(ValueType ty);
bool NCSC_API isUnsigned(ValueType ty);
bool NCSC_API isPrimitive(ValueType ty);
inline bool isNumeric(ValueType ty) {
    return isInt(ty) || isFloat(ty);
}

inline bool hasMask(ValueType in, ValueType mask) {
    return (DWord)in & (DWord)mask;
}

inline ValueType clearMask(ValueType in, ValueType mask) {
    return static_cast<ValueType>((DWord)in & ~(DWord)mask);
}

inline ValueType setMask(ValueType in, ValueType mask) {
    return static_cast<ValueType>((DWord)in | (DWord)mask);
}

inline ValueType makeObjectType(DWord objIdx) {
    return setMask(static_cast<ValueType>(objIdx), ValueType::OBJ_MASK);
}

inline bool isRef(ValueType v) {
    return hasMask(v, ValueType::REF_MASK);
}

inline bool isCPPObject(ValueType v) {
    return hasMask(v, ValueType::CPP_OBJ_MASK);
}

inline bool isScriptObject(ValueType v) {
    return hasMask(v, ValueType::OBJ_MASK);
}

// Returns true for script objects or cpp objects
inline bool isObject(ValueType v) {
    return isScriptObject(v) || isCPPObject(v);
}

inline constexpr size_t getValueTypeSize(ValueType ty) {
    switch (ty) {
        case ValueType::INVALID:
        case ValueType::VOID:
            return 0;

        case ValueType::BOOL:
        case ValueType::INT8:
        case ValueType::UINT8:
            return sizeof(uint8_t);
        case ValueType::INT16:
        case ValueType::UINT16:
            return sizeof(uint16_t);
        case ValueType::INT32:
        case ValueType::UINT32:
            return sizeof(uint32_t);
        case ValueType::INT64:
        case ValueType::UINT64:
            return sizeof(uint64_t);

        case ValueType::FLOAT32: return sizeof(float32_t);
        case ValueType::FLOAT64: return sizeof(float64_t);

        default: return SIZE_MAX;
    }
}

bool      NCSC_API canPromoteType(ValueType from, ValueType to);
ValueType NCSC_API promoteType(ValueType from, ValueType to);

} // namespace NCSC
