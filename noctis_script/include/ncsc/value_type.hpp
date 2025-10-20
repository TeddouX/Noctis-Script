#pragma once
#include <unordered_map>
#include "ncsc.hpp"
#include "token.hpp"

namespace NCSC
{
    
enum class ValueType : DWord {
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
};

ValueType NCSC_API valueTypeFromTok(const Token &tok);

template <typename T>
inline ValueType valueTypeFromLiteral(const T&) {
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
    else                                             return ValueType::INVALID;
}

bool      NCSC_API isInt(ValueType ty);
bool      NCSC_API isFloat(ValueType ty);
bool      NCSC_API isUnsigned(ValueType ty);
bool      NCSC_API isPrimitive(ValueType ty);

bool      NCSC_API canPromoteType(ValueType from, ValueType to);
ValueType NCSC_API promoteType(ValueType from, ValueType to);

std::string NCSC_API valueTypeToString(ValueType vtype);

} // namespace NCSC
