// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/value_type.hpp>

namespace NCSC
{
    
// BOOL < INT8 < UINT8 < INT16 < UINT16 < INT32 < 
// UINT32 < INT64 < UINT64 < FLOAT32 < FLOAT64
static int getRank(ValueType ty);

bool isInt(ValueType ty) {
    return ty == ValueType::INT8 ||
           ty == ValueType::INT16 ||
           ty == ValueType::INT32 ||
           ty == ValueType::INT64 ||
           ty == ValueType::UINT8 ||
           ty == ValueType::UINT16 ||
           ty == ValueType::UINT32 ||
           ty == ValueType::UINT64;
}

bool isFloat(ValueType ty) {
    return ty == ValueType::FLOAT32 || ty == ValueType::FLOAT64;
}

bool isUnsigned(ValueType ty) {
    return ty == ValueType::UINT8 ||
           ty == ValueType::UINT16 ||
           ty == ValueType::UINT32 ||
           ty == ValueType::UINT64;
}

bool isPrimitive(ValueType ty) {
    return isInt(ty) || isFloat(ty) || ty == ValueType::BOOL;
}

bool canPromoteType(ValueType from, ValueType to) {
    if (from == to)
        return true;

    int rankFrom = getRank(from);
    int rankTo = getRank(from);

    return clearMask(to, ValueType::REF_MASK) == ValueType::VOID // Any type can convert to void 
        || rankFrom != -1 && rankTo != -1
        && (getRank(from) <= getRank(to));
}

ValueType promoteType(ValueType from, ValueType to) {
    // Both are the same type
    if (from == to)
        return to;

    // Floats rank above everything
    if (isFloat(from) || isFloat(to)) {
        if (from == ValueType::FLOAT64 || to == ValueType::FLOAT64)
            return ValueType::FLOAT64;
        return ValueType::FLOAT32;
    }

    int rankFrom = getRank(from);
    int rankTo   = getRank(to);

    ValueType higher = (rankFrom > rankTo) ? from : to;

    // If higher is unsigned, result is unsigned
    if (isUnsigned(higher))
        return higher;
 
    // If higher is signed and has higher rank -> signed
    if (!isUnsigned(higher) && rankFrom != rankTo)
        return higher;

    // If same rank but one unsigned -> unsigned version
    if (rankFrom == rankTo) {
        if (isUnsigned(from)) return from;
        if (isUnsigned(to)) return to;
    }

    return higher;  
}

int getRank(ValueType ty) {
    switch (ty) {
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

} // namespace NCSC
