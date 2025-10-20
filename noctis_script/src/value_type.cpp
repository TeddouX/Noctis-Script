// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/value_type.hpp>

namespace NCSC
{
    
// BOOL < INT8 < UINT8 < INT16 < UINT16 < INT32 < 
// UINT32 < INT64 < UINT64 < FLOAT32 < FLOAT64
static int getRank(ValueType ty);

ValueType valueTypeFromTok(const Token &tok) {
    switch(tok.type) {
        case TokenType::INT8_KWD:    return ValueType::INT8;
        case TokenType::INT16_KWD:   return ValueType::INT16;
        case TokenType::INT32_KWD:   return ValueType::INT32;
        case TokenType::INT64_KWD:   return ValueType::INT64;

        case TokenType::UINT8_KWD:   return ValueType::UINT8;
        case TokenType::UINT16_KWD:  return ValueType::UINT16;
        case TokenType::UINT32_KWD:  return ValueType::UINT32;
        case TokenType::UINT64_KWD:  return ValueType::UINT64;
        
        case TokenType::FLOAT32_KWD: return ValueType::FLOAT32;
        case TokenType::FLOAT64_KWD: return ValueType::FLOAT64;
        
        case TokenType::BOOL_KWD:    return ValueType::BOOL;

        default:                     return ValueType::VOID;
    }
}

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
    return getRank(from) <= getRank(to);
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
    ValueType lower  = (rankFrom > rankTo) ? to : from;

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

std::string valueTypeToString(ValueType vtype) {
    switch (vtype) {
        case ValueType::INVALID: return "invalid";
        case ValueType::VOID:    return "Void"; 

        case ValueType::INT8:    return "Int8"; 
        case ValueType::INT16:   return "Int16"; 
        case ValueType::INT32:   return "Int32"; 
        case ValueType::INT64:   return "Int64"; 
        
        case ValueType::UINT8:   return "UInt8"; 
        case ValueType::UINT16:  return "UInt16"; 
        case ValueType::UINT32:  return "UInt32"; 
        case ValueType::UINT64:  return "UInt64"; 
        
        case ValueType::FLOAT32: return "FLoat32"; 
        case ValueType::FLOAT64: return "FLoat64"; 
        
        case ValueType::BOOL:    return "Bool";
        default: break;
    }

    return "unknown";
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
