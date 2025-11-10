// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <string>
#include <memory>

#include "ncsc.hpp"
#include "value_type.hpp"

namespace NCSC
{

struct GarbageCollectedObj {
    void *ptr;
    ValueType type; // Used by the GC at destruction
    
    // GC
    bool marked;
    std::vector<GarbageCollectedObj *> children;
};

class ScriptContext;

struct NCSC_API Value {
    ValueType ty;
    union {
        int8_t  i8;
        int16_t i16;
        int32_t i32; 
        int64_t i64;
        
        uint8_t  ui8; 
        uint16_t ui16;
        uint32_t ui32;
        uint64_t ui64;

        float32_t f32;
        float64_t f64;        

        bool b;

        GarbageCollectedObj *obj;

        Value *ref;
        void *cppRef;
    };

    // Reads the value at the end of the bytes array, 
    static Value fromBytes(const std::vector<Byte> &bytes, size_t readOff, size_t &readSize);

    template <typename T>
    static Value fromLiteral(const T &lit) {
        ValueType ty = valueTypeFromLiteral(lit);
        Value val{ .ty = ty };
        val.setProperty(&lit, ty);
        return val;
    }

    Value operator +(const Value &other);
    Value operator -(const Value &other);
    Value operator *(const Value &other);
    Value operator /(const Value &other);

    Value operator !();
    Value operator <(const Value &other);
    Value operator <=(const Value &other);
    Value operator >(const Value &other);
    Value operator >=(const Value &other);
    Value operator ==(const Value &other);
    Value operator !=(const Value &other);

    std::string getStrRepr(const ScriptContext *ctx = nullptr) const;

    void setProperty(const void *val, ValueType valTy) {
        ty = valTy;
        switch (ty) {
            case ValueType::INT8:    i8   = *(int8_t *)val;     break;
            case ValueType::INT16:   i16  = *(int16_t *)val;    break;
            case ValueType::INT32:   i32  = *(int32_t *)val;    break;
            case ValueType::INT64:   i64  = *(int64_t *)val;    break;
            case ValueType::UINT8:   ui8  = *(uint8_t *)val;    break;
            case ValueType::UINT16:  ui16 = *(uint16_t *)val;   break;
            case ValueType::UINT32:  ui32 = *(uint32_t *)val;   break;
            case ValueType::UINT64:  ui64 = *(uint64_t *)val;   break;
            case ValueType::FLOAT32: f32  = *(float32_t *)val;  break;
            case ValueType::FLOAT64: f64  = *(float64_t *)val;  break;
            case ValueType::BOOL:    b    = *(bool *)val;       break;

            default: return;
        }
    }

    template <typename T>
    T castTo() const {
        switch (ty) {
            case ValueType::INT8:    return static_cast<T>(i8);
            case ValueType::INT32:   return static_cast<T>(i32);
            case ValueType::INT16:   return static_cast<T>(i16);
            case ValueType::INT64:   return static_cast<T>(i64);

            case ValueType::UINT8:   return static_cast<T>(ui8);
            case ValueType::UINT16:  return static_cast<T>(ui16);
            case ValueType::UINT32:  return static_cast<T>(ui32);
            case ValueType::UINT64:  return static_cast<T>(ui64);
            
            case ValueType::FLOAT32: return static_cast<T>(f32);
            case ValueType::FLOAT64: return static_cast<T>(f64);
            
            case ValueType::BOOL:    return static_cast<T>(b);
            
            default:                 return 0;
        }
    }

    size_t getSize() const;
    void getBytes(std::vector<Byte> &bytes, size_t off) const;
};

template <typename T>
inline void makeValueBytes(const T &val, ValueType ty, std::vector<Byte> &bytes, size_t off = 0) {
    // VM will reverse this order when pushind to the stack
    // ValueType bytes first
    makeBytes(static_cast<VTypeWord>(ty), bytes, off);
    // val bytes last
    makeBytes(val, bytes, off + sizeof(ValueType));
}

template <typename T>
inline constexpr size_t getValueSize(const T &) {
    return sizeof(ValueType) + sizeof(T); 
}

} // namespace NCSC
