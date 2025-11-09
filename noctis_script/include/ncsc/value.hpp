// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <string>
#include <memory>

#include "ncsc.hpp"
#include "value_type.hpp"

namespace NCSC
{

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

        std::vector<Value> *obj;
        Value *ref;

        void *cppObj;
        void *cppRef;
    };

    // Reads the value at the end of the bytes array, 
    static Value fromBytes(const std::vector<Byte> &bytes, size_t readOff, size_t &readSize);

    template <typename T>
    static Value fromLiteral(const T &lit) {
        ValueType ty = valueTypeFromLiteral(lit);
        Value val{ .ty = ty };
        val.setProperty(lit, ty);
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

    template <typename T>
    void setProperty(const T &val, ValueType valTy) {
#define ASSURE_T_EQ(ty) assert(typeid(ty) == typeid(T) && "Value and ValueType don't match")
        ty = valTy;
        switch (ty) {
            case ValueType::INT8:    ASSURE_T_EQ(int8_t);    i8 = val;   break;
            case ValueType::INT16:   ASSURE_T_EQ(int16_t);   i16 = val;  break;
            case ValueType::INT32:   ASSURE_T_EQ(int32_t);   i32 = val;  break;
            case ValueType::INT64:   ASSURE_T_EQ(int64_t);   i64 = val;  break;

            case ValueType::UINT8:   ASSURE_T_EQ(uint8_t);   ui8 = val;  break;
            case ValueType::UINT16:  ASSURE_T_EQ(uint16_t);  ui16 = val; break;
            case ValueType::UINT32:  ASSURE_T_EQ(uint32_t);  ui32 = val; break;
            case ValueType::UINT64:  ASSURE_T_EQ(uint64_t);  ui64 = val; break;

            case ValueType::FLOAT32: ASSURE_T_EQ(float32_t); f32 = val;  break;
            case ValueType::FLOAT64: ASSURE_T_EQ(float64_t); f64 = val;  break;

            case ValueType::BOOL:    ASSURE_T_EQ(bool);      b = val;    break;

            default: return;
        }
#undef ASSURE_T_EQ
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

    bool isObject() const { return (DWord)ty & (DWord)ValueType::OBJ_MASK; }
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
