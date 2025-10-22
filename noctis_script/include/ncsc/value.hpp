// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <string>
#include <variant>

#include "ncsc.hpp"
#include "value_type.hpp"

namespace NCSC
{

class NCSC_API Value {
public:
    // Reads the value at the end of the bytes array, 
    static Value fromBytes(const Byte *bytes, size_t readOff, size_t &readSize);

    template <typename T>
    static Value fromLiteral(const T &lit) {
        ValueType ty = valueTypeFromLiteral(lit);
        Value val;
        val.setType(ty);
        val.setProperty(lit, ty);
        return val;
    }

    Value operator + (const Value &other);
    Value operator - (const Value &other);
    Value operator * (const Value &other);
    Value operator / (const Value &other);

    Value operator ! ();
    Value operator < (const Value &other);
    Value operator <=(const Value &other);
    Value operator > (const Value &other);
    Value operator >=(const Value &other);
    Value operator ==(const Value &other);
    Value operator !=(const Value &other);

    operator std::string();

    template <typename T>
    const T &get() const {
        return std::get<T>(values_);
    }

    template <typename T>
    void set(const T &v) {
        ty_ = valueTypeFromLiteral(v);
        values_.emplace<T>(v);
    }

    template <typename T>
    [[deprecated("Use Value::set<>() instead")]]
    void setProperty(const T &val, ValueType valTy) {
        ty_ = valTy;
        if constexpr (std::is_arithmetic_v<T>) {
            values_.emplace<T>(val);
        }
    }

    template <typename T>
    [[deprecated("Use Value::get<>() instead")]]
    T castTo() const {
        return std::visit([](auto&& v) -> T { 
            return static_cast<T>(v); 
        }, values_);
    }

    size_t getSize();
    void getBytes(Byte *bytes, size_t bufSize, size_t off);

    ValueType type() const { return ty_; }
    void setType(ValueType ty) { ty_ = ty; }

private:
    std::variant<
        int8_t,
        int16_t,
        int32_t,
        int64_t,
        uint8_t,
        uint16_t,
        uint32_t,
        uint64_t,
        float32_t,
        float64_t,        
        bool
    > values_;
    ValueType ty_;
};

template <typename T>
inline void makeValueBytes(const T &val, ValueType ty, Byte *bytes, size_t bufSize, size_t off = 0) {
    // VM will reverse this order when pushind to the stack
    // ValueType bytes first
    makeBytes(static_cast<DWord>(ty), bytes, bufSize, off);
    // val bytes last
    makeBytes(val, bytes, bufSize, off + sizeof(ValueType));
}

template <typename T>
inline constexpr size_t getValueSize(const T &) {
    return sizeof(ValueType) + sizeof(T); 
}

} // namespace NCSC
