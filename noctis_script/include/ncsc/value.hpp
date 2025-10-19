#pragma once
#include <string>
#include "ncsc.hpp"
#include "value_type.hpp"

namespace NCSC
{
    
struct NCSC_API Value {
    ValueType ty;
    union {
        int8_t i8; 
        int16_t i16; 
        int32_t i32; 
        int64_t i64;
        
        uint8_t ui8; 
        uint16_t ui16;
        uint32_t ui32;
        uint64_t ui64;

        float32_t f32;
        float64_t f64;        

        bool b;
    };

    Value operator+(const Value &other);
    Value operator-(const Value &other);
    Value operator*(const Value &other);
    Value operator/(const Value &other);

    operator std::string();

    // Reads the value at the end of the bytes array, 
    // big endian meaning that the value is first and the ValueType is last
    static Value fromBytes(const Byte *bytes, size_t readOff, size_t &readSize);
};

template <typename T>
inline void makeValueBytes(const T &val, ValueType ty, Byte *bytes, size_t off = 0) {
    // VM will reverse this order when pushind to the stack
    // ValueType bytes first
    makeBytes(static_cast<DWord>(ty), bytes, off);
    // val bytes last
    makeBytes(val, bytes, off + sizeof(ValueType));
}

template <typename T>
inline constexpr size_t getValueSize(const T &) {
    return sizeof(ValueType) + sizeof(T); 
}

} // namespace NCSC
