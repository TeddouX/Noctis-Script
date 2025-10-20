// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/value.hpp>
#include <bit>

namespace NCSC
{

static size_t setValuePropFromBytes(const Byte *bytes, ValueType ty, Value &val, size_t readOff);

// Utility macro
#define VALUE_OPERATOR(op) Value Value::operator op(const Value &other) {   \
    ValueType resultType = promoteType(ty, other.ty);                       \
    Value val{ .ty = resultType };                                          \
    switch (resultType) {                                                   \
        case ValueType::FLOAT64:                                            \
            val.f64 = castTo<float64_t>() op other.castTo<float64_t>();     \
            break;                                                          \
        case ValueType::FLOAT32:                                            \
            val.f32 = castTo<float32_t>() op other.castTo<float32_t>();     \
            break;                                                          \
        case ValueType::INT64:                                              \
            val.i64 = castTo<int64_t>() op other.castTo<int64_t>();         \
            break;                                                          \
        case ValueType::UINT64:                                             \
            val.ui64 = castTo<uint64_t>() op other.castTo<uint64_t>();      \
            break;                                                          \
        case ValueType::INT32:                                              \
            val.i32 = castTo<int32_t>() op other.castTo<int32_t>();         \
            break;                                                          \
        case ValueType::UINT32:                                             \
            val.ui32 = castTo<uint32_t>() op other.castTo<uint32_t>();      \
            break;                                                          \
        case ValueType::INT16:                                              \
            val.i16 = castTo<int16_t>() op other.castTo<int16_t>();         \
            break;                                                          \
        case ValueType::UINT16:                                             \
            val.ui16 = castTo<uint16_t>() op other.castTo<uint16_t>();      \
            break;                                                          \
        case ValueType::INT8:                                               \
            val.i8 = castTo<int8_t>() op other.castTo<int8_t>();            \
            break;                                                          \
        case ValueType::UINT8:                                              \
            val.ui8 = castTo<uint8_t>() op other.castTo<uint8_t>();         \
            break;                                                          \
        default:                                                            \
            val.ty = ValueType::INVALID;                                    \
            break;                                                          \
    }                                                                       \
    return val;                                                             \
}                                                                           \

VALUE_OPERATOR(+)
VALUE_OPERATOR(-)
VALUE_OPERATOR(*)

// Division always returns a float64
Value Value::operator/(const Value &other) {
    return Value{ .ty = ValueType::FLOAT64, .f64 = castTo<float64_t>() / other.castTo<float64_t>() };
}

Value::operator std::string() {
    switch (ty) {
        case ValueType::INVALID: return "invalid";

        case ValueType::INT8:    return std::to_string(i8) + "i8";
        case ValueType::INT16:   return std::to_string(i16) + "i16";
        case ValueType::INT32:   return std::to_string(i32) + "i32";
        case ValueType::INT64:   return std::to_string(i64) + "i64";
    
        case ValueType::UINT8:   return std::to_string(ui8) + "ui8";
        case ValueType::UINT16:  return std::to_string(ui16) + "ui16";
        case ValueType::UINT32:  return std::to_string(ui32) + "ui32";
        case ValueType::UINT64:  return std::to_string(ui64) + "ui64";

        case ValueType::FLOAT32: return std::to_string(f32) + "f32";
        case ValueType::FLOAT64: return std::to_string(f64) + "f64";

        case ValueType::BOOL:    return std::to_string(b) + "(bool)";

        default: return "empty";
    }
}

Value Value::fromBytes(const Byte *bytes, size_t readOff, size_t &readSize) {
    readSize = sizeof(ValueType);
    
    auto ty = static_cast<ValueType>(readWord<DWord>(bytes, readOff));
    
    Value val{ .ty = ty };
    readSize += setValuePropFromBytes(bytes, ty, val, readOff + readSize);
    
    return val;
}

size_t setValuePropFromBytes(const Byte *bytes, ValueType ty, Value &val, size_t readOff) {
    switch (ty) {
        case ValueType::BOOL: 
            val.b = static_cast<bool>(readWord<int8_t>(bytes, readOff));        
            return sizeof(bool);

        case ValueType::INT8:   val.i8  = readWord<int8_t>(bytes, readOff);    return sizeof(int8_t);
        case ValueType::INT16:  val.i16 = readWord<int16_t>(bytes, readOff);   return sizeof(int16_t);
        case ValueType::INT32:  val.i32 = readWord<int32_t>(bytes, readOff);   return sizeof(int32_t);
        case ValueType::INT64:  val.i64 = readWord<int64_t>(bytes, readOff);   return sizeof(int64_t);

        case ValueType::UINT8:  val.ui8  = readWord<uint8_t>(bytes, readOff);  return sizeof(uint8_t);
        case ValueType::UINT16: val.ui16 = readWord<uint16_t>(bytes, readOff); return sizeof(uint16_t);
        case ValueType::UINT32: val.ui32 = readWord<uint32_t>(bytes, readOff); return sizeof(uint32_t);
        case ValueType::UINT64: val.ui64 = readWord<uint64_t>(bytes, readOff); return sizeof(uint64_t);

        case ValueType::FLOAT32:
            val.f32 = std::bit_cast<float32_t>(readWord<DWord>(bytes, readOff));
            return sizeof(float32_t);
        case ValueType::FLOAT64:
            val.f64 = std::bit_cast<float64_t>(readWord<QWord>(bytes, readOff));
            return sizeof(float64_t);

        default:
            return 0;
    }
}

} // namespace NCSC
