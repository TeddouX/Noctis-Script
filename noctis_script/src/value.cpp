// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/value.hpp>
#include <bit>

namespace NCSC
{

static size_t setValuePropFromBytes(const Byte *bytes, ValueType ty, Value &val, size_t readOff);

// Utility macro
#define VALUE_OPERATOR(op) Value Value::operator op(const Value &other) {                       \
    ValueType resultType = promoteType(ty_, other.ty_);                                         \
    Value val;                                                                                  \
    val.setType(resultType);                                                                    \
    switch (resultType) {                                                                       \
        case ValueType::FLOAT64:                                                                \
            val.set<float64_t>(castTo<float64_t>() op other.castTo<float64_t>());               \
            break;                                                                              \
        case ValueType::FLOAT32:                                                                \
            val.set<float32_t>(castTo<float32_t>() op other.castTo<float32_t>());               \
            break;                                                                              \
        case ValueType::INT64:                                                                  \
            val.set<int64_t>(castTo<int64_t>() op other.castTo<int64_t>());                     \
            break;                                                                              \
        case ValueType::UINT64:                                                                 \
            val.set<uint64_t>(castTo<uint64_t>() op other.castTo<uint64_t>());                  \
            break;                                                                              \
        case ValueType::INT32:                                                                  \
            val.set<int32_t>(castTo<int32_t>() op other.castTo<int32_t>());                     \
            break;                                                                              \
        case ValueType::UINT32:                                                                 \
            val.set<uint32_t>(castTo<uint32_t>() op other.castTo<uint32_t>());                  \
            break;                                                                              \
        case ValueType::INT16:                                                                  \
            val.set<int16_t>(castTo<int16_t>() op other.castTo<int16_t>());                     \
            break;                                                                              \
        case ValueType::UINT16:                                                                 \
            val.set<uint16_t>(castTo<uint16_t>() op other.castTo<uint16_t>());                  \
            break;                                                                              \
        case ValueType::INT8:                                                                   \
            val.set<int8_t>(castTo<int8_t>() op other.castTo<int8_t>());                        \
            break;                                                                              \
        case ValueType::UINT8:                                                                  \
            val.set<uint8_t>(castTo<uint8_t>() op other.castTo<uint8_t>());                     \
            break;                                                                              \
        case ValueType::BOOL:                                                                   \
            val.set<bool>(castTo<uint8_t>() op other.castTo<uint8_t>());                        \
            break;                                                                              \
        default:                                                                                \
            val.setType(ValueType::INVALID);                                                    \
            break;                                                                              \
    }                                                                                           \
    return val;                                                                                 \
}                                                                                               

#define VALUE_COMPARISON_OPERATOR(op) Value Value::operator op(const Value &other) {    \
    ValueType resultType = promoteType(ty_, other.ty_);                                 \
    Value val;                                                                          \
    val.setType(ValueType::BOOL);                                                       \
    switch (resultType) {                                                               \
        case ValueType::FLOAT64:                                                        \
            val.set<bool>(castTo<float64_t>() op other.castTo<float64_t>());            \
            break;                                                                      \
        case ValueType::FLOAT32:                                                        \
            val.set<bool>(castTo<float32_t>() op other.castTo<float32_t>());            \
            break;                                                                      \
        case ValueType::INT64:                                                          \
            val.set<bool>(castTo<int64_t>() op other.castTo<int64_t>());                \
            break;                                                                      \
        case ValueType::UINT64:                                                         \
            val.set<bool>(castTo<uint64_t>() op other.castTo<uint64_t>());              \
            break;                                                                      \
        case ValueType::INT32:                                                          \
            val.set<bool>(castTo<int32_t>() op other.castTo<int32_t>());                \
            break;                                                                      \
        case ValueType::UINT32:                                                         \
            val.set<bool>(castTo<uint32_t>() op other.castTo<uint32_t>());              \
            break;                                                                      \
        case ValueType::INT16:                                                          \
            val.set<bool>(castTo<int16_t>() op other.castTo<int16_t>());                \
            break;                                                                      \
        case ValueType::UINT16:                                                         \
            val.set<bool>(castTo<uint16_t>() op other.castTo<uint16_t>());              \
            break;                                                                      \
        case ValueType::INT8:                                                           \
            val.set<bool>(castTo<int8_t>() op other.castTo<int8_t>());                  \
            break;                                                                      \
        case ValueType::UINT8:                                                          \
            val.set<bool>(castTo<uint8_t>() op other.castTo<uint8_t>());                \
            break;                                                                      \
        case ValueType::BOOL:                                                           \
            val.set<bool>(castTo<bool>() op other.castTo<bool>());                      \
            break;                                                                      \
        default:                                                                        \
            val.setType(ValueType::INVALID);                                            \
            break;                                                                      \
    }                                                                                   \
    return val;                                                                         \
}                                                                                       \

VALUE_OPERATOR(+)
VALUE_OPERATOR(-)
VALUE_OPERATOR(*)

VALUE_COMPARISON_OPERATOR(<)
VALUE_COMPARISON_OPERATOR(<=)
VALUE_COMPARISON_OPERATOR(==)

Value Value::operator !() {
    Value val;
    val.setType(ValueType::BOOL);
    val.set<bool>(!get<bool>());
    return val;
}

Value Value::operator >(const Value &other) {
    return !(*this <= other);
}

Value Value::operator >=(const Value &other) {
    return !(*this < other);
}

Value Value::operator !=(const Value &other) {
    return !(*this == other);
}

Value Value::operator /(const Value &other) {
    Value val;
    // Division always returns a float64
    val.setType(ValueType::FLOAT64);
    val.set<float64_t>(castTo<float64_t>() / other.castTo<float64_t>());
    return val;
}

Value::operator std::string() {
    switch (ty_) {
        case ValueType::INVALID: return "invalid";

        case ValueType::INT8:    return std::to_string(get<int8_t>()) + "i8";
        case ValueType::INT16:   return std::to_string(get<int16_t>()) + "i16";
        case ValueType::INT32:   return std::to_string(get<int32_t>()) + "i32";
        case ValueType::INT64:   return std::to_string(get<int64_t>()) + "i64";
    
        case ValueType::UINT8:   return std::to_string(get<uint8_t>()) + "ui8";
        case ValueType::UINT16:  return std::to_string(get<uint16_t>()) + "ui16";
        case ValueType::UINT32:  return std::to_string(get<uint32_t>()) + "ui32";
        case ValueType::UINT64:  return std::to_string(get<uint64_t>()) + "ui64";

        case ValueType::FLOAT32: return std::to_string(get<float32_t>()) + "f32";
        case ValueType::FLOAT64: return std::to_string(get<float64_t>()) + "f64";

        case ValueType::BOOL:    return std::to_string(get<bool>()) + "b";

        default: return "empty";
    }
}

Value Value::fromBytes(const Byte *bytes, size_t readOff, size_t &readSize) {
    readSize = sizeof(ValueType);
    
    auto ty = static_cast<ValueType>(readWord<DWord>(bytes, readOff));
    
    Value val;
    val.setType(ty);
    readSize += setValuePropFromBytes(bytes, ty, val, readOff + readSize);
    
    return val;
}

size_t Value::getSize() {
    return std::visit([](auto &&val) -> size_t {
        return sizeof(val) + sizeof(ValueType);
    }, values_);
}

void Value::getBytes(Byte *bytes, size_t bufSize, size_t off) {
    return std::visit([this, bytes, bufSize, off](auto &&val) {
        return makeValueBytes(val, this->ty_, bytes, bufSize, off);
    }, values_);
}

size_t setValuePropFromBytes(const Byte *bytes, ValueType ty, Value &val, size_t readOff) {
    switch (ty) {
        case ValueType::BOOL: 
            val.set<bool>(static_cast<bool>(readWord<int8_t>(bytes, readOff)));        
            return sizeof(bool);

        case ValueType::INT8:   
            val.set<int8_t>(readWord<int8_t>(bytes, readOff));    
            return sizeof(int8_t);
        case ValueType::INT16:   
            val.set<int16_t>(readWord<int16_t>(bytes, readOff));    
            return sizeof(int16_t);
        case ValueType::INT32:   
            val.set<int32_t>(readWord<int32_t>(bytes, readOff));    
            return sizeof(int32_t);
        case ValueType::INT64:   
            val.set<int64_t>(readWord<int64_t>(bytes, readOff));    
            return sizeof(int64_t);

        case ValueType::UINT8:   
            val.set<uint8_t>(readWord<uint8_t>(bytes, readOff));    
            return sizeof(uint8_t);
        case ValueType::UINT16:   
            val.set<uint16_t>(readWord<uint16_t>(bytes, readOff));    
            return sizeof(uint16_t);
        case ValueType::UINT32:   
            val.set<uint32_t>(readWord<uint32_t>(bytes, readOff));    
            return sizeof(uint32_t);
        case ValueType::UINT64:   
            val.set<uint64_t>(readWord<uint64_t>(bytes, readOff));    
            return sizeof(uint64_t);

        case ValueType::FLOAT32:
            val.set<float32_t>(std::bit_cast<float32_t>(readWord<DWord>(bytes, readOff)));
            return sizeof(float32_t);
        case ValueType::FLOAT64:
            val.set<float64_t>(std::bit_cast<float64_t>(readWord<QWord>(bytes, readOff)));
            return sizeof(float64_t);

        default:
            return 0;
    }
}

} // namespace NCSC
