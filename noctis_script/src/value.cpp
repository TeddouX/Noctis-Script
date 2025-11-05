// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/value.hpp>
#include <ncsc/script_context.hpp>
#include <bit>
#include <format>

namespace NCSC
{

static size_t setValuePropFromBytes(const std::vector<Byte> &bytes, ValueType ty, Value &val, size_t readOff);

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
        case ValueType::BOOL:                                               \
            val.b = castTo<uint8_t>() op other.castTo<uint8_t>();           \
            break;                                                          \
        default:                                                            \
            val.ty = ValueType::INVALID;                                    \
            break;                                                          \
    }                                                                       \
    return val;                                                             \
}                                                                           \

#define VALUE_COMPARISON_OPERATOR(op) Value Value::operator op(const Value &other) {    \
    ValueType resultType = promoteType(ty, other.ty);                                   \
    Value val{ .ty = ValueType::BOOL };                                                 \
    switch (resultType) {                                                               \
        case ValueType::FLOAT64:                                                        \
            val.b = castTo<float64_t>() op other.castTo<float64_t>();                   \
            break;                                                                      \
        case ValueType::FLOAT32:                                                        \
            val.b = castTo<float32_t>() op other.castTo<float32_t>();                   \
            break;                                                                      \
        case ValueType::INT64:                                                          \
            val.b = castTo<int64_t>() op other.castTo<int64_t>();                       \
            break;                                                                      \
        case ValueType::UINT64:                                                         \
            val.b = castTo<uint64_t>() op other.castTo<uint64_t>();                     \
            break;                                                                      \
        case ValueType::INT32:                                                          \
            val.b = castTo<int32_t>() op other.castTo<int32_t>();                       \
            break;                                                                      \
        case ValueType::UINT32:                                                         \
            val.b = castTo<uint32_t>() op other.castTo<uint32_t>();                     \
            break;                                                                      \
        case ValueType::INT16:                                                          \
            val.b = castTo<int16_t>() op other.castTo<int16_t>();                       \
            break;                                                                      \
        case ValueType::UINT16:                                                         \
            val.b = castTo<uint16_t>() op other.castTo<uint16_t>();                     \
            break;                                                                      \
        case ValueType::INT8:                                                           \
            val.b = castTo<int8_t>() op other.castTo<int8_t>();                         \
            break;                                                                      \
        case ValueType::UINT8:                                                          \
            val.b = castTo<uint8_t>() op other.castTo<uint8_t>();                       \
            break;                                                                      \
        default:                                                                        \
            val.ty = ValueType::INVALID;                                                \
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
    return Value{ .ty = ValueType::BOOL, .b = !b };
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
    return Value{ .ty = ValueType::FLOAT64, .f64 = castTo<float64_t>() / other.castTo<float64_t>() };
}

std::string Value::getStrRepr(const ScriptContext *ctx) const {
    if (hasMask(ty, ValueType::REF_MASK))
        return std::format("ref 0x{:X}", (intptr_t)ref);
    else if (hasMask(ty, ValueType::OBJ_MASK)) {
        std::string res;
        if (ctx)
            res = ctx->getTypeName(ty);
        res += "{ ";

        for (size_t i = 0; i < obj->size(); i++) {
            const Value &val = obj->at(i);

            if (val.isObject() && ctx)
                res += ctx->getTypeName(val.ty);

            res += val.getStrRepr();

            // Don't add a comma to the last value
            if (i < obj->size() - 1)
                res += ", ";
        }
        res += " }";

        return res;
    }

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

        case ValueType::BOOL:    return std::to_string(b) + "b";

        default: return "unknown";
    }
}

Value Value::fromBytes(const std::vector<Byte> &bytes, size_t readOff, size_t &readSize) {
    readSize = sizeof(ValueType);
    
    auto ty = static_cast<ValueType>(readWord<DWord>(bytes, readOff));
    
    Value val{ .ty = ty };
    readSize += setValuePropFromBytes(bytes, ty, val, readOff + readSize);
    
    return val;
}

size_t Value::getSize() const {
    if (isObject())
        return obj->size();
    return sizeof(ValueType) + getValueTypeSize(ty);
}

void Value::getBytes(std::vector<Byte> &bytes, size_t off) const {
    makeBytes(ty, bytes, off);

    switch (ty) {
        case ValueType::INT8:  makeBytes(i8, bytes,  off + sizeof(ValueType)); return;
        case ValueType::INT16: makeBytes(i16, bytes, off + sizeof(ValueType)); return;
        case ValueType::INT32: makeBytes(i32, bytes, off + sizeof(ValueType)); return;
        case ValueType::INT64: makeBytes(i64, bytes, off + sizeof(ValueType)); return;

        case ValueType::UINT8:  makeBytes(ui8, bytes,  off + sizeof(ValueType)); return;
        case ValueType::UINT16: makeBytes(ui16, bytes, off + sizeof(ValueType)); return;
        case ValueType::UINT32: makeBytes(ui32, bytes, off + sizeof(ValueType)); return;
        case ValueType::UINT64: makeBytes(ui64, bytes, off + sizeof(ValueType)); return;

        case ValueType::FLOAT32: makeBytes(f32, bytes, off + sizeof(ValueType)); return;
        case ValueType::FLOAT64: makeBytes(f64, bytes, off + sizeof(ValueType)); return;
        
        case ValueType::BOOL: makeBytes(b, bytes, off + sizeof(ValueType)); return;

        default: break;
    }

    if (isObject()) {
        assert(0 && "Getting bytes from a script object is not yet supported");
    }
}

size_t setValuePropFromBytes(const std::vector<Byte> &bytes, ValueType ty, Value &val, size_t readOff) {
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
