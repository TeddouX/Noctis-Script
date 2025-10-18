#include <ncsc/value.hpp>
#include <bit>

namespace NCSC
{
    
// BOOL < INT16 < UINT16 < INT32 < UINT32 < INT64 < UINT64 < FLOAT32 < FLOAT64
static int getRank(ValueType ty);

static bool isUnsigned(ValueType ty);
static bool isFloat(ValueType ty);

// Examples: 
// INT16 + UINT16 -> UINT16
// INT32 + UINT16 -> INT32
// UINT32 + INT32 -> UINT32
// INT32 + FLOAT64 -> FLOAT64
// UINT64 + INT32 -> UINT64
static ValueType promoteType(ValueType a, ValueType b);

static size_t setValueProp(const Byte *bytes, ValueType ty, Value &val, size_t readOff);

template <typename T>
static T toNumType(const Value& v) {
    switch (v.ty) {
        case ValueType::INT64:   return static_cast<T>(v.i64);
        case ValueType::UINT64:  return static_cast<T>(v.ui64);
        case ValueType::INT32:   return static_cast<T>(v.i32);
        case ValueType::UINT32:  return static_cast<T>(v.ui32);
        case ValueType::INT16:   return static_cast<T>(v.i16);
        case ValueType::UINT16:  return static_cast<T>(v.ui16);
        case ValueType::FLOAT32: return static_cast<T>(v.f32);
        case ValueType::FLOAT64: return static_cast<T>(v.f64);
        case ValueType::BOOL:    return static_cast<T>(v.b);
        default:                 return 0;
    }
}

// Utility macro
#define VALUE_OPERATOR(op) Value Value::operator op(const Value &other) {           \
    ValueType resultType = promoteType(ty, other.ty);                               \
    Value val{ .ty = resultType };                                                  \
    switch (resultType) {                                                           \
        case ValueType::FLOAT64:                                                    \
            val.f64 = toNumType<float64_t>(*this) op toNumType<float64_t>(other);   \
            break;                                                                  \
        case ValueType::FLOAT32:                                                    \
            val.f32 = toNumType<float32_t>(*this) op toNumType<float32_t>(other);   \
            break;                                                                  \
        case ValueType::INT64:                                                      \
            val.i64 = toNumType<int64_t>(*this) op toNumType<int64_t>(other);       \
            break;                                                                  \
        case ValueType::UINT64:                                                     \
            val.ui64 = toNumType<uint64_t>(*this) op toNumType<uint64_t>(other);    \
            break;                                                                  \
        case ValueType::INT32:                                                      \
            val.i32 = toNumType<int32_t>(*this) op toNumType<int32_t>(other);       \
            break;                                                                  \
        case ValueType::UINT32:                                                     \
            val.ui32 = toNumType<uint32_t>(*this) op toNumType<uint32_t>(other);    \
            break;                                                                  \
        case ValueType::INT16:                                                      \
            val.i16 = toNumType<int16_t>(*this) op toNumType<int16_t>(other);       \
            break;                                                                  \
        case ValueType::UINT16:                                                     \
            val.ui16 = toNumType<uint16_t>(*this) op toNumType<uint16_t>(other);    \
            break;                                                                  \
        default:                                                                    \
            val.ty = ValueType::INVALID;                                            \
            break;                                                                  \
    }                                                                               \
    return val;                                                                     \
}                                                                                   \

VALUE_OPERATOR(+)
VALUE_OPERATOR(-)
VALUE_OPERATOR(*)

// Division always returns a float64
Value Value::operator/(const Value &other) {
    return Value{ .ty = ValueType::FLOAT64, .f64 = toNumType<float64_t>(*this) / toNumType<float64_t>(other) };
}

Value::operator std::string() {
    switch (ty) {
        case ValueType::INT16: return std::to_string(i16) + "i16";
        case ValueType::INT32: return std::to_string(i32) + "i32";
        case ValueType::INT64: return std::to_string(i64) + "i64";
    
        case ValueType::UINT16: return std::to_string(ui16) + "ui16";
        case ValueType::UINT32: return std::to_string(ui32) + "ui32";
        case ValueType::UINT64: return std::to_string(ui64) + "ui64";

        case ValueType::FLOAT32: return std::to_string(f32) + "f32";
        case ValueType::FLOAT64: return std::to_string(f64) + "f64";
        default: return "";
    }
}

Value Value::fromBytes(const Byte *bytes, size_t readOff, size_t &readSize) {
    readSize = sizeof(ValueType);
    
    auto ty = static_cast<ValueType>(readWord<DWord>(bytes, readOff));
    
    Value val{ .ty = ty };
    readSize += setValueProp(bytes, ty, val, readOff + readSize);
    
    return val;
}

int getRank(ValueType ty) {
    switch (ty) {
        case ValueType::BOOL:    return 0;
        case ValueType::INT16:   return 1;
        case ValueType::UINT16:  return 2;
        case ValueType::INT32:   return 3;
        case ValueType::UINT32:  return 4;
        case ValueType::INT64:   return 5;
        case ValueType::UINT64:  return 6;
        case ValueType::FLOAT32: return 7;
        case ValueType::FLOAT64: return 8;
        default:                 return -1;
    }
}

bool isUnsigned(ValueType ty) {
    return ty == ValueType::UINT16 ||
           ty == ValueType::UINT32 ||
           ty == ValueType::UINT64;
}

bool isFloat(ValueType ty) {
    return ty == ValueType::FLOAT32 || ty == ValueType::FLOAT64;
}

ValueType promoteType(ValueType a, ValueType b) {
    // Both are the same type
    if (a == b)
        return a;

    // Floats rank above everything
    if (isFloat(a) || isFloat(b)) {
        if (a == ValueType::FLOAT64 || b == ValueType::FLOAT64)
            return ValueType::FLOAT64;
        return ValueType::FLOAT32;
    }

    int rankA = getRank(a);
    int rankB = getRank(b);

    ValueType higher = (rankA > rankB) ? a : b;
    ValueType lower  = (rankA > rankB) ? b : a;

    // If higher is unsigned, result is unsigned
    if (isUnsigned(higher))
        return higher;
 
    // If higher is signed and has higher rank -> signed
    if (!isUnsigned(higher) && rankA != rankB)
        return higher;

    // If same rank but one unsigned -> unsigned version
    if (rankA == rankB) {
        if (isUnsigned(a)) return a;
        if (isUnsigned(b)) return b;
    }

    return higher;  
}

size_t setValueProp(const Byte *bytes, ValueType ty, Value &val, size_t readOff) {
    switch (ty)
    {
        case ValueType::INT16: val.i16 = readWord<int16_t>(bytes, readOff); return sizeof(int16_t);
        case ValueType::INT32: val.i32 = readWord<int32_t>(bytes, readOff); return sizeof(int32_t);
        case ValueType::INT64: val.i64 = readWord<int64_t>(bytes, readOff); return sizeof(int64_t);

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
