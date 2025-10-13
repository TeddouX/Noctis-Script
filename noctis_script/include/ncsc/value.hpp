#pragma once
#include "ncsc.hpp"

#include <string>

namespace NCSC
{
    
enum class ValueType {
    UNINITIALIZED,
    INT,
    FLOAT,
};

struct Value {
    ValueType type;
    union { 
        int64_t i; 
        float64_t f;
    };

    operator std::string() { 
        switch (type) {
            case ValueType::UNINITIALIZED: return "{UNINITIALIZED}";
            case ValueType::INT: return "{INT; " + std::to_string(i) + "}";
            case ValueType::FLOAT: return "{FLOAT; " + std::to_string(f) + "}";
            default: return "";
        }
    }
};

inline float64_t toFloat(const Value& v) {
    return v.type == ValueType::INT ? static_cast<float64_t>(v.i) : v.f;
}

inline Value operator+(Value lhs, const Value& rhs) { 
    if (lhs.type == ValueType::INT && rhs.type == ValueType::INT) 
        return Value{ .type = ValueType::INT, .i = lhs.i + rhs.i, }; 
    else return Value{ .type = ValueType::FLOAT, .f = toFloat(lhs) + toFloat(rhs), }; 
}

inline Value operator-(Value lhs, const Value& rhs) { 
    if (lhs.type == ValueType::INT && rhs.type == ValueType::INT) 
        return Value{ .type = ValueType::INT, .i = lhs.i - rhs.i, }; 
    else return Value{ .type = ValueType::FLOAT, .f = toFloat(lhs) - toFloat(rhs), }; 
}

inline Value operator*(Value lhs, const Value& rhs) { 
    if (lhs.type == ValueType::INT && rhs.type == ValueType::INT) 
        return Value{ .type = ValueType::INT, .i = lhs.i * rhs.i, }; 
    else return Value{ .type = ValueType::FLOAT, .f = toFloat(lhs) * toFloat(rhs), }; 
}

inline Value operator/(Value lhs, const Value& rhs) { 
    return Value{ .type = ValueType::FLOAT, .f = toFloat(lhs) / toFloat(rhs), };
}

} // namespace NCSC
