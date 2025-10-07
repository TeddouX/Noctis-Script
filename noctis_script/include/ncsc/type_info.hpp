#pragma once
#include "script_node.hpp"

namespace NCSC
{

class NCSC_API TypeInfo {
public:
    TypeInfo(const ScriptNode &token);
    TypeInfo(const Token &token)
        : token_(token) {}

    bool isPrimitive() { return isInt() || isFloat(); }

    bool isInt()   { return token_.type == TokenType::INT_KWD || token_.type == TokenType::INT_CONSTANT; }
    bool isFloat() { return token_.type == TokenType::FLOAT_KWD || token_.type == TokenType::FLOAT_CONSTANT; }

    void getDefaultValue(Byte *bytes, size_t size);

private:
    Token token_;
};

} // namespace NCSC
