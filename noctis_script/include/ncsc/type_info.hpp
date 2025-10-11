#pragma once
#include "script_node.hpp"

namespace NCSC
{

class NCSC_API TypeInfo {
public:
    TypeInfo() = default;
    TypeInfo(const ScriptNode &token);
    TypeInfo(const Token &token)
        : token_(token) {}

    bool isPrimitive() { return isInt() || isFloat(); }

    bool isInt() const { return token_.type == TokenType::INT_KWD || token_.type == TokenType::INT_CONSTANT; }
    bool isFloat() const { return token_.type == TokenType::FLOAT_KWD || token_.type == TokenType::FLOAT_CONSTANT; }
    bool isVoid() const { return token_.type == TokenType::INVALID; }

    void getDefaultValue(Byte *bytes, size_t size);

private:
    Token token_;
};

} // namespace NCSC
