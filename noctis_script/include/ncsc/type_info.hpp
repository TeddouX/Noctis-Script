#pragma once
#include "script_node.hpp"

namespace NCSC
{

class NCSC_API TypeInfo {
public:
    TypeInfo(const ScriptNode &token);

    bool isPrimitive() { return isInt() || isFloat(); }

    bool isInt()   { return node_.token->type == TokenType::INT_KWD; }
    bool isFloat() { return node_.token->type == TokenType::FLOAT_KWD; }

    void getDefaultValue(Byte *bytes, size_t size);

private:
    ScriptNode node_;
};

} // namespace NCSC
