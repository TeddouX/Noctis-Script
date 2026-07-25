#pragma once
#include <string>

namespace NCSC
{
    
enum class TokenType 
{
    INVALID,

    ID,             // abc123
    INT_CONSTANT,   // 123
    FLOAT_CONSTANT, // 1.23

    PLUS,           // +
    MINUS,          // -
    STAR,           // *
    SLASH,          // /

    END_OF_FILE,
};


struct Token 
{
    TokenType   type{TokenType::INVALID};
    std::string value{""};

    Token(TokenType type, const std::string &value)
        : type(type)
        , value(value)
    {}
};

} // namespace NCSC
