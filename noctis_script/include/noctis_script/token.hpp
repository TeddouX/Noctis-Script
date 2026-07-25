#pragma once
#include <string>

namespace NCSC
{
    
enum class TokenType 
{
    INVALID,

    ID,                 // abc123
    INT_CONSTANT,       // 123
    FLOAT_CONSTANT,     // 1.23

    PLUS,               // +
    MINUS,              // -
    STAR,               // *
    SLASH,              // /
    EQUAL,              // =

    PLUS_PLUS,          // ++
    MINUS_MINUS,        // --

    PLUS_EQUAL,         // +=
    MINUS_EQUAL,        // -=
    STAR_EQUAL,         // *=
    SLASH_EQUAL,        // /=

    EXCLAMATION_MARK,   // !
    GREATER_THAN,       // >
    LESS_THAN,          // <

    EQUAL_EQUAL,        // ==
    EXCLAMATION_EQUAL,  // !=
    GREATER_THAN_EQUAL, // >=
    LESS_THAN_EQUAL,    // <=

    DOT,                // .

    // Special chars
    END_OF_FILE,
};


struct Token 
{
    TokenType   type{TokenType::INVALID};
    std::string value{""};

    Token(TokenType type, const std::string &value = "")
        : type(type)
        , value(value)
    {}
};

} // namespace NCSC
