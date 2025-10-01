#pragma once
#include <string>
#include <unordered_map>

namespace NCSC
{

enum class TokenType {
    INVALID,

    ID,    // azer123
    INT,   // 123
    FLOAT, // 123.2

    // Binary operations
    PLUS,  // +
    MINUS, // - 
    STAR,  // *
    SLASH, // /

    EQUAL,       // =
    PLUS_EQUAL,  // +=
    MINUS_EQUAL, // -=
    STAR_EQUAL,  // *=
    SLASH_EQUAL, // /=

    DOUBLE_EQUAL, // ==

    SEMICOLON, // ;
    POINT,     // .

    // Reserved keywords
    INT_KEYWORD,   // int
    FLOAT_KEYWORD, // float

    END_OF_FILE,
};

struct Token {
    TokenType type;
    std::string val;
    int line, col;

    Token(TokenType type, const std::string &val)
        : type(type), val(val) {}
};

const std::unordered_map<std::string, TokenType> g_reservedTokensStringToTok = {
    {"+", TokenType::PLUS},
    {"-", TokenType::MINUS},
    {"*", TokenType::STAR},
    {"/", TokenType::SLASH},

    {";", TokenType::SEMICOLON},

    {"int", TokenType::INT_KEYWORD},
    {"float", TokenType::FLOAT_KEYWORD},
};

} // namespace NCSC
