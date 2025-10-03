#pragma once
#include <string>
#include <unordered_map>

#include "ncsc.hpp"

namespace NCSC
{

enum class TokenType : uint8_t {
    INVALID,

    ID,             // azer123
    INT_CONSTANT,   // 123
    FLOAT_CONSTANT, // 123.2

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
    INT_KWD,   // int
    FLOAT_KWD, // float

    // Special tokens
    END_OF_FILE,
};

struct NCSC_API Token {
    TokenType type;
    std::string val;
    uint32_t line, col;

    Token(TokenType type, const std::string &val, uint32_t line, uint32_t column)
        : type(type), val(val), line(line), col(column) {}

    std::string getStrRepr();
    size_t getLength();
};

const std::unordered_map<std::string, TokenType> g_reservedTokensStringToTok = {
    {"+", TokenType::PLUS},
    {"-", TokenType::MINUS},
    {"*", TokenType::STAR},
    {"/", TokenType::SLASH},

    {";", TokenType::SEMICOLON},
    {".", TokenType::POINT},

    {"int", TokenType::INT_KWD},
    {"float", TokenType::FLOAT_KWD},
};

} // namespace NCSC
