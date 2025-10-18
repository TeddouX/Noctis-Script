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
    COMMA,     // ,

    PARENTHESIS_OPEN,  // (
    PARENTHESIS_CLOSE, // )
    CURLY_BRACE_OPEN,  // {
    CURLY_BRACE_CLOSE, // }

    // Reserved keywords
    INT16_KWD,   // Int16
    INT32_KWD,   // Int32 | Int
    INT64_KWD,   // Int64
    UINT16_KWD,  // UInt16
    UINT32_KWD,  // UInt32 | UInt
    UINT64_KWD,  // UInt64
    FLOAT32_KWD, // Float32 | Float
    FLOAT64_KWD, // Float64 | Double
    BOOL_KWD,    // Bool | Byte
    FUN_KWD,     // function
    RETURN_KWD,  // return

    // Special tokens
    END_OF_FILE,
};

struct NCSC_API Token {
    TokenType type;
    std::string val;
    uint32_t line, col;

    Token() : type(TokenType::INVALID) {}

    Token(TokenType type, const std::string &val, uint32_t line, uint32_t column)
        : type(type), val(val), line(line), col(column) {}

    std::string getStrRepr() const;
    size_t getLength() const;
};

const std::unordered_map<std::string, TokenType> tokensStringToTok = {
    { "+", TokenType::PLUS },
    { "-", TokenType::MINUS },
    { "*", TokenType::STAR },
    { "/", TokenType::SLASH },

    { "=",  TokenType::EQUAL },
    { "+=", TokenType::PLUS_EQUAL },
    { "-=", TokenType::MINUS_EQUAL },
    { "*=", TokenType::STAR_EQUAL },
    { "/=", TokenType::SLASH_EQUAL },
    
    { "==", TokenType::DOUBLE_EQUAL },

    { ";", TokenType::SEMICOLON },
    { ".", TokenType::POINT },
    { ",", TokenType::COMMA },

    { "(", TokenType::PARENTHESIS_OPEN },  
    { ")", TokenType::PARENTHESIS_CLOSE }, 
    { "{", TokenType::CURLY_BRACE_OPEN },  
    { "}", TokenType::CURLY_BRACE_CLOSE }, 

    { "Int16", TokenType::INT16_KWD, },
    { "Int", TokenType::INT32_KWD, },
    { "Int32", TokenType::INT32_KWD, },
    { "Int64", TokenType::INT64_KWD, },  
    
    { "UInt16", TokenType::UINT16_KWD, },  
    { "UInt", TokenType::UINT32_KWD, },
    { "UInt32", TokenType::UINT32_KWD, },
    { "UInt64", TokenType::UINT64_KWD, }, 
    
    { "Float", TokenType::FLOAT32_KWD, },
    { "Float32", TokenType::FLOAT32_KWD, },
    { "Double", TokenType::FLOAT64_KWD, },
    { "Float64", TokenType::FLOAT64_KWD, },
    
    { "Bool", TokenType::BOOL_KWD, },
    
    { "fun", TokenType::FUN_KWD },
    { "return", TokenType::RETURN_KWD },
};

} // namespace NCSC
