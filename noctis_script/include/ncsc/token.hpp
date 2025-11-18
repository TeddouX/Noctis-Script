// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
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

    NOT, // !

    PLUS_PLUS,   // ++
    MINUS_MINUS, // --

    STRICTLY_SMALLER, // <
    STRICTLY_BIGGER,  // >

    DOUBLE_EQUAL,  // ==
    NOT_EQUAL,     // !=
    BIGGER_EQUAL,  // >=
    SMALLER_EQUAL, // <=

    LOGICAL_AND, // &&
    LOGICAL_OR,  // ||

    BITWISE_AND, // &
    BITWISE_OR,  // |

    SEMICOLON, // ;
    DOT,       // .
    COMMA,     // ,

    PARENTHESIS_OPEN,  // (
    PARENTHESIS_CLOSE, // )
    CURLY_BRACE_OPEN,  // {
    CURLY_BRACE_CLOSE, // }

    // Reserved keywords
    INT8_KWD,    // Int8 | Byte | Char
    INT16_KWD,   // Int16
    INT32_KWD,   // Int32 | Int
    INT64_KWD,   // Int64
    UINT8_KWD,   // UInt8 | UByte | UChar
    UINT16_KWD,  // UInt16
    UINT32_KWD,  // UInt32 | UInt
    UINT64_KWD,  // UInt64
    FLOAT32_KWD, // Float32 | Float
    FLOAT64_KWD, // Float64 | Double
    BOOL_KWD,    // Bool | Byte
    
    FUN_KWD,     // fun
    RETURN_KWD,  // return
    IF_KWD,      // if
    ELSE_KWD,    // else
    OBJ_KWD,     // obj
    PUBLIC_KWD,  // public
    PRIVATE_KWD, // private
    NEW_KWD,     // new
    NULL_KWD,    // null

    TRUE_KWD,  // true
    FALSE_KWD, // false

    // Special tokens
    END_OF_FILE,
};

struct NCSC_API Token {
    TokenType type;
    std::string val;
    uint32_t line = 0, col = 0;

    Token() : type(TokenType::INVALID) {}

    Token(TokenType type, const std::string &val, uint32_t line, uint32_t column)
        : type(type), val(val), line(line), col(column) {}

    std::string getStrRepr() const;
    size_t getLength() const;
    bool isValid() const { return type != TokenType::INVALID; }
};

const std::unordered_map<std::string, TokenType> tokensStringToTok = {
    { "+",          TokenType::PLUS },
    { "-",          TokenType::MINUS },
    { "*",          TokenType::STAR },
    { "/",          TokenType::SLASH },

    { "!",          TokenType::NOT, },

    { "++",         TokenType::PLUS_PLUS, }, 
    { "--",         TokenType::MINUS_MINUS, }, 

    { "<",          TokenType::STRICTLY_SMALLER, },
    { ">",          TokenType::STRICTLY_BIGGER, },

    { "==",         TokenType::DOUBLE_EQUAL, },
    { "!=",         TokenType::NOT_EQUAL, },
    { ">=",         TokenType::BIGGER_EQUAL, },
    { "<=",         TokenType::SMALLER_EQUAL, },
    
    { "&&",         TokenType::LOGICAL_AND, },
    { "||",         TokenType::LOGICAL_OR, },

    { "&",          TokenType::BITWISE_AND, },
    { "|",          TokenType::BITWISE_OR, },

    { "=",          TokenType::EQUAL },
    { "+=",         TokenType::PLUS_EQUAL },
    { "-=",         TokenType::MINUS_EQUAL },
    { "*=",         TokenType::STAR_EQUAL },
    { "/=",         TokenType::SLASH_EQUAL },
    
    { "==",         TokenType::DOUBLE_EQUAL },

    { ";",          TokenType::SEMICOLON },
    { ".",          TokenType::DOT },
    { ",",          TokenType::COMMA },

    { "(",          TokenType::PARENTHESIS_OPEN },  
    { ")",          TokenType::PARENTHESIS_CLOSE }, 
    { "{",          TokenType::CURLY_BRACE_OPEN },  
    { "}",          TokenType::CURLY_BRACE_CLOSE }, 

    { "Int8",       TokenType::INT8_KWD },
    { "UInt8",      TokenType::UINT8_KWD },
    
    { "Byte",       TokenType::INT8_KWD },
    { "UByte",      TokenType::UINT8_KWD },
    
    { "Char",       TokenType::INT8_KWD },
    { "UChar",      TokenType::UINT8_KWD },

    { "Int16",      TokenType::INT16_KWD, },
    { "Int",        TokenType::INT32_KWD, },
    { "Int32",      TokenType::INT32_KWD, },
    { "Int64",      TokenType::INT64_KWD, },  
    
    { "UInt16",     TokenType::UINT16_KWD, },  
    { "UInt",       TokenType::UINT32_KWD, },
    { "UInt32",     TokenType::UINT32_KWD, },
    { "UInt64",     TokenType::UINT64_KWD, }, 
    
    { "Float",      TokenType::FLOAT32_KWD, },
    { "Float32",    TokenType::FLOAT32_KWD, },
    { "Double",     TokenType::FLOAT64_KWD, },
    { "Float64",    TokenType::FLOAT64_KWD, },
    
    { "Bool",       TokenType::BOOL_KWD, },

    { "true",       TokenType::TRUE_KWD },
    { "false",      TokenType::FALSE_KWD },
    
    { "fun",        TokenType::FUN_KWD },
    { "return",     TokenType::RETURN_KWD },
    { "if",         TokenType::IF_KWD },
    { "else",       TokenType::ELSE_KWD },
    { "obj",        TokenType::OBJ_KWD, },
    { "pub",        TokenType::PUBLIC_KWD, },
    { "priv",       TokenType::PRIVATE_KWD, },
    { "new",        TokenType::NEW_KWD, },
    { "null",       TokenType::NULL_KWD, },
};

} // namespace NCSC
