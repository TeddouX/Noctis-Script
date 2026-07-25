#pragma once
#include <string>
#include <unordered_map>

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

    LET_KWD,            // let

    INT8_KWD,    // int8
    INT16_KWD,   // int16
    INT32_KWD,   // int32 | int
    INT64_KWD,   // int64
    UINT8_KWD,   // uint8
    UINT16_KWD,  // uint16
    UINT32_KWD,  // uint32 | uint
    UINT64_KWD,  // uint64
    FLOAT32_KWD, // float32 | float
    FLOAT64_KWD, // float64 | double
    BOOL_KWD,    // bool

    // Special chars
    END_OF_FILE,
};


struct Token 
{
    TokenType   type;
    std::string value;

    std::size_t line;
    std::size_t column;

    Token()
        : type{TokenType::INVALID}
        , value{}
        , line{0}
        , column{0}
    {}
    
    Token(TokenType type, std::size_t line, std::size_t column, const std::string &value = "")
        : type{type}
        , value{value}
        , line{line}
        , column{column}
    {}

    auto to_string() const -> const std::string &;
    auto length() const -> std::size_t;
};

const std::unordered_map<TokenType, std::string> TOKENS_TO_STRING = {
    { TokenType::PLUS,               "+"  },
    { TokenType::MINUS,              "-"  },
    { TokenType::STAR,               "*"  },
    { TokenType::SLASH,              "/"  },
    { TokenType::EQUAL,              "="  },

    { TokenType::PLUS_PLUS,          "++" },
    { TokenType::MINUS_MINUS,        "--" },

    { TokenType::PLUS_EQUAL,         "+=" },
    { TokenType::MINUS_EQUAL,        "-=" },
    { TokenType::STAR_EQUAL,         "*=" },
    { TokenType::SLASH_EQUAL,        "/=" },

    { TokenType::EXCLAMATION_MARK,   "!"  },
    { TokenType::GREATER_THAN,       ">"  },
    { TokenType::LESS_THAN,          "<"  },

    { TokenType::EQUAL_EQUAL,        "==" },
    { TokenType::EXCLAMATION_EQUAL,  "!=" },
    { TokenType::GREATER_THAN_EQUAL, ">=" },
    { TokenType::LESS_THAN_EQUAL,    "<=" },

    { TokenType::DOT,                "."  }
};

const std::unordered_map<std::string, TokenType> RESERVED_TOKENS = {
    { "let",        TokenType::LET_KWD },
    { "int8",       TokenType::INT8_KWD},
    { "int16",      TokenType::INT16_KWD},
    { "int32",      TokenType::INT32_KWD},
    { "int",        TokenType::INT32_KWD},
    { "int64",      TokenType::INT64_KWD},
    { "uint8",      TokenType::UINT8_KWD},
    { "uint16",     TokenType::UINT16_KWD},
    { "uint32",     TokenType::UINT32_KWD},
    { "uint",       TokenType::UINT32_KWD},
    { "uint64",     TokenType::UINT64_KWD},
    { "float32",    TokenType::FLOAT32_KWD},
    { "float",      TokenType::FLOAT32_KWD},
    { "float64",    TokenType::FLOAT64_KWD},
    { "double",     TokenType::FLOAT64_KWD},
    { "bool",       TokenType::BOOL_KWD},
};

} // namespace NCSC
