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

    // Special chars
    END_OF_FILE,
};


struct Token 
{
    TokenType   type{TokenType::INVALID};
    std::string value{""};

    std::size_t line{0};
    std::size_t column{0};

    Token(TokenType type, std::size_t line, std::size_t column, const std::string &value = "")
        : type(type)
        , value(value)
        , line(line)
        , column(column)
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

} // namespace NCSC
