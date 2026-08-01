#pragma once
#include <string>
#include <unordered_map>

#include "../location.hpp"


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

    NOT,                // !
    GREATER_THAN,       // >
    LESS_THAN,          // <
    EQUAL_EQUAL,        // ==
    NOT_EQUAL,          // !=
    GREATER_THAN_EQUAL, // >=
    LESS_THAN_EQUAL,    // <=

    DOT,                // .
    COMMA,              // ,
    COLON,              // :
    SEMICOLON,          // ;

    PARENTHESIS_OPEN,   // (
    BRACKET_OPEN,       // [
    BRACE_OPEN,         // {

    PARENTHESIS_CLOSE,  // )
    BRACE_CLOSE,        // }
    BRACKET_CLOSE,      // ]

    BINARY_AND,         // &
    BIN_AND_EQUAL,      // &=
    LOGICAL_AND,        // &&

    BINARY_OR,          // |
    BIN_OR_EQUAL,       // |=
    LOGICAL_OR,         // ||

    XOR,                // ^
    XOR_EQUAL,          // ^=

    ARROW,              // ->

    TWO_COLONS,         // ::

    AT,                 // @

    VAR_KWD,            // var
    FUNC_KWD,           // func
    PUBLIC_KWD,         // public
    PRIVATE_KWD,        // private
    TRUE_KWD,           // true
    FALSE_KWD,          // false
    NULL_KWD,           // null
    NEW_KWD,            // new
    IF_KWD,             // if
    RETURN_KWD,         // return
    ELIF_KWD,           // elif
    ELSE_KWD,           // else
    AND_KWD,            // and
    OR_KWD,             // or
    NOT_KWD,            // not
    OBJ_KWD,            // obj
    MODULE_KWD,         // module
    IMPORT_KWD,         // import
    EXPORT_KWD,         // export

    INT8_KWD,           // int8
    INT16_KWD,          // int16
    INT32_KWD,          // int32 | int
    INT64_KWD,          // int64
    UINT8_KWD,          // uint8
    UINT16_KWD,         // uint16
    UINT32_KWD,         // uint32 | uint
    UINT64_KWD,         // uint64
    FLOAT32_KWD,        // float32 | float
    FLOAT64_KWD,        // float64 | double
    BOOL_KWD,           // bool
    CHAR_KWD,           // char
    VOID_KWD,           // void

    // Special chars
    END_OF_FILE,
};


struct Token 
{
    TokenType   type;
    std::string value;
    Location    location;

    Token()
        : type{TokenType::INVALID}
        , value{}
        , location{}
    {}
    
    Token(TokenType type, const std::string &value = "")
        : type{type}
        , value{value}
        , location{}
    {}

    auto is_valid() const -> bool;

    auto to_string() const -> const std::string &;
    auto length() const -> std::size_t;

    auto is_assignment_operator()       const -> bool;
    auto is_binary_operator()           const -> bool;
    auto is_expression_pre_operator()   const -> bool;
    auto is_expression_post_operator()  const -> bool;
    auto is_constant_value()            const -> bool;
    auto is_data_type()                 const -> bool;
    auto is_access_modifier()           const -> bool;
    auto is_comparison_operator()       const -> bool;
    
    // Returns -1 if the token isn't an operator
    auto get_operator_precedence() const -> int;
};

inline static Token INVALID_TOKEN = Token{};

const std::unordered_map<char, TokenType> SINGLE_CHAR_TOKENS = {
    { ',', TokenType::COMMA },
    { ';', TokenType::SEMICOLON },

    { '(', TokenType::PARENTHESIS_OPEN },
    { '{', TokenType::BRACE_OPEN },
    { '[', TokenType::BRACKET_OPEN },
    
    { ')', TokenType::PARENTHESIS_CLOSE },
    { '}', TokenType::BRACE_CLOSE },
    { ']', TokenType::BRACKET_CLOSE },
    
    { '@', TokenType::AT },
};

const std::unordered_map<TokenType, std::string> TOKENS_TO_STRING = {
    { TokenType::PLUS,                  "+"         },
    { TokenType::MINUS,                 "-"         },
    { TokenType::STAR,                  "*"         },
    { TokenType::SLASH,                 "/"         },
    { TokenType::EQUAL,                 "="         },

    { TokenType::PLUS_PLUS,             "++"        },
    { TokenType::MINUS_MINUS,           "--"        },

    { TokenType::PLUS_EQUAL,            "+="        },
    { TokenType::MINUS_EQUAL,           "-="        },
    { TokenType::STAR_EQUAL,            "*="        },
    { TokenType::SLASH_EQUAL,           "/="        },

    { TokenType::NOT,                   "!"         },
    { TokenType::GREATER_THAN,          ">"         },
    { TokenType::LESS_THAN,             "<"         },
    { TokenType::EQUAL_EQUAL,           "=="        },
    { TokenType::NOT_EQUAL,             "!="        },
    { TokenType::GREATER_THAN_EQUAL,    ">="        },
    { TokenType::LESS_THAN_EQUAL,       "<="        },

    { TokenType::DOT,                   "."         },
    { TokenType::COMMA,                 ","         },
    { TokenType::COLON,                 ":"         },
    { TokenType::SEMICOLON,             ";"         },

    { TokenType::PARENTHESIS_OPEN,      "("         },
    { TokenType::BRACKET_OPEN,          "["         },
    { TokenType::BRACE_OPEN,            "{"         },

    { TokenType::PARENTHESIS_CLOSE,     ")"         },
    { TokenType::BRACE_CLOSE,           "}"         },
    { TokenType::BRACKET_CLOSE,         "]"         },

    { TokenType::BINARY_AND,            "&"         },
    { TokenType::BIN_AND_EQUAL,         "&="        },
    { TokenType::LOGICAL_AND,           "&&"        },

    { TokenType::BINARY_OR,             "|"         },
    { TokenType::BIN_OR_EQUAL,          "|="        },
    { TokenType::LOGICAL_OR,            "||"        },

    { TokenType::XOR,                   "^"         },
    { TokenType::XOR_EQUAL,             "^="        },

    { TokenType::ARROW,                 "->"        },

    { TokenType::TWO_COLONS,            "::"        },

    { TokenType::AT,                    "@"         },

    { TokenType::VAR_KWD,               "var"       },
    { TokenType::FUNC_KWD,              "func"      },
    { TokenType::PUBLIC_KWD,            "public"    },
    { TokenType::PRIVATE_KWD,           "private"   },
    { TokenType::TRUE_KWD,              "true"      },
    { TokenType::FALSE_KWD,             "false"     },
    { TokenType::NULL_KWD,              "null"      },
    { TokenType::NEW_KWD,               "new"       },
    { TokenType::IF_KWD,                "if"        },
    { TokenType::RETURN_KWD,            "return"    },
    { TokenType::ELIF_KWD,              "elif"      },
    { TokenType::ELSE_KWD,              "else"      },
    { TokenType::AND_KWD,               "and"       },
    { TokenType::OR_KWD,                "or"        },
    { TokenType::NOT_KWD,               "not"       },
    { TokenType::OBJ_KWD,               "obj"       },
    { TokenType::MODULE_KWD,            "module"    },
    { TokenType::IMPORT_KWD,            "import"    },
    { TokenType::EXPORT_KWD,            "export"    },

    { TokenType::INT8_KWD,              "int8"      },
    { TokenType::INT16_KWD,             "int16"     },
    { TokenType::INT32_KWD,             "int32"     },
    { TokenType::INT64_KWD,             "int64"     },
    { TokenType::UINT8_KWD,             "uint8"     },
    { TokenType::UINT16_KWD,            "uint16"    },
    { TokenType::UINT32_KWD,            "uint32"    },
    { TokenType::UINT64_KWD,            "uint64"    },
    { TokenType::FLOAT32_KWD,           "float32"   },
    { TokenType::FLOAT64_KWD,           "float64"   },
    { TokenType::BOOL_KWD,              "bool"      },
    { TokenType::CHAR_KWD,              "char"      },
    { TokenType::VOID_KWD,              "void"      },
};

const std::unordered_map<std::string, TokenType> RESERVED_TOKENS = {
    { "var",        TokenType::VAR_KWD      },
    { "func",       TokenType::FUNC_KWD     }, 
    { "public",     TokenType::PUBLIC_KWD   },
    { "private",    TokenType::PRIVATE_KWD  },
    { "true",       TokenType::TRUE_KWD     },
    { "false",      TokenType::FALSE_KWD    },
    { "null",       TokenType::NULL_KWD     },
    { "new",        TokenType::NEW_KWD      },
    { "if",         TokenType::IF_KWD       },
    { "return",     TokenType::RETURN_KWD   },
    { "elif",       TokenType::ELIF_KWD     },
    { "else",       TokenType::ELSE_KWD     },
    { "and",        TokenType::AND_KWD      }, 
    { "or",         TokenType::OR_KWD       },  
    { "not",        TokenType::NOT_KWD      }, 
    { "obj",        TokenType::OBJ_KWD      },
    { "module",     TokenType::MODULE_KWD,  },
    { "import",     TokenType::IMPORT_KWD,  },
    { "export",     TokenType::EXPORT_KWD,  },

    { "int8",       TokenType::INT8_KWD     },
    { "int16",      TokenType::INT16_KWD    },
    { "int32",      TokenType::INT32_KWD    },
    { "int",        TokenType::INT32_KWD    },
    { "int64",      TokenType::INT64_KWD    },
    { "uint8",      TokenType::UINT8_KWD    },
    { "uint16",     TokenType::UINT16_KWD   },
    { "uint32",     TokenType::UINT32_KWD   },
    { "uint",       TokenType::UINT32_KWD   },
    { "uint64",     TokenType::UINT64_KWD   },
    { "float32",    TokenType::FLOAT32_KWD  },
    { "float",      TokenType::FLOAT32_KWD  },
    { "float64",    TokenType::FLOAT64_KWD  },
    { "double",     TokenType::FLOAT64_KWD  },
    { "bool",       TokenType::BOOL_KWD     },
    { "char",       TokenType::CHAR_KWD     },
    { "void",       TokenType::VOID_KWD     }
};

const std::unordered_map<TokenType, TokenType> REPLACED_TOKENS = {
    { TokenType::AND_KWD,   TokenType::LOGICAL_AND  },
    { TokenType::OR_KWD,    TokenType::LOGICAL_OR   },
    { TokenType::NOT_KWD,   TokenType::NOT          },
};

} // namespace NCSC
