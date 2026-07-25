#include "lexer.hpp"

#include <optional>
#include <cctype>
#include <print>


namespace NCSC
{

auto tokenize(const std::string &source) -> std::vector<Token>
{
    std::vector<Token> tokens{};
    std::size_t curr_idx{0};
    std::size_t line{1};
    std::size_t column{1};

    auto advance = [&](std::size_t amount = 1) -> void 
    {
        curr_idx += amount;
        column += amount;
    };

    auto append_token = [&](TokenType type, const std::string &value = "") -> void 
    {
        Token tok{type, line, column, value};
        tokens.push_back(tok);
    };

    auto match_next = [&](char next, TokenType single, TokenType combined) -> void
    {
        if (curr_idx + 1 < source.size() && source.at(curr_idx + 1) == next) {
            append_token(combined);
            advance(2);
        }
        else {
            append_token(single);
            advance(1);
        }
    };


    while (curr_idx < source.length())
    {
        char curr_char = source[curr_idx];

        std::println("{}", curr_char);

        // Skip whitespaces
        if (curr_char == ' ' or curr_char == '\t' or curr_char == '\n')
        {
            if (curr_char == '\n')
            {
                curr_idx++;
                line++;
                column = 1;

                continue;
            }

            advance();
            continue;
        }

        // ID
        if (std::isalpha(curr_char) or curr_char == '_')
        {
            std::size_t len{1};

            while (curr_idx + len < source.length())
            {
                char id_char = source[curr_idx + len];
                if (std::isalnum(id_char) or curr_char == '_')
                    len++;
                else
                    break;
            }

            std::string value = source.substr(curr_idx, len);

            append_token(TokenType::ID, value);
            advance(len);

            continue;
        }

        // Numbers
        if (std::isdigit(curr_char) or curr_char == '.')
        {
            bool has_point = curr_char == '.';
            bool has_digits = std::isdigit(curr_char);

            bool is_valid = true;
            
            std::size_t len = 1;
            while (curr_idx + len < source.length())
            {
                char num_char = source[curr_idx + len];
                bool num_char_is_point = num_char == '.';
                bool num_char_is_digit = std::isdigit(num_char);

                if (num_char_is_digit or (num_char_is_point and not has_point)) 
                {
                    has_point = num_char_is_point ? true : has_point;
                    has_digits = num_char_is_digit ? true : has_digits;

                    len++;
                }
                else
                    break;
            }

            std::string value = source.substr(curr_idx, len);

            if (has_point and not has_digits) 
            {
                append_token(TokenType::DOT);
                advance();
                
                continue;
            }

            if (has_point) 
            {
                // .1 -> 0.1
                if (value.front() == '.')
                    value.insert(0, 1, '0');
                // 1. -> 1.0
                else if (value.back() == '.')
                    value.push_back('0');
            }
            
            auto type = has_point ? TokenType::FLOAT_CONSTANT : TokenType::INT_CONSTANT;
            
            append_token(type, value);
            advance(len);
            
            continue;
        }

        switch (curr_char)
        {
            // + ++ +=
            case '+':
                if (curr_idx + 1 >= source.size()) {
                    append_token(TokenType::PLUS);
                    advance();

                    continue;
                }
                
                switch (source[curr_idx + 1])
                {
                    case '+': 
                        append_token(TokenType::PLUS_PLUS);  
                        advance(2);
                        break;

                    case '=': 
                        append_token(TokenType::PLUS_EQUAL);
                        advance(2);
                        break;
                    
                    default: 
                        append_token(TokenType::PLUS);
                        advance();
                        break;
                }

                continue;

            // - -- -=
            case '-':
                if (curr_idx + 1 >= source.size()) {
                    append_token(TokenType::MINUS);
                    advance();

                    continue;
                }
                
                switch (source[curr_idx + 1])
                {
                    case '-': 
                        append_token(TokenType::MINUS_MINUS);  
                        advance(2);
                        break;
                 
                    case '=': 
                        append_token(TokenType::MINUS_EQUAL);
                        advance(2);
                        break;
                 
                    default: 
                        append_token(TokenType::MINUS);
                        advance();
                        break;
                }

                continue;
            
            // * *=
            case '*':
                match_next('=', TokenType::STAR, TokenType::STAR_EQUAL);
                continue;

            // / /=
            case '/':
                match_next('=', TokenType::SLASH, TokenType::SLASH_EQUAL);
                continue;
        }

        advance();
        append_token(TokenType::INVALID);
    }

    append_token(TokenType::END_OF_FILE);

    return tokens;
}

} // namespace NCSC
