#include "lexer.hpp"

#include <optional>
#include <cctype>

namespace NCSC
{

auto tokenize(const std::string &source) -> std::vector<Token>
{
    std::vector<Token> tokens{};
    std::size_t curr_idx{0};
    std::size_t line{0};
    std::size_t column{0};

    auto advance = [&](std::size_t amount) -> void {
        curr_idx += amount;
        column += amount;
    };


    while (curr_idx < source.length())
    {
        char curr_char = source[curr_idx];

        // Skip whitespaces
        if (curr_char == ' ' or curr_char == '\t' or curr_char == '\n')
        {
            if (curr_char == '\n')
            {
                curr_idx++;
                line++;
                column = 0;

                continue;
            }

            advance(1);

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

            advance(len);

            tokens.push_back(Token{TokenType::ID, value});

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
                advance(1);

                tokens.push_back(Token{TokenType::DOT, ""});
                
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
            tokens.push_back(Token{type, value});
            
            advance(len);
            
            continue;
        }

        tokens.push_back(Token{TokenType::INVALID, std::string{curr_char}});
        
        advance(1);
    }

    return tokens;
}

} // namespace NCSC
