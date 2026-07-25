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

    while (curr_idx < source.length())
    {
        char curr_char = source[curr_idx];

        // Skip whitespaces
        if (curr_char == ' ' || curr_char == '\t' || curr_char == '\n')
        {
            if (curr_char == '\n') 
            {
                line++;
                column = 0;
            }
            else
            {
                curr_idx++;
                column++;
            }

            continue;
        }

        // ID
        if (std::isalpha(curr_char) || curr_char == '_') 
        {
            std::size_t len{1};

            while (curr_idx + len < source.length()) 
            {
                char id_char = source.at(curr_idx + len);
                if (std::isalnum(id_char) || curr_char == '_')
                    len++;
                else
                    break;
            }

            std::string value = source.substr(curr_idx, len);
            curr_idx += len;
            column += len;

            tokens.push_back(Token{TokenType::ID, value});

            continue;
        }

        tokens.push_back(Token{TokenType::INVALID, std::string{curr_char}});
        curr_idx++;
        column++;
    }

    return tokens;
}

} // namespace NCSC
