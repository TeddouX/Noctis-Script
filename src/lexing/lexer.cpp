#include "lexing/lexer.hpp"

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
        Token tok{type, value};
        Location location{
            line, line,
            column, column + tok.length() - 1 // Keep column inclusive
        };

        tok.location = location;

        tokens.push_back(tok);
    };

    auto match_next = [&](char next, TokenType single, TokenType combined) -> void
    {
        if (curr_idx + 1 < source.size() && source.at(curr_idx + 1) == next) 
        {
            append_token(combined);
            advance(2);
        }
        else 
        {
            append_token(single);
            advance(1);
        }
    };

    auto match_next_2 = [&](
        char next, char next_2, 
        TokenType single, 
        TokenType combined, TokenType combined_2
    ) -> void
    {
        if (curr_idx + 1 >= source.size()) 
        {
            append_token(single);
            advance(1);

            return;
        }
        
        char source_next = source.at(curr_idx + 1);
        if (source_next == next)
        {
            append_token(combined);
            advance(2);
        }
        else if (source_next == next_2)
        {
            append_token(combined_2);
            advance(2);
        }
        else
        {
            append_token(single);
            advance(1);
        }
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
                if (std::isalnum(id_char) or id_char == '_')
                    len++;
                else
                    break;
            }

            std::string value = source.substr(curr_idx, len);

            auto it = RESERVED_TOKENS.find(value);
            if (it != RESERVED_TOKENS.end()) {
                TokenType reserved_token_type = it->second;

                auto replaced_tokens_it = REPLACED_TOKENS.find(reserved_token_type);
                if (replaced_tokens_it != REPLACED_TOKENS.end())
                    reserved_token_type = replaced_tokens_it->second;

                append_token(reserved_token_type, value);
                advance(len);

                continue;
            }

            append_token(TokenType::ID, value);
            advance(len);

            continue;
        }


        // Numbers
        if (std::isdigit(curr_char) or curr_char == '.' or curr_char == '-')
        {
            bool has_point = curr_char == '.';
            bool has_digits = std::isdigit(curr_char);
            bool is_negative = curr_char == '-';
            
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

            if (is_negative and not has_digits)
                goto numbers_after;

            if (has_point and not has_digits) 
            {
                append_token(TokenType::DOT);
                advance();
                
                continue;
            }

            std::string value = source.substr(curr_idx, len);

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

numbers_after:

        auto single_char_token_it = SINGLE_CHAR_TOKENS.find(curr_char);
        if (single_char_token_it != SINGLE_CHAR_TOKENS.end())
        {
            append_token(single_char_token_it->second);
            advance();
            continue;
        }

        switch (curr_char)
        {
            // ! !=
            case '!':
                match_next('=', TokenType::NOT, TokenType::NOT_EQUAL);
                continue;

            // > >=
            case '>':
                match_next('=', TokenType::GREATER_THAN, TokenType::GREATER_THAN_EQUAL);
                continue;

            // < <=
            case '<':
                match_next('=', TokenType::LESS_THAN, TokenType::LESS_THAN_EQUAL);
                continue;

            // & &= &&
            case '&':
                match_next_2('=', '&', TokenType::BINARY_AND, TokenType::BIN_AND_EQUAL, TokenType::LOGICAL_AND);
                continue;

            // | |= ||
            case '|':
                match_next_2('=', '|', TokenType::BINARY_OR, TokenType::BIN_OR_EQUAL, TokenType::LOGICAL_OR);
                continue;

            // = ==
            case '=':
                match_next('=', TokenType::EQUAL, TokenType::EQUAL_EQUAL);
                continue;

            // ^ ^=
            case '^':
                match_next('=', TokenType::XOR, TokenType::XOR_EQUAL);
                continue;

            // + ++ +=
            case '+':
                match_next_2('+', '=', TokenType::PLUS, TokenType::PLUS_PLUS, TokenType::PLUS_EQUAL);
                continue;

            // - -- -=
            case '-':
                if (curr_idx + 1 >= source.size()) 
                {
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

                    case '>':
                        append_token(TokenType::ARROW);
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

            // : ::
            case ':':
                match_next(':', TokenType::COLON, TokenType::TWO_COLONS);
                continue;
        }

        append_token(TokenType::INVALID);
        advance();
    }

    // Make the EOF token not override the column of the previous token 
    if (source[curr_idx - 1] == '\n')
        line--;
    else
        column++;
    
    append_token(TokenType::END_OF_FILE, " ");

    return tokens;
}

auto tokenize(const PtrRef<ScriptSource> &script_source) -> std::vector<Token>
{
    return tokenize(script_source->get_lines_string());
}

} // namespace NCSC
