#include "token.hpp"

namespace NCSC
{
auto Token::to_string() const -> const std::string &
{
    auto it = TOKENS_TO_STRING.find(type);
    if (it != TOKENS_TO_STRING.end())
        return it->second;

    return value;
}

auto Token::length() const -> std::size_t
{
    return to_string().size();
}

auto Token::is_assignment_operator() const -> bool
{
    switch (type) 
    {
        case TokenType::PLUS_EQUAL:
        case TokenType::MINUS_EQUAL:
        case TokenType::STAR_EQUAL:
        case TokenType::SLASH_EQUAL:
        case TokenType::EQUAL:
            return true;
        default:
            return false;
    }
}

auto Token::is_binary_operator() const -> bool
{
    switch (type) 
    {
        case TokenType::PLUS:
        case TokenType::MINUS:
        case TokenType::STAR:
        case TokenType::SLASH:
        case TokenType::GREATER_THAN:
        case TokenType::GREATER_THAN_EQUAL:
        case TokenType::LESS_THAN:
        case TokenType::LESS_THAN_EQUAL:
        case TokenType::EQUAL_EQUAL:
        case TokenType::EXCLAMATION_EQUAL:
        case TokenType::LOGICAL_AND:
        case TokenType::LOGICAL_OR:
            return true;
        default:
            return false;
    }
}

auto Token::is_expression_pre_operator() const -> bool
{
    switch (type) 
    {
        case TokenType::PLUS_PLUS:
        case TokenType::MINUS_MINUS:
        case TokenType::EXCLAMATION_MARK: 
            return true;
        default: 
            return false;
    }
}

auto Token::is_expression_post_operator() const -> bool
{
    switch (type) 
    {
        case TokenType::PLUS_PLUS:
        case TokenType::MINUS_MINUS:
            return true;
        default:
            return false;
    }
}

auto Token::is_constant_value() const -> bool
{
    switch (type) 
    {
        case TokenType::INT_CONSTANT:
        case TokenType::FLOAT_CONSTANT:
        case TokenType::TRUE_KWD:
        case TokenType::FALSE_KWD:
        case TokenType::NULL_KWD:
            return true;
        default:
            return false;
    }
}

auto Token::is_data_type() const -> bool
{
    switch (type) 
    {
        case TokenType::INT8_KWD:
        case TokenType::INT16_KWD:
        case TokenType::INT32_KWD:
        case TokenType::INT64_KWD:
        case TokenType::UINT8_KWD:
        case TokenType::UINT16_KWD:
        case TokenType::UINT32_KWD:
        case TokenType::UINT64_KWD:
        case TokenType::FLOAT32_KWD:
        case TokenType::FLOAT64_KWD:
        case TokenType::BOOL_KWD:
        case TokenType::CHAR_KWD:
        // Custom types (will get checked by the compiler)
        case TokenType::ID:
            return true;
        default:
            return false;
    }
}

auto Token::get_operator_precedence() const -> int
{
    switch (type) 
    {
        case TokenType::LOGICAL_AND:
            return 1;
        case TokenType::LOGICAL_OR:
            return 2;
        
        case TokenType::PLUS:
        case TokenType::MINUS:
            return 3;
        case TokenType::STAR:
        case TokenType::SLASH:
            return 4;
        
        // Relative comparisons
        case TokenType::LESS_THAN:
        case TokenType::LESS_THAN_EQUAL:
        case TokenType::GREATER_THAN:
        case TokenType::GREATER_THAN_EQUAL:
            return 5;

        // Equality comparisons
        case TokenType::EQUAL_EQUAL:
        case TokenType::EXCLAMATION_EQUAL:
            return 6;
        
        default:
            return -1;
    }
}


} // namespace NCSC
