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

} // namespace NCSC
