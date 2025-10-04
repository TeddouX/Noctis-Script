#include <ncsc/token.hpp>

namespace NCSC
{
    
static bool isSpecialTok(TokenType type) {
    return type >= TokenType::END_OF_FILE;
}

static std::string getTokTypeStrRepr(TokenType type) {
    // "Special" tokens like eof, etc...
    switch (type)
    {
        case TokenType::END_OF_FILE:
            return "<eof>";
            break;
        default:
            break;
    }
        
    // Reserved tokens
    for (const auto &[str, tokTy] : tokensStringToTok) {
        if (tokTy == type)
            return str;
    }

    return "";
} 

std::string Token::getStrRepr() const {
    if (val.empty())
        return getTokTypeStrRepr(type);
    else
        return val;
}

size_t Token::getLength() const {
    if (isSpecialTok(type))
        return 0;
    else
        return getStrRepr().size();
}

} // namespace NCSC
