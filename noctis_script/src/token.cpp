#include <ncsc/token.hpp>

namespace NCSC
{
    
static std::string getTokTypeStrRepr(TokenType type) {
    for (const auto &[str, tokTy] : g_reservedTokensStringToTok) {
        if (tokTy == type)
            return str;
    }

    return "";
} 

std::string Token::getStrRepr() {
    if (val.empty()) {
        return getTokTypeStrRepr(type);
    } else {
        return val;
    }
}

size_t Token::getLength() {
    return getStrRepr().size();
}

} // namespace NCSC
