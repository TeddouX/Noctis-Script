#include <ncsc/lexer.h>
#include <iostream>

namespace NCSC
{
    
static bool isWhitespace(char c) {
    return c == ' ' || c == '\t' || c == '\n';
}

std::vector<Token> Lexer::tokenizeAll() {
    std::vector<Token> tokens;

    for (;;) {
        auto curr = getCurrent();
        if (curr)
            tokens.push_back(*curr);

        if (currIdx_ >= source_.length())
            break;
    }

    tokens.push_back(Token{TokenType::END_OF_FILE, ""});

    return tokens;
}

std::unique_ptr<Token> Lexer::getCurrent() {
    char currChar = source_.at(currIdx_);

    // Remove all whitespaces
    if (isWhitespace(currChar)) {
        int len = 1;
        // Remove until the end of the source
        while (currIdx_ + len < source_.length()) {
            char charAt = source_.at(currIdx_ + len);
            if (!isWhitespace(charAt))
                break;
            len++;
        }

        currIdx_ += len;
        // No token for whitespaces
        return nullptr;
    }

    // 1234 or 123.456 or .1 or 1.
    // Also tokenizes .
    if (std::isdigit(currChar) || currChar == '.') {
        bool hasPoint = false;
        bool hasDigits = false;
        
        if (std::isdigit(currChar))
            hasDigits = true;
        else if (currChar == '.') 
            hasPoint = true;
        
        int len = 1;
        while (currIdx_ + len < source_.length()) {
            char charAt = source_.at(currIdx_ + len);
            bool charAtIsPoint = (charAt == '.');
            bool charAtIsDigit = std::isdigit(charAt);

            if (charAtIsDigit || (charAtIsPoint && !hasPoint)) {
                if (charAtIsPoint)
                    hasPoint = true;
                else if (charAtIsDigit)
                    hasDigits = true;

                len++;
                continue;
            }
            break;
        }

        std::string val = source_.substr(currIdx_, len);
        currIdx_ += len;
        
        if (hasPoint && !hasDigits)
            return std::make_unique<Token>(TokenType::POINT, val);

        if (hasPoint) {
            // .1 -> 0.1
            if (val.at(0) == '.') 
                val = '0' + val;
            // 1. -> 1.0
            if (val.at(val.length() - 1) == '.')
                val += '0';
        }

        TokenType type = hasPoint ? TokenType::FLOAT : TokenType::INT;
        return std::make_unique<Token>(type, val);
    }

    // a1_b2 or reserved token
    if (std::isalpha(currChar) || currChar == '_') {
        int len = 1;
        while (currIdx_ + len < source_.length()) {
            char charAt = source_.at(currIdx_ + len);
            if (std::isalpha(charAt) || charAt == '_') {
                len++;
                continue;
            }
            break;
        }

        std::string val = source_.substr(currIdx_, len);
        currIdx_ += len;

        auto it = g_reservedTokensStringToTok.find(val);
        if (it != g_reservedTokensStringToTok.end())
            return std::make_unique<Token>(it->second, "");
        
        return std::make_unique<Token>(TokenType::ID, val);
    }

    switch (currChar) {
        case '+': {
            currIdx_++;

            if (currIdx_ >= source_.length())
                return std::make_unique<Token>(TokenType::PLUS, "");
            else if (source_.at(currIdx_) == '=') {
                currIdx_++;
                return std::make_unique<Token>(TokenType::PLUS_EQUAL, "");
            }
            break;
        }
        case '-': {
            currIdx_++;

            if (currIdx_ >= source_.length())
                return std::make_unique<Token>(TokenType::MINUS, "");
            else if (source_.at(currIdx_) == '=') {
                currIdx_++;
                return std::make_unique<Token>(TokenType::MINUS_EQUAL, "");
            }
            break;
        }  
        case '*': {
            currIdx_++;

            if (currIdx_ >= source_.length())
                return std::make_unique<Token>(TokenType::STAR, "");
            else if (source_.at(currIdx_) == '=') {
                currIdx_++;
                return std::make_unique<Token>(TokenType::STAR_EQUAL, "");
            }
            break;
        }  
        case '/': {
            currIdx_++;

            if (currIdx_ >= source_.length())
                return std::make_unique<Token>(TokenType::SLASH, "");
            else if (source_.at(currIdx_) == '=') {
                currIdx_++;
                return std::make_unique<Token>(TokenType::SLASH_EQUAL, "");
            }
            break;
        }
        case '=': {
            currIdx_++;
            return std::make_unique<Token>(TokenType::EQUAL, "");
            break;
        }
        
        case ';': {
            currIdx_++;
            return std::make_unique<Token>(TokenType::SEMICOLON, "");
            break;
        }
    }

    currIdx_++;
    return std::make_unique<Token>(
        TokenType::INVALID, 
        std::string(1, currChar));
}

} // namespace NCSC

