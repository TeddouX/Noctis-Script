#include <ncsc/lexer.hpp>
#include <iostream>

namespace NCSC
{
    
static bool isWhitespace(char c) {
    return c == ' ' || c == '\t' || c == '\n';
}

std::vector<Token> Lexer::tokenizeAll() {
    std::vector<Token> tokens;

    while (currIdx_ < source_.length()) {
        if (auto curr = getCurrent())
            tokens.push_back(*curr);
    }

    tokens.push_back(*createToken(TokenType::END_OF_FILE));

    return tokens;
}

std::unique_ptr<Token> Lexer::getCurrent() {
    char currChar = source_.at(currIdx_);

    // Remove all whitespaces
    if (isWhitespace(currChar)) {
        int len = 0;
        int numLines = 0;
        // Remove until the end of the source
        while (currIdx_ + len < source_.length()) {
            char charAt = source_.at(currIdx_ + len);
            
            // Count number of lines and columns
            // column count is also managed inside the token code
            if (charAt == '\n') {
                column_ = 1;
                numLines++;
            }

            if (!isWhitespace(charAt))
                break;
            len++;
        }

        advance(len);

        line_ += numLines;    
        // Every \n is added to the total number of 
        // whitespaces, so remove them
        column_ -= numLines;

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
        advance(len);

        if (hasPoint && !hasDigits)
            return createToken(TokenType::POINT);

        if (hasPoint) {
            // .1 -> 0.1
            if (val.at(0) == '.') 
                val = '0' + val;
            // 1. -> 1.0
            if (val.at(val.length() - 1) == '.')
                val += '0';
        }

        TokenType type = hasPoint ? TokenType::FLOAT_CONSTANT : TokenType::INT_CONSTANT;
        return createToken(type, val);
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
        advance(len);

        auto it = g_reservedTokensStringToTok.find(val);
        if (it != g_reservedTokensStringToTok.end())
            return createToken(it->second);
        
        return createToken(TokenType::ID, val);
    }

    switch (currChar) {
        case '+': {
            advance();

            if (currIdx_ >= source_.length())
                return createToken(TokenType::PLUS);
            else if (source_.at(currIdx_) == '=') {
                advance();
                return createToken(TokenType::PLUS_EQUAL);
            }
            break;
        }
        case '-': {
            advance();

            if (currIdx_ >= source_.length())
                return createToken(TokenType::MINUS);
            else if (source_.at(currIdx_) == '=') {
                advance();
                return createToken(TokenType::MINUS_EQUAL);
            }
            break;
        }  
        case '*': {
            advance();

            if (currIdx_ >= source_.length())
                return createToken(TokenType::STAR);
            else if (source_.at(currIdx_) == '=') {
                advance();
                return createToken(TokenType::STAR_EQUAL);
            }
            break;
        }  
        case '/': {
            advance();

            if (currIdx_ >= source_.length())
                return createToken(TokenType::SLASH);
            else if (source_.at(currIdx_) == '=') {
                advance();
                return createToken(TokenType::SLASH_EQUAL);
            }
            break;
        }
        case '=': {
            advance();
            return createToken(TokenType::EQUAL);
            break;
        }
        
        case ';': {
            advance();
            return createToken(TokenType::SEMICOLON);
            break;
        }
    }

    advance();
    return createToken(TokenType::INVALID, std::string(1, currChar));
}

void Lexer::advance(int amount) {
    currIdx_ += amount;
    column_ += amount;
}

std::unique_ptr<Token> Lexer::createToken(TokenType type, const std::string &val) {
    auto tok = std::make_unique<Token>(type, val, line_, column_);
    // Column starts at the beginning of the token
    tok->col -= tok->getLength();

    return std::move(tok);
}

} // namespace NCSC

