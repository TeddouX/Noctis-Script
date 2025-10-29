// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/lexer.hpp>
#include <iostream>

namespace NCSC
{

static const std::unordered_map<char, TokenType> singleCharTokens = {
    { ';', TokenType::SEMICOLON },
    { '(', TokenType::PARENTHESIS_OPEN },
    { ')', TokenType::PARENTHESIS_CLOSE },
    { '{', TokenType::CURLY_BRACE_OPEN },
    { '}', TokenType::CURLY_BRACE_CLOSE },
    { ',', TokenType::COMMA },
};
    
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

    // Comment
    if (currChar == '/' && source_[currIdx_ + 1] == '/') {
        int len = 0;
        while (currIdx_ + len < source_.length()) {
            char charAt = source_.at(currIdx_ + len);
            len++;
            if (charAt == '\n')
                break;
        }
        
        advance(len);

        line_++;
        column_ = 1;
        
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
            return createToken(TokenType::DOT);

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
            if (std::isalpha(charAt) || std::isdigit(charAt) || charAt == '_') {
                len++;
                continue;
            }
            break;
        }

        std::string val = source_.substr(currIdx_, len);
        advance(len);

        auto it = tokensStringToTok.find(val);
        if (it != tokensStringToTok.end())
            return createToken(it->second);
        
        return createToken(TokenType::ID, val);
    }

    auto it = singleCharTokens.find(currChar);
    if (it != singleCharTokens.end()) {
        advance();
        return createToken(it->second);
    }

    switch (currChar) {
        case '=': return matchOptional('=', TokenType::EQUAL, TokenType::DOUBLE_EQUAL);
        case '+':
            advance();
            switch (source_[currIdx_]) {
                case '=': advance(); return createToken(TokenType::PLUS_EQUAL);
                case '+': advance(); return createToken(TokenType::PLUS_PLUS);
                default:             return createToken(TokenType::PLUS);
            }
        case '-':
            advance();
            switch (source_[currIdx_]) {
                case '=': advance(); return createToken(TokenType::MINUS_EQUAL);
                case '-': advance(); return createToken(TokenType::MINUS_MINUS);
                default:             return createToken(TokenType::MINUS);
            }
        case '/': return matchOptional('=', TokenType::SLASH, TokenType::SLASH_EQUAL);
        case '*': return matchOptional('=', TokenType::STAR,  TokenType::STAR_EQUAL);
    
        case '>': return matchOptional('=', TokenType::STRICTLY_BIGGER, TokenType::BIGGER_EQUAL);
        case '<': return matchOptional('=', TokenType::STRICTLY_SMALLER, TokenType::SMALLER_EQUAL);
        case '!': return matchOptional('=', TokenType::NOT, TokenType::NOT_EQUAL);
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

std::unique_ptr<Token> Lexer::matchOptional(char next, TokenType single, TokenType combined) {
    // Consume the first char
    advance();
    if (source_.at(currIdx_) == next) {
        advance();
        return createToken(combined);
    }
    return createToken(single);
}

} // namespace NCSC

