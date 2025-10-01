#include <ncsc/parser.h>
#include <iostream>

namespace NCSC
{
    
void Parser::parseAll() {
    

    for (size_t i = 0; i < tokens_.size(); i++) {
        const Token &currTok = tokens_[i];
        
    }
}

Token Parser::consume(TokenType type) {
    if (idx_ > tokens_.size()) {
        std::cerr << "Unexpected end of tokens" << std::endl;
        exit(-1);
    }
    
    Token tok = tokens_[idx_];

    if (tok.type != type) {
        std::cerr << "Expected token: " << (int)type << "; Got: " << (int)type << std::endl;
        exit(-1);
    }

    idx_++;
    return tok;
}

TokenType Parser::peek() {
    if (idx_ > tokens_.size()) {
        std::cerr << "Unexpected end of tokens" << std::endl;
        exit(-1);
    }

    return tokens_[idx_].type;
}

} // namespace NCSC

