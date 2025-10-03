#pragma once
#include <vector>
#include <string>
#include <memory>

#include "token.hpp"
#include "ncsc.hpp"

namespace NCSC
{

class NCSC_API Lexer {
public:
    Lexer(const std::string &source)
        : source_(source) {}

    std::vector<Token> tokenizeAll();
    
private:
    std::string source_;
    size_t currIdx_ = 0;
    uint32_t line_ = 1, column_ = 1;
    
    std::unique_ptr<Token> getCurrent();

    void advance(int amount = 1);
    std::unique_ptr<Token> createToken(TokenType type, const std::string &val = "");
};

} // namespace NCSC
