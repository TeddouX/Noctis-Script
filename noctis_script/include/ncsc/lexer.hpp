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
    
    std::unique_ptr<Token> getCurrent();
};

} // namespace NCSC
