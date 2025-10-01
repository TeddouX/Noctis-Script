#include <vector>

#include "token.h"
#include "ncsc.h"

namespace NCSC
{

class NCSC_API Parser {
public:
    Parser(const std::vector<Token> &tokens)
        : tokens_(tokens) {}

    void parseAll();

private:
    std::vector<Token> tokens_;
    size_t idx_;

    Token consume(TokenType type);
    TokenType peek();
};

} // namespace NCSC
