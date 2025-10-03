#include <vector>

#include "script_node.hpp"
#include "token.hpp"
#include "ncsc.hpp"

namespace NCSC
{

class NCSC_API Parser {
public:
    Parser(const std::vector<Token> &tokens)
        : tokens_(tokens) {}

    ScriptNode parseAll();

private:
    std::vector<Token> tokens_;
    size_t idx_ = 0;

    Token &consume();
    Token &peek(int amount = 1);

    bool isDataType(TokenType type);
    bool isConstantValue(TokenType type);
    bool isOperator(TokenType type);

    bool isVariableDeclaration();
    bool isExpressionTerm();

    ScriptNode parseVariableDeclaration(); 
    ScriptNode parseType();
    ScriptNode parseIdentifier();
    ScriptNode parseConstant();
    ScriptNode parseExpression();
    ScriptNode parseExpressionTerm();
    ScriptNode parseExpressionOperator();
};

} // namespace NCSC
