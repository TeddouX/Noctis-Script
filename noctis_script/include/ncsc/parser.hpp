#include <vector>

#include "script_node.hpp"
#include "token.hpp"
#include "ncsc.hpp"

// Error messages
constexpr auto EXPECTED_EXPRESSION_TERM              = "Expected an expression term (function call, constant, variable...)";
constexpr auto EXPECTED_CONSTANT_VALUE               = "Expected a constant value (1234, 123.456, ...)";
constexpr auto EXPECTED_AN_IDENTIFIER                = "Expected an identifier";
constexpr auto EXPECTED_A_DATA_TYPE                  = "Expected a data type (int, float, ...)";
constexpr auto EXPECTED_AN_OPERATOR                  = "Expected an operator ('+', '-', '*', '/', ...)";
constexpr auto UNEXPECTED_TOKEN_DURING_VARIABLE_DECL = "Unexpected token during variable declaration";
constexpr auto I_DUNNO_WHAT_TO_NAME_THIS             = "Only variable declarations are supported for now ;)";

namespace NCSC
{

class NCSC_API Parser {
public:
    Parser(const std::vector<Token> &tokens)
        : tokens_(tokens) {}

    ScriptNode parseAll();
    bool hasErrors() { return hasSyntaxError_; }
    // std::vector<SyntaxError> getErrors() { return syntaxErrors; }

private:
    std::vector<Token> tokens_;
    size_t idx_ = 0;
    bool hasSyntaxError_ = false;
    // std::vector<SyntaxError> syntaxErrors;

    void createSyntaxError(const std::string &message, const Token &tok);

    Token &consume();
    Token &peek(int amount = 1);

    bool isDataType(TokenType type);
    bool isConstantValue(TokenType type);
    bool isOperator(TokenType type);

    bool isVariableDeclaration();

    ScriptNode parseVariableDeclaration(); 
    ScriptNode parseType();
    ScriptNode parseIdentifier();
    ScriptNode parseConstant();
    ScriptNode parseExpression();
    ScriptNode parseExpressionTerm();
    ScriptNode parseExpressionOperator();
};

} // namespace NCSC
