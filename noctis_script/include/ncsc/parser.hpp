#include <vector>

#include "script_node.hpp"
#include "syntax_error.hpp"
#include "token.hpp"
#include "ncsc.hpp"

namespace NCSC
{

// Error messages
constexpr auto EXPECTED_A_SEMICOLON        = "Expected a semicolon (';')";
constexpr auto EXPECTED_EXPRESSION_TERM    = "Expected an expression term (function call, constant, variable...)";
constexpr auto EXPECTED_CONSTANT_VALUE     = "Expected a constant value (1234, 123.456, ...)";
constexpr auto EXPECTED_AN_IDENTIFIER      = "Expected an identifier";
constexpr auto EXPECTED_A_DATA_TYPE        = "Expected a data type (Int, Float, ...)";
constexpr auto EXPECTED_AN_OPERATOR        = "Expected an operator ('+', '-', '*', '/', ...)";
constexpr auto EXPECTED_TOKEN              = "Expected '%s'";
constexpr auto EXPECTED_TOKEN_OR_TOKEN     = "Expected '%s' or '%s'";
constexpr auto UNEXPECTED_IDENTIFIER       = "Unexpected identifier";
constexpr auto UNEXPECTED_EOF              = "Unexpected end of file";

class NCSC_API Parser {
public:
    Parser(const std::vector<Token> &tokens)
        : tokens_(tokens) {}

    ScriptNode parseAll();

    // If the parser has errors compiling will result in some weird 
    // errors, so please don't compile
    bool hasErrors() { return !syntaxErrors_.empty(); }
    const std::vector<SyntaxError> &getErrors() { return syntaxErrors_; }

private:
    std::vector<Token> tokens_;
    size_t idx_ = 0;
    bool hasSyntaxError_ = false;
    std::vector<SyntaxError> syntaxErrors_;

    void createSyntaxError(const char *message, const Token &tok);

    Token &consume();
    Token &peek(int amount = 1);

    bool isDataType(TokenType type);
    bool isConstantValue(TokenType type);
    bool isOperator(TokenType type);

    bool isVariableDeclaration();
    bool isFunction();

    ScriptNode parseVariableDeclaration(); 
    ScriptNode parseType();
    ScriptNode parseIdentifier();
    ScriptNode parseConstant();
    ScriptNode parseExpression();
    ScriptNode parseExpressionTerm();
    ScriptNode parseExpressionOperator();
    ScriptNode parseFunction();
    ScriptNode parseStatementBlock();
};

} // namespace NCSC
