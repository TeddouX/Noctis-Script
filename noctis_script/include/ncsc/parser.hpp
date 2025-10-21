
#pragma once
#include <vector>

#include "script_node.hpp"
#include "error.hpp"
#include "token.hpp"
#include "ncsc.hpp"

namespace NCSC
{

class NCSC_API Parser {
public:
    Parser(const std::vector<Token> &tokens, std::shared_ptr<ScriptSource> src);

    ScriptNode parseAll();

    // If the parser has errors compiling will result in some weird 
    // errors, so please don't compile
    bool hasErrors() { return !syntaxErrors_.empty(); }
    const std::vector<Error> &getErrors() { return syntaxErrors_; }

private:
    std::shared_ptr<ScriptSource> src_;
    std::vector<Token> tokens_;
    size_t idx_ = 0;

    bool hasSyntaxError_ = false;
    std::vector<Error> syntaxErrors_;

    void createSyntaxError(const ErrInfo &info, const Token &tok);

    Token &consume();
    Token &peek(int amount = 1);

    bool isDataType(TokenType type);
    bool isConstantValue(TokenType type);
    bool isOperator(TokenType type);

    bool isVariableDeclaration();
    bool isFunction();
    bool isFunctionCall();

    ScriptNode parseToken(Token &tok);
    ScriptNode parseVariableDeclaration(); 
    ScriptNode parseType();
    ScriptNode parseIdentifier();
    ScriptNode parseConstant();
    ScriptNode parseExpression();
    ScriptNode parseExpressionTerm();
    // ScriptNode parseExpressionOperator();
    ScriptNode parseFunction();
    ScriptNode parseStatementBlock();
    ScriptNode parseStatement();
    ScriptNode parseSimpleStatement();
    ScriptNode parseFunctionCall();

    inline static ErrInfo EXPECTED_A_SEMICOLON        { "SyntaxError", "S", 0,  "Expected a semicolon (';')" };
    inline static ErrInfo EXPECTED_EXPRESSION_TERM    { "SyntaxError", "S", 1,  "Expected an expression term (function call, constant, variable...)" };
    inline static ErrInfo EXPECTED_CONSTANT_VALUE     { "SyntaxError", "S", 2,  "Expected a constant value (1234, 123.456, ...)" };
    inline static ErrInfo EXPECTED_AN_IDENTIFIER      { "SyntaxError", "S", 3,  "Expected an identifier" };
    inline static ErrInfo EXPECTED_A_DATA_TYPE        { "SyntaxError", "S", 4,  "Expected a data type (Int, Float, ...)" };
    inline static ErrInfo EXPECTED_A_DATA_TYPE_OR_FUN { "SyntaxError", "S", 5,  "Expected a data type (Int, Float, ...) or 'fun'" };
    inline static ErrInfo EXPECTED_AN_OPERATOR        { "SyntaxError", "S", 6,  "Expected an operator ('+', '-', '*', '/', ...)" };
    inline static ErrInfo EXPECTED_TOKEN              { "SyntaxError", "S", 7,  "Expected '{}'" };
    inline static ErrInfo EXPECTED_TOKEN_OR_TOKEN     { "SyntaxError", "S", 8,  "Expected '{}' or '{}'" };
    inline static ErrInfo EXPECTED_STATEMENT          { "SyntaxError", "S", 9,  "Expected a statement" };
    inline static ErrInfo UNEXPECTED_TOKEN            { "SyntaxError", "S", 10, "Unexpected token '{}'" };
    inline static ErrInfo UNEXPECTED_EOF              { "SyntaxError", "S", 11, "Unexpected end of file" };
};

} // namespace NCSC
