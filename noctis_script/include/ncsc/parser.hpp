// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
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
    Parser(const std::vector<Token> &tokens)
        : tokens_(tokens) {}

    ScriptNode parseAll();

    // If the parser has errors compiling will result in some weird 
    // errors, so please don't compile
    bool hasErrors() { return !syntaxErrors_.empty(); }
    const std::vector<Error> &getErrors() { return syntaxErrors_; }

private:
    std::vector<Token> tokens_;
    size_t idx_ = 0;

    bool hasSyntaxError_ = false;
    std::vector<Error> syntaxErrors_;

    void createSyntaxError(const std::string &message, const Token &tok);

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

    inline static constexpr std::string_view EXPECTED_A_SEMICOLON        = "Syntax error S0: Expected a semicolon (';')";
    inline static constexpr std::string_view EXPECTED_EXPRESSION_TERM    = "Syntax error S1: Expected an expression term (function call, constant, variable...)";
    inline static constexpr std::string_view EXPECTED_CONSTANT_VALUE     = "Syntax error S2: Expected a constant value (1234, 123.456, ...)";
    inline static constexpr std::string_view EXPECTED_AN_IDENTIFIER      = "Syntax error S3: Expected an identifier";
    inline static constexpr std::string_view EXPECTED_A_DATA_TYPE        = "Syntax error S4: Expected a data type (Int, Float, ...)";
    inline static constexpr std::string_view EXPECTED_A_DATA_TYPE_OR_FUN = "Syntax error S5: Expected a data type (Int, Float, ...) or 'fun'";
    inline static constexpr std::string_view EXPECTED_AN_OPERATOR        = "Syntax error S6: Expected an operator ('+', '-', '*', '/', ...)";
    inline static constexpr std::string_view EXPECTED_TOKEN              = "Syntax error S7: Expected '{}'";
    inline static constexpr std::string_view EXPECTED_TOKEN_OR_TOKEN     = "Syntax error S8: Expected '{}' or '{}'";
    inline static constexpr std::string_view EXPECTED_STATEMENT          = "Syntax error S9: Expected a statement";
    inline static constexpr std::string_view UNEXPECTED_TOKEN            = "Syntax error S10: Unexpected token '{}'";
    inline static constexpr std::string_view UNEXPECTED_EOF              = "Syntax error S11: Unexpected end of file";
};

} // namespace NCSC
