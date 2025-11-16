// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <vector>

#include "ast_node.hpp"
#include "error.hpp"
#include "token.hpp"
#include "ncsc.hpp"

namespace NCSC
{

class NCSC_API Parser {
public:
    Parser(const std::vector<Token> &tokens, std::shared_ptr<ScriptSource> src);

    ASTNode parseAll();

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
    bool tryEscapeSyntaxError(int startingLvl = 0);

    Token &consume();
    Token &peek(int amount = 1);

    int getOperatorPrecedence(const Token &tok);

    bool isVariableDeclaration(bool isMember = false);
    bool isFunction(bool isMethod = false);
    bool isFunctionCall();

    ASTNode parseToken(Token &tok);
    ASTNode parseVariableDeclaration(bool isMember = false); 
    ASTNode parseType();
    ASTNode parseIdentifier();
    ASTNode parseConstant();
    ASTNode parseExpression();
    ASTNode parseExpressionTerm();
    // ASTNode parseExpressionOperator();
    ASTNode parseFunction(bool isMember = false);
    ASTNode parseStatementBlock();
    ASTNode parseStatement();
    ASTNode parseSimpleStatement();
    ASTNode parseFunctionCall();
    ASTNode parseIfStatement();
    ASTNode parseAssignment(bool allowCompoundOps);
    ASTNode parseReturnStatement();
    ASTNode parseAssignmentOperator(bool allowCompoundOps);
    ASTNode parseObject();
    ASTNode parseConstructCall();
    ASTNode parseArgList();
    ASTNode parseExpressionValue();
    ASTNode parseExpressionPreOp();
    ASTNode parseExpressionPostOp();

    inline static ErrInfo EXPECTED_A_SEMICOLON         { "SyntaxError", "S", 0,  "Expected a semicolon (';')" };
    inline static ErrInfo EXPECTED_EXPRESSION_VALUE    { "SyntaxError", "S", 1,  "Expected an expression value (function call, constant, variable...)" };
    inline static ErrInfo EXPECTED_CONSTANT_VALUE      { "SyntaxError", "S", 2,  "Expected a constant value (1234, 123.456, ...)" };
    inline static ErrInfo EXPECTED_AN_IDENTIFIER       { "SyntaxError", "S", 3,  "Expected an identifier" };
    inline static ErrInfo EXPECTED_A_DATA_TYPE         { "SyntaxError", "S", 4,  "Expected a data type (Int, Float, ...)" };
    inline static ErrInfo EXPECTED_A_DATA_TYPE_OR_FUN  { "SyntaxError", "S", 5,  "Expected a data type (Int, Float, ...) or 'fun'" };
    inline static ErrInfo EXPECTED_AN_OPERATOR         { "SyntaxError", "S", 6,  "Expected an operator ('+', '-', '*', '/', ...)" };
    inline static ErrInfo EXPECTED_TOKEN               { "SyntaxError", "S", 7,  "Expected '{}'" };
    inline static ErrInfo EXPECTED_TOKEN_OR_TOKEN      { "SyntaxError", "S", 8,  "Expected '{}' or '{}'" };
    inline static ErrInfo EXPECTED_STATEMENT           { "SyntaxError", "S", 9,  "Expected a statement" };
    inline static ErrInfo UNEXPECTED_TOKEN             { "SyntaxError", "S", 10, "Unexpected token '{}'" };
    inline static ErrInfo UNEXPECTED_EOF               { "SyntaxError", "S", 11, "Unexpected end of file" };
    inline static ErrInfo EXPECTED_ASSIGN_OP           { "SyntaxError", "S", 12, "Expected an assignement operator ('=', '+=', '-=', ...)" };
    inline static ErrInfo EXPECTED_VARDECL_OR_FUNC_DEF { "SyntaxError", "S", 13, "Expected a variable declaration or a function declaration" };
};

} // namespace NCSC
