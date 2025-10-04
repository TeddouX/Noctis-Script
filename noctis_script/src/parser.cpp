#include <ncsc/parser.hpp>
#include <sstream>
#include <iostream>
#include <stack>

#define CHECK_SYNTAX_ERROR if (hasSyntaxError_) return node

namespace NCSC
{

ScriptNode Parser::parseAll() {
    ScriptNode root(ScriptNodeType::SCRIPT);

    while (idx_ < tokens_.size()) {
        const Token &currTok = tokens_[idx_];
        
        // int, float, ...
        if (isDataType(currTok.type)) {
            if (isVariableDeclaration())
                root.addChild(parseVariableDeclaration());
        }
        else if (currTok.type == TokenType::END_OF_FILE) 
            break;
        // Extra semicolons can be ignored
        else if (currTok.type == TokenType::SEMICOLON) {
            consume();
            continue;
        }
        else {
            consume();
            createSyntaxError(UNEXPECTED_IDENTIFIER, currTok);
        }
    }

    return root;
}

void Parser::createSyntaxError(const char *message, const Token &tok) {
    hasSyntaxError_ = true;
    // TODO: make ts better
    syntaxErrors_.push_back(SyntaxError(&tok, message));
}

Token &Parser::consume() {
    if (idx_ >= tokens_.size()) {
        std::cerr << "Unexpected end of tokens" << std::endl;
        exit(-1);
    }
    
    Token &tok = tokens_[idx_];
    idx_++;
    return tok;
}

Token &Parser::peek(int amount) {
    if (idx_ + amount > tokens_.size()) {
        std::cerr << "Unexpected end of tokens" << std::endl;
        exit(-1);
    }
    return tokens_[idx_ + amount];
}

bool Parser::isDataType(TokenType type) {
    // TODO: Compare against a list of cached types
    switch (type) {
        case TokenType::INT_KWD:
        case TokenType::FLOAT_KWD:
            return true;
            break;
        default:
            return false;
            break;
    }
}

bool Parser::isConstantValue(TokenType type) {
    switch (type) {
        case TokenType::INT_CONSTANT:
        case TokenType::FLOAT_CONSTANT:
            return true;
            break;   
        default:
            return false;
            break;
    }
}

bool Parser::isOperator(TokenType type) {
    switch (type) {
        case TokenType::PLUS:
        case TokenType::MINUS:
        case TokenType::STAR:
        case TokenType::SLASH:
            return true;
            break;   
        default:
            return false;
            break;
    }
}

bool Parser::isVariableDeclaration() {
    Token t = peek(0);
    if (!isDataType(t.type))
        return false;

    t = peek(1);
    // Variable declaration should have a name
    if (t.type != TokenType::ID)
        return false;

    t = peek(2);
    // Declaration can stop at a semicolon or it has an equal
    // int a;
    // int a = (...)
    if (t.type == TokenType::SEMICOLON || t.type == TokenType::EQUAL)
        return true;
    else
        return false;
}

ScriptNode Parser::parseVariableDeclaration() {
    ScriptNode node(ScriptNodeType::VARIABLE_DECLARATION);

    node.addChild(parseType());       CHECK_SYNTAX_ERROR;
    node.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;

    Token &t = peek(0);
    if (t.type == TokenType::EQUAL) {
        consume();
        node.addChild(parseExpression()); 
        // There should be an expression after an equal
        CHECK_SYNTAX_ERROR;
    }

    // Variable declaration should end with a semicolon
    Token &t1 = consume();
    if (t1.type != TokenType::SEMICOLON)
        createSyntaxError(EXPECTED_A_SEMICOLON, t1);
    
    return node;
}

ScriptNode Parser::parseType() {
    ScriptNode node(ScriptNodeType::DATA_TYPE);

    Token &t = consume();
    if (!isDataType(t.type)) {
        createSyntaxError(EXPECTED_A_DATA_TYPE, t);
        return node;
    }

    node.token = &t;
    return node;
}

ScriptNode Parser::parseIdentifier() {
    ScriptNode node(ScriptNodeType::IDENTIFIER);

    Token &t = consume();
    if (t.type != TokenType::ID) {
        createSyntaxError(EXPECTED_AN_IDENTIFIER, t);
        return node;
    }

    node.token = &t;
    return node;
}

ScriptNode Parser::parseExpression() {
    ScriptNode node(ScriptNodeType::EXPRESSION);

    node.addChild(parseExpressionTerm());
    // The first part of an expression should always be a term
    CHECK_SYNTAX_ERROR;

    for (int i = 0;; i++) {
        Token &t1 = peek(i);
        if (isOperator(t1.type))
            node.addChild(parseExpressionOperator());
        else
            // The expression is finished, there should be no more terms left over
            break;
        
        node.addChild(parseExpressionTerm());
        // There should always be a term after an operator
        CHECK_SYNTAX_ERROR;
    }

    return node;
}

ScriptNode Parser::parseExpressionTerm() {
    ScriptNode node(ScriptNodeType::EXPRESSION_TERM);

    Token &t = peek(0);
    if (isConstantValue(t.type))
        node.addChild(parseConstant());
    else if (t.type == TokenType::ID)
        node.addChild(parseIdentifier());
    else {
        consume();
        createSyntaxError(EXPECTED_EXPRESSION_TERM, t);
    }

    return node;
}

ScriptNode Parser::parseExpressionOperator() {
    ScriptNode node(ScriptNodeType::EXPRESSION_OPERATOR);

    Token &t = consume();
    if (!isOperator(t.type)) {
        createSyntaxError(EXPECTED_AN_OPERATOR, t);
        return node;
    }

    node.token = &t;
    return node;
}

ScriptNode Parser::parseConstant() {
    ScriptNode node(ScriptNodeType::CONSTANT);

    Token &t = consume();
    if (!isConstantValue(t.type)) {
        createSyntaxError(EXPECTED_CONSTANT_VALUE, t);
        return node;
    }

    node.token = &t;
    return node;
}

} // namespace NCSC

