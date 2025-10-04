#include <ncsc/parser.hpp>
#include <iostream>

namespace NCSC
{

ScriptNode Parser::parseAll() {
    ScriptNode root(ScriptNodeType::SCRIPT);

    for (size_t i = 0; i < tokens_.size(); i++) {
        const Token &currTok = tokens_[i];
        
        // int, float, ...
        if (isDataType(currTok.type)) {
            if (isVariableDeclaration())
                root.addChild(parseVariableDeclaration());
            else {
                Token &t = consume();
                createSyntaxError(I_DUNNO_WHAT_TO_NAME_THIS, t);
            }
        }
    }

    return root;
}

void Parser::createSyntaxError(const std::string &message, const Token &tok) {
    hasSyntaxError_ = true;
    // TODO: make ts better
    std::cout << message << std::endl;
    std::cout << "Location (line:col): " << tok.line << ":" << tok.col << std::endl;
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
    // Variable decleration should have a name
    if (t.type != TokenType::ID)
        return false;

    t = peek(2);
    // Decleration can stop at a semicolon or it has an equal
    // int a;
    // int a = (...)
    if (t.type == TokenType::SEMICOLON || t.type == TokenType::EQUAL)
        return true;
    else
        return false;
}

ScriptNode Parser::parseVariableDeclaration() {
    ScriptNode node(ScriptNodeType::VARIABLE_DECLARATION);

    node.addChild(parseType());
    node.addChild(parseIdentifier());

    Token &t = peek(0);
    if (t.type == TokenType::EQUAL)
        node.addChild(parseExpression());
    else if (t.type == TokenType::SEMICOLON)
        consume();
    else {
        consume();
        createSyntaxError(UNEXPECTED_TOKEN_DURING_VARIABLE_DECL, t);
    }
    
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
    // The first part of an expression should 
    // always be an expression term
    if (hasSyntaxError_) return node;

    for (;;) {
        Token &t1 = peek(0);
        if (isOperator(t1.type))
            node.addChild(parseExpressionOperator());
        else
            // The expression is finished, there should be no more terms left over
            break;
        
        node.addChild(parseExpressionTerm());
        // There should always be a term after an operator
        if (hasSyntaxError_) return node;
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

