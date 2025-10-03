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
        }
    }

    return root;
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

bool Parser::isExpressionTerm() {
    Token t = peek(0);
    if (t.type == TokenType::ID) {
        // TODO: check against a cache of already parsed types
        return true;
    }
    else if (isConstantValue(t.type))
        return true;
}

ScriptNode Parser::parseVariableDeclaration() {
    ScriptNode node(ScriptNodeType::VARIABLE_DECLARATION);

    node.addChild(parseType());
    node.addChild(parseIdentifier());

    Token &t = peek(0);
    if (t.type == TokenType::EQUAL) {
        node.addChild(parseExpression());
        
    }
    else if (t.type == TokenType::SEMICOLON) {
        consume();
        return node;
    }
}

ScriptNode Parser::parseType() {
    ScriptNode node(ScriptNodeType::DATA_TYPE);

    Token &t = consume();
    if (!isDataType(t.type)) {
        std::cerr << "Expected a data type but got: " << t.val;
        exit(-1);
    }

    node.token = &t;
    return node;
}

ScriptNode Parser::parseIdentifier() {
    ScriptNode node(ScriptNodeType::IDENTIFIER);

    Token &t = consume();
    if (t.type != TokenType::ID) {
        std::cerr << "Expected an identifer but got: " << t.val;
        exit(-1);
    }

    node.token = &t;
    return node;
}

ScriptNode Parser::parseExpression() {
    ScriptNode node(ScriptNodeType::EXPRESSION);

    Token &t = consume();
    if (!isExpressionTerm()) {
        std::cerr << "Expected an expression term (func call, constant, ...) but got: " << t.val << std::endl;
        exit(-1);
    }

    // First expression
    node.addChild(parseExpressionTerm());

    for (;;) {
        Token &t1 = peek(0);
        if (isOperator(t1.type))
            node.addChild(parseExpressionOperator());
        else
            // The expression is finished, there should be no more terms left over
            return node;
        
        // There should always be a term after an operator
        node.addChild(parseExpressionTerm());
    }

    return node;
}

ScriptNode Parser::parseExpressionTerm() {
    ScriptNode node(ScriptNodeType::EXPRESSION_TERM);

    Token &t = peek(0);
    if (!isExpressionTerm()) {
        std::cerr << "Expected an expression term (func call, constant, ...) but got: " << t.val << std::endl;
        exit(-1);
    }
    
    if (isConstantValue(t.type))
        node.addChild(parseConstant());
    else if (t.type == TokenType::ID)
        node.addChild(parseIdentifier());

    return node;
}

ScriptNode Parser::parseExpressionOperator() {
    ScriptNode node(ScriptNodeType::EXPRESSION_OPERATOR);

    Token &t = peek(0);
    if (!isExpressionTerm()) {
        std::cerr << "Expected an expression term (func call, constant, ...) but got: " << t.val << std::endl;
        exit(-1);
    }
    
    if (isConstantValue(t.type))
        node.addChild(parseConstant());
    else if (t.type == TokenType::ID)
        node.addChild(parseIdentifier());

    return node;
}

ScriptNode Parser::parseConstant() {
    ScriptNode node(ScriptNodeType::CONSTANT);

    Token &t = consume();
    if (!isConstantValue(t.type)) {
        std::cerr << "Expected a constant value. Instead got: " << t.val << std::endl;
        exit(-1);
    }

    node.token = &t;
    return node;
}

} // namespace NCSC

