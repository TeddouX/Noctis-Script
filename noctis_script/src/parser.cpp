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
    switch (type) {
        case TokenType::INT_KWD:
        case TokenType::FLOAT_KWD:
            return true;
        default:
            return false;
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
    ScriptNode declNode(ScriptNodeType::VARIABLE_DECLARATION);

    declNode.addChild(parseType());
    declNode.addChild(parseIdentifier());

    Token &t = peek(0);
    if (t.type == TokenType::EQUAL) {
        
    }
    else if (t.type == TokenType::SEMICOLON) {
        consume();
        return declNode;
    }
}

ScriptNode Parser::parseType() {
    ScriptNode dataType(ScriptNodeType::DATA_TYPE);

    Token &t = consume();
    if (!isDataType(t.type)) {
        std::cerr << "Expected a data type but got: " << t.val;
        exit(-1);
    }

    dataType.token = &t;
    return dataType;
}

ScriptNode Parser::parseIdentifier() {
    ScriptNode id(ScriptNodeType::IDENTIFIER);

    Token &t = consume();
    if (t.type != TokenType::ID) {
        std::cerr << "Expected an identifer but got: " << t.val;
        exit(-1);
    }

    id.token = &t;
    return id;
}

} // namespace NCSC

