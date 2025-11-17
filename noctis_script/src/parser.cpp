// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/parser.hpp>
#include <iostream>
#include <stdarg.h>

#define CHECK_SYNTAX_ERROR if (hasSyntaxError_) return node

namespace NCSC
{

static bool isDataType(TokenType type);
static bool isConstantValue(TokenType type);
static bool isOperator(TokenType type);
static bool isAssignOp(TokenType type);
static bool isAccessMod(TokenType type);
static bool isExpressionPreOp(TokenType type);
static bool isExpressionPostOp(TokenType type);


Parser::Parser(const std::vector<Token> &tokens, std::shared_ptr<ScriptSource> src)
    : tokens_(tokens), src_(src) 
{
    assert(src_ != nullptr);
}

ASTNode Parser::parseAll() {
    ASTNode root(ASTNodeType::SCRIPT);

    while (idx_ < tokens_.size()) {
        const Token &currTok = tokens_[idx_];
        
        // int, float, ...
        if (isDataType(currTok.type)) {
            if (isVariableDeclaration()) {
                root.addChild(parseVariableDeclaration());
                // if (hasSyntaxError_) tryEscapeSyntaxError();
            }
            else if (isFunction()) {
                root.addChild(parseFunction());
                // if (hasSyntaxError_) tryEscapeSyntaxError();
            }
            else {
                consume();
                createSyntaxError(EXPECTED_VARDECL_OR_FUNC_DEF, currTok);
            }
        }
        else if (currTok.type == TokenType::FUN_KWD) {
            root.addChild(parseFunction());
            // if (hasSyntaxError_) tryEscapeSyntaxError();
        }
        else if (currTok.type == TokenType::OBJ_KWD) {
            root.addChild(parseObject());
            // if (hasSyntaxError_) tryEscapeSyntaxError();
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
            createSyntaxError(UNEXPECTED_TOKEN.format(currTok.getStrRepr().c_str()), currTok);
        }
        
        hasSyntaxError_ = false;
    }

    return root;
}

void Parser::createSyntaxError(const ErrInfo &info, const Token &tok) {
    hasSyntaxError_ = true;

    Error err(info, src_);
    err.setLocation({ tok.line, tok.line, tok.col, tok.col + tok.getLength() });
    syntaxErrors_.push_back(err);
}

bool Parser::tryEscapeSyntaxError(int startingLvl) {
    for (int i = 0;; i++) {
        Token &t = peek(0);
        if (t.type == TokenType::SEMICOLON && startingLvl == 0) {
            // The comma isn't in an unsafe nested block so 
            // its safe to break out of the loop
            consume();
            break;
        }
        else if (t.type == TokenType::CURLY_BRACE_OPEN) {
            consume();
            startingLvl++;
        }
        else if (t.type == TokenType::CURLY_BRACE_CLOSE) {
            // End of nested block. The bracket will get handled 
            // in the next iteration of the outer for loop
            startingLvl--;
            if (startingLvl == 0)
                break;

            consume();
        }
        else if (t.type == TokenType::END_OF_FILE) {
            createSyntaxError(UNEXPECTED_EOF, t);
            return false;
        }
        else
            consume();
    }
    
    hasSyntaxError_ = false;
    return true;
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
    if (idx_ + amount >= tokens_.size()) {
        std::cerr << "Unexpected end of tokens" << std::endl;
        exit(-1);
    }
    return tokens_[idx_ + amount];
}

int Parser::getOperatorPrecedence(const Token &tok) {
    switch (tok.type) {
        case TokenType::LOGICAL_AND:
            return 1;
        case TokenType::LOGICAL_OR:
            return 2;
        
        case TokenType::PLUS:
        case TokenType::MINUS:
            return 3;
        case TokenType::STAR:
        case TokenType::SLASH:
            return 4;
        // Relative comparisons
        case TokenType::STRICTLY_SMALLER:
        case TokenType::SMALLER_EQUAL:
        case TokenType::STRICTLY_BIGGER:
        case TokenType::BIGGER_EQUAL:
            return 5;
        // Equality comparisons
        case TokenType::DOUBLE_EQUAL:
        case TokenType::NOT_EQUAL:
            return 6;
        
        default: break;
    }

    createSyntaxError(EXPECTED_AN_OPERATOR, tok);
    return -1;
}

bool Parser::isVariableDeclaration(bool isMember) {
    size_t size = 0;
    Token t = peek(size);
    if (isMember && isAccessMod(t.type))
        t = peek(++size);

    if (!isDataType(t.type))
        return false;
    
    t = peek(++size);
    // Variable declaration should have a name
    if (t.type != TokenType::ID)
        return false;

    t = peek(++size);
    // Declaration can stop at a semicolon or it has an equal
    // int a;
    // int a = (...)
    return t.type == TokenType::SEMICOLON || t.type == TokenType::EQUAL;
}

bool Parser::isFunction(bool isMethod) {
    size_t size = 0;
    Token t = peek(size);
    if (isMethod && isAccessMod(t.type))
        t = peek(++size);

    if (!isDataType(t.type) && t.type != TokenType::FUN_KWD)
        return false;
    
    size++;

    t = peek(size);
    // Allow constructors
    if (isMethod && t.type == TokenType::PARENTHESIS_OPEN)
        return true;
    if (t.type != TokenType::ID)
        return false;

    t = peek(++size);
    // int a(...
    return t.type == TokenType::PARENTHESIS_OPEN;
}

bool Parser::isFunctionCall() {
    Token t = peek(0);
    if (t.type != TokenType::ID)
        return false;
    
    t = peek(1);
    return t.type == TokenType::PARENTHESIS_OPEN;
}

ASTNode Parser::parseToken(Token &tok) {
    ASTNode node(ASTNodeType::TOKEN);

    node.setToken(tok);
    node.updatePos();
    return node;
}

ASTNode Parser::parseVariableDeclaration(bool isMember) {
    ASTNode node(ASTNodeType::VARIABLE_DECLARATION);

    Token &t = peek(0);
    if (isMember && isAccessMod(t.type)) {
        Token &t1 = consume();
        node.addChild(parseToken(t1));
    }

    node.addChild(parseType()); CHECK_SYNTAX_ERROR;

    auto &t1 = peek(0);
    if (t1.type != TokenType::SEMICOLON)
        node.addChild(parseAssignment(false)); CHECK_SYNTAX_ERROR;

    // Variable declaration should end with a semicolon
    auto &t2 = consume();
    if (t2.type != TokenType::SEMICOLON)
        createSyntaxError(EXPECTED_A_SEMICOLON, t2);
    
    return node;
}

ASTNode Parser::parseType() {
    ASTNode node(ASTNodeType::DATA_TYPE);

    Token &t = consume();
    if (!isDataType(t.type)) {
        createSyntaxError(EXPECTED_A_DATA_TYPE, t);
        return node;
    }

    node.setToken(t);
    node.updatePos();
    return node;
}

ASTNode Parser::parseIdentifier() {
    ASTNode node(ASTNodeType::IDENTIFIER);

    Token &t = consume();
    if (t.type != TokenType::ID) {
        createSyntaxError(EXPECTED_AN_IDENTIFIER, t);
        return node;
    }

    node.setToken(t);
    node.updatePos();
    return node;
}

ASTNode Parser::parseExpression() {
    ASTNode node(ASTNodeType::EXPRESSION);

    node.addChild(parseExpressionTerm());
    // The first part of an expression should always be a term
    CHECK_SYNTAX_ERROR;

    // Has multiple terms 
    bool complexExpr = false;
    for (;;) {
        Token &t1 = peek(0);
        if (isOperator(t1.type)) {
            consume();
            complexExpr = true;
            
            ASTNode binOp(ASTNodeType::BINOP);
            binOp.setToken(t1);
            binOp.updatePos();
            node.addChild(binOp);
        }
        else
            // The expression is finished, there should be no more terms left over
            break;
        
        node.addChild(parseExpressionTerm());
        // There should always be a term after an operator
        CHECK_SYNTAX_ERROR;
    }

    // Only one term
    if (!complexExpr)
        return node;

    // Assure operator precedence
    while (node.numChildren() > 1) {
        int depth = 0;
        int highestPre = 0;
        int highestPreIdx = 0;
        for (int i = 1; i < node.numChildren(); i += 2) {
            const Token &tok = node.child(i).token();
            int opPre = getOperatorPrecedence(tok); CHECK_SYNTAX_ERROR;
            highestPreIdx = (opPre > highestPre) ? i : highestPreIdx;
            highestPre = std::max(highestPre, opPre);
        }

        ASTNode opNode = node.children_[highestPreIdx];
        opNode.addChild(node.children_[highestPreIdx - 1]);
        opNode.addChild(node.children_[highestPreIdx + 1]);

        node.children_.erase(
            std::next(node.children_.begin(), highestPreIdx - 1), 
            std::next(node.children_.begin(), highestPreIdx + 2));

        node.children_.insert(
            std::next(node.children_.begin(), highestPreIdx - 1), 
            opNode);
    }

    return node;
}

ASTNode Parser::parseExpressionTerm() {
    ASTNode node(ASTNodeType::EXPRESSION_TERM);
    
    for (;;) {
        Token &t = peek(0);
        if (!isExpressionPreOp(t.type))
            break;
        
        node.addChild(parseExpressionPreOp());
    }

    node.addChild(parseExpressionValue()); CHECK_SYNTAX_ERROR;

    for (;;) {
        Token &t = peek(0);
        if (!isExpressionPostOp(t.type)) 
            break;
        
        node.addChild(parseExpressionPostOp());
    }

    return node;
}

ASTNode Parser::parseConstant() {
    ASTNode node(ASTNodeType::CONSTANT);

    Token &t = consume();
    if (!isConstantValue(t.type)) {
        createSyntaxError(EXPECTED_CONSTANT_VALUE, t);
        return node;
    }

    node.setToken(t);
    node.updatePos();
    return node;
}

ASTNode Parser::parseFunction(bool isMethod) {
    ASTNode node(ASTNodeType::FUNCTION);

    Token t = peek(0);
    if (isMethod && isAccessMod(t.type)) {
        Token &t1 = consume();
        node.addChild(parseToken(t1));
    }

    Token &t1 = peek(0);
    if (isDataType(t1.type))
        node.addChild(parseType()); 
    else if (t1.type == TokenType::FUN_KWD) {
        consume();
        node.addChild(parseToken(t1));
    }
    else {
        createSyntaxError(EXPECTED_A_DATA_TYPE_OR_FUN, t1);
        return node;
    }
    
    if (isMethod) {
        Token &t2 = peek(0);
        // Allow constructors by not throwing an error
        // if the next token isn't an identifier
        if (t2.type != TokenType::PARENTHESIS_OPEN) {
            node.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;
        }
    } else {
        node.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;
    }

    t = consume();
    if (t.type != TokenType::PARENTHESIS_OPEN) {
        createSyntaxError(EXPECTED_TOKEN.format("("), t);
        return node; 
    }

    Token &t2 = peek(0);
    ASTNode argListNode(ASTNodeType::PARAMETER_LIST);
    // The function has arguments
    if (t2.type != TokenType::PARENTHESIS_CLOSE) {
        for (;;) {
            argListNode.addChild(parseType());       CHECK_SYNTAX_ERROR;
            argListNode.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;

            Token &t3 = consume();
            if (t3.type == TokenType::COMMA)
                continue;
            else if (t3.type == TokenType::PARENTHESIS_CLOSE) 
                break;
            else {
                createSyntaxError(EXPECTED_TOKEN_OR_TOKEN.format(',', ')'), t3);
                break;
            }
        }
    }
    else
        consume();

    node.addChild(argListNode);           CHECK_SYNTAX_ERROR;
    node.addChild(parseStatementBlock()); CHECK_SYNTAX_ERROR;

    return node;
}

ASTNode Parser::parseStatementBlock() {
    ASTNode node(ASTNodeType::STATEMENT_BLOCK);

    Token &t = consume();
    if (t.type != TokenType::CURLY_BRACE_OPEN) {
        createSyntaxError(EXPECTED_TOKEN.format('{'), t);
        return node;
    }

    node.updatePos(t);

    for (;;) {
        Token t1 = peek(0);
        if (isVariableDeclaration())
            node.addChild(parseVariableDeclaration());
        else if (t1.type == TokenType::CURLY_BRACE_CLOSE) {
            consume();

            node.updatePos(t1);

            break;
        } 
        else if (t1.type == TokenType::END_OF_FILE) {
            createSyntaxError(UNEXPECTED_EOF, t1);
            break;
        } 
        else
            node.addChild(parseStatement());

        if (hasSyntaxError_ && !tryEscapeSyntaxError()) 
            break;
    }

    return node;
}

ASTNode Parser::parseStatement() {
    Token &t = peek(0);
    if (t.type == TokenType::IF_KWD)
        return parseIfStatement();
    else if (t.type == TokenType::RETURN_KWD)
        return parseReturnStatement();
    else
        return parseSimpleStatement();
}

ASTNode Parser::parseSimpleStatement() {
    Token t = peek(0);
    // Just a semicolon is alright
    if (t.type == TokenType::SEMICOLON) {
        // node.setPos(t);
        consume();
        // Empty assignement
        return ASTNode{ASTNodeType::ASSIGNMENT};
    }

    ASTNode node = parseAssignment(true); CHECK_SYNTAX_ERROR;

    t = consume();
    if (t.type != TokenType::SEMICOLON)
        createSyntaxError(EXPECTED_A_SEMICOLON, t);

    return node;
}

ASTNode Parser::parseFunctionCall() {
    ASTNode node(ASTNodeType::FUNCTION_CALL);

    node.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;
    node.addChild(parseArgList()); CHECK_SYNTAX_ERROR;

    return node;
}

ASTNode Parser::parseIfStatement() {
    ASTNode node(ASTNodeType::IF_STATEMENT);

    Token t = consume();
    node.setPos(t);

    if (t.type != TokenType::IF_KWD) {
        createSyntaxError(EXPECTED_TOKEN.format("if"), t);
        return node;
    }

    t = consume();
    if (t.type != TokenType::PARENTHESIS_OPEN) {
        createSyntaxError(EXPECTED_TOKEN.format('('), t);
        return node;
    }

    node.addChild(parseExpression());     CHECK_SYNTAX_ERROR;

    t = consume();
    if (t.type != TokenType::PARENTHESIS_CLOSE) {
        createSyntaxError(EXPECTED_TOKEN.format(')'), t);
        return node;
    }

    node.addChild(parseStatementBlock()); CHECK_SYNTAX_ERROR;

    t = peek(0);
    if (t.type == TokenType::ELSE_KWD) {
        Token &t2 = consume();

        ASTNode elseBrNode(ASTNodeType::ELSE_BRANCH);
        elseBrNode.setPos(t2);

        t = peek(0);
        if (t.type == TokenType::IF_KWD)
            elseBrNode.addChild(parseIfStatement());
        else
            elseBrNode.addChild(parseStatementBlock());
    
        node.addChild(elseBrNode);        CHECK_SYNTAX_ERROR;
    }

    return node;
}

ASTNode Parser::parseAssignment(bool allowCompoundOps) {
    ASTNode node(ASTNodeType::ASSIGNMENT);

    node.addChild(parseExpressionTerm());                     CHECK_SYNTAX_ERROR;

    Token &t = peek(0);
    if (t.type == TokenType::SEMICOLON) {
        return node;
    }

    node.addChild(parseAssignmentOperator(allowCompoundOps)); CHECK_SYNTAX_ERROR;
    node.addChild(parseExpression());                         CHECK_SYNTAX_ERROR;

    return node;
}

ASTNode Parser::parseReturnStatement() {
    ASTNode node(ASTNodeType::RETURN_STMT);
    
    Token t = consume();
    node.setPos(t);
    if (t.type != TokenType::RETURN_KWD) {
        createSyntaxError(EXPECTED_TOKEN.format("return"), t);
        return node;
    } 

    t = peek(0);
    if (t.type == TokenType::SEMICOLON) {
        consume();
    } else {
        node.addChild(parseExpression());

        t = consume();
        if (t.type != TokenType::SEMICOLON) {
            createSyntaxError(EXPECTED_A_SEMICOLON, t);
            return node;
        }
    }

    return node;
}

ASTNode Parser::parseAssignmentOperator(bool allowCompoundOps) {
    ASTNode node(ASTNodeType::BINOP);
    
    Token &t = consume();
    if (!allowCompoundOps && t.type != TokenType::EQUAL) {
        createSyntaxError(EXPECTED_TOKEN.format('='), t);
        return node;
    } else if (!isAssignOp(t.type)) {
        createSyntaxError(EXPECTED_ASSIGN_OP, t);
        return node;
    }

    node.setToken(t);
    node.updatePos();

    return node;
}

ASTNode Parser::parseObject() {
    ASTNode node(ASTNodeType::OBJECT);

    Token t = consume();
    if (t.type != TokenType::OBJ_KWD) {
        createSyntaxError(EXPECTED_TOKEN.format("obj"), t);
        return node;
    }

    node.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;

    t = consume();
    if (t.type != TokenType::CURLY_BRACE_OPEN) {
        createSyntaxError(EXPECTED_TOKEN.format("obj"), t);
        return node;
    }

    for (;;) {
        Token t = peek(0);
        if (t.type == TokenType::CURLY_BRACE_CLOSE) {
            consume();
            break;
        } 
        else if (isVariableDeclaration(true)) {
            node.addChild(parseVariableDeclaration(true)); CHECK_SYNTAX_ERROR;   
        } 
        else if (isFunction(true)) {
            node.addChild(parseFunction(true)); CHECK_SYNTAX_ERROR;
        } 
        else {
            consume();
            createSyntaxError(EXPECTED_VARDECL_OR_FUNC_DEF, t);
            if (!tryEscapeSyntaxError()) return node;
            consume(); // Consume remaining closing curly brace
        }
    }

    return node;
}

ASTNode Parser::parseConstructCall() {
    ASTNode node(ASTNodeType::CONSTRUCT_CALL);

    Token &t = consume();
    if (t.type != TokenType::NEW_KWD) {
        createSyntaxError(EXPECTED_TOKEN.format("new"), t);
        return node;
    }

    node.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;
    node.addChild(parseArgList()); CHECK_SYNTAX_ERROR;

    return node;
}

ASTNode Parser::parseArgList() {
    ASTNode node(ASTNodeType::ARGUMENT_LIST);
    Token &t = consume();
    if (t.type != TokenType::PARENTHESIS_OPEN) {
        createSyntaxError(EXPECTED_TOKEN.format('('), t);
        return node;
    }
    node.updatePos(t);

    Token &t1 = peek(0); 
    if (t1.type != TokenType::PARENTHESIS_CLOSE) {
        for (;;) {
            node.addChild(parseExpression()); CHECK_SYNTAX_ERROR;

            Token &t3 = consume();
            if (t3.type == TokenType::COMMA)
                continue;
            else if (t3.type == TokenType::PARENTHESIS_CLOSE) {
                node.updatePos(t3);
                break;
            }
            else {
                createSyntaxError(EXPECTED_TOKEN_OR_TOKEN.format(',', ')'), t3);
                break;
            }
        }
    } else {
        node.updatePos(t1);
        consume();
    }

    return node;
}

ASTNode Parser::parseExpressionValue() {
    ASTNode node(ASTNodeType::EXPRESSION_VALUE);

    Token &t = peek(0);
    if (isConstantValue(t.type)) {
        node.addChild(parseConstant()); CHECK_SYNTAX_ERROR;
    } 
    else if (t.type == TokenType::ID) {
        if (isFunctionCall()) {
            node.addChild(parseFunctionCall()); CHECK_SYNTAX_ERROR;
        } 
        else {
            node.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;
        }
    }
    else if (t.type == TokenType::NEW_KWD) {
        node.addChild(parseConstructCall()); CHECK_SYNTAX_ERROR;
    }
    else if (t.type == TokenType::PARENTHESIS_OPEN) {
        consume();
        node.addChild(parseExpression()); CHECK_SYNTAX_ERROR;

        Token &t1 = consume();
        if (t1.type != TokenType::PARENTHESIS_CLOSE) {
            createSyntaxError(EXPECTED_TOKEN.format('('), t1);
            return node;
        }
    }
    else if (t.type == TokenType::NULL_KWD) {
        node.addChild(parseToken(t));
        consume();
    }
    else {
        consume();
        createSyntaxError(EXPECTED_EXPRESSION_VALUE, t);
    }

    return node;
}

ASTNode Parser::parseExpressionPreOp() {
    ASTNode node(ASTNodeType::EXPRESSION_PREOP);

    Token &t = consume();
    node.setToken(t);
    node.updatePos();

    return node;
}

ASTNode Parser::parseExpressionPostOp() {
    ASTNode node(ASTNodeType::EXPRESSION_POSTOP);

    Token &t = consume();
    node.setPos(t);

    if (t.type == TokenType::DOT) {
        if (isFunctionCall()) {
            node.addChild(parseFunctionCall()); CHECK_SYNTAX_ERROR;
        } else {
            node.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;
        }
    } else
        node.addChild(parseToken(t));

    return node;
}


bool isDataType(TokenType type) {
    switch (type) {
        case TokenType::INT8_KWD:
        case TokenType::INT16_KWD:
        case TokenType::INT32_KWD:
        case TokenType::INT64_KWD:
        case TokenType::UINT8_KWD:
        case TokenType::UINT16_KWD:
        case TokenType::UINT32_KWD:
        case TokenType::UINT64_KWD:
        case TokenType::FLOAT32_KWD:
        case TokenType::FLOAT64_KWD:
        case TokenType::BOOL_KWD:
        // Custom types (will get checked by the compiler)
        case TokenType::ID:
            return true;
        default:
            return false;
    }
}

bool isConstantValue(TokenType type) {
    switch (type) {
        case TokenType::INT_CONSTANT:
        case TokenType::FLOAT_CONSTANT:
        case TokenType::TRUE_KWD:
        case TokenType::FALSE_KWD:
            return true;
        default:
            return false;
    }
}

bool isOperator(TokenType type) {
    switch (type) {
        case TokenType::PLUS:
        case TokenType::MINUS:
        case TokenType::STAR:
        case TokenType::SLASH:
        case TokenType::BIGGER_EQUAL:
        case TokenType::SMALLER_EQUAL:
        case TokenType::DOUBLE_EQUAL:
        case TokenType::NOT_EQUAL:
        case TokenType::STRICTLY_BIGGER:
        case TokenType::STRICTLY_SMALLER:
        case TokenType::LOGICAL_AND:
        case TokenType::LOGICAL_OR:
            return true;
        default:
            return false;
    }
}

bool isAssignOp(TokenType type) {
    switch (type) {
        case TokenType::PLUS_EQUAL:
        case TokenType::MINUS_EQUAL:
        case TokenType::STAR_EQUAL:
        case TokenType::SLASH_EQUAL:
        case TokenType::EQUAL:
            return true;
        default:
            return false;
    }
}

bool isAccessMod(TokenType type) {
    switch (type) {
        case TokenType::PUBLIC_KWD:
        case TokenType::PRIVATE_KWD:
            return true; 
        default:
            return false;
    }
}

bool isExpressionPreOp(TokenType type) {
    switch (type) {
        case TokenType::PLUS_PLUS:
        case TokenType::MINUS_MINUS:
        case TokenType::NOT: 
            return true;
        default: 
            return false;
    }
}

bool isExpressionPostOp(TokenType type) {
    switch (type) {
        case TokenType::PLUS_PLUS:
        case TokenType::MINUS_MINUS:
        case TokenType::DOT: 
            return true;
        default: 
            return false;
    }
}

} // namespace NCSC
