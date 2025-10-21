// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/parser.hpp>
#include <iostream>
#include <stdarg.h>

#define CHECK_SYNTAX_ERROR if (hasSyntaxError_) return node

namespace NCSC
{

Parser::Parser(const std::vector<Token> &tokens, std::shared_ptr<ScriptSource> src)
    : tokens_(tokens), src_(src) 
{
    assert(src_ != nullptr);
}

ScriptNode Parser::parseAll() {
    ScriptNode root(ScriptNodeType::SCRIPT);

    while (idx_ < tokens_.size()) {
        const Token &currTok = tokens_[idx_];
        
        // int, float, ...
        if (isDataType(currTok.type)) {
            if (isVariableDeclaration())
                root.addChild(parseVariableDeclaration());
            else if (isFunction())
                root.addChild(parseFunction());
        }
        else if (currTok.type == TokenType::FUN_KWD)
            root.addChild(parseFunction());
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
    err.setLocation(tok.line, tok.col, tok.col + tok.getLength());
    syntaxErrors_.push_back(err);
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
        case TokenType::TRUE_KWD:
        case TokenType::FALSE_KWD:
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

bool Parser::isFunction() {
    Token t = peek(0);
    if (!isDataType(t.type))
        return false;

    t = peek(1);
    if (t.type != TokenType::ID)
        return false;

    t = peek(2);
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

ScriptNode Parser::parseToken(Token &tok) {
    ScriptNode node(ScriptNodeType::TOKEN);

    node.token = &tok;
    node.updatePos();
    return node;
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
    node.updatePos();
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
    node.updatePos();
    return node;
}

ScriptNode Parser::parseExpression() {
    ScriptNode node(ScriptNodeType::EXPRESSION);

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
            
            ScriptNode binOp(ScriptNodeType::BINOP);
            binOp.token = &t1;
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
    while (node.children.size() > 1) {
        int highestPre = 0;
        int highestPreIdx = 0;
        for (int i = 1; i < node.children.size(); i += 2) {
            TokenType ty = node.children[i].token->type;
            if ((ty == TokenType::PLUS || ty == TokenType::MINUS) && highestPre == 0) {
                highestPre = 1;
                highestPreIdx = i;
            }
            else if (ty == TokenType::STAR || ty == TokenType::SLASH) { 
                highestPre = 2;
                highestPreIdx = i;
                break;
            }
        }

        ScriptNode opNode = node.children[highestPreIdx];
        opNode.addChild(node.children[highestPreIdx - 1]);
        opNode.addChild(node.children[highestPreIdx + 1]);

        node.children.erase(
            std::next(node.children.begin(), highestPreIdx - 1), 
            std::next(node.children.begin(), highestPreIdx + 2));

        node.children.insert(
            std::next(node.children.begin(), highestPreIdx - 1), 
            opNode);
    }

    return node;
}

ScriptNode Parser::parseExpressionTerm() {
    ScriptNode node(ScriptNodeType::EXPRESSION_TERM);
    
    Token &t = peek(0);
    if (isConstantValue(t.type)) {
        node.addChild(parseConstant());         CHECK_SYNTAX_ERROR;
    } 
    else if (t.type == TokenType::ID) {
        if (isFunctionCall()) {
            node.addChild(parseFunctionCall()); CHECK_SYNTAX_ERROR;
        } 
        else {
            node.addChild(parseIdentifier());   CHECK_SYNTAX_ERROR;
        }
    } 
    else {
        consume();
        createSyntaxError(EXPECTED_EXPRESSION_TERM, t);
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
    node.updatePos();
    return node;
}

ScriptNode Parser::parseFunction() {
    ScriptNode node(ScriptNodeType::FUNCTION);

    Token &t = peek(0);
    if (isDataType(t.type)) {
        node.addChild(parseType()); 
        // Sanity check
        CHECK_SYNTAX_ERROR;
    }
    else if (t.type == TokenType::FUN_KWD) {
        consume();
        node.addChild(parseToken(t));
    } 
    else {
        createSyntaxError(EXPECTED_A_DATA_TYPE_OR_FUN, t);
        return node;
    }

    node.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;

    Token &t1 = consume();
    if (t1.type != TokenType::PARENTHESIS_OPEN) {
        createSyntaxError(EXPECTED_TOKEN.format("("), t1);
        return node; 
    }

    Token &t2 = peek(0);
    ScriptNode argListNode(ScriptNodeType::PARAMETER_LIST);
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

ScriptNode Parser::parseStatementBlock() {
    ScriptNode node(ScriptNodeType::STATEMENT_BLOCK);

    Token &t = consume();
    if (t.type != TokenType::CURLY_BRACE_OPEN) {
        createSyntaxError(EXPECTED_TOKEN.format('{'), t);
        return node;
    }

    for (;;) {
        Token t1 = peek(0);
        if (isVariableDeclaration())
            node.addChild(parseVariableDeclaration());
        else if (t1.type == TokenType::CURLY_BRACE_CLOSE) {
            consume();
            return node;
        } 
        else if (t1.type == TokenType::END_OF_FILE) {
            createSyntaxError(UNEXPECTED_EOF, t1);
            return node;
        } 
        else
            node.addChild(parseStatement());

        // Try to recover from syntax errors
        if (hasSyntaxError_) {
            // Try exitting the problematic line or block of code
            int level = 0;
            for (int i = 0;; i++) {
                t1 = peek(0);
                if (t1.type == TokenType::SEMICOLON && level == 0) {
                    // The comma isn't in an unsafe nested block so 
                    // its safe to break out of the loop
                    consume();
                    break;
                }
                else if (t1.type == TokenType::CURLY_BRACE_OPEN) {
                    consume();
                    level++;
                }
                else if (t1.type == TokenType::CURLY_BRACE_CLOSE) {
                    // End of nested block. The bracket will get handled 
                    // in the next iteration of the outer for loop
                    if (level == 0)
                        break;

                    consume();
                    level--;
                }
                else if (t1.type == TokenType::END_OF_FILE) {
                    createSyntaxError(UNEXPECTED_EOF, t1);
                    return node;
                }
                else
                    consume();
            }
            
            hasSyntaxError_ = false;
        }
    }

    return node;
}

ScriptNode Parser::parseStatement() {
    return parseSimpleStatement();
}

ScriptNode Parser::parseSimpleStatement() {
    ScriptNode node(ScriptNodeType::SIMPLE_STATEMENT);

    Token &t = peek(0);
    if (isFunctionCall())
        node.addChild(parseFunctionCall());
    else if (t.type == TokenType::RETURN_KWD) {
        ScriptNode returnNode(ScriptNodeType::RETURN);
        returnNode.token = &t;
        returnNode.updatePos();

        consume();

        returnNode.addChild(parseExpression());
        node.addChild(returnNode); CHECK_SYNTAX_ERROR;
    }
    else if (t.type == TokenType::SEMICOLON)
        consume();
    else {
        createSyntaxError(EXPECTED_STATEMENT, t);
        return node;
    }

    Token &t1 = consume();
    if (t1.type != TokenType::SEMICOLON)
        createSyntaxError(EXPECTED_A_SEMICOLON, t1);

    return node;
}

ScriptNode Parser::parseFunctionCall() {
    ScriptNode node(ScriptNodeType::FUNCTION_CALL);

    node.addChild(parseIdentifier()); CHECK_SYNTAX_ERROR;

    ScriptNode argListNode(ScriptNodeType::ARGUMENT_LIST);
    Token &t = consume();
    if (t.type != TokenType::PARENTHESIS_OPEN) {
        createSyntaxError(EXPECTED_TOKEN.format('('), t);
        return node;
    }

    Token &t1 = peek(0); 
    if (t1.type != TokenType::PARENTHESIS_CLOSE) {
        for (;;) {
            argListNode.addChild(parseExpression()); CHECK_SYNTAX_ERROR;

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
    } else
        consume();

    node.addChild(argListNode); CHECK_SYNTAX_ERROR;

    return node;
}

} // namespace NCSC
