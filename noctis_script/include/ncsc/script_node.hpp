// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <vector>

#include "token.hpp"
#include "ncsc.hpp"

namespace NCSC
{
    
enum class ASTNodeType : uint8_t {
    TOKEN,
    SCRIPT,
    VARIABLE_DECLARATION,
    DATA_TYPE,
    IDENTIFIER,
    EXPRESSION,
    EXPRESSION_TERM,
    FUNCTION_CALL,
    // SIMPLE_STATEMENT,
    RETURN_STMT,
    BINOP,
    CONSTANT,
    ARGUMENT_LIST,
    PARAMETER_LIST,
    FUNCTION,
    STATEMENT_BLOCK,
    IF_STATEMENT,
    ELSE_BRANCH,
    CONDITION,
    ASSIGNMENT,
    OBJECT,
    CONSTRUCT_CALL,
    EXPRESSION_VALUE,
    EXPRESSION_PREOP,
    EXPRESSION_POSTOP,
};

struct NCSC_API ASTNode {
public:
    ASTNode(ASTNodeType type)
        : type_(type) {}
    
    uint32_t line = 0, col = 0, colEnd = 0;

    // Recursively iterate through its children
    std::string getStrRepr() const;

    void setPos(const Token &tok);
    void updatePos();

    bool hasChildren() const noexcept { return !children_.empty(); }
    void addChild(const ASTNode &child);
    const ASTNode &getChild(size_t idx) const { return children_[idx]; }
    const ASTNode &getLastChild() const { return children_.back(); }
    const std::vector<ASTNode> &getAllChildren() const { return children_; }
    size_t getNumChildren() const { return children_.size(); }

    ASTNodeType getType() const { return type_; }
    const Token *getToken() const { return token_; }
    void setToken(const Token *tok) { token_ = tok; }

    friend class Parser;

private:
    ASTNodeType type_;
    const Token *token_ = nullptr;

    std::vector<ASTNode> children_;

};

} // namespace NCSC
