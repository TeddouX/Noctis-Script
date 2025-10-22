// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <vector>

#include "token.hpp"
#include "ncsc.hpp"

namespace NCSC
{
    
enum class ScriptNodeType : uint8_t {
    TOKEN,
    SCRIPT,
    VARIABLE_DECLARATION,
    DATA_TYPE,
    IDENTIFIER,
    EXPRESSION,
    EXPRESSION_TERM,
    FUNCTION_CALL,
    SIMPLE_STATEMENT,
    RETURN,
    OP,
    CONSTANT,
    ARGUMENT_LIST,
    PARAMETER_LIST,
    FUNCTION,
    STATEMENT_BLOCK,
    IF_STATEMENT,
    ELSE_BRANCH,
    CONDITION,
};

struct NCSC_API ScriptNode {
public:
    ScriptNode(ScriptNodeType type)
        : type_(type) {}
    
    uint32_t line = 0, col = 0, colEnd = 0;

    // Recursively iterate through its children
    std::string getStrRepr() const;

    void setPos(const Token &tok);
    void updatePos();

    bool hasChildren() const noexcept { return !children_.empty(); }
    void addChild(const ScriptNode &child);
    const ScriptNode &getChild(size_t idx) const { return children_[idx]; }
    const ScriptNode &getLastChild() const { return children_.back(); }
    const std::vector<ScriptNode> &getAllChildren() const { return children_; }
    size_t getNumChildren() const { return children_.size(); }

    ScriptNodeType getType() const { return type_; }
    const Token *getToken() const { return token_; }
    void setToken(const Token *tok) { token_ = tok; }

    friend class Parser;

private:
    ScriptNodeType type_;
    const Token *token_ = nullptr;

    std::vector<ScriptNode> children_;

};

} // namespace NCSC
