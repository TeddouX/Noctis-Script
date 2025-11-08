// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/script_node.hpp>
#include <sstream>
#include <stack>

namespace NCSC
{
    
static std::string getNodeTypeStrRepr(ASTNodeType type) {
    switch (type) {
        case ASTNodeType::SCRIPT:                return "<SCRIPT>";
        case ASTNodeType::VARIABLE_DECLARATION:  return "<VARIABLE_DECLARATION>";
        case ASTNodeType::DATA_TYPE:             return "<DATA_TYPE>";
        case ASTNodeType::IDENTIFIER:            return "<IDENTIFIER>";
        case ASTNodeType::EXPRESSION:            return "<EXPRESSION>";
        case ASTNodeType::EXPRESSION_TERM:       return "<EXPRESSION_TERM>";
        case ASTNodeType::BINOP:                 return "<BINOP>";
        case ASTNodeType::CONSTANT:              return "<CONSTANT>";
        case ASTNodeType::PARAMETER_LIST:        return "<PARAMETER_LIST>";
        case ASTNodeType::FUNCTION:              return "<FUNCTION>";
        case ASTNodeType::STATEMENT_BLOCK:       return "<STATEMENT_BLOCK>";
        case ASTNodeType::TOKEN:                 return "<TOKEN>";
        case ASTNodeType::ARGUMENT_LIST:         return "<ARGUMENT_LIST>";
        case ASTNodeType::FUNCTION_CALL:         return "<FUNCTION_CALL>";
        case ASTNodeType::RETURN_STMT:           return "<RETURN_STMT>";
        case ASTNodeType::IF_STATEMENT:          return "<IF_STATEMENT>";
        case ASTNodeType::ELSE_BRANCH:           return "<ELSE_BRANCH>";
        case ASTNodeType::ASSIGNMENT:            return "<ASSIGNMENT>";
        case ASTNodeType::OBJECT:                return "<OBJECT>";
        case ASTNodeType::CONSTRUCT_CALL:        return "<CONSTRUCT_CALL>";
        case ASTNodeType::EXPRESSION_VALUE:      return "<EXPRESSION_VALUE>";
        case ASTNodeType::EXPRESSION_PREOP:      return "<EXPRESSION_PREOP>";
        case ASTNodeType::EXPRESSION_POSTOP:     return "<EXPRESSION_POSTOP>";
        default:                                 return "<please add the missing node(s)>";
    }
}

std::string ASTNode::getStrRepr() const {
    std::ostringstream oss;
    std::stack<std::pair<const ASTNode &, int>> stack;
    stack.push({*this, 0});

    while (!stack.empty()) {
        auto [node, depth] = stack.top();
        stack.pop();

        for (int i = 0; i < depth; ++i) oss << "|  ";
        oss << getNodeTypeStrRepr(node.type_);

        if (node.token_.isValid()) 
            oss << " (" << node.token_.getStrRepr() << ")" << "\n";
        else
            oss << "\n";

        // Push children in reverse order so the first one is processed first
        for (int i = node.children_.size() - 1; i >= 0; --i)
            stack.push({node.children_[i], depth + 1});
    }

    return oss.str();
}

void ASTNode::setPos(const Token &tok) {
    line = tok.line;
    lineEnd = tok.line;
    
    col = tok.col;
    colEnd = col + tok.getLength();
}

void ASTNode::updatePos(const Token &tok) {
    if (!tok.isValid())
        return;

    if (!hasPosBeenUpdated_) {
        line = tok.line;
        col = tok.col;
    }

    lineEnd = std::max(tok.line, lineEnd);
    colEnd = std::max(tok.col + static_cast<uint32_t>(tok.getLength()), colEnd);

    hasPosBeenUpdated_ = true;
}

void ASTNode::updatePos() {
    updatePos(token_);
}

void ASTNode::addChild(const ASTNode &child) {
    // Update the node's position according to the added child
    if (children_.empty()) {
        // First child added
        line = child.line;
        lineEnd = child.lineEnd;

        col = child.col;
        colEnd = child.colEnd;
    } else {
        lineEnd = std::max(lineEnd, child.lineEnd);
        colEnd = std::max(colEnd, child.colEnd);
    }
   
    children_.push_back(child); 
}

} // namespace NCSC
