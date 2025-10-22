// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/script_node.hpp>
#include <sstream>
#include <stack>

namespace NCSC
{
    
static std::string getNodeTypeStrRepr(ScriptNodeType type) {
    switch (type) {
        case ScriptNodeType::SCRIPT:                return "<SCRIPT>";
        case ScriptNodeType::VARIABLE_DECLARATION:  return "<VARIABLE_DECLARATION>";
        case ScriptNodeType::DATA_TYPE:             return "<DATA_TYPE>";
        case ScriptNodeType::IDENTIFIER:            return "<IDENTIFIER>";
        case ScriptNodeType::EXPRESSION:            return "<EXPRESSION>";
        case ScriptNodeType::EXPRESSION_TERM:       return "<EXPRESSION_TERM>";
        case ScriptNodeType::OP:                    return "<OP>";
        case ScriptNodeType::CONSTANT:              return "<CONSTANT>";
        case ScriptNodeType::PARAMETER_LIST:        return "<PARAMETER_LIST>";
        case ScriptNodeType::FUNCTION:              return "<FUNCTION>";
        case ScriptNodeType::STATEMENT_BLOCK:       return "<STATEMENT_BLOCK>";
        case ScriptNodeType::SIMPLE_STATEMENT:      return "<SIMPLE_STATEMENT>";
        case ScriptNodeType::TOKEN:                 return "<TOKEN>";
        case ScriptNodeType::ARGUMENT_LIST:         return "<ARGUMENT_LIST>";
        case ScriptNodeType::FUNCTION_CALL:         return "<FUNCTION_CALL>";
        case ScriptNodeType::RETURN_STMT:           return "<RETURN_STMT>";
        case ScriptNodeType::IF_STATEMENT:          return "<IF_STATEMENT>";
        case ScriptNodeType::ELSE_BRANCH:           return "<ELSE_BRANCH>";
        case ScriptNodeType::ASSIGNMENT:            return "<ASSIGNMENT>";
        default:                                    return "<please add the missing node(s)>";
    }
}

std::string ScriptNode::getStrRepr() const {
    std::ostringstream oss;
    std::stack<std::pair<const ScriptNode &, int>> stack;
    stack.push({*this, 0});

    while (!stack.empty()) {
        auto [node, depth] = stack.top();
        stack.pop();

        for (int i = 0; i < depth; ++i) oss << "|  ";
        oss << getNodeTypeStrRepr(node.type_);

        if (node.token_) oss << " (" << node.token_->getStrRepr() << ")" << "\n";
        else             oss << "\n";

        // Push children in reverse order so the first one is processed first
        for (int i = node.children_.size() - 1; i >= 0; --i)
            stack.push({node.children_[i], depth + 1});
    }

    return oss.str();
}

void ScriptNode::setPos(const Token &tok) {
    line = tok.line;
    col = tok.col;
    colEnd = col + tok.getLength();
}

void ScriptNode::updatePos() {
    if (!token_)
        return;

    line = token_->line;
    col = token_->col;
    colEnd = col + token_->getLength();
}

void ScriptNode::addChild(const ScriptNode &child) {
    // Update the node's position according to the added child
    if (children_.empty()) {
        // First child added
        line = child.line;
        col = child.col;
        colEnd = child.colEnd;
    } else {
        line = std::max(line, child.line);
        colEnd = std::max(colEnd, child.colEnd);
    }
   
    children_.push_back(child); 
}

} // namespace NCSC
