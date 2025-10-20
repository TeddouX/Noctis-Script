// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/script_node.hpp>
#include <sstream>
#include <stack>

namespace NCSC
{
    
static std::string getNodeTypeStrRepr(ScriptNodeType type) {
    switch (type) {
        case ScriptNodeType::SCRIPT:               return "<SCRIPT>";
        case ScriptNodeType::VARIABLE_DECLARATION: return "<VARIABLE_DECLARATION>";
        case ScriptNodeType::DATA_TYPE:            return "<DATA_TYPE>";
        case ScriptNodeType::IDENTIFIER:           return "<IDENTIFIER>";
        case ScriptNodeType::EXPRESSION:           return "<EXPRESSION>";
        case ScriptNodeType::EXPRESSION_TERM:      return "<EXPRESSION_TERM>";
        case ScriptNodeType::BINOP:                return "<BINOP>";
        case ScriptNodeType::CONSTANT:             return "<CONSTANT>";
        case ScriptNodeType::PARAMETER_LIST:       return "<PARAMETER_LIST>";
        case ScriptNodeType::FUNCTION:             return "<FUNCTION>";
        case ScriptNodeType::STATEMENT_BLOCK:      return "<STATEMENT_BLOCK>";
        case ScriptNodeType::SIMPLE_STATEMENT:     return "<SIMPLE_STATEMENT>";
        case ScriptNodeType::TOKEN:                return "<TOKEN>";
        case ScriptNodeType::ARGUMENT_LIST:        return "<ARGUMENT_LIST>";
        case ScriptNodeType::FUNCTION_CALL:        return "<FUNCTION_CALL>";
        case ScriptNodeType::RETURN:               return "<RETURN>";
        default:                                   return "<please add the missing node(s)>";
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
        oss << getNodeTypeStrRepr(node.type);

        if (node.token) oss << " (" << node.token->getStrRepr() << ")" << "\n";
        else            oss << "\n";

        // Push children in reverse order so the first one is processed first
        for (int i = node.children.size() - 1; i >= 0; --i)
            stack.push({node.children[i], depth + 1});
    }

    return oss.str();
}

void ScriptNode::addChild(const ScriptNode &child) { 
    children.push_back(child); 
    children.back().parent = this;
}

} // namespace NCSC
