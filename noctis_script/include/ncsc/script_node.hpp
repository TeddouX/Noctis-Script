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
    BINOP,
    CONSTANT,
    ARGUMENT_LIST,
    PARAMETER_LIST,
    FUNCTION,
    STATEMENT_BLOCK,
};

struct NCSC_API ScriptNode {
    ScriptNodeType type;
    const Token *token = nullptr;

    ScriptNode *parent = nullptr;
    std::vector<ScriptNode> children;

    ScriptNode(ScriptNodeType type)
        : type(type) {}

    // Recursively iterate through its children
    std::string getStrRepr() const;

    bool hasChildren() const noexcept { return !children.empty(); }
    void addChild(const ScriptNode &child);
};

} // namespace NCSC
