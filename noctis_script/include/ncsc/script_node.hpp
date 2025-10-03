#include <vector>

#include "token.hpp"
#include "ncsc.hpp"

namespace NCSC
{
    
enum class ScriptNodeType {
    SCRIPT,
    VARIABLE_DECLARATION,
    DATA_TYPE,
    IDENTIFIER,
    EXPRESSION,
    EXPRESSION_TERM,
    EXPRESSION_OPERATOR,
    CONSTANT,
};

struct NCSC_API ScriptNode {
    ScriptNodeType type;
    Token *token = nullptr;

    ScriptNode *prev = nullptr;
    ScriptNode *next = nullptr;
    ScriptNode *parent = nullptr;
    std::vector<ScriptNode> children;

    ScriptNode(ScriptNodeType type)
        : type(type) {}

    void addChild(ScriptNode child);
};

} // namespace NCSC
