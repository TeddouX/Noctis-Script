#include <vector>

#include "token.hpp"

namespace NCSC
{
    
enum class ScriptNodeType {
    SCRIPT,
    VARIABLE_DECLARATION,
    DATA_TYPE,
    IDENTIFIER,
};

struct ScriptNode {
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
