#include <vector>

namespace NCSC
{
    
enum class ScriptNodeType {
    PROGRAM,
    VAR_DECL
};

struct ScriptNode {
    ScriptNodeType type;
    
    ScriptNode *prev;
    ScriptNode *next;
    ScriptNode *parent;
    std::vector<ScriptNode *> children;

    void addChild(ScriptNode *child);
};

} // namespace NCSC
