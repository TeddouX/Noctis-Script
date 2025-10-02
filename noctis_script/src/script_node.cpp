#include <ncsc/script_node.hpp>

namespace NCSC
{
    
void ScriptNode::addChild(ScriptNode child) {

    if (children.empty()) {
        child.parent = this;

        children.push_back(child);
    }
    else {
        ScriptNode &oldLastChild = children.at(children.size() - 1);
    
        oldLastChild.next = &child;
        child.prev = &oldLastChild;
        child.parent = this;
        
        children.push_back(child);
    }
}

} // namespace NCSC
