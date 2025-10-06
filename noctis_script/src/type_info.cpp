#include <ncsc/type_info.hpp>

namespace NCSC
{

TypeInfo::TypeInfo(const ScriptNode &node) 
    : node_(node) {
    assert(node.type == ScriptNodeType::DATA_TYPE && node.token != nullptr);
}

void TypeInfo::getDefaultValue(Byte *bytes, size_t size) {
    if (isInt() || isFloat()) {
        assert(size == 8);

        for (int i = 0; i < 8; i++)
            bytes[i] = 0;
    }
}

} // namespace NCSC
