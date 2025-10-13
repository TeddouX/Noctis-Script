#define NCSC_BUILD // Remove errors from defining the static variables
#include <ncsc/type_info.hpp>

namespace NCSC
{

const TypeInfo TypeInfo::INT   = TypeInfo(TokenType::INT_KWD);
const TypeInfo TypeInfo::FLOAT = TypeInfo(TokenType::FLOAT_KWD);
const TypeInfo TypeInfo::VOID  = TypeInfo(TokenType::INVALID);

void TypeInfo::getDefaultValue(Byte* bytes, size_t size) const {
    if (isInt() || isFloat()) {
        assert(size == 8);

        for (int i = 0; i < 8; i++)
            bytes[i] = 0;
    }
}

} // namespace NCSC
