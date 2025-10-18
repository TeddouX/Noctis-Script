#define NCSC_BUILD // Remove errors from defining the static variables
#include <ncsc/type_info.hpp>

namespace NCSC
{

const TypeInfo TypeInfo::INT16 =   TypeInfo(TokenType::INT16_KWD);
const TypeInfo TypeInfo::INT32 =   TypeInfo(TokenType::INT32_KWD);
const TypeInfo TypeInfo::INT64 =   TypeInfo(TokenType::INT64_KWD);
const TypeInfo TypeInfo::UINT16 =  TypeInfo(TokenType::UINT16_KWD);
const TypeInfo TypeInfo::UINT32 =  TypeInfo(TokenType::UINT32_KWD);
const TypeInfo TypeInfo::UINT64 =  TypeInfo(TokenType::UINT64_KWD);
const TypeInfo TypeInfo::FLOAT32 = TypeInfo(TokenType::FLOAT32_KWD);
const TypeInfo TypeInfo::FLOAT64 = TypeInfo(TokenType::FLOAT64_KWD);
const TypeInfo TypeInfo::BOOL =    TypeInfo(TokenType::BOOL_KWD);
const TypeInfo TypeInfo::VOID =    TypeInfo(TokenType::INVALID);

void TypeInfo::getDefaultValue(Byte* bytes, size_t size) const {
    if (isInt() || isFloat()) {
        assert(size == 8);

        for (int i = 0; i < 8; i++)
            bytes[i] = 0;
    }
}

} // namespace NCSC
