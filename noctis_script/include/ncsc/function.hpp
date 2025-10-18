#pragma once
#include "ncsc.hpp"
#include "value.hpp"
#include "type_info.hpp"

#include <string>
#include <vector>

namespace NCSC
{

struct NCSC_API Function {
    std::string name;
    std::vector<Byte> bytecode;
    // std::vector<Value> constants;

    std::vector<TypeInfo> paramTypes;
    TypeInfo returnType;

    Word   numLocals = 0;
    size_t numParams = 0;
    size_t requiredStackSize = 0;
};

} // namespace NCSC
