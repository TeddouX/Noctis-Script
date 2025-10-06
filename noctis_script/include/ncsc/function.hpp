#pragma once
#include "ncsc.hpp"

#include <string>
#include <vector>

namespace NCSC
{

struct NCSC_API Function {
    std::string name;
    std::vector<Byte> bytecode;
    std::vector<Value> constants;

    size_t numLocals;
    size_t numArgs;
    size_t requiredStackSize;
};

} // namespace NCSC
