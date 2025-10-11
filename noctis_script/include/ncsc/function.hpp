#pragma once
#include "ncsc.hpp"
#include "value.hpp"

#include <string>
#include <vector>

namespace NCSC
{

struct NCSC_API Function {
    std::string name;
    std::vector<Byte> bytecode;
    std::vector<Value> constants;

    Word   numLocals = 0;
    size_t numArgs = 0;
    size_t requiredStackSize = 0;

    std::string getBytecodeStrRepr() const;
};

} // namespace NCSC
