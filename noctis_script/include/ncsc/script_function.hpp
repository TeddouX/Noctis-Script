#pragma once
#include <vector>
#include <string>

#include "ncsc.hpp"
#include "value_type.hpp"
#include "function.hpp"

namespace NCSC
{
    
struct ScriptFunction : public IFunction {
    std::vector<Byte> bytecode;
    // std::vector<Value> constants;

    Word   numLocals = 0;
    size_t requiredStackSize = 0;
};

} // namespace NCSC
