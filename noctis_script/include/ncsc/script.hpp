#pragma once
#include "script_node.hpp"
#include "function.hpp"
#include <vector>

namespace NCSC
{
    
struct NCSC_API Script {
    std::vector<Function> functions;

    void addFunction(const Function &fun) { functions.push_back(fun); }
};

} // namespace NCSC
