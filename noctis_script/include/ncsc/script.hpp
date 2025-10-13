#pragma once
#include "script_node.hpp"
#include "function.hpp"
#include <vector>

namespace NCSC
{
    
class NCSC_API Script {
public:
    Script() = default;

    DWord numGlobals;

    void addFunction(const Function &fun) { functions_.push_back(fun); }
    
    const Function *getFunction(const std::string &name);
    const Function *getFunction(DWord idx);

    const DWord getFunctionIdx(const std::string &name);

    const std::vector<Function> &getAllFunctions() { return functions_; }

private:
    std::vector<Function> functions_;
};

} // namespace NCSC
