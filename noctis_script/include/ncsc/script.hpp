#pragma once
#include "script_node.hpp"
#include "function.hpp"
#include <vector>

namespace NCSC
{
    
class NCSC_API Script {
public:
    Script() = default;

    void addFunction(const Function &fun) { functions_.push_back(fun); }
    const Function *getFunction(const std::string &name);
    const Function *getFunction(DWord idx);
    const DWord getFunctionIdx(const std::string &name);

private:
    std::vector<Function> functions_;
};

} // namespace NCSC
