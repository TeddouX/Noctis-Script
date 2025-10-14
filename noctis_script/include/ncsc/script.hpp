#pragma once
#include "script_node.hpp"
#include "function.hpp"
#include <vector>

namespace NCSC
{
    
struct GlobalVar {
    std::string name;
    std::vector<Byte> bytecode;
    size_t requiredStackSize;
    TypeInfo type;
};

class NCSC_API Script {
public:
    Script() = default;

    DWord numGlobalVariables;
    // Global's bytecode gets ran before executing any functions
    
    void addFunction(const Function &fun) { functions_.push_back(fun); }
    
    const Function *getFunction(const std::string &name) const;
    const Function *getFunction(DWord idx) const;
    DWord getFunctionIdx(const std::string &name) const;
    const std::vector<Function> &getAllFunctions() const { return functions_; }
    
    void addGlovalVar(const GlobalVar &var) { globalVars_.push_back(var); }
    const GlobalVar *getGlobalVar(DWord idx) const;
    DWord getGlobalVarIdx(const std::string &name) const;
    const std::vector<GlobalVar> &getAllGlobalVars() const { return globalVars_; }

private:
    std::vector<Function> functions_;
    std::vector<GlobalVar> globalVars_;
};

} // namespace NCSC
