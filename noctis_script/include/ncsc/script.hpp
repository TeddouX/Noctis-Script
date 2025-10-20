// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "script_node.hpp"
#include "script_function.hpp"
#include "script_context.hpp"

#include <vector>
#include <memory>

namespace NCSC
{
    
struct GlobalVar {
    std::string name;
    std::vector<Byte> bytecode;
    size_t requiredStackSize;
    ValueType type;
};

class NCSC_API Script {
public:
    Script() = default;

    std::shared_ptr<ScriptContext> ctx;
    DWord numGlobalVariables;
    // Global's bytecode gets ran before executing any functions
    
    void addFunction(const ScriptFunction &fun) { functions_.push_back(fun); }
    
    const ScriptFunction *getFunction(const std::string &name) const;
    const ScriptFunction *getFunction(DWord idx) const;
    DWord getFunctionIdx(const std::string &name) const;
    const std::vector<ScriptFunction> &getAllFunctions() const { return functions_; }
    
    void addGlovalVar(const GlobalVar &var) { globalVars_.push_back(var); }
    const GlobalVar *getGlobalVar(DWord idx) const;
    DWord getGlobalVarIdx(const std::string &name) const;
    const std::vector<GlobalVar> &getAllGlobalVars() const { return globalVars_; }

private:
    std::vector<ScriptFunction> functions_;
    std::vector<GlobalVar> globalVars_;
};

} // namespace NCSC
