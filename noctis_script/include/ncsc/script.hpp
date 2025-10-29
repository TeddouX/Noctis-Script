// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include "script_node.hpp"
#include "script_function.hpp"
#include "script_context.hpp"
#include "script_object.hpp"
#include "variable.hpp"

#include <vector>
#include <memory>

namespace NCSC
{
    
struct GlobalVar : public Variable {
    // Global's bytecode shoudld get ran before executing any functions
    std::vector<Byte> bytecode;
    size_t requiredStackSize;
};

class NCSC_API Script {
public:
    Script() = default;

    std::shared_ptr<ScriptContext> ctx;
    DWord numGlobalVariables;
    
    void                               addFunction(const ScriptFunction &fun) { functions_.push_back(fun); }
    const ScriptFunction              *getFunction(const std::string &name) const;
    const ScriptFunction              *getFunction(DWord idx) const;
    DWord                              getFunctionIdx(const std::string &name) const;
    const std::vector<ScriptFunction> &getAllFunctions() const { return functions_; }
    
    bool                          hasGlobalVar(const std::string &name) { return getGlobalVarIdx(name) != NCSC_INVALID_IDX; }
    void                          addGlovalVar(const GlobalVar& var) { globalVars_.push_back(var); numGlobalVariables++; }
    GlobalVar                    *getGlobalVar(DWord idx);
    DWord                         getGlobalVarIdx(const std::string &name) const;
    const std::vector<GlobalVar> &getAllGlobalVars() const { return globalVars_; }

    bool                             hasObject(const std::string &name) { return getObjectIdx(name) != NCSC_INVALID_IDX; }
    void                             addObject(const ScriptObject &obj) { objects_.push_back(obj); }
    const ScriptObject              *getObject(const std::string &name) const;
    const ScriptObject              *getObject(DWord idx) const;
    DWord                            getObjectIdx(const std::string &name) const;
    const std::vector<ScriptObject> &getAllObjects() const { return objects_; }

private:
    std::vector<ScriptFunction> functions_;
    std::vector<ScriptObject>   objects_;
    std::vector<GlobalVar>      globalVars_;
};

} // namespace NCSC
