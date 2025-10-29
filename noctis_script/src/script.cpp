// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/script.hpp>

namespace NCSC
{
    
template <typename T>
const T* get(const std::vector<T>& vec, const std::string& name) {
    for (auto& v : vec)
        if (v.name == name)
            return &v;
    return nullptr;
}

template <typename T>
DWord getIndex(const std::vector<T>& vec, const std::string& name) {
    for (size_t i = 0; i < vec.size(); ++i)
        if (vec[i].name == name)
            return static_cast<DWord>(i);
    return UINT32_MAX;
}


const ScriptFunction *Script::getFunction(const std::string &name) const {
    return get(functions_, name);
}

const ScriptFunction *Script::getFunction(DWord idx) const {
    if (idx > functions_.size())
        return nullptr;
    return &functions_[idx];
}

DWord Script::getFunctionIdx(const std::string &name) const {
    return getIndex(functions_, name);
}

GlobalVar *Script::getGlobalVar(DWord idx) {
    if (idx > globalVars_.size())
        return nullptr;
    return &globalVars_[idx];
}

DWord Script::getGlobalVarIdx(const std::string &name) const {
    return getIndex(globalVars_, name);
}

const ScriptObject *Script::getObject(const std::string &name) const {
    return get(objects_, name);
}

const ScriptObject *Script::getObject(DWord idx) const {
    if (idx > objects_.size())
        return nullptr;
    return &objects_[idx];
}

DWord Script::getObjectIdx(const std::string &name) const {
    return getIndex(objects_, name);
}

} // namespace NCSC
