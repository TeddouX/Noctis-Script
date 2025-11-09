// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/script_object.hpp>

namespace NCSC
{

ScriptMethod *ScriptObject::getConstructor() {
    for (auto &m : methods_) {
        if (m.name == name) 
            return static_cast<ScriptMethod *>(&m);
    }
    return nullptr;
}

DWord ScriptObject::getConstructorIdx() const {
    for (DWord i = 0; i < methods_.size(); i++) {
        if (methods_[i].name == name) 
            return i;
    }
    return INVALID_IDX;
}
    
}