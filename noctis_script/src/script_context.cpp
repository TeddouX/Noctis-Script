// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/script_context.hpp>

namespace NCSC
{

std::shared_ptr<ScriptContext> ScriptContext::create() {
    return std::shared_ptr<ScriptContext>(new ScriptContext());
}

Value ScriptContext::callCppFunction(DWord idx, const std::vector<Value> &args) {
    CPPFunction &globalFun = cppFunctions_[idx];
    return globalFun.registryFun(args);
}

Value ScriptContext::callObjectNew(DWord objIdx) {
    CPPObject &obj = cppObjects_[objIdx];
    return obj.newFun();
}

Value ScriptContext::callObjectMethod(DWord objIdx, DWord methodIdx, const std::vector<Value> &args) {
    CPPObject &obj = cppObjects_[objIdx];
    CPPMethod *method = obj.getMethod(methodIdx);
    
    if (!method)
        return Value{};
    
    return method->registryFun(args);
}

Value ScriptContext::getObjectMember(DWord idx, const Value &object, bool asRef) {
    if (!isCPPObject(object.ty))
        return Value{};

    DWord objIdx = (VTypeWord)clearMask(object.ty, ValueType::CPP_OBJ_MASK);
    CPPObject *cppObj = getCppObject(objIdx);
    if (!cppObj) 
        return Value{};

    auto *member = cppObj->getMember(idx);
    if (!member)
        return Value{};

    return member->registryFun(object, asRef);
}

std::string ScriptContext::getTypeName(ValueType ty) const {
    std::string refStr = hasMask(ty, ValueType::REF_MASK) ? " ref" : "";
    ty = clearMask(ty, ValueType::REF_MASK);

    auto builtinIt = BUILTIN_VTYPES_NAMES.find(ty);
    if (builtinIt != BUILTIN_VTYPES_NAMES.end())
        return builtinIt->second + refStr;

    auto it = typeNames_.find(ty);
    if (it != typeNames_.end())
        return it->second + refStr;
    return "";
}

} // namespace NCSC
