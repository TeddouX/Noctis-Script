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

Value ScriptContext::callObjectNew(DWord objIdx, GarbageCollectorObj *obj) {
    CPPObject &cppObj = cppObjects_[objIdx];
    return cppObj.newFun(obj);
}

void ScriptContext::destroyObject(DWord objIdx, GarbageCollectorObj *obj) {
    CPPObject &cppObj = cppObjects_[objIdx];
    return cppObj.destructorFun(obj);
}

Value ScriptContext::callObjectMethod(DWord objIdx, DWord methodIdx, const std::vector<Value> &args) {
    CPPObject &obj = cppObjects_[objIdx];
    CPPMethod *method = obj.getMethod(methodIdx);
    
    if (!method)
        return Value{};
    
    return method->registryFun(args);
}

Value ScriptContext::getObjectMember(DWord idx, const Value &object) {
    CPPMember *member = getMember(idx, object);
    return member->getterFun(object);
}

void ScriptContext::setObjectMember(DWord idx, const Value &object, const Value &val) {
    CPPMember *member = getMember(idx, object);
    return member->setterFun(object, val);
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

CPPMember *ScriptContext::getMember(DWord idx, const Value &object) {
    if (!isCPPObject(object.ty))
        return nullptr;

    DWord objIdx = (VTypeWord)clearMask(object.ty, ValueType::CPP_OBJ_MASK);
    CPPObject *cppObj = getCppObject(objIdx);
    if (!cppObj) 
        return nullptr;

    return cppObj->getMember(idx);
}

} // namespace NCSC
