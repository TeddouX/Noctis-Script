// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#pragma once
#include <string>
#include <vector>
#include <functional>
#include <typeinfo>
#include <memory>

#include "ncsc.hpp"
#include "value.hpp"
#include "named_utils.hpp"
#include "object.hpp"
#include "function.hpp"

namespace NCSC
{

struct CPPFunction : public Function {
    std::function<Value (const std::vector<Value> &)> registryFun;
};


struct CPPMethod : public Method {
    std::function<Value (const std::vector<Value> &)> registryFun;
};

struct CPPMember : public Member {
    std::function<Value (const Value &)> getterFun;
    std::function<void (const Value &, const Value &)> setterFun;
};

class CPPObject : public Object {
public:
    // Calls new(sizeof(Obj_))
    std::function<Value (GarbageCollectedObj *obj)> newFun;
    // Calls Obj_.~Obj_()
    std::function<void (GarbageCollectedObj *obj)> destructorFun;

    const std::type_info *classInfo;

    NCSC_GETTERS_SETTERS_FOR_NAMED_VECTOR(Method, methods_, CPPMethod)
    NCSC_GETTERS_SETTERS_FOR_NAMED_VECTOR(Member, members_, CPPMember)

private:
    std::vector<CPPMethod> methods_;
    std::vector<CPPMember> members_;
};


// Context bridges the script and CPP
class NCSC_API ScriptContext {
public:
    static std::shared_ptr<ScriptContext> create();


    template <typename RetTy_, typename... FuncArgs_>
    void registerGlobalFunction(const std::string &name, RetTy_(*fun)(FuncArgs_...));

    template <typename Obj_>
    void registerObject(const std::string &name);

    // Constructor has to be marked as noexcept for now
    template <
        typename Obj_, 
        typename... CtorArgs_, 
        bool IsDefaultCtor_ = (sizeof...(CtorArgs_) == 0)>
    requires(
        // We don't want the constructor to throw an error
        // when calling it as long as there is no proper error 
        // handling in the VM and in the context
        std::is_nothrow_constructible_v<Obj_, CtorArgs_...> ||
        // Check if the object is default constructible 
        // if the caller passed 0 arguments
        (IsDefaultCtor_ && std::is_default_constructible_v<Obj_>)
    )
    void registerObjectCtor(bool isPublic = true);

    // Setting isPublic to false wouldn't be really useful
    template <typename Obj_, typename RetTy_, typename... MethodArgs_>
    void registerObjectMethod(const std::string &name, RetTy_ (Obj_::*methodPtr)(MethodArgs_...), bool isPublic = true);

    template <typename Obj_, typename MemberTy_>
    void registerObjectMember(const std::string &name, MemberTy_ Obj_::*memberPtr, bool isPublic = true);


    // INTERNAL
    Value callCppFunction(DWord idx, const std::vector<Value> &args);
    // INTERNAL
    // obj: default allocated GarbageCollectedObj from the garbage collector
    Value callObjectNew(DWord objIdx, GarbageCollectedObj *obj);
    // INTERNAL
    void destroyObject(DWord objIdx, GarbageCollectedObj *obj);
    // INTERNAL
    Value callObjectMethod(DWord objIdx, DWord methodIdx, const std::vector<Value> &args);
    // INTERNAL
    Value getObjectMember(DWord idx, const Value &object);
    // INTERNAL
    void setObjectMember(DWord idx, const Value &object, const Value &val);
    

    std::string getTypeName(ValueType ty) const;


    NCSC_GETTERS_SETTERS_FOR_NAMED_VECTOR(CppFunction, cppFunctions_, CPPFunction)
    NCSC_GETTERS_SETTERS_FOR_NAMED_VECTOR(CppObject, cppObjects_, CPPObject)

private:
    friend class Compiler;

    std::vector<CPPFunction> cppFunctions_;
    std::vector<CPPObject>   cppObjects_;

    std::unordered_map<ValueType, std::string> typeNames_;
    
    // Force the user to create a ptr to the class 
    // instead of a regular instance
    ScriptContext() = default;
    

    template <typename RetTy_, typename... FuncArgs_, size_t... I>
    Value callCPPFunction(RetTy_(*fun)(FuncArgs_...), const std::vector<Value> &args, std::index_sequence<I...>);

    template <typename Obj_, typename... CtorArgs_>
    Value callCPPCtor(const std::vector<Value> &args);

    template <typename Obj_, typename RetTy_, typename... MethodArgs_>
    Value callCPPMethod(RetTy_ (Obj_::*method)(MethodArgs_...), const std::vector<Value> &args);

    template <typename Obj_, typename MemberTy_>
    Value getCPPMember(MemberTy_ Obj_::*member, const Value &arg);

    template <typename Obj_, typename MemberTy_>
    void setCPPMember(MemberTy_ Obj_::*member, const Value &arg, const Value &val);


    template <typename Obj_>
    CPPObject *getObject();

    void addTypeName(ValueType ty, const std::string &name) { typeNames_.emplace(ty, name); }

    CPPMember *getMember(DWord idx, const Value &object);
};


// --------------------
// Template definitions
// --------------------

template <typename RetTy_, typename... FuncArgs_>
void ScriptContext::registerGlobalFunction(const std::string &name, RetTy_(*fun)(FuncArgs_...)) {
    CPPFunction glblFun{};
    glblFun.name = name;
    glblFun.paramTypes = { valueTypeFromCPPType<FuncArgs_>()... }; 
    glblFun.returnTy = valueTypeFromCPPType<RetTy_>();
    glblFun.numParams = sizeof...(FuncArgs_);
    glblFun.registryFun = [this, fun](const std::vector<Value> &args) -> Value {
        return this->callCPPFunction(fun, args, std::index_sequence_for<FuncArgs_...>{});
    };

    cppFunctions_.push_back(glblFun);
}

template <typename Obj_>
void ScriptContext::registerObject(const std::string &name) {
    CPPObject obj{};
    ValueType objType = setMask((ValueType)cppObjects_.size(), ValueType::CPP_OBJ_MASK); 

    obj.type = objType;
    obj.name = name;
    obj.classInfo = &typeid(Obj_);
    obj.newFun = [objType](GarbageCollectedObj *obj) -> Value {
        // Allocate raw memory for the object
        obj->ptr = std::malloc(sizeof(Obj_));
        obj->type = objType;
        return Value{ .ty = objType, .obj = obj };
    };
    obj.destructorFun = [](GarbageCollectedObj *obj) {
        delete static_cast<Obj_ *>(obj->ptr);
    };

    cppObjects_.push_back(obj);
    typeNames_.emplace(obj.type, obj.name);
}

template <
    typename Obj_, 
    typename... CtorArgs_, 
    bool IsDefaultCtor_>
requires(
    std::is_nothrow_constructible_v<Obj_, CtorArgs_...> ||
    (IsDefaultCtor_ && std::is_default_constructible_v<Obj_>)
)
void ScriptContext::registerObjectCtor(bool isPublic) {
    CPPObject *obj = getObject<Obj_>();
    if (!obj)
        return;

    CPPMethod ctor{};
    ctor.name = obj->name;
    ctor.paramTypes = { valueTypeFromCPPType<CtorArgs_>()... };
    ctor.numParams = sizeof...(CtorArgs_) + 1;
    ctor.isPublic = isPublic;

    if constexpr (IsDefaultCtor_) {
        ctor.registryFun = [](const std::vector<Value> &args) -> Value {
            // Return empty value if first argument is invalid
            if (!isCPPObject(args[0].ty)) 
                return Value{};

            void *cppObj = args[0].obj->ptr; 
            cppObj = new(cppObj) Obj_();
            return args[0];
        };
    } else {
        ctor.registryFun = [this](const std::vector<Value> &args) -> Value {
            return this->callCPPCtor<Obj_, CtorArgs_...>(args);
        };
    }

    obj->addMethod(ctor);
}

template <typename Obj_, typename RetTy_, typename... MethodArgs_>
void ScriptContext::registerObjectMethod(const std::string &name, RetTy_ (Obj_::*methodPtr)(MethodArgs_...), bool isPublic) {
    CPPObject *obj = getObject<Obj_>();
    if (!obj)
        return;
    
    CPPMethod method{};
    method.name = name;
    method.paramTypes = { valueTypeFromCPPType<MethodArgs_>()... }; 
    method.returnTy = valueTypeFromCPPType<RetTy_>();
    // + 1 because it takes a pointer to the actual object
    method.numParams = sizeof...(MethodArgs_) + 1;
    method.isPublic = isPublic;
    method.registryFun = [this, methodPtr](const std::vector<Value> &args) -> Value {
        return this->callCPPMethod(methodPtr, args);
    };

    obj->addMethod(method);
}

template <typename Obj_, typename MemberTy_>
void ScriptContext::registerObjectMember(const std::string &name, MemberTy_ Obj_::*memberPtr,  bool isPublic) {
    CPPObject *obj = getObject<Obj_>();
    if (!obj) 
        return;
    
    CPPMember member{};
    member.name = name;
    member.type = valueTypeFromCPPType<MemberTy_>();
    member.isPublic = isPublic;
    member.getterFun = [this, memberPtr](const Value &obj) -> Value {
        return this->getCPPMember(memberPtr, obj);
    };
    member.setterFun = [this, memberPtr](const Value &obj, const Value &val) {
        this->setCPPMember(memberPtr, obj, val);
    };

    obj->addMember(member);
}

template <typename RetTy_, typename... FuncArgs_, size_t... I>
Value ScriptContext::callCPPFunction(RetTy_(*fun)(FuncArgs_...), const std::vector<Value> &args, std::index_sequence<I...>) {
    if constexpr (std::is_void_v<RetTy_>) {
        // Unsafe casting but will do for now
        // TODO: Compare types before casting
        fun(args[I].castTo<FuncArgs_>()...);
        return {};
    } 
    else {
        RetTy_ ret = fun(args[I].castTo<FuncArgs_>()...);
        return Value::fromLiteral(ret);
    }
}

template <typename Obj_, typename... CtorArgs_>
Value ScriptContext::callCPPCtor(const std::vector<Value> &args) {
    Value objArg = args[0];
    if (!isCPPObject(objArg.ty))
        return Value{};

    // Garbage memory for now
    void *objPtr = objArg.obj->ptr;

    size_t idx = 1;
    new(objPtr) Obj_(args[idx++].castTo<CtorArgs_>()...);

    return objArg;
}

template <typename Obj_, typename RetTy_, typename... MethodArgs_>
Value ScriptContext::callCPPMethod(RetTy_ (Obj_::*method)(MethodArgs_...), const std::vector<Value> &args) {
    Value objArg = args[0];
    if (!isCPPObject(objArg.ty))
        return Value{};

    Obj_ *objPtr = static_cast<Obj_ *>(objArg.obj->ptr);

    size_t idx = 1;
    if constexpr (std::is_void_v<RetTy_>) {
        (objPtr->*method)(args[idx++].castTo<MethodArgs_>()...);
        return Value{};
    } 
    else {
        RetTy_ ret = (objPtr->*method)(args[idx++].castTo<MethodArgs_>()...);
        return Value::fromLiteral(ret);
    }
}

template <typename Obj_, typename MemberTy_>
Value ScriptContext::getCPPMember(MemberTy_ Obj_::*member, const Value &arg) {
    if (!isCPPObject(arg.ty))
        return Value{};

    Obj_ *objPtr = static_cast<Obj_ *>(arg.obj->ptr);
    MemberTy_ memberVal = objPtr->*member;

    return Value::fromLiteral(memberVal);
}

template <typename Obj_, typename MemberTy_>
void ScriptContext::setCPPMember(MemberTy_ Obj_::*member, const Value &arg, const Value &val) {
    if (!isCPPObject(arg.ty))
        return;

    Obj_ *objPtr = static_cast<Obj_ *>(arg.obj->ptr);
    // Type checking is hopefully done by the compiler
    objPtr->*member = val.castTo<MemberTy_>();
}

template <typename Obj_>
CPPObject *ScriptContext::getObject() {
    for (auto &obj : cppObjects_) {
        if (*obj.classInfo == typeid(Obj_))
            return &obj;
    }
    return nullptr;
}

} // namespace NCSC
