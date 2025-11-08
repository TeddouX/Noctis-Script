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

struct CPPObject : public Object {
    struct Method : public NCSC::Method {
        std::function<Value (const std::vector<Value> &)> registryFun;
    };

    struct Member : public NCSC::Member {
        std::function<Value (const Value &)> registryFun;
    };
    
    ValueType type;
    const std::type_info *classInfo;

    GETTERS_SETTERS_FOR_NAMED_VECTOR_CASTS(Method, methods, CPPObject::Method)
    GETTERS_SETTERS_FOR_NAMED_VECTOR_CASTS(Member, members, CPPObject::Member)
};

// Context bridges the script and CPP
class NCSC_API ScriptContext {
public:
    static std::shared_ptr<ScriptContext> create();

    template <typename RetTy_, typename... FuncArgs_>
    void registerGlobalFunction(const std::string &name, RetTy_(*fun)(FuncArgs_...)) {
        CPPFunction glblFun{};
        glblFun.name = name;
        glblFun.paramTypes = { valueTypeFromCPPType<FuncArgs_>()... }; 
        glblFun.returnTy = valueTypeFromCPPType<RetTy_>();
        glblFun.numParams = sizeof...(FuncArgs_);
        glblFun.registryFun = [this, fun](const std::vector<Value> &args) -> Value {
            return this->callCPPFunction(fun, args);
        };

        cppFunctions_.push_back(glblFun);
    }

    CPPFunction *getGlobalFunction(DWord idx);
    CPPFunction *getGlobalFunction(const std::string &name);
    DWord getGlobalFunctionIdx(const std::string &name) const;
    
    Value callGlobalFunction(DWord idx, const std::vector<Value> &args);

    template <typename Obj_>
    void registerObject(const std::string &name) {
        CPPObject obj{};
        obj.type = setMask((ValueType)cppObjects_.size(), ValueType::CPP_OBJ_MASK);
        obj.name = name;
        obj.classInfo = &typeid(Obj_);

        cppObjects_.push_back(obj);
        typeNames_.emplace(obj.type, obj.name);
    }

    // Setting isPublic to false wouldn't be really useful
    template <typename Obj_, typename RetTy_, typename... MethodArgs_>
    void registerObjectMethod(const std::string &name, RetTy_ (Obj_::*methodPtr)(MethodArgs_...), bool isPublic = true) {
        CPPObject *obj = getObject<Obj_>();
        if (!obj)
            return;
        
        CPPObject::Method method{};
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
    void registerObjectMember(const std::string &name, MemberTy_ Obj_::*memberPtr,  bool isPublic = true) {
        CPPObject *obj = getObject<Obj_>();
        if (!obj) 
            return;
        
        CPPObject::Member member{};
        member.name = name;
        member.type = valueTypeFromCPPType<MemberTy_>();
        member.isPublic = isPublic;
        member.registryFun = [this, memberPtr](const Value &obj) -> Value {
            return this->getCPPMember(memberPtr, obj);
        };

        obj->addMember(member);
    }

    std::string getTypeName(ValueType ty) const;

private:
    friend class Compiler;

    std::vector<CPPFunction> cppFunctions_;
    std::vector<CPPObject>   cppObjects_;
    std::unordered_map<ValueType, std::string> typeNames_;
    
    // Force the user to create a ptr to the class 
    // instead of a regular instance
    ScriptContext() = default;

    // Holy cpp gibberish
    
    template <typename RetTy_, typename... FuncArgs_>
    Value callCPPFunction(RetTy_(*fun)(FuncArgs_...), const std::vector<Value> &args) {
        size_t idx = 0;
        if constexpr (std::is_void_v<RetTy_>) {
            // Unsafe casting but will do for now
            // TODO: Compare types before casting
            fun(args[++idx].castTo<FuncArgs_>()...);
            return {};
        } 
        else {
            RetTy_ ret = fun(args[++idx].castTo<FuncArgs_>()...);
            return Value::fromLiteral(ret);
        }
    }

    // How can this be even worse

    template <typename Obj_, typename RetTy_, typename... MethodArgs_>
    Value callCPPMethod(RetTy_ (Obj_::*method)(MethodArgs_...), const std::vector<Value> &args) {
        Value objArg = args[0];
        if (!isCPPObject(objArg.ty))
            return Value{};

        Obj_ *objPtr = static_cast<Obj_ *>(objArg.cppObj);

        // Discard the objArg
        std::vector<Value> argsReal(args.begin() + 1, args.end());
        size_t idx = 0;
        if constexpr (std::is_void_v<RetTy_>) {
            (objPtr->*method)(argsReal[++idx].castTo<MethodArgs_>()...);
            return Value{};
        } 
        else {
            RetTy_ ret = (objPtr->*method)(argsReal[++idx].castTo<MethodArgs_>()...);
            return Value::fromLiteral(ret);
        }
    }

    template <typename Obj_, typename MemberTy_>
    Value getCPPMember(MemberTy_ Obj_::*member, const Value &arg) {
        if (!isCPPObject(arg.ty))
            return Value{};

        Obj_ *objPtr = static_cast<Obj_ *>(arg.cppObj);
        MemberTy_ memberVal = objPtr->*member;
        return Value::fromLiteral(memberVal);
    }

    template <typename Obj_>
    CPPObject *getObject() {
        for (auto &obj : cppObjects_) {
            if (*obj.classInfo == typeid(Obj_))
                return &obj;
        }
        return nullptr;
    }

    void addTypeName(ValueType ty, const std::string &name) { typeNames_.emplace(ty, name); }
};

} // namespace NCSC
