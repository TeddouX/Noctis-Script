// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/vm.hpp>
#include <ncsc/instructions.hpp>
#include <sstream>

namespace NCSC
{


VM::VM(std::shared_ptr<Script> script, std::shared_ptr<GarbageCollector> gc)
    : script_(script),
      ctx_(script->ctx),
      garbageCollector_(gc)
{
    globalVariables_.resize(script_->numGlobalVariables);
}

void VM::executeNext() {
    CallFrame &currFrame = callStack_.back();

    const auto &bytecode = *currFrame.bytecode;
    QWord &ip = currFrame.ip;
    size_t &bp = currFrame.bp;

    if (ip >= bytecode.size()) {
        error("Bytecode ended without return");
        return;
    }

#define INSTR(x) case NCSC::Instruction::x
#define END_INSTR(numBytes) ip += numBytes; break

    Instruction instr = static_cast<Instruction>(bytecode[ip]);
    switch (instr) {
        INSTR(NOOP):
            END_INSTR(1);
        
        INSTR(PUSH): {
            size_t operandSize = 0;
            push(Value::fromBytes(bytecode, ip + 1, operandSize));
            END_INSTR(operandSize + 1);
        }
        INSTR(POP): {
            pop();
            END_INSTR(1);
        }
        INSTR(DUP): {
            Value val = pop();
            push(val);
            push(val);
            END_INSTR(1);
        }

        INSTR(LOADLOCAL): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            push(stack_[bp + idx]);

            END_INSTR(sizeof(DWord) + 1);
        }
        INSTR(STORELOCAL): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            stack_[bp + idx] = pop();
            END_INSTR(sizeof(DWord) + 1);
        }
        INSTR(LOADGLOBAL): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            push(globalVariables_[idx]);
            END_INSTR(sizeof(DWord) + 1);
        }
        INSTR(STOREGLOBAL): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            globalVariables_[idx] = pop();
            END_INSTR(sizeof(DWord) + 1);
        }
        INSTR(LOADMEMBER): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            Value obj = pop();
            const ValueType objTy = obj.ty;

            if (!isObject(objTy)) {
                error(std::string(TRYED_ACCESSING_MEMB_OF_INV_OB));
                return;
            }
            
            if (isCPPObject(objTy))
                push(ctx_->getObjectMember(idx, obj));
            else {
                auto members = static_cast<std::vector<Value> *>(obj.obj->ptr);
                push(members->at(idx));
            }

            END_INSTR(sizeof(DWord) + 1);
        }
        INSTR(STOREMEMBER): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            Value obj = pop();
            const ValueType objTy = obj.ty;

            if (!isObject(objTy)) {
                error(std::string(TRYED_ACCESSING_MEMB_OF_INV_OB));
                return;
            }
            
            if (isCPPObject(objTy))
                ctx_->setObjectMember(idx, obj, pop());
            else {
                auto members = static_cast<std::vector<Value> *>(obj.obj->ptr);
                members->at(idx) = pop();
            }

            END_INSTR(sizeof(DWord) + 1);
        }

#define OP_INSTR(op) \
    Value a = pop(); \
    Value b = pop(); \
    push(a op b);    \
    END_INSTR(1)     \

        INSTR(ADD): { OP_INSTR(+); }
        INSTR(SUB): { OP_INSTR(-); }
        INSTR(MUL): { OP_INSTR(*); }
        INSTR(DIV): { OP_INSTR(/); }

        INSTR(CMPST): { OP_INSTR(<);  }
        INSTR(CMPSE): { OP_INSTR(<=); }
        INSTR(CMPGT): { OP_INSTR(>);  }
        INSTR(CMPGE): { OP_INSTR(>=); }
        INSTR(CMPEQ): { OP_INSTR(==); }
        INSTR(CMPNE): { OP_INSTR(!=); }

#undef OP_INSTR

        INSTR(INC): {
            Value a = pop();

#define CASE_INC(TYPE, FIELD) case ValueType::TYPE: ++a.FIELD; break;
            switch (a.ty) {
                CASE_INC(INT8, i8)
                CASE_INC(INT16, i16)
                CASE_INC(INT32, i32)
                CASE_INC(INT64, i64)

                CASE_INC(UINT8, ui8)
                CASE_INC(UINT16, ui16)
                CASE_INC(UINT32, ui32)
                CASE_INC(UINT64, ui64)

                case ValueType::FLOAT32: a.f32 += 1.0f; break;
                case ValueType::FLOAT64: a.f64 += 1.0; break;

                default: error(std::string(CANT_INC_OR_DEC_NON_NUM));
            }
#undef CASE_INC

            push(a);

            END_INSTR(1);
        }
        
        INSTR(DEC): {
            Value a = pop();

#define CASE_DEC(TYPE, FIELD) case ValueType::TYPE: --a.FIELD; break;
            switch (a.ty) {
                CASE_DEC(INT8, i8)
                CASE_DEC(INT16, i16)
                CASE_DEC(INT32, i32)
                CASE_DEC(INT64, i64)

                CASE_DEC(UINT8, ui8)
                CASE_DEC(UINT16, ui16)
                CASE_DEC(UINT32, ui32)
                CASE_DEC(UINT64, ui64)

                case ValueType::FLOAT32: a.f32 -= 1.0f; break;
                case ValueType::FLOAT64: a.f64 -= 1.0; break;

                default: error(std::string(CANT_INC_OR_DEC_NON_NUM));
            }
#undef CASE_DEC

            push(a);

            END_INSTR(1);
        }
        INSTR(NOT): {
            Value v = pop();
            if (v.ty != ValueType::BOOL) {
                error(std::string(CANT_INVERT_NON_BOOLEAN));
                break;
            }

            push(Value{ .ty = ValueType::BOOL, .b = !v.b });

            END_INSTR(1);
        }

        INSTR(JMP): {
            ip = readWord<QWord>(bytecode, ip + 1);
            END_INSTR(0);
        }
        INSTR(JMPFALSE): {
            Value v = pop();
            if (v.b == false) {
                ip = readWord<QWord>(bytecode, ip + 1);
                
                END_INSTR(0);
            } else
                END_INSTR(1 + sizeof(QWord));
        }

        INSTR(TYCAST): {
            Value val = pop();
            ValueType castTy = static_cast<ValueType>(readWord<DWord>(bytecode, ip + 1));
            if (!canPromoteType(val.ty, castTy)) {
                // Might mess up the stack and cause weird errors
                error(std::format(UNSAFE_CAST, ctx_->getTypeName(val.ty), ctx_->getTypeName(castTy)));
                break;
            }
            val.ty = castTy;
            push(val);

            END_INSTR(1 + sizeof(DWord));
        }

        INSTR(CALLSCRFUN): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            ip += 1 + sizeof(DWord);

            const ScriptFunction *fun = script_->getFunction(idx);
            sp_ -= fun->numParams;

            prepareScriptFunction(fun);

            break;
        }
        INSTR(CALLMETHOD): {
            ip += 1;

            DWord objIdx = readWord<DWord>(bytecode, ip);
            ip += sizeof(DWord);

            DWord methodIdx = readWord<DWord>(bytecode, ip);
            ip += sizeof(DWord);
            
            ScriptObject *scriptObj = script_->getObject(objIdx);
            const ScriptFunction *method = scriptObj->getMethod(methodIdx);

            sp_ -= method->numParams;
            prepareScriptFunction(method);

            break;
        }

        INSTR(CALLCPPFUN): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);

            const CPPFunction *fun = ctx_->getCppFunction(idx);

            // Build vector of arguments from the top n values on the stack
            auto spIt = stack_.begin() + sp_;
            std::vector<Value> args(spIt - fun->numParams, spIt);
            sp_ -= fun->numParams;

            // Call function and push the result, if one exists
            Value ret = ctx_->callCppFunction(idx, args);
            if (fun->returnTy != ValueType::VOID)
                push(ret);

            END_INSTR(1 + sizeof(DWord));
        }

        INSTR(CALLCPPMETHOD): {
            ip += 1;

            DWord objIdx = readWord<DWord>(bytecode, ip);
            ip += sizeof(DWord);

            DWord methodIdx = readWord<DWord>(bytecode, ip);
            ip += sizeof(DWord);

            CPPObject *obj = ctx_->getCppObject(objIdx);
            if (!obj) break;
            const CPPMethod *method = obj->getMethod(methodIdx);
            if (!method) break;

            auto spIt = stack_.begin() + sp_;
            std::vector<Value> args(spIt - method->numParams, spIt);
            sp_ -= method->numParams;

            Value ret = ctx_->callObjectMethod(objIdx, methodIdx, args);
            if (method->returnTy != ValueType::VOID)
                push(ret);

            break;
        }

        INSTR(RET): {
            Value ret = pop();
            // Don't resize if its the first function pushed on the 
            // callstack is being returned from
            if (callStack_.size() > 1) {
                const CallFrame &lastFrame = callStack_[callStack_.size() - 2];
                stack_.resize(lastFrame.stackSize);
                sp_ = lastFrame.sp;
            }
                        
            callStack_.pop_back();

            push(ret);

            END_INSTR(1);
        }
        INSTR(RETVOID): {
            if (callStack_.size() > 1) {
                const CallFrame &lastFrame = callStack_[callStack_.size() - 2];
                stack_.resize(lastFrame.stackSize);
                sp_ = lastFrame.sp;
            }

            callStack_.pop_back();

            END_INSTR(1);
        }

        INSTR(NEW): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            ScriptObject *scriptObj = script_->getObject(idx);
            auto objVals = new std::vector<Value>;
            objVals->resize(scriptObj->getMemberCount());

            GarbageCollectorObj *gcObj = garbageCollector_->allocateObj();
            gcObj->ptr = objVals;
            gcObj->type = scriptObj->type;

            Value obj{
                .ty = setMask((ValueType)idx, ValueType::OBJ_MASK),
                .obj = gcObj,
            };

            push(obj);

            END_INSTR(1 + sizeof(DWord));
        }

        INSTR(CPPNEW): {
            DWord objIdx = readWord<DWord>(bytecode, ip + 1);
            
            GarbageCollectorObj *gcObj = garbageCollector_->allocateObj();
            push(ctx_->callObjectNew(objIdx, gcObj));

            END_INSTR(1 + sizeof(DWord));
        }

        default:
            error(std::string(INVALID_OR_CORRUPTED_BC));
            break;
    }

    // std::println("Instr: {} SP: {} BP: {}", 
    //     INSTR_INFO.at(instr).first, 
    //     sp_, 
    //     callStack_.empty() ? 0 : callStack_.back().bp);
    // std::println("Stack: {}", getStackStrRepr());
    // std::println("Globals: {}", getStackStrRepr(globalVariables_));
    // std::println();

    garbageCollector_->gc(stack_);

#undef INSTR
#undef END_INSTR
}

void VM::prepareScriptFunction(const ScriptFunction *fun) {
    CallFrame frame {
        .bytecode = &fun->bytecode,
        .bp = sp_,
        .ip = 0,
        .sp = sp_ + fun->numLocals,
        // Parameters are already on the stack so no
        .stackSize = callStack_.back().stackSize + fun->requiredStackSize + fun->numLocals - fun->numParams,
    };

    callStack_.back().sp = sp_;

    sp_ = frame.sp;
    stack_.resize(sp_ + fun->requiredStackSize);

    currFun_ = fun;
    callStack_.push_back(frame);
}

Value VM::makeReference(Value &val) {
    bool isRef = NCSC::isRef(val.ty);
    return Value{
        .ty = setMask(val.ty, ValueType::REF_MASK),
        .ref = isRef ? val.ref : &val,
    };
}

void VM::prepareFunction(const ScriptFunction *fun) {
    if (!fun)
        return;

    currFun_ = fun;
    stack_.clear();
    stack_.resize(currFun_->numLocals + currFun_->requiredStackSize);
    
    callStack_.clear();
    callStack_.reserve(1);
}

bool VM::computeGlobals() {
    if (!script_) {
        error(std::string(NO_SCRIPT_ATTACHED));
        return false;
    }

    // Compute values for all global variables
    for (const auto &global : script_->getAllGlobalVariables()) {
        CallFrame cf {
            .bytecode = &global.bytecode,
            .stackSize = global.requiredStackSize,
        };
        
        sp_ = 0;
        callStack_.push_back(cf);
        stack_.resize(global.requiredStackSize);

        while (callStack_.size() > 0 && !hasError_)
            executeNext();
        
        stack_.clear();
        callStack_.clear();

        if (hasError_)
            return false;
    }

    return true;
}

bool VM::execute() {
    if (!garbageCollector_)
        garbageCollector_ = std::make_shared<GarbageCollector>(ctx_, DEFAULT_GC_CONF_);
    
    executionFinished_ = false;
    hasError_ = false;

    if (!currFun_) {
        error(std::string(NO_FUN_PREPD));
        return false;
    }

    CallFrame baseFrame{
        .bytecode = &currFun_->bytecode,
        .sp = currFun_->numLocals,
        .stackSize = currFun_->requiredStackSize + currFun_->numLocals,
    };

    sp_ = baseFrame.sp;
    callStack_.push_back(baseFrame);

    while (callStack_.size() > 0 && !hasError_)
        executeNext();

    executionFinished_ = true;
    return !hasError_;
}

std::string VM::getStackStrRepr() const {
    if (stack_.empty())
        return "Stack is empty";

    std::string res = "[ ";
    for (size_t i = 0; i < stack_.size(); i++) {
        const Value &val = stack_[i];
    
        if (i >= sp_)
            res += "invalid";
        else
            res += val.getStrRepr(ctx_.get());

        // Don't add a comma to the last value
        if (i < stack_.size() - 1)
            res += ", ";
    }
    res += " ]";

    return res;
}

Value VM::pop() {
    if (sp_ - 1 == UINT64_MAX) {
        error(std::string(STACK_UNDERFLOW_EMPTY));
        return Value{};
    }

    if (currFun_ && !callStack_.empty()) {
        size_t frameBase = callStack_.back().bp;
        if (sp_ < frameBase) {
            error(std::string(STACK_UNDERFLOW_STACK_FRAME));
            return Value{};
        }
    }

    sp_--;
    Value val = stack_[sp_];
    return val;
}

void VM::push(const Value &val) {
    if (sp_ >= stack_.size()) {
        error(std::string(STACK_OVERFLOW));
        return;
    }

    stack_[sp_] = val;
    sp_++;
}

void VM::error(const std::string &mess) {
    hasError_ = true;

    const CallFrame &lastFrame = callStack_.back();
    std::string location = std::format(" (in function '{}', bytecode location: {})", currFun_->name, lastFrame.ip);
    lastError_ = mess + location;
}


} // namespace NCSC
