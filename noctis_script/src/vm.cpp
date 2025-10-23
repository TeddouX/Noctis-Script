// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/vm.hpp>
#include <ncsc/instructions.hpp>
#include <sstream>

namespace NCSC
{

static std::string getStackString(const std::vector<Value> &stack) {
    if (stack.empty())
        return "empty";

    std::ostringstream oss;
    for (auto val : stack)
        oss << val.operator std::string() << ", ";
    return oss.str();
}


VM::VM(std::shared_ptr<Script> script)
    : script_(script) 
{
    globalVariables_.resize(script_->numGlobalVariables);
}

void VM::executeNext() {
    CallFrame &currFrame = callStack_.back();

    const Byte *bytecode = currFrame.bytecode;
    size_t bytecodeSize = currFrame.bytecodeSize;
    QWord &ip = currFrame.ip;
    size_t &bp = currFrame.bp;

    if (ip > bytecodeSize) {
        error("Bytecode ended without return");
        return;
    }

#define INSTR(x) case NCSC::Instruction::x
#define END_INSTR(numBytes) ip += numBytes; break
#define OP_INSTR(op) \
    Value b = pop(); \
    Value a = pop(); \
    push(a op b);    \
    END_INSTR(1)     \

    Instruction instr = static_cast<Instruction>(bytecode[ip]);
    switch (instr) {
        INSTR(NOOP):
            END_INSTR(1);
        
        INSTR(PUSH): {
            size_t operandSize = 0;
            push(Value::fromBytes(bytecode, bytecodeSize, ip + 1, operandSize));
            END_INSTR(operandSize + 1);
        }
        INSTR(POP): {
            pop();
            END_INSTR(1);
        }

        INSTR(STORELOCAL): {
            DWord idx = readWord<DWord>(bytecode, bytecodeSize, ip + 1);
            stack_[bp + idx] = pop();
            objReg_ = nullptr;

            END_INSTR(sizeof(DWord) + 1);
        }
        INSTR(LOADLOCAL): {
            DWord idx = readWord<DWord>(bytecode, bytecodeSize, ip + 1);
            Value &v = stack_[bp + idx]; 
            push(v);
            objReg_ = &v;

            END_INSTR(sizeof(DWord) + 1);
        }
        INSTR(STOREGLOBAL): {
            DWord idx = readWord<DWord>(bytecode, bytecodeSize, ip + 1);
            globalVariables_[idx] = pop();
            objReg_ = nullptr;
            
            END_INSTR(sizeof(DWord) + 1);
        }
        INSTR(LOADGLOBAL): {
            DWord idx = readWord<DWord>(bytecode, bytecodeSize, ip + 1);
            Value &v = globalVariables_[idx]; 
            push(v);
            objReg_ = &v;

            END_INSTR(sizeof(DWord) + 1);
        }

        INSTR(ADD): { OP_INSTR(+); }
        INSTR(SUB): { 
            Value b = pop(); 
            Value a = pop(); 
            push(a - b); 
            ip += 1; 
            break; }
        INSTR(MUL): { OP_INSTR(*); }
        INSTR(DIV): { OP_INSTR(/); }

        INSTR(CMPST): { OP_INSTR(<);  }
        INSTR(CMPSE): { OP_INSTR(<=); }
        INSTR(CMPGT): { OP_INSTR(>);  }
        INSTR(CMPGE): { OP_INSTR(>=); }
        INSTR(CMPEQ): { OP_INSTR(==); }
        INSTR(CMPNE): { OP_INSTR(!=); }
        
        INSTR(JMP): {
            ip = readWord<QWord>(bytecode, bytecodeSize, ip + 1);
            END_INSTR(0);
        }
        INSTR(JMPFALSE): {
            Value v = pop();
            if (v.get<bool>() == false) {
                ip = readWord<QWord>(bytecode, bytecodeSize, ip + 1);
                
                END_INSTR(0);
            } else
                END_INSTR(1 + sizeof(QWord));
        }
        
        INSTR(SETOBJ): {
            Value val = pop();
            *objReg_ = val;
            END_INSTR(1);
        }
        INSTR(LOADOBJ): {
            push(*objReg_);
            END_INSTR(1);
        }

        INSTR(TYCAST): {
            Value val = pop();
            ValueType castTy = static_cast<ValueType>(readWord<DWord>(bytecode, bytecodeSize, ip + 1));
            if (!canPromoteType(val.type(), castTy)) {
                // Might mess up the stack and cause weird errors
                error(std::format(UNSAFE_CAST, valueTypeToString(val.type()), valueTypeToString(castTy)));
                break;
            }
            val.setType(castTy);
            push(val);

            END_INSTR(1 + sizeof(DWord));
        }

        INSTR(CALLSCRFUN): {
            DWord idx = readWord<DWord>(bytecode, bytecodeSize, ip + 1);
            ip += 1 + sizeof(DWord);

            const ScriptFunction *fun = script_->getFunction(idx);
            sp_ -= fun->numParams;

            prepareScriptFunction(fun);

            END_INSTR(0);
        }

        INSTR(CLGLBLCPPFUN): {
            DWord idx = readWord<DWord>(bytecode, bytecodeSize, ip + 1);

            const GlobalCPPFunctionRepr *fun = script_->ctx->getGlobalFunction(idx);

            // Build vector of arguments from the top n values on the stack
            auto spIt = stack_.begin() + sp_;
            std::vector<Value> args(spIt - fun->numParams, spIt);
            sp_ -= fun->numParams;

            // Call function and push the result, if one exists
            Value ret = script_->ctx->callGlobalFunction(idx, args);
            if (fun->returnTy != ValueType::VOID)
                push(ret);

            END_INSTR(1 + sizeof(DWord));
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
    }

    // std::println("Stack: {}", getStackStrRepr());
    // std::println("Globals: {}", getStackString(globalVariables_));
    // std::println("Instr: {} SP: {}", INSTR_INFO.at(instr).first, sp_);
    // std::println();

#undef INSTR
#undef END_INSTR
#undef OP_INSTR
}

void VM::prepareScriptFunction(const ScriptFunction *fun) {
    CallFrame frame {
        .bytecode = fun->bytecode.data(),
        .bytecodeSize = fun->bytecode.size(),
        .bp = sp_,
        .ip = 0,
        .sp = sp_ + fun->numLocals,
        .stackSize = fun->requiredStackSize + fun->numLocals,
    };

    callStack_.back().sp = sp_;

    sp_ = frame.sp;
    stack_.resize(sp_ + fun->requiredStackSize);

    currFun_ = fun;
    callStack_.push_back(frame);
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
    for (const auto &global : script_->getAllGlobalVars()) {
        CallFrame cf {
            .bytecode = global.bytecode.data(),
            .bytecodeSize = global.bytecode.size(),
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
    executionFinished_ = false;
    hasError_ = false;

    if (!currFun_) {
        error(std::string(NO_FUN_PREPD));
        return false;
    }

    CallFrame baseFrame{
        .bytecode = currFun_->bytecode.data(),
        .bytecodeSize = currFun_->bytecode.size(),
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
    return getStackString(stack_);
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
    lastError_ = mess;
}


} // namespace NCSC
