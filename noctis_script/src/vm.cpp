#include <ncsc/vm.hpp>
#include <sstream>
#include <print>

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
    size_t &ip = currFrame.ip;
    size_t &bp = currFrame.bp;

    if (ip > currFrame.bytecodeSize) {
        error("Bytecode ended without return");
        return;
    }

#define INSTR(x) case (NCSC::Byte)NCSC::Instruction::x
#define END_INSTR(numBytes) ip += numBytes; break

    switch (bytecode[ip]) {
        INSTR(NOOP):
            END_INSTR(1);
        
        INSTR(STORELOCAL): {
            Word idx = readWord<Word>(bytecode, ip + 1);
            stack_[bp + idx] = pop();
            END_INSTR(sizeof(Word) + 1);
        }
        INSTR(LOADLOCAL): {
            Word idx = readWord<Word>(bytecode, ip + 1);
            push(stack_[bp + idx]);
            END_INSTR(sizeof(Word) + 1);
        }
        INSTR(STOREGLOBAL): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            globalVariables_[idx] = pop();
            END_INSTR(sizeof(DWord) + 1);
        }
        INSTR(LOADGLOBAL): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            push(globalVariables_[idx]);
            END_INSTR(sizeof(DWord) + 1);
        }

        INSTR(PUSH): {
            size_t operandSize = 0;
            push(Value::fromBytes(bytecode, ip + 1, operandSize));
            END_INSTR(operandSize + 1);
        }

        INSTR(ADD): {
            Value b = pop();
            Value a = pop();

            push(a + b);

            END_INSTR(1);
        }
        INSTR(SUB): {
            Value b = pop();
            Value a = pop();

            push(a - b);

            END_INSTR(1);
        }
        INSTR(MUL): {
            Value b = pop();
            Value a = pop();

            push(a * b);

            END_INSTR(1);
        }
        INSTR(DIV): {
            Value b = pop();
            Value a = pop();

            push(a / b);

            END_INSTR(1);
        }

        INSTR(TYCAST): {
            Value val = pop();
            ValueType castTy = static_cast<ValueType>(readWord<DWord>(bytecode, ip + 1));
            if (!canPromoteType(val.ty, castTy)) {
                // Might mess up the stack and cause weird errors
                error("Unsafe cast");
                break;
            }
            val.ty = castTy;
            push(val);

            END_INSTR(1 + sizeof(DWord));
        }

        INSTR(CALLSCRFUN): {
            DWord idx = readWord<DWord>(bytecode, ip + 1);
            ip += 1 + sizeof(DWord);

            const Function *fun = script_->getFunction(idx);
            sp_ -= fun->numParams;

            prepareScriptFunction(fun);

            END_INSTR(0);
        }

        INSTR(RET): {
            Value ret = pop();
            if (callStack_.size() >= 2) {
                const CallFrame &lastFrame = callStack_[callStack_.size() - 2];
                stack_.resize(lastFrame.stackSize);
                sp_ = lastFrame.sp;
            }
                        
            callStack_.pop_back();

            push(ret);

            END_INSTR(1);
        }
        INSTR(RETVOID): {
            if (callStack_.size() >= 2) {
                const CallFrame &lastFrame = callStack_[callStack_.size() - 2];
                stack_.resize(lastFrame.stackSize);
                sp_ = lastFrame.sp;
            }

            callStack_.pop_back();

            END_INSTR(1);
        }
    }
}

void VM::prepareScriptFunction(const Function *fun) {
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

    currFun = fun;
    callStack_.push_back(frame);
}

void VM::prepareFunction(const Function *fun) {
    if (!fun)
        return;

    currFun = fun;
    stack_.clear();
    stack_.resize(currFun->numParams + currFun->numLocals + currFun->requiredStackSize);
    
    callStack_.clear();
    callStack_.reserve(1);
}

bool VM::computeGlobals() {
    if (!script_) {
        error("No script attached to the VM");
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
    if (!currFun)
        return false;

    CallFrame baseFrame{
        .bytecode = currFun->bytecode.data(),
        .bytecodeSize = currFun->bytecode.size(),
        .sp = currFun->numParams + currFun->numLocals,
        .stackSize = currFun->requiredStackSize + currFun->numParams + currFun->numLocals,
    };

    sp_ = baseFrame.sp;
    callStack_.push_back(baseFrame);

    while (callStack_.size() > 0 && !hasError_)
        executeNext();

    return !hasError_;
}

std::string VM::getStackStrRepr() const {
    return getStackString(stack_);
}

Value VM::pop() {
    if (sp_ - 1 == UINT64_MAX) {
        error("Stack underflow (empty stack)");
        return Value{};
    }

    if (currFun) {
        size_t frameBase = callStack_.back().bp;
        if (sp_ < frameBase) {
            error("Stack underflow (below current frame)");
            return Value{};
        }
    }

    sp_--;
    Value val = stack_[sp_];
    return val;
}

void VM::push(const Value &val) {
    if (sp_ > stack_.size()) {
        error("Stack overflow");
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
