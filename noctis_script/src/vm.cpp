#include <ncsc/vm.hpp>
#include <sstream>

namespace NCSC
{

static std::string getStackString(const std::vector<Value> &stack) {
    if (stack.empty())
        return "empty";

    std::ostringstream oss;
    for (auto val : stack)
        oss << val.operator std::string() << " ";
    return oss.str();
}


template <typename T>
static T readWord(const Byte *bytes, size_t idx) {
    T words = 0;
    for (size_t i = 0; i < sizeof(T); ++i)
        words |= ((T)bytes[idx + i] >> (i * 8)) & 0xFF;
    return words;
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

        INSTR(PUSHINT): {
            push(Value{
                .type = ValueType::INT,
                .i = static_cast<int64_t>(readWord<QWord>(bytecode, ip + 1)),
            });
            END_INSTR(sizeof(QWord) + 1);
        }
        INSTR(PUSHFLOAT): {
            push(Value{
                .type = ValueType::FLOAT,
                .f = static_cast<float64_t>(readWord<QWord>(bytecode, ip + 1)),
            });
            END_INSTR(sizeof(QWord) + 1);
        }
        INSTR(POP):
            pop();
            END_INSTR(1);

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

            // std::println("ret");

            END_INSTR(1);
        }
        INSTR(RETVOID): {
            if (callStack_.size() >= 2) {
                const CallFrame &lastFrame = callStack_[callStack_.size() - 2];
                stack_.resize(lastFrame.stackSize);
                sp_ = lastFrame.sp;
            }

            callStack_.pop_back();
            // std::println("ret");

            END_INSTR(1);
        }
    }

    // std::println("EndOP");
    // printValues(stack_);
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

    // std::println("Calling {}, bp={}, num args={}, num locals={}, starting sp={}, required stack size={}", 
    //     fun->name, frame.bp, fun->numParams, fun->numLocals, frame.sp, fun->requiredStackSize);
    // std::println("{}", fun->getBytecodeStrRepr());

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

bool VM::execute() {
    if (!currFun)
        return false;

    CallFrame baseFrame{
        .bytecode = currFun->bytecode.data(),
        .bytecodeSize = currFun->bytecode.size(),
        .bp = 0,
        .ip = 0,
        .sp = currFun->numParams + currFun->numLocals,
        .stackSize = currFun->requiredStackSize + currFun->numParams + currFun->numLocals,
    };
    callStack_.push_back(baseFrame);
    sp_ = baseFrame.sp;

    while (callStack_.size() > 0 && !hasError_)
        executeNext();

    return !hasError_;
}

std::string VM::getStackStrRepr() const {
    return getStackString(stack_);
}

Value VM::pop() {
    if (sp_ < callStack_.back().bp + currFun->numLocals) {
        error("Stack underflow");
        return Value{};
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

    if (sp_ == 0 && stack_[0].type == ValueType::UNINITIALIZED) {
        stack_[0] = val;
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
