#include <ncsc/vm.hpp>
#include <print>

namespace NCSC
{

void printValues(const std::vector<Value> &stack) {
    if (stack.empty())
        std::print("empty");
    
    for (auto val : stack)
        std::print("{} ", val.operator std::string());
    std::println();
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
            stack_.push_back(pop());
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
            
            prepareScriptFunction(script_->getFunction(idx));

            END_INSTR(0);
        }

        INSTR(RET): {
            CallFrame frame = callStack_.back();
            Value ret = pop();
            
            callStack_.pop_back();
            stack_.resize(frame.bp);
            
            push(ret);

            END_INSTR(1);
        }
        INSTR(RETVOID): {
            CallFrame frame = callStack_.back();
            callStack_.pop_back();
            stack_.resize(frame.bp);

            END_INSTR(1);
        }
    }

    printValues(stack_);
}

void VM::prepareScriptFunction(const Function *fun) {
    CallFrame frame {
        .bytecode = fun->bytecode.data(),
        .bytecodeSize = fun->bytecode.size(),
        .bp = stack_.size(),
        .ip = 0,
    };

    currFun = fun;
    std::println("fun {}:\n{}", currFun->name, currFun->getBytecodeStrRepr());
    callStack_.push_back(frame);
}

void VM::prepareFunction(const Function *fun) {
    if (!fun)
        return;

    currFun = fun;
    stack_.clear();
    stack_.reserve(currFun->numLocals + currFun->requiredStackSize);
    
    callStack_.clear();
    callStack_.reserve(1);
}

bool VM::execute() {
    if (!currFun)
        return false;

    CallFrame baseFrame{
        .bytecode = currFun->bytecode.data(),
        .bytecodeSize = currFun->bytecode.size(),
        .bp = stack_.size(),
        .ip = 0
    };
    callStack_.push_back(baseFrame);

    while (callStack_.size() > 0 && !hasError_)
        executeNext();

    return hasError_;
}

Value VM::pop() {
    Value val = stack_.back();
    stack_.pop_back();
    return val;
}

void VM::push(const Value &val) {
    stack_.push_back(val);
}

void VM::error(const std::string &mess) {
    hasError_ = true;
    lastError_ = mess;
}


} // namespace NCSC
