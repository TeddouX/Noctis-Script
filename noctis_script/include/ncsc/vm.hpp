#pragma once
#include "function.hpp"
#include "script.hpp"
#include "value.hpp"

#include <memory>

namespace NCSC
{

struct CallFrame {
    const Byte *bytecode;
    size_t bytecodeSize;
    size_t bp = 0;
    size_t ip = 0;
    size_t sp = 0;
    size_t stackSize = 0;
};

class NCSC_API VM {
public:
    VM(std::shared_ptr<Script> script);

    bool computeGlobals();

    void prepareFunction(const Function *fun);
    // Returns false if there was an error during execution
    bool execute();
    std::string getStackStrRepr() const;

    const std::string &getLastError() { return lastError_; }

private:
    std::shared_ptr<Script> script_; 
    const Function *currFun = nullptr;
    
    // Stack
    std::vector<Value> stack_;
    // In this implementation, the stack pointer points at the 
    // next free spot on the stack
    size_t sp_ = 0;
    std::vector<CallFrame> callStack_;

    // Computed when the vm instance is created
    std::vector<Value> globalVariables_;

    bool hasError_ = false;
    std::string lastError_;

    Value pop();
    void push(const Value &val);

    void error(const std::string &mess);

    void executeNext();

    void prepareScriptFunction(const Function *fun);
};
 
} // namespace NCSC

