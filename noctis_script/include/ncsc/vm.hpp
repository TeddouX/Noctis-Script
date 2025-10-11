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
    size_t bp;
    size_t ip;
};

class NCSC_API VM {
public:
    VM(std::shared_ptr<Script> script)
        : script_(script) {}

    void prepareFunction(const Function *fun);
    bool execute();

private:
    std::shared_ptr<Script> script_; 
    const Function *currFun = nullptr;
    
    // Stack
    std::vector<Value> stack_;
    std::vector<CallFrame> callStack_;

    bool hasError_ = false;
    std::string lastError_;

    Value pop();
    void push(const Value &val);

    void error(const std::string &mess);

    void executeNext();

    void prepareScriptFunction(const Function *fun);
};
 
} // namespace NCSC

