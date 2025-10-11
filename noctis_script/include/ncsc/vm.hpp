#include "function.hpp"
#include "value.hpp"

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
    VM() = default;

    void prepareFunction(const Function *fun);
    bool execute();

private:
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
};
 
} // namespace NCSC

