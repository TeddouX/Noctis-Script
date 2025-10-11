#include <ncsc/script.hpp>

namespace NCSC
{
    
const Function *Script::getFunction(const std::string &name) {
    for (const auto &fun : functions_)
        if (fun.name == name)
            return &fun;
    return nullptr;
}

const Function *Script::getFunction(DWord idx) {
    if (idx > functions_.size())
        return nullptr;
    return &functions_[idx];
}

const DWord Script::getFunctionIdx(const std::string &name) {
    for (int i = 0; i < functions_.size(); i++)
        if (functions_[i].name == name)
            return i;

    // DWord is unsigned, this will underflow
    return -1;
}

} // namespace NCSC
