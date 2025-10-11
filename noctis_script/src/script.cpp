#include <ncsc/script.hpp>

namespace NCSC
{
    
const Function *Script::getFunction(const std::string &name) {
    for (const auto &fun : functions_)
        if (fun.name == name)
            return &fun;
    return nullptr;
}

} // namespace NCSC
