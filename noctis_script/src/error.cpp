#include <ncsc/error.hpp>
#include <format>

namespace NCSC
{
    
Error::Error(const std::string &mess, const ScriptNode &node) 
    : mess_(mess) 
{
    const Token *tok = node.token;
    if (tok) {
        line_ = tok->line;
        col_ = tok->col;
    }
}

std::string Error::getString() {
    return std::format("{}\nLocation: {}:{}", mess_, line_, col_);
}

} // namespace NCSC
