#include "compiler/compiler.hpp"


namespace NCSC
{
    
Compiler::Compiler(bool is_debug = false)
    : curr_scope_{nullptr}
    , is_debug_{is_debug}
{}

} // namespace NCSC
