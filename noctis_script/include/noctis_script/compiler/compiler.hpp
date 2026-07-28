#pragma once
#include <string>
#include <stack>

#include "bytecode.hpp"
#include "value_type.hpp"
#include "compiler_scope.hpp"
#include "compiler_function.hpp"
#include "compiler_object.hpp"


namespace NCSC
{

class Compiler
{
public:
    Compiler(bool is_debug = false);

    auto compile_script(const std::string &script) -> Bytecode;
    auto compile_script(std::shared_ptr<ScriptSource> src) -> Bytecode;

private:
    std::vector<CompilerData::Object>   objects_;
    std::vector<CompilerData::Function> functions_;
    std::shared_ptr<ScriptSource>       src_;

    Bytecode bytecode_;

    std::stack<CompilerData::Scope>     scope_stack_;
    CompilerData::Scope                *curr_scope_;

    bool is_debug_;
};

} // namespace NCSC
