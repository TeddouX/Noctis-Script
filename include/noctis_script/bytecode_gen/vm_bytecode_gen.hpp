#pragma once
#include <string>
#include <deque>

#include "bytecode.hpp"
#include "bytecode_gen_data.hpp"
#include "value_type.hpp"
#include "../parser/ast_node.hpp"
#include "../error.hpp"
#include "value_type.hpp"


namespace NCSC
{

class VMBytecodeGenerator
{
public:
    VMBytecodeGenerator(bool is_debug = false);

    auto compile_script(std::shared_ptr<ScriptSource> src) -> Bytecode;
    auto compile_script(const std::string &script) -> Bytecode;
    // Do not try to compile if the code has syntax errors as it may lead to crashes
    auto compile_script(const ASTNode &root_node, std::shared_ptr<ScriptSource> src = nullptr) -> Bytecode;

    auto has_compile_errors() const -> bool;
    // Only useful if you haven't parsed the script yourself
    auto has_syntax_errors() const -> bool;
    
    auto compile_errors() const -> const std::vector<Error> &;
    // Only useful if you haven't parsed the script yourself
    auto syntax_errors() const -> const std::vector<Error> &;

private:
    std::vector<Internal::Object>   objects_;
    std::vector<Internal::Function> functions_;
    std::shared_ptr<ScriptSource>   script_source_;

    Bytecode bytecode_;

    std::deque<Internal::Scope>     scope_deque_;
    Internal::Scope                *curr_scope_;

    std::vector<Error>              compile_errors_;
    std::vector<Error>              syntax_errors_;

    bool                            is_debug_;

    
    template <typename... _Args>
    auto error(const std::shared_ptr<ErrorInfo> &err_info, const Location &location, _Args&&... args) -> void
    {
        std::string err_message = err_info->get_formatted(std::forward<_Args>(args)...);
        Error err{err_info, err_message, script_source_, location};

        compile_errors_.push_back(err);
    }

    auto enter_new_scope() -> void;
    auto exit_scope() -> void;
    auto reset_scopes() -> void;

    auto compile_declaration_body(const ASTNode &decl_body) -> void;
    auto compile_function_declaration(const ASTNode &func_decl) -> void;
    auto compile_method_declaration(const ASTNode &func_decl) -> void;

    inline static std::shared_ptr<ErrorInfo> ERR_INVALID_AST_NODE {ErrorInfo::create("Internal Compiler Error", "IC1", "ASTNode type '{}' doesn't match the expected '{}'")};
};

} // namespace NCSC
