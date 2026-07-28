#include "bytecode_gen/vm_bytecode_gen.hpp"

#include "lexer/lexer.hpp"
#include "parser/parser.hpp"


#define CHECK_NODE_TYPE(node, expected)                                                             \
    do {                                                                                            \
        if (node.type() != expected)                                                                \
        {                                                                                           \
            error(ERR_INVALID_AST_NODE, Location{}, to_string(expected), to_string(node.type()));   \
            return;                                                                                 \
        }                                                                                           \
    } while(0)

namespace NCSC
{
    
VMBytecodeGenerator::VMBytecodeGenerator(bool is_debug)
    : curr_scope_{nullptr}
    , is_debug_{is_debug}
{}

auto VMBytecodeGenerator::compile_script(const std::string &script) -> Bytecode
{
    std::shared_ptr<ScriptSource> src = ScriptSource::from_contents(script);
    return compile_script(src);
}

auto VMBytecodeGenerator::compile_script(std::shared_ptr<ScriptSource> src) -> Bytecode
{
    std::vector<Token> tokens = tokenize(src);

    Parser parser{tokens, src};
    ASTNode root = parser.parse();

    if (parser.has_syntax_errors()) 
    {
        syntax_errors_ = parser.get_syntax_errors();   
        return Bytecode{};
    }

    return compile_script(root, src);
}

auto VMBytecodeGenerator::compile_script(const ASTNode &root_node, std::shared_ptr<ScriptSource> src) -> Bytecode
{
    script_source_ = src;

    compile_declaration_body(root_node.children()[0]);

    return bytecode_;
}

auto VMBytecodeGenerator::has_compile_errors() const -> bool
{
    return not compile_errors_.empty();
}

auto VMBytecodeGenerator::compile_errors() const -> const std::vector<Error> &
{
    return compile_errors_;
}

auto VMBytecodeGenerator::has_syntax_errors() const -> bool
{
    return not syntax_errors_.empty();
}

auto VMBytecodeGenerator::syntax_errors() const -> const std::vector<Error> &
{
    return syntax_errors_;
}

auto VMBytecodeGenerator::enter_new_scope() -> void
{
    scope_deque_.push_back(Internal::Scope{});
    curr_scope_ = &scope_deque_.back();

    // Set the new scope's parent if there's more than one scope on the stack
    if (scope_deque_.size() > 1)
        curr_scope_->parent = &scope_deque_[scope_deque_.size() - 2];
}

auto VMBytecodeGenerator::exit_scope() -> void
{
    scope_deque_.pop_back();

    if (scope_deque_.empty()) 
    {
        curr_scope_ = nullptr;
        return;
    }

    curr_scope_ = &scope_deque_.back();
}

auto VMBytecodeGenerator::reset_scopes() -> void
{
    scope_deque_.clear();
    curr_scope_ = nullptr;
}

auto VMBytecodeGenerator::compile_declaration_body(const ASTNode &decl_body) -> void
{
    CHECK_NODE_TYPE(decl_body, ASTNodeType::DECLARATION_BODY);
    
    for (const ASTNode &child : decl_body.children())
    {
        switch (child.type())
        {
            case ASTNodeType::FUNCTION_DECLARATION:
                compile_function_declaration(child);
                break;
            case ASTNodeType::VARIABLE_DECLARATION:
                break;
            case ASTNodeType::OBJ_DECLARATION:
                break;
        }
    }
}

auto VMBytecodeGenerator::compile_function_declaration(const ASTNode &func_decl) -> void
{
    CHECK_NODE_TYPE(func_decl, ASTNodeType::FUNCTION_DECLARATION);

    bool is_method = func_decl.get_metadata<bool>("is_method");
    if (is_method)
    {
        compile_method_declaration(func_decl);
        return;
    }

    const auto &name_node = func_decl.children()[0];
}

auto VMBytecodeGenerator::compile_method_declaration(const ASTNode &method_decl) -> void
{
    CHECK_NODE_TYPE(method_decl, ASTNodeType::FUNCTION_DECLARATION);
}

} // namespace NCSC
