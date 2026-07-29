#include "bytecode_gen/bytecode_gen.hpp"

#include "lexer/lexer.hpp"
#include "parser/parser.hpp"
#include "utils/vector_utils.hpp"


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
    
BytecodeGenerator::BytecodeGenerator(bool is_debug)
    : curr_scope_{nullptr}
    , is_debug_{is_debug}
{}

auto BytecodeGenerator::compile_script(const std::string &script) -> Bytecode
{
    std::shared_ptr<ScriptSource> src = ScriptSource::from_contents(script);
    return compile_script(src);
}

auto BytecodeGenerator::compile_script(std::shared_ptr<ScriptSource> src) -> Bytecode
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

auto BytecodeGenerator::compile_script(const ASTNode &root_node, std::shared_ptr<ScriptSource> src) -> Bytecode
{
    script_source_ = src;

    handle_declaration_body(root_node.children()[0]);

    return bytecode_;
}

auto BytecodeGenerator::has_compile_errors() const -> bool
{
    return not compile_errors_.empty();
}

auto BytecodeGenerator::compile_errors() const -> const std::vector<Error> &
{
    return compile_errors_;
}

auto BytecodeGenerator::has_syntax_errors() const -> bool
{
    return not syntax_errors_.empty();
}

auto BytecodeGenerator::syntax_errors() const -> const std::vector<Error> &
{
    return syntax_errors_;
}

auto BytecodeGenerator::enter_new_scope() -> void
{
    scope_deque_.push_back(Internal::Scope{});
    curr_scope_ = &scope_deque_.back();

    // Set the new scope's parent if there's more than one scope on the stack
    if (scope_deque_.size() > 1)
        curr_scope_->parent = &scope_deque_[scope_deque_.size() - 2];
}

auto BytecodeGenerator::exit_scope() -> void
{
    scope_deque_.pop_back();

    if (scope_deque_.empty()) 
    {
        curr_scope_ = nullptr;
        return;
    }

    curr_scope_ = &scope_deque_.back();
}

auto BytecodeGenerator::reset_scopes() -> void
{
    scope_deque_.clear();
    curr_scope_ = nullptr;
}

auto BytecodeGenerator::search_symbol(const std::string &symbol_name, Internal::Object *obj = nullptr) -> SymbolSearchRes
{
    using namespace Internal;

    // Search for members or methods in the object
    if (obj) 
    {
        isize_t member_idx = Utils::find_named_idx(symbol_name, obj->member_variables);
        if (member_idx >= 0) 
        {
            MemberVariable *member_var = &obj->member_variables[member_idx];
            return SymbolSearchRes
            {
                .member_var = member_var,
                .idx = static_cast<dword_t>(member_idx),
                .found_type = member_var->type,
                .ty = SymbolSearchRes::Type::MEMBER_VAR,
            };
        }

        isize_t method_idx = Utils::find_named_idx(symbol_name, obj->methods);
        if (method_idx >= 0) 
        {
            Method *method = &obj->methods[member_idx];
            return SymbolSearchRes
            {
                .method = method,
                .idx = static_cast<dword_t>(method_idx),
                .found_type = method->return_type,
                .ty = SymbolSearchRes::Type::METHOD,
            };
        }

        return SymbolSearchRes{};
    }

    // Local variable
    if (curr_scope_) 
    {
        isize_t var_idx = curr_scope_->get_local_var_index(symbol_name);
        if (var_idx >= 0) 
        {
            Variable *var = &curr_scope_->local_variables[var_idx]; 
            return SymbolSearchRes {
                .var = var,
                .idx = static_cast<dword_t>(var_idx),
                .found_type = var->type,
                .ty = SymbolSearchRes::Type::LOCAL_VAR,
            };
        }
    }

    // Look for an object
    isize_t object_idx = Utils::find_named_idx(symbol_name, objects_);
    if (object_idx >= 0) 
    {
        Object* object = &objects_[object_idx];
        return SymbolSearchRes
        {
            .obj = object,
            .idx = static_cast<dword_t>(object_idx),
            .found_type = object->type,
            .ty = SymbolSearchRes::Type::OBJECT,
        };
    }

    // Member variable or function
    if (curr_object_) 
    {        
        isize_t member_idx = Utils::find_named_idx(symbol_name, curr_object_->member_variables);
        if (member_idx >= 0) {
            MemberVariable *member_var = &curr_object_->member_variables[member_idx];
            return SymbolSearchRes
            {
                .member_var = member_var,
                .idx = static_cast<dword_t>(member_idx),
                .found_type = member_var->type,
                .ty = SymbolSearchRes::Type::MEMBER_VAR,
            };
        }

        isize_t method_idx = Utils::find_named_idx(symbol_name, curr_object_->methods);
        if (method_idx >= 0) 
        {
            Method *method = &curr_object_->methods[method_idx];
            return SymbolSearchRes
            {
                .method = method,
                .idx = static_cast<dword_t>(method_idx),
                .found_type = method->return_type,
                .ty = SymbolSearchRes::Type::METHOD,
            };
        } 
    }

    // Global variable or function
    isize_t func_idx = Utils::find_named_idx(symbol_name, functions_);
    if (func_idx >= 0) 
    {
        Function* func = &functions_[func_idx];
        return SymbolSearchRes
        {
            .func = func,
            .idx = static_cast<dword_t>(func_idx),
            .found_type = func->return_type,
            .ty = SymbolSearchRes::Type::FUNCTION,
        };
    }

    isize_t global_var_idx = Utils::find_named_idx(symbol_name, global_vars_);
    if (global_var_idx >= 0) 
    {
        GlobalVariable *global_var = &global_vars_[global_var_idx];
        return SymbolSearchRes
        {
            .global_var = global_var,
            .idx = static_cast<dword_t>(global_var_idx),
            .found_type = global_var->type,
            .ty = SymbolSearchRes::Type::GLOBAL_VAR,
        };
    }
}

auto BytecodeGenerator::handle_declaration_body(const ASTNode &decl_body) -> void
{
    CHECK_NODE_TYPE(decl_body, ASTNodeType::DECLARATION_BODY);

    // Gen data for objects first
    for (const ASTNode &child : decl_body.children())
    {
        if (child.type() == ASTNodeType::OBJ_DECLARATION)
        {

        }
    }

    // Gen data for functions and global variables
    for (const ASTNode &child : decl_body.children())
    {
        if (child.type() == ASTNodeType::FUNCTION_DECLARATION)
        {
            handle_function_declaration(child, true);
        }
        else if (child.type() == ASTNodeType::VARIABLE_DECLARATION)
        {

        }
    }
    
    // Gen bytecode for everything
    for (const ASTNode &child : decl_body.children())
    {
        switch (child.type())
        {
            case ASTNodeType::FUNCTION_DECLARATION:
                handle_function_declaration(child, false);
                break;
            case ASTNodeType::VARIABLE_DECLARATION:
                break;
            case ASTNodeType::OBJ_DECLARATION:
                break;
        }
    }
}

auto BytecodeGenerator::handle_function_declaration(const ASTNode &func_decl, bool quick) -> void
{
    CHECK_NODE_TYPE(func_decl, ASTNodeType::FUNCTION_DECLARATION);

    bool is_method = func_decl.get_metadata<bool>("is_method");
    if (is_method)
    {
        handle_method_declaration(func_decl, quick);
        return;
    }

    // Function was already defined, we should only generate bytecode now
    if (not quick)
    {
        return;
    }

    Internal::Function function{};

    const auto &name_node = func_decl.children()[0];
    function.name = name_node.token().value;

    const auto &param_node = func_decl.children()[1];
    const auto &params = param_node.children();
    for (std::size_t i = 1; i < params.size(); i += 2)
    {
        const auto &type_node = params[i];
    
        ValueType param_type = value_type_from_node(type_node); 
        function.param_types.push_back(param_type);
    }
}

auto BytecodeGenerator::handle_method_declaration(const ASTNode &method_decl, bool quick) -> void
{
    CHECK_NODE_TYPE(method_decl, ASTNodeType::FUNCTION_DECLARATION);
}

auto BytecodeGenerator::value_type_from_node(const ASTNode &type_node) -> ValueType
{
    if (type_node.type() != ASTNodeType::TOKEN and type_node.type() != ASTNodeType::DATA_TYPE)
    {
        error(ERR_INVALID_AST_NODE, Location{}, to_string(ASTNodeType::DATA_TYPE), to_string(type_node.type()));
        return;
    }

    const Token &tok = type_node.token();
    switch (tok.type) 
    {
        case TokenType::INT8_KWD:       return ValueType::INT8;
        case TokenType::INT16_KWD:      return ValueType::INT16;
        case TokenType::INT32_KWD:      return ValueType::INT32;
        case TokenType::INT64_KWD:      return ValueType::INT64;

        case TokenType::UINT8_KWD:      return ValueType::UINT8;
        case TokenType::UINT16_KWD:     return ValueType::UINT16;
        case TokenType::UINT32_KWD:     return ValueType::UINT32;
        case TokenType::UINT64_KWD:     return ValueType::UINT64;
        
        case TokenType::FLOAT32_KWD:    return ValueType::FLOAT32;
        case TokenType::FLOAT64_KWD:    return ValueType::FLOAT64;
        
        case TokenType::BOOL_KWD:       return ValueType::BOOL;

        default:                        break;
    }

    SymbolSearchRes sres = search_symbol(tok.value);
    if (sres.ty == SymbolSearchRes::Type::OBJECT)
        return sres.found_type;

    error(ERR_NOT_A_TYPE, type_node.location(), tok.to_string());

    return ValueType::INVALID;
}


} // namespace NCSC
