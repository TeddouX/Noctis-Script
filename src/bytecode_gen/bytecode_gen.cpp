#include "bytecode_gen/bytecode_gen.hpp"

#include <algorithm>
#include <print>

#include "lexing/lexer.hpp"
#include "parsing/parser.hpp"
#include "utils/vector_utils.hpp"
#include "bytecode_gen/value_type.hpp"

#define CHECK_NODE_TYPE_RET(node, expected, ret)                                                    \
    do {                                                                                            \
        if (node.type() != expected)                                                                \
        {                                                                                           \
            error(ERR_INVALID_AST_NODE, Location{}, to_string(expected), to_string(node.type()));   \
            return ret;                                                                             \
        }                                                                                           \
    } while(0)

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
    , curr_object_{nullptr}
{}

auto BytecodeGenerator::generate(const std::string &script) -> Bytecode
{
    std::shared_ptr<ScriptSource> src = ScriptSource::from_contents(script);
    return generate(src);
}

auto BytecodeGenerator::generate(std::shared_ptr<ScriptSource> src) -> Bytecode
{
    std::vector<Token> tokens = tokenize(src);

    Parser parser{tokens, src};
    ASTNode root = parser.parse();

    if (parser.has_syntax_errors()) 
    {
        syntax_errors_ = parser.get_syntax_errors();   
        return Bytecode{};
    }

    return generate(root, src);
}

auto BytecodeGenerator::generate(const ASTNode &root_node, std::shared_ptr<ScriptSource> src) -> Bytecode
{
    script_source_ = src;
    temp_bytecode_ = Bytecode{script_source_, is_debug_};

    handle_declaration_body(root_node.children()[0]);

    return finalize_bytecode();
}

auto BytecodeGenerator::reset() -> void
{
    objects_.clear();
    functions_.clear();
    global_vars_.clear();

    script_source_ = nullptr;

    scope_deque_.clear();
    curr_scope_ = nullptr;

    generation_errors_.clear();
    syntax_errors_.clear();

    curr_object_ = nullptr;

    label_num_ = 0;
}

auto BytecodeGenerator::has_generation_errors() const -> bool
{
    return not generation_errors_.empty();
}

auto BytecodeGenerator::generation_errors() const -> const std::vector<Error> &
{
    return generation_errors_;
}

auto BytecodeGenerator::has_syntax_errors() const -> bool
{
    return not syntax_errors_.empty();
}

auto BytecodeGenerator::syntax_errors() const -> const std::vector<Error> &
{
    return syntax_errors_;
}

auto BytecodeGenerator::finalize_bytecode() -> Bytecode
{
    Bytecode final_bytecode{};

    for (auto &global_var : global_vars_)
        append_globals_bytecode(global_var, final_bytecode.bytes_);
    
    for (auto &func : functions_)
        append_functions_bytecode(func, final_bytecode.bytes_);

    for (const auto &obj : objects_)
        append_objects_data(obj, final_bytecode.bytes_);

    BytecodeHeader header = make_bytecode_header(final_bytecode);

    return final_bytecode;
}

auto BytecodeGenerator::make_bytecode_header(Bytecode &bytecode) -> BytecodeHeader
{
    BytecodeHeader header{};
    return header;
}

auto BytecodeGenerator::append_globals_bytecode(Internal::GlobalVariable &global_var, std::vector<byte_t> &bytes) -> void
{
    resolve_jumps(global_var.bytecode.bytes_);
}

auto BytecodeGenerator::append_functions_bytecode(Internal::Function &function, std::vector<byte_t> &bytes) -> void
{
    resolve_jumps(function.bytecode.bytes_);
}

auto BytecodeGenerator::append_objects_data(const Internal::Object &object, std::vector<byte_t> &bytes) -> void
{

}

auto BytecodeGenerator::resolve_jumps(std::vector<byte_t> &bytes) -> void
{

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

auto BytecodeGenerator::type_name(GenValueType type) -> std::string
{
    std::string ref_str = gen_vtype_has_mask(type, GenValueType::REF_MASK) ? " ref" : "";
    type = gen_vtype_clear_mask(type, GenValueType::REF_MASK);

    auto builtin_it = BUILTIN_VALUE_TYPES_NAMES.find(type);
    if (builtin_it != BUILTIN_VALUE_TYPES_NAMES.end())
        return builtin_it->second + ref_str;

    auto it = type_names_.find(type);
    if (it != type_names_.end())
        return it->second + ref_str;
    
    return "";
}

auto BytecodeGenerator::emit(const std::vector<byte_t> &bytes, const ASTNode *node) -> void
{
    for (auto byte : bytes)
        emit(byte, node);
}

auto BytecodeGenerator::emit(byte_t byte, const ASTNode *node) -> void
{
    auto &bytes = temp_bytecode_.bytes_;

    if (is_debug_ && node)
    {
        temp_bytecode_.location_entries_.push_back(
            { temp_bytecode_.bytes_.size(), node->location() }
        );
    }

    temp_bytecode_.bytes_.push_back(byte);
}

auto BytecodeGenerator::emit(word_t word, const ASTNode *node) -> void
{
    std::vector<byte_t> bytes {
        static_cast<byte_t>(word & 0xFF),
        static_cast<byte_t>((word >> 8) & 0xFF),
    };
    emit(bytes, node);
}

auto BytecodeGenerator::emit(dword_t dword, const ASTNode *node) -> void
{
    std::vector<byte_t> bytes {
        static_cast<byte_t>(dword & 0xFF),
        static_cast<byte_t>((dword >> 8) & 0xFF),
        static_cast<byte_t>((dword >> 16) & 0xFF),
        static_cast<byte_t>((dword >> 24) & 0xFF),
    };
    emit(bytes, node);
}

auto BytecodeGenerator::emit(qword_t qword, const ASTNode *node) -> void
{
    std::vector<byte_t> bytes {
        static_cast<byte_t>(qword & 0xFF),
        static_cast<byte_t>((qword >> 8) & 0xFF),
        static_cast<byte_t>((qword >> 16) & 0xFF),
        static_cast<byte_t>((qword >> 24) & 0xFF),
        static_cast<byte_t>((qword >> 32) & 0xFF),
        static_cast<byte_t>((qword >> 40) & 0xFF),
        static_cast<byte_t>((qword >> 48) & 0xFF),
        static_cast<byte_t>((qword >> 56) & 0xFF),
    };
    emit(bytes, node);
}

auto BytecodeGenerator::emit(VMInstruction instr, const ASTNode *node) -> void
{
    emit(static_cast<vm_instruction_size_t>(instr), node);
}

auto BytecodeGenerator::can_promote_gen_vtype(const GenValueType &from, const GenValueType &to) -> bool
{
    if (from == to)
        return true;

    int from_rank = gen_vtype_get_rank(from);
    int to_rank = gen_vtype_get_rank(to);

    if (from_rank <= 0 or to_rank <= 0)
        return false;
        
    // Any type can convert to void
    if (gen_vtype_remove_const_ref(to) == GenValueType::VOID)
        return true;

    return from_rank <= to_rank;
}

auto BytecodeGenerator::promote_gen_vtype(GenValueType from, GenValueType to) -> GenValueType
{
    // Both are the same type
    if (from == to)
        return to;

    // Floats rank above everything
    if (gen_vtype_is_float(from) || gen_vtype_is_float(to)) 
    {
        if (to == GenValueType::FLOAT64)
            return GenValueType::FLOAT64;
        // This should never happen
        else if (from == GenValueType::FLOAT64 and to == GenValueType::FLOAT32)
            return GenValueType::ERROR_TYPE;
        
        return GenValueType::FLOAT32;
    }

    int from_rank = gen_vtype_get_rank(from);
    int to_rank   = gen_vtype_get_rank(to);

    GenValueType higher = (from_rank > to_rank) ? from : to;

    // If same rank but one unsigned -> unsigned version
    if (from_rank == to_rank) 
    {
        if (gen_vtype_is_unsigned_int(from)) 
            return from;
        else if (gen_vtype_is_unsigned_int(to)) 
            return to;
    }

    return higher;  
}

auto BytecodeGenerator::gen_vtype_as_object(GenValueType ty) -> Internal::Object *
{
    if (not gen_vtype_is_object(ty))
        return nullptr;

    GenValueType idx_vtype = gen_vtype_clear_mask(ty, GenValueType::OBJ_MASK);
    value_type_size_t idx = static_cast<value_type_size_t>(idx_vtype);

    if (idx > objects_.size())
        return nullptr;

    return &objects_[idx];
}

auto BytecodeGenerator::search_symbol(const std::string &symbol_name, Internal::Object *obj) -> SymbolSearchRes
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
                .has_found = true,
                .member_var = member_var,
                .found_idx = static_cast<dword_t>(member_idx),
                .found_gen_vtype = member_var->type,
                .found_location = member_var->defined_at,
                .type = SymbolSearchRes::Type::MEMBER_VAR,
            };
        }

        auto member_it = obj->method_offsets.find(symbol_name);
        if (member_it != obj->method_offsets.end())
        {
            std::size_t off = member_it->second;
            Function *func = &functions_[off];
            // Sanity check
            if (func->name == symbol_name)
            {
                return SymbolSearchRes
                {
                    .has_found = true,
                    .func = func,
                    .found_idx = static_cast<dword_t>(off),
                    .found_gen_vtype = func->return_type,
                    .found_location = func->defined_at,
                    .type = SymbolSearchRes::Type::METHOD,
                };
            }
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
                .has_found = true,
                .var = var,
                .found_idx = static_cast<dword_t>(var_idx),
                .found_gen_vtype = var->type,
                .found_location = var->defined_at,
                .type = SymbolSearchRes::Type::LOCAL_VAR,
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
            .has_found = true,
            .obj = object,
            .found_idx = static_cast<dword_t>(object_idx),
            .found_gen_vtype = object->type,
            .found_location = object->defined_at,
            .type = SymbolSearchRes::Type::OBJECT,
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
                .has_found = true,
                .member_var = member_var,
                .found_idx = static_cast<dword_t>(member_idx),
                .found_gen_vtype = member_var->type,
                .found_location = member_var->defined_at,
                .type = SymbolSearchRes::Type::MEMBER_VAR,
            };
        }

        auto member_it = curr_object_->method_offsets.find(symbol_name);
        if (member_it != curr_object_->method_offsets.end())
        {
            std::size_t off = member_it->second;
            Function *func = &functions_[off];
            if (func->name == symbol_name)
            {
                return SymbolSearchRes
                {
                    .has_found = true,
                    .func = func,
                    .found_idx = static_cast<dword_t>(off),
                    .found_gen_vtype = func->return_type,
                    .found_location = func->defined_at,
                    .type = SymbolSearchRes::Type::METHOD,
                };
            }
        }
    }

    // Global variable or function
    isize_t func_idx = Utils::find_named_idx(symbol_name, functions_);
    if (func_idx >= 0) 
    {
        Function* func = &functions_[func_idx];
        return SymbolSearchRes
        {
            .has_found = true,
            .func = func,
            .found_idx = static_cast<dword_t>(func_idx),
            .found_gen_vtype = func->return_type,
            .found_location = func->defined_at,
            .type = SymbolSearchRes::Type::FUNCTION,
        };
    }

    isize_t global_var_idx = Utils::find_named_idx(symbol_name, global_vars_);
    if (global_var_idx >= 0) 
    {
        GlobalVariable *global_var = &global_vars_[global_var_idx];
        return SymbolSearchRes
        {
            .has_found = true,
            .global_var = global_var,
            .found_idx = static_cast<dword_t>(global_var_idx),
            .found_gen_vtype = global_var->type,
            .found_location = global_var->defined_at,
            .type = SymbolSearchRes::Type::GLOBAL_VAR,
        };
    }

    return SymbolSearchRes{};
}

auto BytecodeGenerator::is_symbol_defined_elsewhere(const ASTNode &identifer) -> bool
{
    const std::string &name = identifer.token().value;
    SymbolSearchRes sres = search_symbol(name);
    if (sres.has_found)
    {
        error(ERR_ALREADY_DEFINED, identifer.location(), name);
        error(INFO_DEFINED_HERE, sres.found_location, name);

        return true;
    }

    return false;
}

auto BytecodeGenerator::value_type_from_node(const ASTNode &type_node) -> GenValueType
{
    if (type_node.type() != ASTNodeType::TOKEN and type_node.type() != ASTNodeType::DATA_TYPE)
    {
        error(ERR_INVALID_AST_NODE, Location{}, to_string(ASTNodeType::DATA_TYPE), to_string(type_node.type()));
        return GenValueType::ERROR_TYPE;
    }

    const Token &tok = type_node.token();
    switch (tok.type) 
    {
        case TokenType::VOID_KWD:       return GenValueType::VOID;

        case TokenType::INT8_KWD:       return GenValueType::INT8;
        case TokenType::INT16_KWD:      return GenValueType::INT16;
        case TokenType::INT32_KWD:      return GenValueType::INT32;
        case TokenType::INT64_KWD:      return GenValueType::INT64;

        case TokenType::UINT8_KWD:      return GenValueType::UINT8;
        case TokenType::UINT16_KWD:     return GenValueType::UINT16;
        case TokenType::UINT32_KWD:     return GenValueType::UINT32;
        case TokenType::UINT64_KWD:     return GenValueType::UINT64;
        
        case TokenType::FLOAT32_KWD:    return GenValueType::FLOAT32;
        case TokenType::FLOAT64_KWD:    return GenValueType::FLOAT64;
        
        case TokenType::BOOL_KWD:       return GenValueType::BOOL;

        default:                        break;
    }

    SymbolSearchRes sres = search_symbol(tok.value);
    if (sres.type == SymbolSearchRes::Type::OBJECT)
        return sres.found_gen_vtype;

    error(ERR_NOT_A_TYPE, type_node.location(), tok.to_string());

    return GenValueType::ERROR_TYPE;
}

auto BytecodeGenerator::access_mod_from_token(const Token &tok) -> Internal::AccessModifier
{
    if (tok.value == "public")
        return Internal::AccessModifier::PUBLIC;
    else
        return Internal::AccessModifier::PRIVATE;
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
        // Global variable declaration
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

    const auto &name_node = func_decl.children()[0];
    const std::string &function_name = name_node.token().value;

    // Generate only the function's data
    if (quick)
    {
        if (is_symbol_defined_elsewhere(name_node))
            return;

        Internal::Function function{};
        function.name = function_name;
        function.defined_at = name_node.location();

        const auto &param_node = func_decl.children()[1];
        const auto &params = param_node.children();
        for (std::size_t i = 0; i < params.size(); i += 2)
        {
            const auto &name_node = params[i];
            const auto &type_node = params[i + 1];
        
            const std::string &param_name = name_node.token().value;
            GenValueType param_type = value_type_from_node(type_node); 

            Internal::Variable param {
                .name = param_name,
                .type = param_type,
            };
            function.params.push_back(param);
        }

        const auto &return_node = func_decl.children()[2];
        GenValueType return_type = value_type_from_node(return_node);
        function.return_type = return_type;

        functions_.push_back(function);

        return;
    }

    const auto &func_stmt_block = func_decl.children()[3];
    enter_new_scope();
    handle_statement_block(func_stmt_block);
    exit_scope();

    SymbolSearchRes sres = search_symbol(function_name);
    // Sanity check
    if (not sres.has_found or sres.type != SymbolSearchRes::Type::FUNCTION)
        return;

    std::println("Function {}", function_name);
    for (auto byte : temp_bytecode_.bytes_)
        std::print("{:03} ", byte);
    std::print("\n");

    sres.func->bytecode = temp_bytecode_;
    temp_bytecode_ = Bytecode{};
}

auto BytecodeGenerator::handle_method_declaration(const ASTNode &method_decl, bool quick) -> void
{
    CHECK_NODE_TYPE(method_decl, ASTNodeType::FUNCTION_DECLARATION);
}

auto BytecodeGenerator::handle_statement_block(const ASTNode &stmt_block) -> void
{
    CHECK_NODE_TYPE(stmt_block, ASTNodeType::STATEMENT_BLOCK);

    for (const auto &statement : stmt_block.children())
    {
        switch (statement.type())
        {
            case ASTNodeType::IF_STATEMENT:
                handle_if_statement(statement);
                break;
            case ASTNodeType::RETURN_STATEMENT:
                handle_return_statement(statement);
                break;
            case ASTNodeType::VARIABLE_DECLARATION:
                handle_variable_declaration(statement);
                break;
            case ASTNodeType::ASSIGNMENT:
                handle_assignment(statement);
                break;
        }
    }
}

auto BytecodeGenerator::handle_if_statement(const ASTNode &if_stmt) -> void
{
    CHECK_NODE_TYPE(if_stmt, ASTNodeType::IF_STATEMENT);
}

auto BytecodeGenerator::handle_return_statement(const ASTNode &return_stmt) -> void
{
    CHECK_NODE_TYPE(return_stmt, ASTNodeType::RETURN_STATEMENT);
}

auto BytecodeGenerator::handle_variable_declaration(const ASTNode &var_decl) -> void
{
    CHECK_NODE_TYPE(var_decl, ASTNodeType::VARIABLE_DECLARATION);

    // Type erased variable
    std::unique_ptr<Internal::Variable> var;
    std::size_t child_idx = 0;

    bool is_member_var = var_decl.get_metadata<bool>("is_member_var");
    if (is_member_var)
    {
        const auto &access_mod = var_decl.children()[child_idx];
        child_idx++;

        auto member_var = std::make_unique<Internal::MemberVariable>();
        member_var->access_mod = access_mod_from_token(access_mod.token());
    }
    else
    {
        var = std::make_unique<Internal::Variable>();
    }

    const auto &name_node = var_decl.children()[child_idx++];
    if (is_symbol_defined_elsewhere(name_node))
        return;
    
    var->defined_at = name_node.location();

    const std::string &variable_name = name_node.token().value;

    var->name = variable_name;

    const auto &type_node = var_decl.children()[child_idx++];
    GenValueType type = value_type_from_node(type_node);
    var->type = type;

    // Add the member variable to the current object
    if (is_member_var and curr_object_)
    {
        auto member_var = static_cast<Internal::MemberVariable *>(var.get()); 
        curr_object_->member_variables.push_back(*member_var);
    }
    else if (curr_scope_)
    {
        curr_scope_->local_variables.push_back(*var);
    }

    const auto &expression_node = var_decl.children()[child_idx++];
    GenValueType expr_type = handle_expression(expression_node, type, false);

    if (expr_type == GenValueType::ERROR_TYPE)
        return;

    handle_store(name_node);
}

auto BytecodeGenerator::handle_assignment(const ASTNode &assigment) -> void
{
    CHECK_NODE_TYPE(assigment, ASTNodeType::ASSIGNMENT);

    // Simple statement
    const auto &first_child = assigment.children()[0];
    if (assigment.children().size() == 1)
    {
        handle_expression_term(first_child, GenValueType::ANY, false, false);
        return;
    }
}

auto BytecodeGenerator::handle_expression(const ASTNode &expr, const GenValueType &expected_ty, bool should_be_assignable) -> GenValueType
{
    CHECK_NODE_TYPE_RET(expr, ASTNodeType::EXPRESSION, GenValueType::ERROR_TYPE);

    if (expr.children().empty())
        return GenValueType::ERROR_TYPE;
    
    const auto &first_child = expr.children()[0];
    if (first_child.type() == ASTNodeType::EXPRESSION_TERM)
        return handle_expression_term(first_child, expected_ty, should_be_assignable, true);

    // Handle math expressions
    return recursively_handle_expression_child(first_child, expected_ty);
}

auto BytecodeGenerator::recursively_handle_expression_child(const ASTNode &expr_child, const GenValueType &expected_ty) -> GenValueType
{
    ASTNodeType expr_child_type = expr_child.type();
    if (expr_child_type == ASTNodeType::EXPRESSION_TERM)
        return handle_expression_term(expr_child, expected_ty, false, true);
    else if (expr_child_type == ASTNodeType::BINOP)
        return handle_binop(expr_child, expected_ty);
    else
        return GenValueType::ERROR_TYPE;
}

auto BytecodeGenerator::handle_expression_term(
    const ASTNode &expr_term, 
    const GenValueType &expected_ty, 
    bool should_be_assignable, 
    bool should_leave_val_on_stack) -> GenValueType
{
    CHECK_NODE_TYPE_RET(expr_term, ASTNodeType::EXPRESSION_TERM, GenValueType::ERROR_TYPE);

    // Only the value
    if (expr_term.children().size() == 1)
    {
        const ASTNode &expr_value = expr_term.children()[0];
        return handle_expression_value(expr_value, expected_ty, should_be_assignable);
    }

    isize_t expr_value_idx = -1;
    for (isize_t i = 0; i < expr_term.children().size(); i++) {
        if (expr_term.children()[i].type() == ASTNodeType::EXPRESSION_VALUE) {
            expr_value_idx = i;
            break;
        }
    }

    if (expr_value_idx < 0)
        return GenValueType::ERROR_TYPE;

    const ASTNode &expr_value = expr_term.children()[expr_value_idx];

    const ASTNode *last_node_on_stack = &expr_value;

    bool val_on_stack_lvalue = true;
    bool has_val_on_stack = false;

    GenValueType last_type_on_stack = GenValueType::ERROR_TYPE;

    // If no value is on the stack, handle the expression value
    // If the last value on the stack isn't an lvalue, return false
    // If the last value type on the stack isn't numeric, return false
    auto inc_dec_checks = [&]() -> bool
    {
        if (not has_val_on_stack)
        {
            // Value type of any so we can check the type ourselfes
            last_type_on_stack = handle_expression_value(expr_value, GenValueType::ANY, true);
            if (last_type_on_stack == GenValueType::ERROR_TYPE)
                return false;
        }

        if (not val_on_stack_lvalue)
        {
            error(ERR_NOT_ASSIGNABLE, last_node_on_stack->location());
            return false;
        }

        if (not gen_vtype_is_numeric(last_type_on_stack))
        {
            error(ERR_EXPECTED_NUMERIC_TY, 
                last_node_on_stack->location(), 
                type_name(last_type_on_stack)
            );
            return false;
        }

        return true;
    };

    for (std::size_t i = expr_value_idx + 1; i < expr_term.children().size(); i++)
    {
        const auto &postop = expr_term.children()[i];
        ASTNodeType postop_type = postop.type();

        if (postop_type != ASTNodeType::EXPRESSION_POSTOP)
            break;

        switch (postop_type)
        {
            // Method call
            case ASTNodeType::FUNCTION_CALL:
                continue;
            // Member access
            case ASTNodeType::IDENTIFIER:
                continue;
        }

        TokenType postop_tok_ty = postop.children()[0].token().type;
        if (postop_tok_ty == TokenType::PLUS_PLUS or postop_tok_ty == TokenType::MINUS_MINUS)
        {
            if (not inc_dec_checks())
                return GenValueType::ERROR_TYPE;
            
            bool is_inc = postop_tok_ty == TokenType::PLUS_PLUS;
            // One value gets incremented and stored into the variable
            // and the other stays on the stack
            if (should_leave_val_on_stack)
                emit(VMInstruction::DUP, &postop);

            if (is_inc) 
                emit(VMInstruction::INC, &postop);
            else
                emit(VMInstruction::DEC, &postop);
            
            handle_store(expr_term);

            val_on_stack_lvalue = false;
        }
    }

    // PRE-OPERATORS
    bool has_not_preop = false;
    for (std::size_t i = 0; i < expr_value_idx; i++)
    {
        const auto &preop = expr_term.children()[i];
        if (preop.type() != ASTNodeType::EXPRESSION_PREOP)
            break;

        TokenType preop_type = preop.token().type;

        if (preop_type == TokenType::PLUS_PLUS or preop_type == TokenType::MINUS_MINUS)
        {
            if (not inc_dec_checks())
                return GenValueType::ERROR_TYPE;

            bool is_inc = preop_type == TokenType::PLUS_PLUS;

            if (is_inc) 
                emit(VMInstruction::INC, &preop);
            else
                emit(VMInstruction::DEC, &preop);

            if (should_leave_val_on_stack)
                emit(VMInstruction::DUP, &preop);

            // Store the value back in the variable
            handle_store(expr_term);

            val_on_stack_lvalue = false;
        }
        else if (preop_type == TokenType::NOT)
        {
            if (last_type_on_stack != GenValueType::BOOL)
            {
                error(ERR_EXPECTED_TY, 
                    expr_value.location(), 
                    type_name(GenValueType::BOOL), 
                    type_name(last_type_on_stack)
                );
                return GenValueType::ERROR_TYPE;
            }

            if (has_not_preop)
            {
                emit(VMInstruction::NOT, &preop);
                continue;
            }

            val_on_stack_lvalue = false;
            has_not_preop = true;
        }
    }

    if (not val_on_stack_lvalue and should_be_assignable)
    {
        error(ERR_NOT_ASSIGNABLE, expr_term.location());
        return GenValueType::ERROR_TYPE;
    }

    return last_type_on_stack;
}

auto BytecodeGenerator::handle_binop(const ASTNode &binop, const GenValueType &expected_ty) -> GenValueType
{
    CHECK_NODE_TYPE_RET(binop, ASTNodeType::BINOP, GenValueType::ERROR_TYPE);

    const Token &binop_tok = binop.token();
    TokenType binop_tok_ty = binop.token().type;
    const auto &left_operand = binop.children()[0];
    const auto &right_operand = binop.children()[1];

    if (binop_tok_ty == TokenType::LOGICAL_AND or binop_tok_ty == TokenType::LOGICAL_OR)
    {
        if (expected_ty != GenValueType::BOOL and expected_ty != GenValueType::ANY)
        {
            error(ERR_EXPECTED_TY,
                binop.location(),
                type_name(expected_ty), 
                type_name(GenValueType::BOOL)
            );
            return GenValueType::ERROR_TYPE;
        }

        /*
        
        When executing and, we push false at the first operand that is false, 
        else if all of them are true, we push true.
        When executing or, we push true at the first operand that is true,
        else if all of them are false, we push false.

        That means that certain operands may not get executed.

        For example:
            - 'false && true' compiles to:

                PUSH false
                // in this example the jump would succeed and the right operand wouldn't be executed
                JMPFALSE operand_jump_label_num
                PUSH true
                JMPFALSE operand_jump_label_num

                PUSH true
                JMP binop_end_label_num

                LABEL operand_jump_label_num
                PUSH false

                LABEL binop_end_label_num

            - 'true || false' compiles to:

                PUSH true
                // in this example the jump would succeed and the right operand wouldn't be executed
                JMPTRUE operand_jump_label_num
                PUSH false
                JMPTRUE operand_jump_label_num

                PUSH false
                JMP binop_end_label_num

                LABEL operand_jump_label_num
                PUSH true

                LABEL binop_end_label_num

        */

        bool is_and = binop_tok_ty == TokenType::LOGICAL_AND;

        recursively_handle_expression_child(left_operand, GenValueType::BOOL);

        if (is_and)
            // If the left operand was false, skip over everything else
            emit(VMInstruction::JMPFALSE, &binop);
        else
            // If the left operand was true, skip over everything else
            emit(VMInstruction::JMPTRUE, &binop);

        usize_t operand_jump_label_num = label_num_++;
        emit(operand_jump_label_num, &binop);

        recursively_handle_expression_child(right_operand, GenValueType::BOOL);

        if (is_and) emit(VMInstruction::JMPFALSE, &binop);
        else        emit(VMInstruction::JMPTRUE, &binop);
    
        emit(operand_jump_label_num, &binop);
    
        emit(VMInstruction::PUSH, &binop);
        if (is_and)
            // If we didn't jump over this, that means and is true
            emit_constant(true, &binop);
        else
            // If we didn't jump over this, that means or is false
            emit_constant(false, &binop);

        usize_t binop_end_label_num = label_num_++;
        emit(VMInstruction::JMP, &binop);
        emit(binop_end_label_num, &binop);

        emit(VMInstruction::LABEL, &binop);
        emit(operand_jump_label_num, &binop);

        emit(VMInstruction::PUSH, &binop);
        if (is_and)
            emit_constant(false, &binop);
        else
            emit_constant(true, &binop);

        emit(VMInstruction::LABEL, &binop);
        emit(binop_end_label_num, &binop);
    
        return GenValueType::ERROR_TYPE;
    }
    else
    {
        if (binop_tok.is_comparison_operator() and expected_ty != GenValueType::BOOL and expected_ty != GenValueType::ANY)
        {
            error(ERR_EXPECTED_TY,
                binop.location(),
                type_name(expected_ty), 
                type_name(GenValueType::BOOL)
            );
                
            return GenValueType::ERROR_TYPE;
        }

        GenValueType type_right = recursively_handle_expression_child(right_operand, expected_ty);
        GenValueType type_left = recursively_handle_expression_child(left_operand, expected_ty);

        if (not gen_vtype_is_numeric(type_left))
        {
            error(ERR_EXPECTED_NUMERIC_TY, left_operand.location(), type_name(type_left));
            return GenValueType::ERROR_TYPE;
        }

        if (not gen_vtype_is_numeric(type_right))
        {
            error(ERR_EXPECTED_NUMERIC_TY, right_operand.location(), type_name(type_right));
            return GenValueType::ERROR_TYPE;
        }

        if (not can_promote_gen_vtype(type_left, type_right))
        {
            error(ERR_CANT_PROMOTE_TY, 
                right_operand.location(), 
                type_name(type_left), 
                type_name(type_right)
            );
            return GenValueType::ERROR_TYPE;
        }

        GenValueType op_type = promote_gen_vtype(type_left, type_right);

        switch (binop_tok_ty)
        {
            case TokenType::PLUS:             emit(VMInstruction::ADD, &binop);   return op_type;
            case TokenType::MINUS:            emit(VMInstruction::SUB, &binop);   return op_type;
            case TokenType::STAR:             emit(VMInstruction::MUL, &binop);   return op_type;
            case TokenType::SLASH: 
            {
                if (not can_promote_gen_vtype(GenValueType::FLOAT64, expected_ty)) 
                {
                    error(ERR_DIV_RETURNS_F64, binop.location(), type_name(expected_ty));
                    return GenValueType::ERROR_TYPE;
                }

                emit(VMInstruction::DIV, &binop);
                return GenValueType::FLOAT64;
            }

            case TokenType::LESS_THAN:              emit(VMInstruction::CMPST, &binop); return op_type;
            case TokenType::LESS_THAN_EQUAL:        emit(VMInstruction::CMPSE, &binop); return op_type;
            case TokenType::GREATER_THAN:           emit(VMInstruction::CMPGT, &binop); return op_type;
            case TokenType::GREATER_THAN_EQUAL:     emit(VMInstruction::CMPGE, &binop); return op_type;
            case TokenType::EQUAL_EQUAL:            emit(VMInstruction::CMPEQ, &binop); return op_type;
            case TokenType::NOT_EQUAL:              emit(VMInstruction::CMPNE, &binop); return op_type;

            default: emit(VMInstruction::NOOP, &binop);  return GenValueType::ERROR_TYPE;
        }
    }
}

auto BytecodeGenerator::handle_expression_value(const ASTNode &expr_value, const GenValueType &expected_ty, bool should_be_assignable) -> GenValueType
{
    CHECK_NODE_TYPE_RET(expr_value, ASTNodeType::EXPRESSION_VALUE, GenValueType::ERROR_TYPE);

    const auto &first_child = expr_value.children()[0];
    ASTNodeType first_child_ty = first_child.type();

    bool is_constant = first_child_ty == ASTNodeType::CONSTANT;
    bool is_func_call = first_child_ty == ASTNodeType::FUNCTION_CALL;

    if ((is_func_call or is_constant) and should_be_assignable) 
    {
        error(ERR_NOT_ASSIGNABLE, first_child.location());
        return GenValueType::ERROR_TYPE;
    }

    switch (first_child_ty)
    {
        case ASTNodeType::CONSTANT:
            return handle_constant(first_child, expected_ty);
        case ASTNodeType::FUNCTION_CALL:
            return handle_function_call(first_child, expected_ty);
        case ASTNodeType::IDENTIFIER:
            return handle_variable_access(first_child, expected_ty);
        case ASTNodeType::CONSTRUCT_CALL:
            return GenValueType::ERROR_TYPE;
        case ASTNodeType::EXPRESSION:
            return handle_expression(first_child, expected_ty, should_be_assignable);
        default:
            return GenValueType::ERROR_TYPE;
    }
}

auto BytecodeGenerator::handle_store(const ASTNode &expr) -> void
{
    auto handle_var_store = [&](const std::string &var_name, bool is_last_child, const ASTNode &node) -> GenValueType
    {
        SymbolSearchRes sres = search_symbol(var_name);
        if (sres.type != SymbolSearchRes::Type::GLOBAL_VAR 
            and sres.type != SymbolSearchRes::Type::LOCAL_VAR 
            and sres.type != SymbolSearchRes::Type::MEMBER_VAR
        ) {
            error(ERR_VAR_NOT_FOUND, node.location(), var_name);
            return GenValueType::ERROR_TYPE;
        }
        
        if (sres.type == SymbolSearchRes::Type::GLOBAL_VAR) 
        {
            // Only store into the variable if it's the last child
            emit(is_last_child ? VMInstruction::STOREGLOBAL : VMInstruction::LOADGLOBAL, &node);
        }
        else if (sres.type == SymbolSearchRes::Type::LOCAL_VAR) 
        {
            emit(is_last_child ? VMInstruction::STORELOCAL : VMInstruction::LOADLOCAL, &node);
        }
        // Accessing a member without the 'this' keyword
        else if (sres.type == SymbolSearchRes::Type::MEMBER_VAR) 
        {
            emit(VMInstruction::LOADLOCAL, &node);
            emit((dword_t)0, &node); // idx 0 is 'this'

            emit(is_last_child ? VMInstruction::STOREMEMBER : VMInstruction::LOADMEMBER, &node);
        }

        emit(sres.found_idx, &node);

        return sres.found_gen_vtype;
    };

    if (expr.type() == ASTNodeType::IDENTIFIER)
    {
        const std::string &var_name = expr.token().value;
        handle_var_store(var_name, true, expr);
        
        return;
    }

    GenValueType last_type = GenValueType::ERROR_TYPE;

    const ASTNode *expr_value = nullptr;
    std::size_t num_children = expr.children().size();
    for (std::size_t i = 0; i < num_children; i++)
    {
        const auto &expr_child = expr.children()[i];
        ASTNodeType expr_child_type = expr_child.type();

        // Skip over preops
        if (expr_child_type == ASTNodeType::EXPRESSION_PREOP)
            continue;

        bool is_last_child = i + 1 >= num_children
            // Next child is an inc or a dec
            || expr.children()[i + 1].children()[0].type() == ASTNodeType::TOKEN;

        if (expr_child_type == ASTNodeType::EXPRESSION_VALUE)
        {
            const auto &child = expr_child.children()[0];
            // Are we storing into a variable
            if (child.type() != ASTNodeType::IDENTIFIER)
                continue;

            const std::string &var_name = child.token().value;

            last_type = handle_var_store(var_name, is_last_child, child);
            if (last_type == GenValueType::ERROR_TYPE)
                return;

            expr_value = &expr_child;
        }
        else if (expr_child_type == ASTNodeType::EXPRESSION_POSTOP)
        {
            const ASTNode *prev_postop = nullptr;
            for (const auto &postop : expr_child.children())
            {
                // Method call
                if (postop.type() == ASTNodeType::FUNCTION_CALL)
                {
                    if (is_last_child)
                    {
                        error(ERR_NOT_ASSIGNABLE, postop.location());
                        return;
                    }

                    Internal::Object *obj = gen_vtype_as_object(last_type);
                    if (not obj)
                    {
                        const ASTNode *err_node = prev_postop != nullptr ? prev_postop : expr_value;
                        error(ERR_EXPECTED_OBJECT, err_node->location(), type_name(last_type));
                        return; 
                    }

                    const std::string &method_name = postop.children()[0].token().value;
                    SymbolSearchRes sres = search_symbol(method_name, obj);
                    if (sres.type != SymbolSearchRes::Type::METHOD)
                    {
                        error(ERR_METHOD_NOT_FOUND, postop.location(), method_name, obj->name);
                        return;
                    }

                    emit(VMInstruction::CALLFUNC, &postop);
                    emit(sres.found_idx, &postop);
                }
                // Member access
                else if (postop.type() == ASTNodeType::IDENTIFIER)
                {
                    Internal::Object *obj = gen_vtype_as_object(last_type);
                    if (not obj)
                    {
                        const ASTNode *err_node = prev_postop != nullptr ? prev_postop : expr_value;
                        error(ERR_EXPECTED_OBJECT, err_node->location(), type_name(last_type));
                        return; 
                    }

                    const std::string &member_name = postop.children()[0].token().value;
                    SymbolSearchRes sres = search_symbol(member_name, obj);
                    if (sres.type != SymbolSearchRes::Type::MEMBER_VAR)
                    {
                        error(ERR_MEMBER_NOT_FOUND, postop.location(), member_name, obj->name);
                        return;
                    }

                    if (is_last_child)
                        emit(VMInstruction::STOREMEMBER, &postop);
                    else
                        emit(VMInstruction::LOADMEMBER, &postop);
                    emit(sres.found_idx, &postop);

                    last_type = sres.found_gen_vtype;
                }
            }
        }
    }
}

auto BytecodeGenerator::handle_constant(const ASTNode &constant, GenValueType expected_ty) -> GenValueType
{
    CHECK_NODE_TYPE_RET(constant, ASTNodeType::CONSTANT, GenValueType::ERROR_TYPE);

    const Token &const_tok = constant.token();
    TokenType const_tok_ty = const_tok.type;
    const std::string &value = const_tok.value;

    emit(VMInstruction::PUSH, &constant);
    if (const_tok_ty == TokenType::FLOAT_CONSTANT)
    {
        switch (expected_ty)
        {
            case GenValueType::FLOAT32:
            {
                float32_t val = std::stof(value);
                std::uint32_t val_bits = std::bit_cast<std::uint32_t>(val);

                emit_constant(val_bits, &constant);

                return expected_ty;
            }

            case GenValueType::ANY:
            case GenValueType::FLOAT64:
            {
                float64_t val = std::stod(value);
                std::uint64_t val_bits = std::bit_cast<std::uint64_t>(val);

                emit_constant(val_bits, &constant);

                return GenValueType::FLOAT64;
            }

            default:
                error(ERR_EXPECTED_TY, 
                    constant.location(), 
                    type_name(expected_ty), 
                    type_name(GenValueType::FLOAT64)
                );
                return GenValueType::ERROR_TYPE;
        }
    }
    else if (const_tok_ty == TokenType::INT_CONSTANT)
    {
        switch (expected_ty)
        {
            case GenValueType::INT8:    emit_int_constant_from_str<std::int8_t>(value,   &constant); return expected_ty;
            case GenValueType::INT16:   emit_int_constant_from_str<std::int16_t>(value,  &constant); return expected_ty;
            
            case GenValueType::ANY:
            case GenValueType::INT32:   emit_int_constant_from_str<std::int32_t>(value,  &constant); return GenValueType::INT32;

            case GenValueType::INT64:   emit_int_constant_from_str<std::int64_t>(value,  &constant); return expected_ty;

            case GenValueType::UINT8:   emit_int_constant_from_str<std::uint8_t>(value,  &constant); return expected_ty;
            case GenValueType::UINT16:  emit_int_constant_from_str<std::uint16_t>(value, &constant); return expected_ty;
            case GenValueType::UINT32:  emit_int_constant_from_str<std::uint32_t>(value, &constant); return expected_ty;
            case GenValueType::UINT64:  emit_int_constant_from_str<std::uint64_t>(value, &constant); return expected_ty;

            // If a float is expected for the expression, we can safely emit an int
            // and the VM will be able to use it as a float
            case GenValueType::FLOAT32: emit_int_constant_from_str<std::int32_t>(value,  &constant); return expected_ty;
            case GenValueType::FLOAT64: emit_int_constant_from_str<std::int64_t>(value,  &constant); return expected_ty;

            default:
                error(ERR_EXPECTED_TY, 
                    constant.location(), 
                    type_name(expected_ty),
                    type_name(GenValueType::INT32)
                );
                return GenValueType::ERROR_TYPE;
        }
    }
    else if (const_tok_ty == TokenType::TRUE_KWD or const_tok_ty == TokenType::FALSE_KWD)
    {
        if (expected_ty != GenValueType::BOOL and expected_ty != GenValueType::ANY)
        {
            error(ERR_EXPECTED_TY, 
                constant.location(), 
                type_name(expected_ty),
                type_name(GenValueType::BOOL)
            );
            return GenValueType::ERROR_TYPE;
        }

        if (const_tok_ty == TokenType::TRUE_KWD)
            emit_constant(true, &constant);
        else
            emit_constant(false, &constant);

        return GenValueType::BOOL;
    }
    else if (const_tok_ty == TokenType::NULL_KWD)
    {
        if (not gen_vtype_has_mask(expected_ty, GenValueType::OBJ_MASK) and expected_ty != GenValueType::ANY)
        {
            error(ERR_EXPECTED_TY, 
                constant.location(), 
                type_name(expected_ty),
                "null"
            );
            return GenValueType::ERROR_TYPE;
        }

        emit_constant(nullptr, &constant);

        return GenValueType::NULL_OBJ;
    }

    return GenValueType::ERROR_TYPE;
}

auto BytecodeGenerator::handle_function_call(const ASTNode &func_call, GenValueType expected_ty) -> GenValueType
{
    CHECK_NODE_TYPE_RET(func_call, ASTNodeType::FUNCTION_CALL, GenValueType::ERROR_TYPE);

    const auto &name_node = func_call.children()[0];
    const std::string &function_name = name_node.token().value;

    SymbolSearchRes sres = search_symbol(function_name);
    if (not sres.has_found)
    {
        error(ERR_FUNC_NOT_FOUND, name_node.location(), function_name);
        return GenValueType::ERROR_TYPE;
    }
    else if (sres.type != SymbolSearchRes::Type::FUNCTION)
    {
        error(ERR_NOT_A_FUNC, name_node.location(), function_name);
        return GenValueType::ERROR_TYPE;
    }

    GenValueType func_ret_ty = sres.found_gen_vtype;
    if (func_ret_ty == GenValueType::VOID and expected_ty != GenValueType::ANY)
    {
        error(ERR_HAS_VOID_RET_TY, name_node.location(), function_name);
        error(INFO_FUNC_DEFINED_HERE, sres.found_location, function_name);

        return GenValueType::ERROR_TYPE;
    }
    else if (not can_promote_gen_vtype(func_ret_ty, expected_ty))
    {
        error(ERR_CANT_PROMOTE_TY, name_node.location(), type_name(func_ret_ty), type_name(expected_ty));
        error(INFO_FUNC_DEFINED_HERE, sres.found_location, function_name);

        return GenValueType::ERROR_TYPE;
    }

    const auto &args_node = func_call.children()[1];
    handle_arguments(args_node, *sres.func);

    emit(VMInstruction::CALLFUNC, &func_call);
    emit(sres.found_idx, &func_call);

    return func_ret_ty;
}

auto BytecodeGenerator::handle_variable_access(const ASTNode &identifier, GenValueType expected_ty) -> GenValueType
{
    CHECK_NODE_TYPE_RET(identifier, ASTNodeType::IDENTIFIER, GenValueType::ERROR_TYPE);
    
    std::string var_name = identifier.token().value;
    SymbolSearchRes sres = search_symbol(var_name);

    if (not sres.has_found) 
    {
        error(ERR_VAR_NOT_FOUND, identifier.location(), var_name);
        return GenValueType::ERROR_TYPE;
    }

    if (sres.type != SymbolSearchRes::Type::LOCAL_VAR 
     && sres.type != SymbolSearchRes::Type::GLOBAL_VAR 
     && sres.type != SymbolSearchRes::Type::MEMBER_VAR) 
    {
        error(ERR_NOT_A_VAR, identifier.location(), var_name);
        return GenValueType::ERROR_TYPE;
    }

    GenValueType var_type = sres.var->type;
    if (!can_promote_gen_vtype(var_type, expected_ty)) 
    {
        error(ERR_EXPECTED_TY,
            identifier.location(),
            type_name(expected_ty), 
            type_name(var_type)
        );
        return GenValueType::ERROR_TYPE;
    }

    if (sres.type == SymbolSearchRes::Type::LOCAL_VAR) 
    {
        // Variables that are not primitives don't need to be loaded as a reference
        // because objects technically are references    
        emit(VMInstruction::LOADLOCAL, &identifier);
    }
    else if (sres.type == SymbolSearchRes::Type::GLOBAL_VAR) 
    {
        emit(VMInstruction::LOADGLOBAL, &identifier);
    } 
    // Member access without the 'this' keyword
    else if (sres.type == SymbolSearchRes::Type::MEMBER_VAR) 
    {
        emit(VMInstruction::LOADLOCAL, &identifier);
        emit((dword_t)0, &identifier); // 'this'
        
        emit(VMInstruction::LOADMEMBER, &identifier);
    }

    dword_t idx = sres.found_idx;
    emit(idx, &identifier);

    return var_type;
}

auto BytecodeGenerator::handle_arguments(const ASTNode &args, const Internal::Function &func) -> void
{
    CHECK_NODE_TYPE(args, ASTNodeType::ARGUMENT_LIST);

    std::size_t num_args = args.children().size();
    std::size_t num_params = func.params.size();

    if (num_params != num_args)
    {
        error(ERR_EXPECTED_NUM_ARGS, args.location(), num_params, num_args);
        error(INFO_FUNC_DEFINED_HERE, func.defined_at, func.name);

        return;
    }

    for (std::size_t i = 0; i < num_args; i++)
    {
        const auto &child = args.children()[i];
        handle_expression(child, func.params[i].type, false);
    }
}

} // namespace NCSC
