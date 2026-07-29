#include "bytecode_gen/bytecode_gen.hpp"

#include <algorithm>

#include "lexer/lexer.hpp"
#include "parser/parser.hpp"
#include "utils/vector_utils.hpp"
#include "bytecode_gen/value_type.hpp"


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
    bytecode_ = Bytecode{script_source_, is_debug_};

    handle_declaration_body(root_node.children()[0]);

    return bytecode_;
}

auto BytecodeGenerator::reset() -> void
{
    objects_.clear();
    functions_.clear();
    global_vars_.clear();

    script_source_ = nullptr;

    scope_deque_.clear();
    curr_scope_ = nullptr;

    compile_errors_.clear();
    syntax_errors_.clear();

    curr_object_ = nullptr;

    label_num_ = 0;
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

auto BytecodeGenerator::type_name(GenValueType type) -> std::string
{
    std::string ref_str = vtype_has_mask(type, GenValueType::REF_MASK) ? " ref" : "";
    type = vtype_clear_mask(type, GenValueType::REF_MASK);

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
    auto &bytes = bytecode_.bytes_;

    if (is_debug_ && node)
    {
        bytecode_.location_entries_.push_back(
            { bytecode_.bytes_.size(), node->location() }
        );
    }

    bytecode_.bytes_.push_back(byte);
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

auto BytecodeGenerator::can_promote_vtype(const GenValueType &from, const GenValueType &to) -> bool
{
    if (from == to)
        return true;

    int from_rank = get_vtype_rank(from);
    int to_rank = get_vtype_rank(to);

    if (from_rank <= 0 or to_rank <= 0)
        return false;
        
    // Any type can convert to void
    if (vtype_remove_const_ref(to) == GenValueType::VOID)
        return true;

    return from_rank <= to_rank;
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
                .idx = static_cast<dword_t>(member_idx),
                .found_type = member_var->type,
                .found_location = member_var->defined_at,
                .ty = SymbolSearchRes::Type::MEMBER_VAR,
            };
        }

        isize_t method_idx = Utils::find_named_idx(symbol_name, obj->methods);
        if (method_idx >= 0) 
        {
            Method *method = &obj->methods[member_idx];
            return SymbolSearchRes
            {
                .has_found = true,
                .method = method,
                .idx = static_cast<dword_t>(method_idx),
                .found_type = method->return_type,
                .found_location = method->defined_at,
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
                .has_found = true,
                .var = var,
                .idx = static_cast<dword_t>(var_idx),
                .found_type = var->type,
                .found_location = var->defined_at,
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
            .has_found = true,
            .obj = object,
            .idx = static_cast<dword_t>(object_idx),
            .found_type = object->type,
            .found_location = object->defined_at,
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
                .has_found = true,
                .member_var = member_var,
                .idx = static_cast<dword_t>(member_idx),
                .found_type = member_var->type,
                .found_location = member_var->defined_at,
                .ty = SymbolSearchRes::Type::MEMBER_VAR,
            };
        }

        isize_t method_idx = Utils::find_named_idx(symbol_name, curr_object_->methods);
        if (method_idx >= 0) 
        {
            Method *method = &curr_object_->methods[method_idx];
            return SymbolSearchRes
            {
                .has_found = true,
                .method = method,
                .idx = static_cast<dword_t>(method_idx),
                .found_type = method->return_type,
                .found_location = method->defined_at,
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
            .has_found = true,
            .func = func,
            .idx = static_cast<dword_t>(func_idx),
            .found_type = func->return_type,
            .found_location = func->defined_at,
            .ty = SymbolSearchRes::Type::FUNCTION,
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
            .idx = static_cast<dword_t>(global_var_idx),
            .found_type = global_var->type,
            .found_location = global_var->defined_at,
            .ty = SymbolSearchRes::Type::GLOBAL_VAR,
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
        return GenValueType::INVALID;
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
    if (sres.ty == SymbolSearchRes::Type::OBJECT)
        return sres.found_type;

    error(ERR_NOT_A_TYPE, type_node.location(), tok.to_string());

    return GenValueType::INVALID;
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

        return;
    }

    const auto &func_stmt_block = func_decl.children()[3];
    handle_statement_block(func_stmt_block);
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
    if (is_member_var && curr_object_)
    {
        auto member_var = static_cast<Internal::MemberVariable *>(var.get()); 
        curr_object_->member_variables.push_back(*member_var);
    }

    const auto &expression_node = var_decl.children()[child_idx++];
    handle_expression(expression_node, type);
}

auto BytecodeGenerator::handle_assignment(const ASTNode &assigment) -> void
{
    CHECK_NODE_TYPE(assigment, ASTNodeType::ASSIGNMENT);
}

auto BytecodeGenerator::handle_expression(const ASTNode &expr, const GenValueType &expected_ty) -> void
{
    CHECK_NODE_TYPE(expr, ASTNodeType::EXPRESSION);

    if (expr.children().empty())
        return;
    
    const auto &first_child = expr.children()[0];
    if (first_child.type() == ASTNodeType::EXPRESSION_TERM)
    {
        handle_expression_term(first_child, expected_ty);
        return;
    }

    recursively_handle_expression_child(first_child, expected_ty);
}

auto BytecodeGenerator::recursively_handle_expression_child(const ASTNode &expr_child, const GenValueType &expected_ty) -> void
{
    ASTNodeType expr_child_type = expr_child.type();
    if (expr_child_type == ASTNodeType::EXPRESSION_TERM)
        handle_expression_term(expr_child, expected_ty);
    else if (expr_child_type == ASTNodeType::BINOP)
        handle_binop(expr_child, expected_ty);
}

auto BytecodeGenerator::handle_expression_term(const ASTNode &expr_term, const GenValueType &expected_ty) -> void
{
    CHECK_NODE_TYPE(expr_term, ASTNodeType::EXPRESSION_TERM);

    emit(VMInstruction::PUSH, nullptr);
    emit_constant((byte_t)67, nullptr);
}

auto BytecodeGenerator::handle_binop(const ASTNode &binop, const GenValueType &expected_ty) -> void
{
    CHECK_NODE_TYPE(binop, ASTNodeType::BINOP);

    const Token &binop_tok = binop.token();
    TokenType binop_tok_ty = binop.token().type;
    const auto &left_operand = binop.children()[0];
    const auto &right_operand = binop.children()[1];

    if (binop_tok_ty == TokenType::LOGICAL_AND or binop_tok_ty == TokenType::LOGICAL_OR)
    {
        if (expected_ty == GenValueType::BOOL)
        {
            error(ERR_EXPECTED_TY,
                binop.location(),
                type_name(expected_ty), 
                type_name(GenValueType::BOOL)
            );
            return;
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
    }
    else
    {
        if (binop_tok.is_comparison_operator() && expected_ty != GenValueType::BOOL)
        {
            error(ERR_EXPECTED_TY,
                binop.location(),
                type_name(expected_ty), 
                type_name(GenValueType::BOOL)
            );
                
            return;
        }

        recursively_handle_expression_child(right_operand, expected_ty);
        recursively_handle_expression_child(left_operand, expected_ty);

        switch (binop_tok_ty) 
        {
            case TokenType::PLUS:             emit(VMInstruction::ADD, &binop);   return;
            case TokenType::MINUS:            emit(VMInstruction::SUB, &binop);   return;
            case TokenType::STAR:             emit(VMInstruction::MUL, &binop);   return;
            case TokenType::SLASH: 
            {
                if (!can_promote_vtype(GenValueType::FLOAT64, expected_ty)) 
                {
                    error(ERR_DIV_RETURNS_F64, binop.location(), type_name(expected_ty));
                    return;
                }

                emit(VMInstruction::DIV, &binop);
                return;
            }

            case TokenType::LESS_THAN:              emit(VMInstruction::CMPST, &binop); return;
            case TokenType::LESS_THAN_EQUAL:        emit(VMInstruction::CMPSE, &binop); return;
            case TokenType::GREATER_THAN:           emit(VMInstruction::CMPGT, &binop); return;
            case TokenType::GREATER_THAN_EQUAL:     emit(VMInstruction::CMPGE, &binop); return;
            case TokenType::EQUAL_EQUAL:            emit(VMInstruction::CMPEQ, &binop); return;
            case TokenType::NOT_EQUAL:              emit(VMInstruction::CMPNE, &binop); return;

            default: emit(VMInstruction::NOOP, &binop);  return;
        }
    }
}

} // namespace NCSC
