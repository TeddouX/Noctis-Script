#pragma once
#include <string>
#include <deque>

#include "bytecode.hpp"
#include "bytecode_gen_data.hpp"
#include "value_type.hpp"
#include "../error.hpp"
#include "../parsing/ast_node.hpp"
#include "../vm/vm_instructions.hpp"
#include "../vm/builtin_types.hpp"


namespace NCSC
{

class BytecodeGenerator
{
public:
    BytecodeGenerator(bool is_debug = false);

    auto generate(std::shared_ptr<ScriptSource> src) -> Bytecode;
    auto generate(const std::string &script) -> Bytecode;
    // Do not try to compile if the code has syntax errors as it may lead to crashes
    auto generate(const ASTNode &root_node, std::shared_ptr<ScriptSource> src = nullptr) -> Bytecode;

    auto reset() -> void;

    auto has_generation_errors() const -> bool;
    // Only useful if you haven't parsed the script yourself
    auto has_syntax_errors() const -> bool;
    
    auto generation_errors() const -> const std::vector<Error> &;
    // Only useful if you haven't parsed the script yourself
    auto syntax_errors() const -> const std::vector<Error> &;

private:
    std::vector<Internal::Object>                   objects_;
    std::vector<Internal::Function>                 functions_;
    std::vector<Internal::GlobalVariable>           global_vars_;
    std::shared_ptr<ScriptSource>                   script_source_;

    Bytecode                                        temp_bytecode_;

    std::deque<Internal::Scope>                     scope_deque_;
    Internal::Scope                                *curr_scope_;

    std::vector<Error>                              generation_errors_;
    std::vector<Error>                              syntax_errors_;

    bool                                            is_debug_;

    // Used for searching symbols
    Internal::Object                               *curr_object_;

    usize_t                                         label_num_;

    std::unordered_map<GenValueType, std::string>   type_names_;

    
    template <typename... _Args>
    auto error(const std::shared_ptr<ErrorInfo> &err_info, const Location &location, _Args&&... args) -> void
    {
        std::string err_message = err_info->get_formatted(std::forward<_Args>(args)...);
        Error err{err_info, err_message, script_source_, location};

        generation_errors_.push_back(err);
    }

    auto finalize_bytecode() -> Bytecode;
    auto make_bytecode_header(Bytecode &bytecode) -> BytecodeHeader;
    auto append_globals_bytecode(Internal::GlobalVariable &global_var, std::vector<byte_t> &bytes) -> void;
    auto append_functions_bytecode(Internal::Function &function, std::vector<byte_t> &bytes) -> void;
    auto append_objects_data(const Internal::Object &object, std::vector<byte_t> &bytes) -> void;
    auto resolve_jumps(std::vector<byte_t> &bytes) -> void;
    
    auto enter_new_scope() -> void;
    auto exit_scope() -> void;
    auto reset_scopes() -> void;
    
    auto type_name(GenValueType type) -> std::string;
    
    template <typename T_>
    auto emit_constant(const T_ &constant, const ASTNode *node) -> void
    {
        BuiltinType type;

        if constexpr      (std::is_same_v<T_, int8_t>)      type = BuiltinType::INT8;
        else if constexpr (std::is_same_v<T_, int16_t>)     type = BuiltinType::INT16;
        else if constexpr (std::is_same_v<T_, int32_t>)     type = BuiltinType::INT32;
        else if constexpr (std::is_same_v<T_, int64_t>)     type = BuiltinType::INT64;
        else if constexpr (std::is_same_v<T_, uint8_t>)     type = BuiltinType::UINT8;
        else if constexpr (std::is_same_v<T_, uint16_t>)    type = BuiltinType::UINT16;
        else if constexpr (std::is_same_v<T_, uint32_t>)    type = BuiltinType::UINT16;
        else if constexpr (std::is_same_v<T_, uint64_t>)    type = BuiltinType::UINT16;
        else if constexpr (std::is_same_v<T_, float32_t>)   type = BuiltinType::FLOAT32;
        else if constexpr (std::is_same_v<T_, float64_t>)   type = BuiltinType::FLOAT64;
        else if constexpr (std::is_same_v<T_, bool>)        type = BuiltinType::BOOL;
        else if constexpr (std::is_same_v<T_, nullptr_t>)   type = BuiltinType::OBJ_NULL;
        else                                                type = BuiltinType::VOID;

        if (type == BuiltinType::VOID)
        {
            error(ERR_INVALID_BUILTIN_TYPE, Location{}, typeid(T_).name());
            return;
        }

        emit(static_cast<builtin_type_size_t>(type), node);
    
        constexpr std::size_t T_size = sizeof(T_);
        if constexpr (T_size == 1)
            emit((byte_t)constant, node);
        else if constexpr (T_size == 2)
            emit((word_t)constant, node);
        else if constexpr (T_size == 4)
            emit((dword_t)constant, node);
        else if constexpr (T_size == 8)
            emit((qword_t)constant, node);
    }

    template <typename T_>
        requires(std::is_integral_v<T_>)
    auto emit_int_constant_from_str(const std::string &value, const ASTNode *constant)
    {
        using IntermediateTy_ = std::conditional_t<std::is_signed_v<T_>, std::int64_t, std::uint64_t>;
        IntermediateTy_ intermediate_int{};

        if constexpr (std::is_signed_v<IntermediateTy_>)
            intermediate_int = std::strtoll(value.c_str(), nullptr, 0);
        else 
            intermediate_int = std::strtoull(value.c_str(), nullptr, 0);

        T_ val = static_cast<T_>(intermediate_int);
        emit_constant(val, constant);
    }

    auto emit(const std::vector<byte_t> &bytes,     const ASTNode *node) -> void;
    auto emit(byte_t byte,                          const ASTNode *node) -> void;
    auto emit(word_t word,                          const ASTNode *node) -> void;
    auto emit(dword_t dword,                        const ASTNode *node) -> void;
    auto emit(qword_t qword,                        const ASTNode *node) -> void;
    auto emit(VMInstruction instr,                  const ASTNode *node) -> void;

    auto can_promote_gen_vtype(const GenValueType &from, const GenValueType &to) -> bool;
    auto promote_gen_vtype(GenValueType from, GenValueType to) -> GenValueType;
    // The pointer that is returned isn't meant to be stored
    // Returns nullptr if the ty isn't a valid object 
    auto gen_vtype_as_object(GenValueType ty) -> Internal::Object *;

    auto handle_declaration_body(const ASTNode &decl_body) -> void;
    auto handle_function_declaration(const ASTNode &func_decl, bool quick) -> void;
    auto handle_method_declaration(const ASTNode &func_decl, bool quick) -> void;
    auto handle_statement_block(const ASTNode &stmt_block) -> void;
    auto handle_if_statement(const ASTNode &if_stmt) -> void;
    auto handle_return_statement(const ASTNode &return_stmt) -> void;
    auto handle_variable_declaration(const ASTNode &var_decl) -> void;
    auto handle_assignment(const ASTNode &assigment) -> void;
    auto handle_expression(const ASTNode &expr, const GenValueType &expected_ty, bool should_be_assignable) -> GenValueType;
    auto recursively_handle_expression_child(const ASTNode &expr_child, const GenValueType &expected_ty) -> GenValueType; 
    auto handle_expression_term(
        const ASTNode &expr_term, 
        const GenValueType &expected_ty, 
        bool should_be_assignable, 
        bool should_leave_val_on_stack
    ) -> GenValueType;
    auto handle_binop(const ASTNode &binop, const GenValueType &expected_ty) -> GenValueType;
    auto handle_expression_value(
        const ASTNode &expr_value, 
        const GenValueType &expected_ty, 
        bool should_be_assignable
    ) -> GenValueType;
    auto handle_store(const ASTNode &expr) -> void;
    auto handle_constant(const ASTNode &constant, GenValueType expected_ty) -> GenValueType;
    auto handle_function_call(const ASTNode &func_call, GenValueType expected_ty) -> GenValueType;
    auto handle_variable_access(const ASTNode &identifier, GenValueType expected_ty) -> GenValueType;
    auto handle_arguments(const ASTNode &args, const Internal::Function &func) -> void;

    auto is_symbol_defined_elsewhere(const ASTNode &identifer) -> bool;

    auto value_type_from_node(const ASTNode &type_node) -> GenValueType;
    auto access_mod_from_token(const Token &tok) -> Internal::AccessModifier;

    struct SymbolSearchRes {
        bool has_found = false;

        union {
            Internal::Object           *obj;
            Internal::Function         *func;
            Internal::Variable         *var;
            Internal::GlobalVariable   *global_var;
            Internal::MemberVariable   *member_var;
        };

        dword_t found_idx = INVALID_INDEX;
        GenValueType found_gen_vtype = GenValueType::INVALID;
        Location found_location{};

        enum class Type {
            INVALID,
            GLOBAL_VAR,
            LOCAL_VAR,
            
            FUNCTION,
            METHOD,
            OBJECT,
            MEMBER_VAR,
        } type = Type::INVALID;
    };
    auto search_symbol(const std::string &symbol_name, Internal::Object *obj = nullptr) -> SymbolSearchRes;

    inline static auto ERR_INVALID_AST_NODE     {ErrorInfo::create("Internal Generation",   "IG1",  "ASTNode type '{}' doesn't match the expected '{}'")};
    inline static auto ERR_INVALID_BUILTIN_TYPE {ErrorInfo::create("Internal Generation",   "IG2",  "Invalid type '{}' for BuiltinType, maybe it hasn't been implemented yet?")};

    inline static auto ERR_NOT_A_TYPE           {ErrorInfo::create("Generation",            "G1",   "'{}' is not a type.")};
    inline static auto ERR_ALREADY_DEFINED      {ErrorInfo::create("Generation",            "G2",   "'{}' was already defined somewhere else.")};
    inline static auto ERR_EXPECTED_TY          {ErrorInfo::create("Generation",            "G3",   "Expected type '{}', instead got '{}'")};
    inline static auto ERR_DIV_RETURNS_F64      {ErrorInfo::create("Generation",            "G4",   "Division always returns a float64 (double), which can't be converted to '{}'")};
    inline static auto ERR_EXPECTED_NUMERIC_TY  {ErrorInfo::create("Generation",            "G5",   "Expected a numeric type (int or float), instead got '{}'")};
    inline static auto ERR_NOT_ASSIGNABLE       {ErrorInfo::create("Generation",            "G6",   "Expected an lvalue (assignable) term")};
    inline static auto ERR_CANT_PROMOTE_TY      {ErrorInfo::create("Generation",            "G7",   "Can't convert '{}' to '{}'")};
    // inline static auto ERR_SYMBOL_NOT_FOUND     {ErrorInfo::create("Generation",            "G8",   "Symbol '{}' was not found (maybe check spelling ?)")};
    inline static auto ERR_EXPECTED_OBJECT      {ErrorInfo::create("Generation",            "G9",   "Expected an object, instead got '{}'")};
    inline static auto ERR_METHOD_NOT_FOUND     {ErrorInfo::create("Generation",            "G10",  "Method '{}' not found in object '{}' (maybe check spelling ?)")};
    inline static auto ERR_MEMBER_NOT_FOUND     {ErrorInfo::create("Generation",            "G11",  "Member '{}' not found in object '{}' (maybe check spelling ?)")};
    inline static auto ERR_VAR_NOT_FOUND        {ErrorInfo::create("Generation",            "G12",  "Variable '{}' not found (maybe check spelling ?)")};
    inline static auto ERR_NOT_A_VAR            {ErrorInfo::create("Generation",            "G13",  "'{}' can't be used as a variable")};
    inline static auto ERR_FUNC_NOT_FOUND       {ErrorInfo::create("Generation",            "G14",  "Can't find function named '{}' (maybe check spelling ?)")};
    inline static auto ERR_NOT_A_FUNC           {ErrorInfo::create("Generation",            "G15",  "'{}' can't be used a function")};
    inline static auto ERR_HAS_VOID_RET_TY      {ErrorInfo::create("Generation",            "G16",  "Function '{}' has a void return type, but is still getting used in an expression")};
    inline static auto ERR_EXPECTED_NUM_ARGS    {ErrorInfo::create("Generation",            "G17",  "Expected {} arguments, instead got {}")};

    inline static auto INFO_DEFINED_HERE        {ErrorInfo::create("Generation",            "GI1",  "'{}' defined here:", ErrorLevel::INFO)};
    inline static auto INFO_FUNC_DEFINED_HERE   {ErrorInfo::create("Generation",            "GI2",  "Function '{}' defined here:", ErrorLevel::INFO)};
};

} // namespace NCSC
