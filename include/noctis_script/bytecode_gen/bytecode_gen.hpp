#pragma once
#include <string>
#include <deque>

#include "bytecode.hpp"
#include "bytecode_gen_data.hpp"
#include "value_type.hpp"
#include "../error.hpp"
#include "../parser/ast_node.hpp"
#include "../vm/vm_instructions.hpp"
#include "../vm/builtin_types.hpp"


namespace NCSC
{

class BytecodeGenerator
{
public:
    BytecodeGenerator(bool is_debug = false);

    auto compile_script(std::shared_ptr<ScriptSource> src) -> Bytecode;
    auto compile_script(const std::string &script) -> Bytecode;
    // Do not try to compile if the code has syntax errors as it may lead to crashes
    auto compile_script(const ASTNode &root_node, std::shared_ptr<ScriptSource> src = nullptr) -> Bytecode;

    auto reset() -> void;

    auto has_compile_errors() const -> bool;
    // Only useful if you haven't parsed the script yourself
    auto has_syntax_errors() const -> bool;
    
    auto compile_errors() const -> const std::vector<Error> &;
    // Only useful if you haven't parsed the script yourself
    auto syntax_errors() const -> const std::vector<Error> &;

private:
    std::vector<Internal::Object>               objects_;
    std::vector<Internal::Function>             functions_;
    std::vector<Internal::GlobalVariable>       global_vars_;
    std::shared_ptr<ScriptSource>               script_source_;

    Bytecode bytecode_;

    std::deque<Internal::Scope>                 scope_deque_;
    Internal::Scope                            *curr_scope_;

    std::vector<Error>                          compile_errors_;
    std::vector<Error>                          syntax_errors_;

    bool                                        is_debug_;

    // Used for searching symbols
    Internal::Object                           *curr_object_;

    usize_t                                     label_num_;

    std::unordered_map<GenValueType, std::string>  type_names_;

    
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
    
    auto type_name(GenValueType type) -> std::string;
    
    template <typename _T>
    auto emit_constant(const _T &&constant, const ASTNode *node)
    {
        BuiltinType type;

        if constexpr      (std::is_same_v<_T, int8_t>)      type = BuiltinType::INT8;
        else if constexpr (std::is_same_v<_T, int16_t>)     type = BuiltinType::INT16;
        else if constexpr (std::is_same_v<_T, int32_t>)     type = BuiltinType::INT32;
        else if constexpr (std::is_same_v<_T, int64_t>)     type = BuiltinType::INT64;
        else if constexpr (std::is_same_v<_T, uint8_t>)     type = BuiltinType::UINT8;
        else if constexpr (std::is_same_v<_T, uint16_t>)    type = BuiltinType::UINT16;
        else if constexpr (std::is_same_v<_T, uint32_t>)    type = BuiltinType::UINT16;
        else if constexpr (std::is_same_v<_T, uint64_t>)    type = BuiltinType::UINT16;
        else if constexpr (std::is_same_v<_T, float32_t>)   type = BuiltinType::FLOAT32;
        else if constexpr (std::is_same_v<_T, float64_t>)   type = BuiltinType::FLOAT64;
        else if constexpr (std::is_same_v<_T, bool>)        type = BuiltinType::BOOL;
        else if constexpr (std::is_same_v<_T, nullptr_t>)   type = BuiltinType::OBJ_NULL;
        else                                                type = BuiltinType::VOID;

        if (type == BuiltinType::VOID)
        {
            error(ERR_INVALID_BUILTIN_TYPE, Location{}, typeid(_T).name());
            return;
        }

        emit(static_cast<builtin_type_size_t>(type), node);
    
        constexpr std::size_t _T_size = sizeof(_T);
        if constexpr (_T_size == 1)
            emit((byte_t)constant, node);
        else if constexpr (_T_size == 2)
            emit((word_t)constant, node);
        else if constexpr (_T_size == 4)
            emit((dword_t)constant, node);
        else if constexpr (_T_size == 8)
            emit((qword_t)constant, node);
    }

    auto emit(const std::vector<byte_t> &bytes,     const ASTNode *node) -> void;
    auto emit(byte_t byte,                          const ASTNode *node) -> void;
    auto emit(word_t word,                          const ASTNode *node) -> void;
    auto emit(dword_t dword,                        const ASTNode *node) -> void;
    auto emit(qword_t qword,                        const ASTNode *node) -> void;
    auto emit(VMInstruction instr,                  const ASTNode *node) -> void;

    auto can_promote_vtype(const GenValueType &from, const GenValueType &to) -> bool;

    auto handle_declaration_body(const ASTNode &decl_body) -> void;
    auto handle_function_declaration(const ASTNode &func_decl, bool quick) -> void;
    auto handle_method_declaration(const ASTNode &func_decl, bool quick) -> void;
    auto handle_statement_block(const ASTNode &stmt_block) -> void;
    auto handle_if_statement(const ASTNode &if_stmt) -> void;
    auto handle_return_statement(const ASTNode &return_stmt) -> void;
    auto handle_variable_declaration(const ASTNode &var_decl) -> void;
    auto handle_assignment(const ASTNode &assigment) -> void;
    auto handle_expression(const ASTNode &expr, const GenValueType &expected_ty) -> void;
    auto recursively_handle_expression_child(const ASTNode &expr_child, const GenValueType &expected_ty) -> void; 
    auto handle_expression_term(const ASTNode &expr_term, const GenValueType &expected_ty) -> void;
    auto handle_binop(const ASTNode &binop, const GenValueType &expected_ty) -> void;

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
            Internal::Method           *method;
        };

        dword_t idx = INVALID_INDEX;
        GenValueType found_type = GenValueType::INVALID;
        Location found_location{};

        enum class Type {
            INVALID,
            GLOBAL_VAR,
            LOCAL_VAR,
            
            FUNCTION,
            METHOD,
            OBJECT,
            MEMBER_VAR,
        } ty = Type::INVALID;
    };
    auto search_symbol(const std::string &symbol_name, Internal::Object *obj = nullptr) -> SymbolSearchRes;

    inline static auto ERR_INVALID_AST_NODE     {ErrorInfo::create("Internal Compiler Error",   "IC1",  "ASTNode type '{}' doesn't match the expected '{}'")};
    inline static auto ERR_INVALID_BUILTIN_TYPE {ErrorInfo::create("Internal Compiler Error",   "IC1",  "Invalid type '{}' for BuiltinType, maybe it hasn't been implemented yet?")};

    inline static auto ERR_NOT_A_TYPE           {ErrorInfo::create("Compiler Error",            "C1",   "'{}' is not a type.")};
    inline static auto ERR_ALREADY_DEFINED      {ErrorInfo::create("Compiler Error",            "C2",   "'{}' was already defined somewhere else.")};
    inline static auto ERR_EXPECTED_TY          {ErrorInfo::create("Compiler Error",            "C3",   "Expected type '{}', instead got '{}'")};
    inline static auto ERR_DIV_RETURNS_F64      {ErrorInfo::create("Compiler Error",            "C4",   "Division always returns a float64 (double), which can't be converted to '{}'")};

    inline static auto INFO_DEFINED_HERE        {ErrorInfo::create("Compiler Info",             "CI1",  "'{}' defined here:")};
};

} // namespace NCSC
