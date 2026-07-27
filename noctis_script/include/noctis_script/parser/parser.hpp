#pragma once
#include <vector>

#include "../lexer/token.hpp"
#include "../error.hpp"
#include "ast_node.hpp"


namespace NCSC
{
    
class Parser
{
public:
    Parser(const std::vector<Token> &tokens);

    auto parse() -> ASTNode;

    auto has_errors() const -> bool;
    auto get_errors() const -> const std::vector<Error> &;
    
private:
    std::vector<Token> tokens_;
    std::size_t curr_token_idx_;
    std::shared_ptr<ScriptSource> script_source_;

    std::vector<Error> syntax_errors_;
    bool has_syntax_error_;

    auto consume() -> const Token &;
    auto peek(std::size_t amount = 0) -> const Token &;

    template <typename... _Args>
    auto create_syntax_error(const std::shared_ptr<ErrorInfo> &err_info, const Token &tok, _Args&&... args) -> void
    {
        has_syntax_error_ = true;

        std::string err_message = err_info->get_formatted(std::forward<_Args>(args)...);
        Error err{err_info, err_message, script_source_, tok.location};

        syntax_errors_.push_back(err);
    }

    auto is_function_call() -> bool;

    // Parses functions declaration, object declarartion and global variable declarations
    // Can be used to parse the body of an object
    auto parse_declaration_body(bool is_inside_obj) -> ASTNode;
    auto parse_variable_declaration(bool is_inside_obj) -> ASTNode;
    auto parse_function_declaration(bool is_inside_obj) -> ASTNode;
    auto parse_object_declaration(bool is_inside_obj) -> ASTNode;
    auto parse_statement_block() -> ASTNode;
    auto parse_statement() -> ASTNode;
    auto parse_simple_statement() -> ASTNode;
    auto parse_if_statement(bool is_elif) -> ASTNode;
    auto parse_return_statement() -> ASTNode;
    auto parse_assignment() -> ASTNode;
    auto parse_expression() -> ASTNode;
    auto parse_expression_term() -> ASTNode;
    auto parse_assignment_operator(bool allow_compound_ops) -> ASTNode;
    auto parse_expression_pre_operator() -> ASTNode;
    auto parse_expression_value() -> ASTNode;
    auto parse_expression_post_operator() -> ASTNode;
    auto parse_token(const Token &t) -> ASTNode;
    auto parse_identifier() -> ASTNode;
    auto parse_function_call() -> ASTNode;
    auto parse_argument_list() -> ASTNode;
    auto parse_constant() -> ASTNode;
    auto parse_construct_call() -> ASTNode;
    auto parse_type() -> ASTNode;

    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_TOKEN          {ErrorInfo::create("Syntax Error", "S1",  "Expected '{}'")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_TOKEN_OR_TOKEN {ErrorInfo::create("Syntax Error", "S2",  "Expected '{}' or '{}'")};
    inline static std::shared_ptr<ErrorInfo> ERR_UNEXPECTED_EOF          {ErrorInfo::create("Syntax Error", "S3",  "Unexpected end of file")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_DECLARATION    {ErrorInfo::create("Syntax Error", "S4",  "Expected a declaration")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_SEMICOLON      {ErrorInfo::create("Syntax Error", "S5",  "Expected a semicolon to end the line")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_ASSIGN_OP      {ErrorInfo::create("Syntax Error", "S6",  "Expected and assignment operator (=, +=, -=, etc...)")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_EXPR_VALUE     {ErrorInfo::create("Syntax Error", "S7",  "Expected an expression value")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_IDENTIFIER     {ErrorInfo::create("Syntax Error", "S8",  "Expected an identifier")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_CONSTANT_VAL   {ErrorInfo::create("Syntax Error", "S9",  "Expected a constant value")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_DATA_TYPE      {ErrorInfo::create("Syntax Error", "S10", "Expected a data type")};
    inline static std::shared_ptr<ErrorInfo> ERR_UNEXPECTED_TOKEN        {ErrorInfo::create("Syntax Error", "S11", "Unexpected token")};
};

} // namespace NCSC
