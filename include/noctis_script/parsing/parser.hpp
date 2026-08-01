#pragma once
#include <vector>
#include <memory>

#include "../lexing/token.hpp"
#include "../error.hpp"
#include "ast_node.hpp"


namespace NCSC
{
    
class Parser
{
public:
    Parser(const std::vector<Token> &tokens, std::shared_ptr<ScriptSource> script_source = nullptr);

    auto parse() -> std::shared_ptr<ASTNode>;

    auto has_syntax_errors() const -> bool;
    auto get_syntax_errors() const -> const std::vector<Error> &;
    
private:
    std::vector<Token>              tokens_;
    std::size_t                     curr_token_idx_;
    std::shared_ptr<ScriptSource>   script_source_;

    std::vector<Error>              syntax_errors_;
    bool                            has_syntax_error_;

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
    auto parse_declaration_body(bool is_inside_obj)         -> std::shared_ptr<ASTNode>;

    // VARIABLE_DECLARATION CHILD ORDER:
    //      For normal variables:
    //          0: name                     (IDENTIFIER)
    //          1: type                     (DATA_TYPE)
    //          2: expression (optional)    (EXPRESSION)
    //      For members:
    //          0: access_modifier          (TOKEN)
    //          1: name                     (IDENTIFIER)
    //          2: type                     (DATA_TYPE)
    //          3: expression (optional)    (EXPRESSION)
    auto parse_variable_declaration(bool is_inside_obj)     -> std::shared_ptr<ASTNode>;

    // FUNCTION DECLARATION CHILD ORDER:
    //      For normal functions:
    //          0: name             (IDENTIFIER)
    //          1: param_list       (PARAMETER_LIST)
    //          2: return type      (DATA_TYPE)
    //          3: statement block  (STATEMENT_BLOCK)
    //      For methods:
    //          0: access_modifier  (TOKEN)
    //          1: name             (IDENTIFIER)
    //          2: param_list       (PARAMETER_LIST)
    //          3: return type      (DATA_TYPE)
    //          4: statement block  (STATEMENT_BLOCK)
    auto parse_function_declaration(bool is_inside_obj)     -> std::shared_ptr<ASTNode>;
    auto parse_object_declaration(bool is_inside_obj)       -> std::shared_ptr<ASTNode>;
    
    // STATEMENT BLOCK:
    //      Statement[]
    auto parse_statement_block()                            -> std::shared_ptr<ASTNode>;

    // STATEMENT:
    //      - if/else/elif: (IF_STATEMENT)
    //      or return:      (RETURN_STATEMENT)
    //      or var:         (VARIABLE_DECLARATION)
    //      or assigment:   (ASSIGNMENT)
    auto parse_statement()                                  -> std::shared_ptr<ASTNode>;

    // -> ASSIGNMENT
    auto parse_simple_statement()                           -> std::shared_ptr<ASTNode>;
    auto parse_if_statement(bool is_elif)                   -> std::shared_ptr<ASTNode>;
    auto parse_return_statement()                           -> std::shared_ptr<ASTNode>;
    auto parse_assignment()                                 -> std::shared_ptr<ASTNode>;
    auto parse_expression()                                 -> std::shared_ptr<ASTNode>;
    
    // EXPRESSION TERM:
    //      Any number of EXPRESSION_PREOPs
    //      An EXPRESSION_VALUE
    //      Any number of EXPRESSION_POSTOPs
    auto parse_expression_term()                            -> std::shared_ptr<ASTNode>;
    auto parse_assignment_operator(bool allow_compound_ops) -> std::shared_ptr<ASTNode>;
    auto parse_expression_pre_operator()                    -> std::shared_ptr<ASTNode>;

    // EXPRESSION VALUE:
    //      - CONSTANT
    //      or FUNCTION_CALL
    //      or IDENTIFIER
    //      or CONSTRUCT_CALL
    //      or EXPRESSION
    auto parse_expression_value()                           -> std::shared_ptr<ASTNode>;
    auto parse_expression_post_operator()                   -> std::shared_ptr<ASTNode>;
    auto parse_token(const Token &t)                        -> std::shared_ptr<ASTNode>;
    auto parse_identifier()                                 -> std::shared_ptr<ASTNode>;
    auto parse_function_call()                              -> std::shared_ptr<ASTNode>;
    auto parse_argument_list()                              -> std::shared_ptr<ASTNode>;
    auto parse_constant()                                   -> std::shared_ptr<ASTNode>;
    auto parse_construct_call()                             -> std::shared_ptr<ASTNode>;
    auto parse_type()                                       -> std::shared_ptr<ASTNode>;
    auto parse_type(const Token &t)                         -> std::shared_ptr<ASTNode>;

    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_TOKEN          {ErrorInfo::create("Syntax", "S1",  "Expected '{}'")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_TOKEN_OR_TOKEN {ErrorInfo::create("Syntax", "S2",  "Expected '{}' or '{}'")};
    inline static std::shared_ptr<ErrorInfo> ERR_UNEXPECTED_EOF          {ErrorInfo::create("Syntax", "S3",  "Unexpected end of file")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_DECLARATION    {ErrorInfo::create("Syntax", "S4",  "Expected a declaration")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_SEMICOLON      {ErrorInfo::create("Syntax", "S5",  "Expected a semicolon to end the line")};
    // Unreachable
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_ASSIGN_OP      {ErrorInfo::create("Syntax", "S6",  "Expected and assignment operator (=, +=, -=, etc...)")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_EXPR_VALUE     {ErrorInfo::create("Syntax", "S7",  "Expected a value")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_IDENTIFIER     {ErrorInfo::create("Syntax", "S8",  "Expected an identifier")};
    // Unreachable
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_CONSTANT_VAL   {ErrorInfo::create("Syntax", "S9",  "Expected a constant value")};
    inline static std::shared_ptr<ErrorInfo> ERR_EXPECTED_DATA_TYPE      {ErrorInfo::create("Syntax", "S10", "Expected a data type")};
    inline static std::shared_ptr<ErrorInfo> ERR_UNEXPECTED_TOKEN        {ErrorInfo::create("Syntax", "S11", "Unexpected token")};
};

} // namespace NCSC
