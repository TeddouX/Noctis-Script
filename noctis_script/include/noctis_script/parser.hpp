#pragma once
#include <vector>

#include "token.hpp"
#include "ast_node.hpp"


namespace NCSC
{
    
class Parser
{
public:
    Parser(const std::vector<Token> &tokens);

    auto parse() -> ASTNode;

private:
    std::vector<Token> tokens_;
    std::size_t curr_token_idx_;

    std::vector<std::string> syntax_errors_;
    bool has_syntax_error_;

    auto consume() -> const Token &;
    auto peek(std::size_t amount = 0) -> const Token &;
    auto create_syntax_error(const std::string &error) -> void;

    auto is_function_call() -> bool;

    auto parse_variable_declaration() -> ASTNode;
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
};

} // namespace NCSC
