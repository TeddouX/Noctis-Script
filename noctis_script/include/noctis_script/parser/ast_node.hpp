#pragma once
#include <vector>

#include "../lexer/token.hpp"
#include "../location.hpp"


namespace NCSC
{

enum class ASTNodeType 
{
    ROOT,
    VARIABLE_DECLARATION,
    IDENTIFIER,
    EXPRESSION,
    EXPRESSION_TERM,
    EXPRESSION_VALUE,
    EXPRESSION_PREOP,
    EXPRESSION_POSTOP,
    TOKEN,
    BINOP,
    ASSIGNMENT,
    FUNCTION_CALL,
    ARGUMENT_LIST,
    CONSTANT,
    CONSTRUCT_CALL,
    DATA_TYPE,
    FUNCTION_DECLARATION,
    PARAMETER_LIST,
    STATEMENT_BLOCK,
    IF_STATEMENT,
    RETURN_STATEMENT,
    ELIF_BRANCH,
    ELSE_BRANCH,
    OBJ_DECLARATION,
    DECLARATION_BODY,
};

class ASTNode
{
public:
    explicit ASTNode(ASTNodeType type);

    auto add_child(const ASTNode &child) -> void;
    auto set_token(const Token &token) -> void;
    auto update_location(const Token &token) -> void;
    auto set_location(const Location &location) -> void;

    auto type() const -> ASTNodeType;
    auto token() const -> const Token &;
    auto children() const -> const std::vector<ASTNode> &;
    auto location() const -> const Location &;

    auto ast_string(bool is_root = true, const std::string &prefix = "", bool is_last = false) const -> std::string;
    auto to_string() const -> std::string;

private:
    friend class Parser;

    ASTNodeType type_;
    Token       token_;
    Location    location_;

    bool has_loc_been_set_;

    std::vector<ASTNode> children_;
};

} // namespace NCSC
