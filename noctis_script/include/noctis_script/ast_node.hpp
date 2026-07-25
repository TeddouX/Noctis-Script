#pragma once
#include <vector>

#include "token.hpp"
#include "location.hpp"


namespace NCSC
{

enum class ASTNodeType 
{
    ROOT,
    VARIABLE_DECLARATION,
};

class ASTNode
{
public:
    explicit ASTNode(ASTNodeType type);

    auto add_child(const ASTNode &child) -> void;
    auto set_token(const Token &token) -> void;
    auto set_location(const Location &location) -> void;

    auto type() const -> ASTNodeType;
    auto token() const -> const Token &;
    auto children() const -> const std::vector<ASTNode> &;
    auto location() const -> const Location &;

private:
    ASTNodeType m_type;
    Token       m_token;
    Location    m_location;

    std::vector<ASTNode> m_children;
};

} // namespace NCSC
