#include "ast_node.hpp"
#include <print>

namespace NCSC
{
    
ASTNode::ASTNode(ASTNodeType type)
    : m_type{type}
    , m_token{}
    , m_children{}
{}

auto ASTNode::add_child(const ASTNode &child) -> void
{
    if (not m_children.empty()) 
    {
        std::size_t child_col = child.location().column;
        std::size_t child_col_end = child.location().column_end;

        // Expand to the left of this node if the child is placed before it
        std::size_t new_line = std::min(m_location.line, child.location().line);
        bool line_changed = new_line != m_location.line;

        m_location.line = new_line;
        m_location.column = line_changed ? child_col : std::min(child_col, m_location.column);

        // Expand to the right of this node if the child is placed after it
        std::println("m_location.line_end: {}; child.location().line_end: {}", m_location.line_end, child.location().line_end);
        std::size_t new_line_end = std::max(m_location.line_end, child.location().line_end);
        bool line_end_changed = new_line_end != m_location.line_end;

        m_location.line_end = new_line_end;
        m_location.column_end = line_end_changed ? child_col_end : std::max(child_col_end, m_location.column_end);
    }
    else 
    {
        // First child added, update position
        m_location = child.location();
    }

    m_children.push_back(child);
}

auto ASTNode::set_token(const Token &token) -> void
{
    m_token = token;

    m_location.line = token.line;
    m_location.line_end = token.line;

    m_location.column = token.column;
    // Keep column end exclusive
    m_location.column_end = m_location.column + token.length() - 1;
}

auto ASTNode::set_location(const Location &location) -> void
{
    m_location = location;
}

auto ASTNode::type() const -> ASTNodeType
{
    return m_type;
}

auto ASTNode::token() const -> const Token &
{
    return m_token;
}

auto ASTNode::children() const -> const std::vector<ASTNode> &
{
    return m_children;
}

auto ASTNode::location() const -> const Location &
{
    return m_location;
}

} // namespace NCSC
