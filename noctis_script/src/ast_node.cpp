#include "ast_node.hpp"
#include <print>

namespace NCSC
{
    
ASTNode::ASTNode(ASTNodeType type)
    : type_{type}
    , token_{}
    , children_{}
    , location_{}
    , has_loc_been_set_{false}
{}

auto ASTNode::add_child(const ASTNode &child) -> void
{
    if (not children_.empty()) 
    {
        std::size_t child_col = child.location().column;
        std::size_t child_col_end = child.location().column_end;

        // Expand to the left of this node if the child is placed before it
        std::size_t new_line = std::min(location_.line, child.location().line);
        bool line_changed = new_line != location_.line;

        location_.line = new_line;
        location_.column = line_changed ? child_col : std::min(child_col, location_.column);

        // Expand to the right of this node if the child is placed after it
        std::println("location_.line_end: {}; child.location().line_end: {}", location_.line_end, child.location().line_end);
        std::size_t new_line_end = std::max(location_.line_end, child.location().line_end);
        bool line_end_changed = new_line_end != location_.line_end;

        location_.line_end = new_line_end;
        location_.column_end = line_end_changed ? child_col_end : std::max(child_col_end, location_.column_end);
    }
    else 
    {
        // First child added, update position
        location_ = child.location();
    }

    children_.push_back(child);

    has_loc_been_set_ = true;
}

auto ASTNode::set_token(const Token &token) -> void
{
    token_ = token;

    location_.line = token.line;
    location_.line_end = token.line;

    location_.column = token.column;
    // Keep column end exclusive
    location_.column_end = location_.column + token.length() - 1;

    has_loc_been_set_ = true;
}

auto ASTNode::update_location(const Token &token) -> void
{
    if (not has_loc_been_set_)
    {
        location_.line = token.line;
        location_.column = token.column;
    }

    location_.line_end = std::max(token.line, location_.line_end);
    location_.column_end = std::max(token.column + token.length(), location_.column_end);
 
    has_loc_been_set_ = true;
}

auto ASTNode::set_location(const Location &location) -> void
{
    location_ = location;
}

auto ASTNode::type() const -> ASTNodeType
{
    return type_;
}

auto ASTNode::token() const -> const Token &
{
    return token_;
}

auto ASTNode::children() const -> const std::vector<ASTNode> &
{
    return children_;
}

auto ASTNode::location() const -> const Location &
{
    return location_;
}

} // namespace NCSC
