#include "ast_node.hpp"

#include <sstream>
#include <stack>


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

auto ASTNode::to_string() const -> std::string
{
    switch (type_)
    {
        using enum ASTNodeType;
        case ROOT:                  return "<ROOT>";
        case VARIABLE_DECLARATION:  return "<VARIABLE_DECLARATION>";
        case IDENTIFIER:            return "<IDENTIFIER>";
        case EXPRESSION:            return "<EXPRESSION>";
        case EXPRESSION_TERM:       return "<EXPRESSION_TERM>";
        case EXPRESSION_VALUE:      return "<EXPRESSION_VALUE>";
        case EXPRESSION_PREOP:      return "<EXPRESSION_PREOP>";
        case EXPRESSION_POSTOP:     return "<EXPRESSION_POSTOP>";
        case TOKEN:                 return "<TOKEN>";
        case BINOP:                 return "<BINOP>";
        case ASSIGNMENT:            return "<ASSIGNMENT>";
        case FUNCTION_CALL:         return "<FUNCTION_CALL>";
        case ARGUMENT_LIST:         return "<ARGUMENT_LIST>";
        case CONSTANT:              return "<CONSTANT>";
        case CONSTRUCT_CALL:        return "<CONSTRUCT_CALL>";
        case DATA_TYPE:             return "<DATA_TYPE>";
        default:                    return "<UNKNOWN>";
    }
}

auto ASTNode::ast_string(bool is_root, const std::string &prefix, bool is_last) const -> std::string
{
    std::ostringstream oss;
    std::string child_prefix = prefix;
    if (not is_root)
        child_prefix += (is_last ? "    " : "│   ");

    oss << prefix;
    if (not is_root)
        oss << (is_last ? "└── " : "├── ");
    oss << to_string();
    if (token_.is_valid())
        oss << "(" << token_.to_string() << ")";
    oss << "\n";

    for (size_t i = 0; i < children_.size(); i++) 
    {
        bool is_last_child = (i == children_.size() - 1);
        oss << children_[i].ast_string(false, child_prefix, is_last_child);
    }
    
    if (is_root) {
        std::string output = oss.str();

        if (!output.empty() && output.back() == '\n')
            output.pop_back();

        return output;
    }

    return oss.str();
}

} // namespace NCSC
