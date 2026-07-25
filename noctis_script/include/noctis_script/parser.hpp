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
    std::vector<Token> m_tokens;
    std::size_t m_token_idx;

    auto consume() -> const Token &;
};

} // namespace NCSC
