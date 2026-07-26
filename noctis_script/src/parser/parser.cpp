#include "parser/parser.hpp"

#include <print>
#include <iostream>

#define CHECK_SYNTAX_ERROR_RET_VALUE(ret) if (has_syntax_error_) return ret
#define CHECK_SYNTAX_ERROR() CHECK_SYNTAX_ERROR_RET_VALUE(node)


namespace NCSC
{
    
Parser::Parser(const std::vector<Token> &tokens)
    : tokens_{tokens}
    , has_syntax_error_{false}
    , curr_token_idx_{0zu}
{}

auto Parser::parse() -> ASTNode
{
    ASTNode root{ASTNodeType::ROOT};

    while (curr_token_idx_ < tokens_.size() and not has_syntax_error_)
    {
        const Token &curr_tok = tokens_[curr_token_idx_];
        
        if (curr_tok.type == TokenType::END_OF_FILE)
            break;
        else if (curr_tok.type == TokenType::LET_KWD)
            root.add_child(parse_variable_declaration(false));
        else if (curr_tok.type == TokenType::FUNC_KWD)
            root.add_child(parse_function_declaration(false));
        else
            create_syntax_error(std::format("Unexpected token '{}'", curr_tok.to_string()));

        // has_syntax_error_ = false;
    }

    std::println(std::cerr, "{}", root.ast_string());

    return root;
}

auto Parser::consume() -> const Token &
{
    if (curr_token_idx_ >= tokens_.size()) 
    {
        std::println("Unexpected end of tokens");
        has_syntax_error_ = true;

        return INVALID_TOKEN;
    }

    return tokens_[curr_token_idx_++];
}

auto Parser::peek(std::size_t amount) -> const Token &
{
    if (curr_token_idx_ >= tokens_.size()) 
    {
        std::println("Unexpected end of tokens");
        has_syntax_error_ = true;

        return INVALID_TOKEN;
    }

    return tokens_[curr_token_idx_ + amount];
}

auto Parser::create_syntax_error(const std::string &error) -> void
{
    has_syntax_error_ = true;
    syntax_errors_.push_back(error);
}

auto Parser::is_function_call() -> bool 
{
    const Token &t = peek();
    CHECK_SYNTAX_ERROR_RET_VALUE(false);

    if (t.type != TokenType::ID)
        return false;
    
    const Token &t1 = peek(1);
    CHECK_SYNTAX_ERROR_RET_VALUE(false);

    return t1.type == TokenType::PARENTHESIS_OPEN;
}

auto Parser::parse_variable_declaration(bool is_inside_obj) -> ASTNode
{
    ASTNode node{ASTNodeType::VARIABLE_DECLARATION};

    const Token &let_tok = consume();
    CHECK_SYNTAX_ERROR();
    if (let_tok.type != TokenType::LET_KWD) 
    {
        create_syntax_error("Expected 'let' as a the first token of a variable declaration.");
        return node;    
    }

    node.add_child(parse_identifier());
    CHECK_SYNTAX_ERROR();

    const Token &t = consume();
    CHECK_SYNTAX_ERROR();
    if (t.type != TokenType::COLON)
    {
        create_syntax_error("Expected ':'");
        return node;
    }

    node.add_child(parse_type());
    CHECK_SYNTAX_ERROR();

    const Token &t1 = peek();
    CHECK_SYNTAX_ERROR();
    if (t1.type != TokenType::SEMICOLON) 
    {
        const Token &t2 = consume();
        CHECK_SYNTAX_ERROR();

        if (t2.type != TokenType::EQUAL)
        {
            create_syntax_error("Expected an '='");
            return node;
        }

        node.add_child(parse_expression());
        CHECK_SYNTAX_ERROR();
    }

    const Token &t2 = consume();
    CHECK_SYNTAX_ERROR();
    if (t2.type != TokenType::SEMICOLON)
        create_syntax_error("Expected a semicolon");

    return node;
}

auto Parser::parse_function_declaration(bool is_inside_obj) -> ASTNode
{
    ASTNode node(ASTNodeType::FUNCTION_DECLARATION);

    const Token &t = peek(0);
    if (is_inside_obj && t.is_access_modifier()) 
    {
        const Token &t1 = consume();
        node.add_child(parse_token(t1));
    }

    const Token &t1 = consume();
    if (t1.type != TokenType::FUNC_KWD) 
    {
        create_syntax_error("Expected 'func'");
        return node;
    }
    
    // Function name
    node.add_child(parse_identifier()); 
    CHECK_SYNTAX_ERROR();

    const Token &t2 = consume();
    if (t2.type != TokenType::PARENTHESIS_OPEN) 
    {
        create_syntax_error("Expected '('");
        return node; 
    }

    const Token &t3 = peek();

    ASTNode param_list_node(ASTNodeType::PARAMETER_LIST);
    // The function has parameters
    if (t3.type != TokenType::PARENTHESIS_CLOSE) 
    {
        for (;;) 
        {
            param_list_node.add_child(parse_identifier()); 
            CHECK_SYNTAX_ERROR();

            const Token &t4 = consume();
            if (t4.type != TokenType::COLON)
            {
                create_syntax_error("Expected ':'");
                return node;
            }

            param_list_node.add_child(parse_type());       
            CHECK_SYNTAX_ERROR();

            const Token &t5 = consume();
            if (t5.type == TokenType::COMMA)
            {
                continue;
            }
            else if (t5.type == TokenType::PARENTHESIS_CLOSE)
            {
                break;
            }
            else 
            {
                create_syntax_error("Expected ',' or ')'");
                return node;
            }
            
            // node.update_location(t5);
        }
    }
    else {
        consume();
        node.update_location(t3);
    }

    node.add_child(param_list_node);
    CHECK_SYNTAX_ERROR();

    const Token &t4 = consume();
    if (t4.type != TokenType::ARROW)
    {
        create_syntax_error("Expected '->'");
        return node;
    }

    node.add_child(parse_type());
    CHECK_SYNTAX_ERROR();

    node.add_child(parse_statement_block()); 
    CHECK_SYNTAX_ERROR();

    return node;
}

auto Parser::parse_statement_block() -> ASTNode 
{
    ASTNode node(ASTNodeType::STATEMENT_BLOCK);

    const Token &t = consume();
    if (t.type != TokenType::BRACE_OPEN) 
    {
        create_syntax_error("Expected '{'");
        return node;
    }

    node.update_location(t);

    for (;;) 
    {
        const Token &t1 = peek(0);
        if (t1.type == TokenType::BRACE_CLOSE)
        {
            consume();

            node.update_location(t1);

            break;
        } 
        else if (t1.type == TokenType::END_OF_FILE)
        {
            create_syntax_error("Unexpected end of file");
            break;
        } 
        else
        {
            node.add_child(parse_statement());
        }

        if (has_syntax_error_) 
            break;
    }

    return node;
}

auto Parser::parse_statement() -> ASTNode
{
    const Token &t = peek();
    if (t.type == TokenType::IF_KWD)
        return parse_if_statement(false);
    else if (t.type == TokenType::RETURN_KWD)
        return parse_return_statement();
    else if (t.type == TokenType::LET_KWD)
        return parse_variable_declaration(false);
    else
        return parse_simple_statement();
}

auto Parser::parse_simple_statement() -> ASTNode 
{
    const Token &t = peek();
    // Just a semicolon is alright
    if (t.type == TokenType::SEMICOLON) {
        consume();
        return ASTNode{ASTNodeType::ASSIGNMENT};
    }

    ASTNode node = parse_assignment(); 
    CHECK_SYNTAX_ERROR();

    const Token &t1 = consume();
    if (t1.type != TokenType::SEMICOLON)
        create_syntax_error("Expected a semicolon");

    return node;
}

auto Parser::parse_if_statement(bool is_elif) -> ASTNode 
{
    ASTNode node(is_elif ? ASTNodeType::ELIF_BRANCH : ASTNodeType::IF_STATEMENT);

    const Token &t = consume();
    node.update_location(t);

    if (not is_elif and t.type != TokenType::IF_KWD) 
    {
        create_syntax_error("Expected 'if'");
        return node;
    }
    else if (is_elif and t.type != TokenType::ELIF_KWD)
    {
        create_syntax_error("Expected 'elif'");
        return node;
    }

    node.add_child(parse_expression());     
    CHECK_SYNTAX_ERROR();

    node.add_child(parse_statement_block()); 
    CHECK_SYNTAX_ERROR();

    // Let the parent if handle other elif or else branches
    if (is_elif)
        return node;

    for (;;)
    {
        const Token &t2 = peek();
        if (t2.type == TokenType::ELIF_KWD)
        {
            node.add_child(parse_if_statement(true));
            CHECK_SYNTAX_ERROR();
        }
        else if (t2.type == TokenType::ELSE_KWD)
        {
            consume();

            ASTNode else_branch_node{ASTNodeType::ELSE_BRANCH};
            else_branch_node.update_location(t2);

            else_branch_node.add_child(parse_statement_block());
            CHECK_SYNTAX_ERROR();

            node.add_child(else_branch_node);

            break;
        }
        else
        {
            break;
        }
    }

    return node;
}

auto Parser::parse_return_statement() -> ASTNode
{
    ASTNode node(ASTNodeType::RETURN_STATEMENT);
    
    const Token &t = consume();
    node.update_location(t);
    if (t.type != TokenType::RETURN_KWD) 
    {
        create_syntax_error("Expected 'return");
        return node;
    } 

    const Token &t1 = peek(0);
    // void return
    if (t1.type == TokenType::SEMICOLON) 
    {
        consume();
    }
    // value return
    else 
    {
        node.add_child(parse_expression());

        const Token &t2 = consume();
        if (t2.type != TokenType::SEMICOLON) 
        {
            create_syntax_error("Expected a semicolon");
            return node;
        }
    }

    return node;
}

auto Parser::parse_assignment() -> ASTNode
{
    ASTNode node{ASTNodeType::ASSIGNMENT};

    node.add_child(parse_expression_term());
    CHECK_SYNTAX_ERROR();

    const Token &t = peek(0);
    CHECK_SYNTAX_ERROR();
    if (t.type == TokenType::SEMICOLON)
        return node;

    node.add_child(parse_assignment_operator(false));
    CHECK_SYNTAX_ERROR();

    node.add_child(parse_expression());
    CHECK_SYNTAX_ERROR();

    return node;
}

auto Parser::parse_expression() -> ASTNode
{
    ASTNode node(ASTNodeType::EXPRESSION);

    node.add_child(parse_expression_term());
    // The first part of an expression should always be a term
    CHECK_SYNTAX_ERROR();

    // Has multiple terms 
    bool complex_expr = false;
    for (;;)
    {
        const Token &t1 = peek();
        CHECK_SYNTAX_ERROR();
        if (t1.is_binary_operator()) 
        {
            consume();
            complex_expr = true;
            
            ASTNode bin_op(ASTNodeType::BINOP);
            bin_op.set_token(t1);
            node.add_child(bin_op);
        }
        else
            // The expression is finished, there should be no more terms left over
            break;
        
        node.add_child(parse_expression_term());
        // There should always be a term after an operator
        CHECK_SYNTAX_ERROR();
    }

    // Only one term
    if (!complex_expr)
        return node;

    // Assure operator precedence
    while (node.children_.size() > 1) 
    {
        int highest_precedence = 0;
        int highest_precedence_idx = 0;

        for (int i = 1; i < node.children_.size(); i += 2) 
        {
            const Token &tok = node.children_[i].token();
            int op_precedence = tok.get_operator_precedence(); 

            highest_precedence_idx = (op_precedence > highest_precedence) ? i : highest_precedence_idx;
            highest_precedence = std::max(highest_precedence, op_precedence);
        }

        ASTNode op_node = node.children_[highest_precedence_idx];
        // Left operand
        op_node.add_child(node.children_[highest_precedence_idx - 1]);
        // Right operand
        op_node.add_child(node.children_[highest_precedence_idx + 1]);

        // Remove [left, op, right]
        node.children_.erase(
            std::next(node.children_.begin(), highest_precedence_idx - 1), 
            std::next(node.children_.begin(), highest_precedence_idx + 2)
        );

        // Place it where the left operand originally was to leave no empty space
        node.children_.insert(
            std::next(node.children_.begin(), highest_precedence_idx - 1), 
            op_node
        );
    }

    return node;
}

auto Parser::parse_expression_term() -> ASTNode
{
    ASTNode node(ASTNodeType::EXPRESSION_TERM);
    
    for (;;) 
    {
        const Token &t = peek(0);
        CHECK_SYNTAX_ERROR();
        if (not t.is_expression_pre_operator())
            break;
        
        node.add_child(parse_expression_pre_operator());
        CHECK_SYNTAX_ERROR();
    }

    node.add_child(parse_expression_value());
    CHECK_SYNTAX_ERROR();

    for (;;) 
    {
        const Token &t = peek(0);
        CHECK_SYNTAX_ERROR();
        if (not t.is_expression_post_operator()) 
            break;
        
        node.add_child(parse_expression_post_operator());
        CHECK_SYNTAX_ERROR();
    }

    return node;
}

auto Parser::parse_assignment_operator(bool allow_compound_ops) -> ASTNode
{
    ASTNode node{ASTNodeType::BINOP};

    const Token &op = consume();
    CHECK_SYNTAX_ERROR();
    if (not allow_compound_ops and op.type != TokenType::EQUAL) 
    {
        create_syntax_error("Expected '='");
        return node;
    }
    else if (not op.is_assignment_operator())
    {
        create_syntax_error("Expected an assignment operator.");
        return node;
    }

    node.set_token(op);
    return node;
}

auto Parser::parse_expression_pre_operator() -> ASTNode
{
    ASTNode node(ASTNodeType::EXPRESSION_PREOP);

    const Token &t = consume();
    CHECK_SYNTAX_ERROR();
    node.set_token(t);

    return node;
}

auto Parser::parse_expression_value() -> ASTNode
{
    ASTNode node(ASTNodeType::EXPRESSION_VALUE);

    const Token &t = peek();
    CHECK_SYNTAX_ERROR();
    if (t.is_constant_value()) 
    {
        node.add_child(parse_constant()); 
        CHECK_SYNTAX_ERROR();
    } 
    else if (t.type == TokenType::ID) 
    {
        if (is_function_call()) 
        {
            node.add_child(parse_function_call()); 
            CHECK_SYNTAX_ERROR();
        } 
        else 
        {
            node.add_child(parse_identifier()); 
            CHECK_SYNTAX_ERROR();
        }
    }
    else if (t.type == TokenType::NEW_KWD) 
    {
        node.add_child(parse_construct_call()); 
        CHECK_SYNTAX_ERROR();
    }
    else if (t.type == TokenType::PARENTHESIS_OPEN) 
    {
        consume();
        node.add_child(parse_expression()); 
        CHECK_SYNTAX_ERROR();

        const Token &t1 = consume();
        CHECK_SYNTAX_ERROR();
        if (t1.type != TokenType::PARENTHESIS_CLOSE) 
        {
            create_syntax_error("Expected token '('");
            return node;
        }
    }
    else 
    {
        consume();
        create_syntax_error("Expected an expression value");
    }

    return node;
}

auto Parser::parse_expression_post_operator() -> ASTNode
{
    ASTNode node(ASTNodeType::EXPRESSION_POSTOP);

    const Token &t = peek();
    CHECK_SYNTAX_ERROR();
    if (t.type == TokenType::DOT) 
    {
        for (;;)
        {
            const Token &t1 = peek();
            CHECK_SYNTAX_ERROR();
            if (t1.type == TokenType::DOT)
                consume();
            else
                break;

            if (is_function_call())
            {
                node.add_child(parse_function_call()); 
                CHECK_SYNTAX_ERROR();
            }
            else
            {
                node.add_child(parse_identifier());
                CHECK_SYNTAX_ERROR();
            }
        }
    }
    else
    {
        node.add_child(parse_token(t));
        consume();
    }

    return node;
}

auto Parser::parse_token(const Token &t) -> ASTNode
{
    ASTNode node(ASTNodeType::TOKEN);
    node.set_token(t);
    return node;
}

auto Parser::parse_identifier() -> ASTNode
{
    ASTNode node(ASTNodeType::IDENTIFIER);

    const Token &t = consume();
    CHECK_SYNTAX_ERROR();
    if (t.type != TokenType::ID) 
    {
        create_syntax_error("Expected an identifier");
        return node;
    }

    node.set_token(t);
    
    return node;
}

auto Parser::parse_function_call() -> ASTNode
{
    ASTNode node(ASTNodeType::FUNCTION_CALL);

    node.add_child(parse_identifier()); 
    CHECK_SYNTAX_ERROR();

    node.add_child(parse_argument_list()); 
    CHECK_SYNTAX_ERROR();

    return node;
}

auto Parser::parse_argument_list() -> ASTNode 
{
    ASTNode node(ASTNodeType::ARGUMENT_LIST);

    const Token &t = consume();
    CHECK_SYNTAX_ERROR();
    if (t.type != TokenType::PARENTHESIS_OPEN) 
    {
        create_syntax_error("Expected token '('");
        return node;
    }

    node.update_location(t);

    const Token &t1 = peek(0); 
    CHECK_SYNTAX_ERROR();
    if (t1.type != TokenType::PARENTHESIS_CLOSE) 
    {
        for (;;) 
        {
            node.add_child(parse_expression()); 
            CHECK_SYNTAX_ERROR();

            const Token &t3 = consume();
            CHECK_SYNTAX_ERROR();
            if (t3.type == TokenType::COMMA)
                continue;
            else if (t3.type == TokenType::PARENTHESIS_CLOSE)
                break;
            else {
                create_syntax_error("Expected ')' or ','");
                break;
            }

            node.update_location(t3);
        }
    }
    else 
    {
        node.update_location(t1);
        consume();
    }

    return node;
}

auto Parser::parse_constant() -> ASTNode
{
    ASTNode node(ASTNodeType::CONSTANT);

    const Token &t = consume();
    CHECK_SYNTAX_ERROR();
    if (not t.is_constant_value()) 
    {
        create_syntax_error("Expected a constant value");
        return node;
    }
    
    node.set_token(t);
    
    return node;
}

auto Parser::parse_construct_call() -> ASTNode
{
    ASTNode node(ASTNodeType::CONSTRUCT_CALL);

    const Token &t = consume();
    CHECK_SYNTAX_ERROR();
    if (t.type != TokenType::NEW_KWD) 
    {
        create_syntax_error("Expected 'new'");
        return node;
    }

    node.add_child(parse_identifier()); 
    CHECK_SYNTAX_ERROR();

    node.add_child(parse_argument_list()); 
    CHECK_SYNTAX_ERROR();

    return node;
}

auto Parser::parse_type() -> ASTNode
{
    ASTNode node(ASTNodeType::DATA_TYPE);

    const Token &t = consume();
    CHECK_SYNTAX_ERROR();
    if (not t.is_data_type()) 
    {
        create_syntax_error("Expected a data type");
        return node;
    }
    
    node.set_token(t);
    
    return node;
}

} // namespace NCSC

