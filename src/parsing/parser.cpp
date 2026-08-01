#include "parsing/parser.hpp"

#include <print>
#include <iostream>

#include "parsing/ast_node/all_ast_nodes.hpp"


#define CHECK_SYNTAX_ERROR_RET_VALUE(ret) if (has_syntax_error_) return ret
#define CHECK_SYNTAX_ERROR() CHECK_SYNTAX_ERROR_RET_VALUE(node)

#define SAFE_CONSUME() consume(); CHECK_SYNTAX_ERROR()

#define SAFE_PEEK() peek(); CHECK_SYNTAX_ERROR()
#define SAFE_PEEK_AMOUNT(amount) peek(amount); CHECK_SYNTAX_ERROR()
#define SAFE_PEEK_RET_VALUE(amount, ret) peek(amount); CHECK_SYNTAX_ERROR_RET_VALUE(ret)


namespace NCSC
{
    
Parser::Parser(const std::vector<Token> &tokens, std::shared_ptr<ScriptSource> script_source)
    : tokens_{tokens}
    , has_syntax_error_{false}
    , curr_token_idx_{0zu}
    , script_source_{script_source}
{}

auto Parser::parse() -> std::shared_ptr<ASTNode>
{
    auto root = std::make_shared<ASTNode>(ASTNodeType::ROOT);

    root->add_child(parse_declaration_body(false));

    std::println(std::cerr, "{}", root->ast_string());
    for (const auto &err: syntax_errors_)
        std::println(std::cerr, "{}", err.get_error_message_with_source());

    return root;
}

auto Parser::has_syntax_errors() const -> bool
{
    return not syntax_errors_.empty();
}

auto Parser::get_syntax_errors() const -> const std::vector<Error> &
{
    return syntax_errors_;
}

auto Parser::consume() -> const Token &
{
    const Token &t = tokens_[curr_token_idx_++];

    if (t.type == TokenType::END_OF_FILE) 
    {
        create_syntax_error(ERR_UNEXPECTED_EOF, t);
        return t;
    }

    return t;
}

auto Parser::peek(std::size_t amount) -> const Token &
{
    if (curr_token_idx_ + amount >= tokens_.size()) 
    {
        create_syntax_error(ERR_UNEXPECTED_EOF, tokens_[tokens_.size() - 1]);
        return INVALID_TOKEN;
    }

    const Token &t = tokens_[curr_token_idx_ + amount];
    if (t.type == TokenType::END_OF_FILE) 
    {
        create_syntax_error(ERR_UNEXPECTED_EOF, t);
        return t;
    }

    return tokens_[curr_token_idx_ + amount];
}

auto Parser::is_function_call() -> bool 
{
    const Token &t = SAFE_PEEK_RET_VALUE(0, false);

    if (t.type != TokenType::ID)
        return false;
    
    const Token &t1 = SAFE_PEEK_RET_VALUE(1, false);
    return t1.type == TokenType::PARENTHESIS_OPEN;
}

auto Parser::parse_declaration_body(bool is_inside_obj) -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::DECLARATION_BODY);

    if (is_inside_obj)
    {
        const Token &t1 = SAFE_CONSUME();
        if (t1.type != TokenType::BRACE_OPEN)
        {
            create_syntax_error(ERR_EXPECTED_TOKEN, t1, '{');
            return node;
        }
    }

    while (curr_token_idx_ < tokens_.size() and not has_syntax_error_)
    {
        const Token &curr_tok = tokens_[curr_token_idx_];
        
        switch (curr_tok.type)
        {
            case TokenType::END_OF_FILE:
                if (not is_inside_obj)
                    return node;
                
                create_syntax_error(ERR_UNEXPECTED_EOF, curr_tok);
                return node;
            
            case TokenType::VAR_KWD:
                node->add_child(parse_variable_declaration(is_inside_obj));
                continue;

            case TokenType::FUNC_KWD:
                node->add_child(parse_function_declaration(is_inside_obj));
                continue;

            case TokenType::OBJ_KWD:
                node->add_child(parse_object_declaration(is_inside_obj));
                continue;

            case TokenType::SEMICOLON:
                node->update_location(curr_tok);
                SAFE_CONSUME();

                continue;

            default:
                break;
        }

        if (is_inside_obj and curr_tok.is_access_modifier())
        {
            const Token &t = SAFE_PEEK_AMOUNT(1);

            switch (t.type)
            {
                case TokenType::VAR_KWD:
                    node->add_child(parse_variable_declaration(is_inside_obj));
                    continue;

                case TokenType::FUNC_KWD:
                    node->add_child(parse_function_declaration(is_inside_obj));
                    continue;

                case TokenType::OBJ_KWD:
                    node->add_child(parse_object_declaration(is_inside_obj));
                    continue;
                
                default:
                    SAFE_CONSUME();
                    create_syntax_error(ERR_EXPECTED_DECLARATION, t);
                    break;
            }
        }
        else if (is_inside_obj and curr_tok.type == TokenType::BRACE_CLOSE)
        {
            node->update_location(curr_tok);
            SAFE_CONSUME();
            
            break;
        }
        else
        {
            SAFE_CONSUME();
            create_syntax_error(ERR_UNEXPECTED_TOKEN, curr_tok);
        }

        has_syntax_error_ = false;
    }

    return node;
}

auto Parser::parse_variable_declaration(bool is_inside_obj) -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<Parsing::VarDeclASTNode>(ASTNodeType::VARIABLE_DECLARATION);
    node->set_metadata("is_member_var", is_inside_obj);

    const Token &t4 = SAFE_PEEK();
    if (is_inside_obj)
    {
        if (t4.is_access_modifier())
        {
            SAFE_CONSUME();

            node->add_child(parse_token(t4));
            CHECK_SYNTAX_ERROR();
        }
        // Assume private
        else 
        {
            node->add_child(parse_token(Token{TokenType::PRIVATE_KWD}));
            CHECK_SYNTAX_ERROR();
        }
    }

    const Token &t3 = SAFE_CONSUME();
    CHECK_SYNTAX_ERROR();
    if (t3.type != TokenType::VAR_KWD) 
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t3, "var");
        return node;    
    }

    node->add_child(parse_identifier());
    CHECK_SYNTAX_ERROR();

    const Token &t = SAFE_CONSUME();
    if (t.type != TokenType::COLON)
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t, ':');
        return node;
    }

    node->add_child(parse_type());
    CHECK_SYNTAX_ERROR();

    const Token &t1 = SAFE_PEEK();
    if (t1.type != TokenType::SEMICOLON) 
    {
        const Token &t2 = SAFE_CONSUME();

        if (t2.type != TokenType::EQUAL)
        {
            create_syntax_error(ERR_EXPECTED_TOKEN_OR_TOKEN, t2, '=', ';');
            return node;
        }

        node->add_child(parse_expression());
        CHECK_SYNTAX_ERROR();
    }
    else
    {
        // Empty expression
        node->add_child(std::make_shared<ASTNode>(ASTNodeType::EXPRESSION));
        CHECK_SYNTAX_ERROR();
    }

    const Token &t2 = SAFE_CONSUME();
    if (t2.type != TokenType::SEMICOLON)
        create_syntax_error(ERR_EXPECTED_SEMICOLON, t2);

    return node;
}

auto Parser::parse_function_declaration(bool is_inside_obj) -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<Parsing::FuncDeclASTNode>(ASTNodeType::FUNCTION_DECLARATION);
    node->set_metadata("is_method", is_inside_obj);

    const Token &t = SAFE_PEEK();
    if (is_inside_obj and t.is_access_modifier()) 
    {
        if (t.is_access_modifier())
        {
            const Token &t1 = SAFE_CONSUME();
            node->add_child(parse_token(t1));
            CHECK_SYNTAX_ERROR();
        }
        else
        {
            node->add_child(parse_token(Token{TokenType::VOID_KWD}));
            CHECK_SYNTAX_ERROR();
        }
    }

    const Token &t1 = SAFE_CONSUME();
    if (t1.type != TokenType::FUNC_KWD) 
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t1, "func");
        return node;
    }
    
    // Function name
    node->add_child(parse_identifier()); 
    CHECK_SYNTAX_ERROR();

    const Token &t2 = SAFE_CONSUME();
    if (t2.type != TokenType::PARENTHESIS_OPEN) 
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t2, '(');
        return node; 
    }

    const Token &t3 = SAFE_PEEK();

    auto param_list_node = std::make_shared<ASTNode>(ASTNodeType::PARAMETER_LIST);
    // The function has parameters
    if (t3.type != TokenType::PARENTHESIS_CLOSE)
    {
        if (t3.type != TokenType::ID)
        {
            create_syntax_error(ERR_EXPECTED_TOKEN, t3, ')');
            return node;
        }

        for (;;) 
        {
            param_list_node->add_child(parse_identifier()); 
            CHECK_SYNTAX_ERROR();

            const Token &t4 = SAFE_CONSUME();
            if (t4.type != TokenType::COLON)
            {
                create_syntax_error(ERR_EXPECTED_TOKEN, t4, ':');
                return node;
            }

            param_list_node->add_child(parse_type());
            CHECK_SYNTAX_ERROR();

            const Token &t5 = SAFE_CONSUME();
            if (t5.type == TokenType::COMMA)
            {
                continue;
            }
            else if (t5.type == TokenType::PARENTHESIS_CLOSE)
            {
                node->update_location(t5);
                break;
            }
            else 
            {
                create_syntax_error(ERR_EXPECTED_TOKEN_OR_TOKEN, t5, ',', ')');
                return node;
            }
        }
    }
    else 
    {
        SAFE_CONSUME();
        node->update_location(t3);
    }

    node->add_child(param_list_node);
    CHECK_SYNTAX_ERROR();

    const Token &t4 = SAFE_PEEK();
    if (t4.type == TokenType::ARROW)
    {
        SAFE_CONSUME();

        node->add_child(parse_type());
        CHECK_SYNTAX_ERROR();
    }
    else
    {
        // Assume void
        node->add_child(parse_type(Token{TokenType::VOID_KWD}));
        CHECK_SYNTAX_ERROR();
    }


    node->add_child(parse_statement_block()); 
    CHECK_SYNTAX_ERROR();

    return node;
}

auto Parser::parse_object_declaration(bool is_inside_obj) -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<Parsing::ObjDeclASTNode>(ASTNodeType::OBJ_DECLARATION);

    const Token &t = SAFE_CONSUME();
    if (t.type != TokenType::OBJ_KWD)
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t, "obj");
        return node;
    }

    node->add_child(parse_identifier());
    CHECK_SYNTAX_ERROR();

    node->add_child(parse_declaration_body(true));
    CHECK_SYNTAX_ERROR();
    
    return node;
}


auto Parser::parse_statement_block() -> std::shared_ptr<ASTNode> 
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::STATEMENT_BLOCK);

    const Token &t = SAFE_CONSUME();
    if (t.type != TokenType::BRACE_OPEN) 
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t, '{');
        return node;
    }

    node->update_location(t);

    for (;;) 
    {
        const Token &t1 = SAFE_PEEK();
        if (t1.type == TokenType::BRACE_CLOSE)
        {
            SAFE_CONSUME();

            node->update_location(t1);

            break;
        }
        else
        {
            node->add_child(parse_statement());
            CHECK_SYNTAX_ERROR();
        }

        if (has_syntax_error_) 
            break;
    }

    return node;
}

auto Parser::parse_statement() -> std::shared_ptr<ASTNode>
{
    const Token &t = SAFE_PEEK_RET_VALUE(0, parse_simple_statement());
    if (t.type == TokenType::IF_KWD)
        return parse_if_statement(false);
    else if (t.type == TokenType::RETURN_KWD)
        return parse_return_statement();
    else if (t.type == TokenType::VAR_KWD)
        return parse_variable_declaration(false);
    else
        return parse_simple_statement();
}

auto Parser::parse_simple_statement() -> std::shared_ptr<ASTNode> 
{
    const Token &t = SAFE_PEEK_RET_VALUE(0, std::make_shared<ASTNode>(ASTNodeType::ASSIGNMENT));
    // Just a semicolon is alright
    if (t.type == TokenType::SEMICOLON) 
    {
        consume();
        return std::make_shared<ASTNode>(ASTNodeType::ASSIGNMENT);
    }

    auto node = parse_assignment(); 
    CHECK_SYNTAX_ERROR();

    const Token &t1 = SAFE_CONSUME();
    if (t1.type != TokenType::SEMICOLON)
        create_syntax_error(ERR_EXPECTED_SEMICOLON, t1);

    return node;
}

auto Parser::parse_if_statement(bool is_elif) -> std::shared_ptr<ASTNode> 
{
    auto node = std::make_shared<ASTNode>(is_elif ? ASTNodeType::ELIF_BRANCH : ASTNodeType::IF_STATEMENT);

    const Token &t = SAFE_CONSUME();
    node->update_location(t);

    if (not is_elif and t.type != TokenType::IF_KWD) 
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t, "if");
        return node;
    }
    else if (is_elif and t.type != TokenType::ELIF_KWD)
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t, "elif");
        return node;
    }

    node->add_child(parse_expression());     
    CHECK_SYNTAX_ERROR();

    node->add_child(parse_statement_block()); 
    CHECK_SYNTAX_ERROR();

    // Let the parent if handle other elif or else branches
    if (is_elif)
        return node;

    for (;;)
    {
        const Token &t2 = SAFE_PEEK();
        if (t2.type == TokenType::ELIF_KWD)
        {
            node->add_child(parse_if_statement(true));
            CHECK_SYNTAX_ERROR();
        }
        else if (t2.type == TokenType::ELSE_KWD)
        {
            SAFE_CONSUME();

            auto else_branch_node = std::make_shared<ASTNode>(ASTNodeType::ELSE_BRANCH);
            else_branch_node->update_location(t2);

            else_branch_node->add_child(parse_statement_block());
            CHECK_SYNTAX_ERROR();

            node->add_child(else_branch_node);

            break;
        }
        else
        {
            break;
        }
    }

    return node;
}

auto Parser::parse_return_statement() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::RETURN_STATEMENT);
    
    const Token &t = SAFE_CONSUME();
    node->update_location(t);
    if (t.type != TokenType::RETURN_KWD) 
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t, "return");
        return node;
    } 

    const Token &t1 = SAFE_PEEK();
    // void return
    if (t1.type == TokenType::SEMICOLON) 
    {
        SAFE_CONSUME();
    }
    // value return
    else 
    {
        node->add_child(parse_expression());

        const Token &t2 = SAFE_CONSUME();
        if (t2.type != TokenType::SEMICOLON) 
        {
            create_syntax_error(ERR_EXPECTED_SEMICOLON, t2);
            return node;
        }
    }

    return node;
}

auto Parser::parse_assignment() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::ASSIGNMENT);

    node->add_child(parse_expression_term());
    CHECK_SYNTAX_ERROR();

    const Token &t = SAFE_PEEK();
    if (t.type == TokenType::SEMICOLON)
    {
        return node;
    }
    else if (not t.is_assignment_operator())
    {
        create_syntax_error(ERR_UNEXPECTED_TOKEN, t);
        return node;
    }

    node->add_child(parse_assignment_operator(false));
    CHECK_SYNTAX_ERROR();

    node->add_child(parse_expression());
    CHECK_SYNTAX_ERROR();

    return node;
}

auto Parser::parse_expression() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::EXPRESSION);

    node->add_child(parse_expression_term());
    // The first part of an expression should always be a term
    CHECK_SYNTAX_ERROR();

    // Has multiple terms 
    bool complex_expr = false;
    for (;;)
    {
        const Token &t1 = SAFE_PEEK();
        if (t1.is_binary_operator()) 
        {
            SAFE_CONSUME();
            complex_expr = true;
            
            auto bin_op = std::make_shared<ASTNode>(ASTNodeType::BINOP);
            bin_op->set_token(t1);
            node->add_child(bin_op);
        }
        else
            // The expression is finished, there should be no more terms left over
            break;
        
        node->add_child(parse_expression_term());
        // There should always be a term after an operator
        CHECK_SYNTAX_ERROR();
    }

    // Only one term
    if (!complex_expr)
        return node;

    // Assure operator precedence
    while (node->children_.size() > 1) 
    {
        int highest_precedence = 0;
        int highest_precedence_idx = 0;

        for (int i = 1; i < node->children_.size(); i += 2) 
        {
            const Token &tok = node->children_[i]->token();
            int op_precedence = tok.get_operator_precedence(); 

            highest_precedence_idx = (op_precedence > highest_precedence) ? i : highest_precedence_idx;
            highest_precedence = std::max(highest_precedence, op_precedence);
        }

        auto op_node = node->children_[highest_precedence_idx];
        // Left operand
        op_node->add_child(node->children_[highest_precedence_idx - 1]);
        // Right operand
        op_node->add_child(node->children_[highest_precedence_idx + 1]);

        // Remove [left, op, right]
        node->children_.erase(
            std::next(node->children_.begin(), highest_precedence_idx - 1), 
            std::next(node->children_.begin(), highest_precedence_idx + 2)
        );

        // Place it where the left operand originally was to leave no empty space
        node->children_.insert(
            std::next(node->children_.begin(), highest_precedence_idx - 1), 
            op_node
        );
    }

    return node;
}

auto Parser::parse_expression_term() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::EXPRESSION_TERM);
    
    for (;;) 
    {
        const Token &t = SAFE_PEEK();
        if (not t.is_expression_pre_operator())
            break;
        
        node->add_child(parse_expression_pre_operator());
        CHECK_SYNTAX_ERROR();
    }

    node->add_child(parse_expression_value());
    CHECK_SYNTAX_ERROR();

    for (;;) 
    {
        const Token &t = SAFE_PEEK();
        if (not t.is_expression_post_operator()) 
            break;
        
        node->add_child(parse_expression_post_operator());
        CHECK_SYNTAX_ERROR();
    }

    return node;
}

auto Parser::parse_assignment_operator(bool allow_compound_ops) -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::BINOP);

    const Token &op = SAFE_CONSUME();
    if (not allow_compound_ops and op.type != TokenType::EQUAL) 
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, op, '=');
        return node;
    }
    else if (not op.is_assignment_operator())
    {
        create_syntax_error(ERR_EXPECTED_ASSIGN_OP, op);
        return node;
    }

    node->set_token(op);
    return node;
}

auto Parser::parse_expression_pre_operator() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::EXPRESSION_PREOP);

    const Token &t = SAFE_CONSUME();
    if (not t.is_expression_pre_operator())
        return node;
    
    node->set_token(t);

    return node;
}

auto Parser::parse_expression_value() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::EXPRESSION_VALUE);

    const Token &t = SAFE_PEEK();
    if (t.is_constant_value()) 
    {
        node->add_child(parse_constant()); 
        CHECK_SYNTAX_ERROR();
    } 
    else if (t.type == TokenType::ID) 
    {
        if (is_function_call()) 
        {
            node->add_child(parse_function_call()); 
            CHECK_SYNTAX_ERROR();
        } 
        else 
        {
            node->add_child(parse_identifier()); 
            CHECK_SYNTAX_ERROR();
        }
    }
    else if (t.type == TokenType::NEW_KWD) 
    {
        node->add_child(parse_construct_call()); 
        CHECK_SYNTAX_ERROR();
    }
    else if (t.type == TokenType::PARENTHESIS_OPEN) 
    {
        SAFE_CONSUME();

        node->add_child(parse_expression()); 
        CHECK_SYNTAX_ERROR();

        const Token &t1 = SAFE_CONSUME();
        if (t1.type != TokenType::PARENTHESIS_CLOSE) 
        {
            create_syntax_error(ERR_EXPECTED_TOKEN, t1, ')');
            return node;
        }
    }
    else 
    {
        SAFE_CONSUME();
        create_syntax_error(ERR_EXPECTED_EXPR_VALUE, t);
    }

    return node;
}

auto Parser::parse_expression_post_operator() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::EXPRESSION_POSTOP);

    const Token &t = SAFE_PEEK();
    if (t.type == TokenType::DOT) 
    {
        for (;;)
        {
            const Token &t1 = SAFE_PEEK();
            if (t1.type == TokenType::DOT)
            {
                SAFE_CONSUME();
            }
            else
            {
                break;
            }

            if (is_function_call())
            {
                node->add_child(parse_function_call()); 
                CHECK_SYNTAX_ERROR();
            }
            else
            {
                node->add_child(parse_identifier());
                CHECK_SYNTAX_ERROR();
            }
        }
    }
    else
    {
        node->add_child(parse_token(t));
        SAFE_CONSUME();
    }

    return node;
}

auto Parser::parse_token(const Token &t) -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::TOKEN);
    node->set_token(t);
    return node;
}

auto Parser::parse_identifier() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::IDENTIFIER);

    const Token &t = SAFE_CONSUME();
    if (t.type != TokenType::ID) 
    {
        create_syntax_error(ERR_EXPECTED_IDENTIFIER, t);
        return node;
    }

    node->set_token(t);
    
    return node;
}

auto Parser::parse_function_call() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::FUNCTION_CALL);

    node->add_child(parse_identifier()); 
    CHECK_SYNTAX_ERROR();

    node->add_child(parse_argument_list()); 
    CHECK_SYNTAX_ERROR();

    return node;
}

auto Parser::parse_argument_list() -> std::shared_ptr<ASTNode> 
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::ARGUMENT_LIST);

    const Token &t = SAFE_CONSUME();
    if (t.type != TokenType::PARENTHESIS_OPEN) 
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t, '(');
        return node;
    }

    node->update_location(t);

    const Token &t1 = SAFE_PEEK(); 
    if (t1.type != TokenType::PARENTHESIS_CLOSE) 
    {
        for (;;) 
        {
            node->add_child(parse_expression()); 
            CHECK_SYNTAX_ERROR();

            const Token &t3 = SAFE_CONSUME();
            if (t3.type == TokenType::COMMA)
                continue;
            else if (t3.type == TokenType::PARENTHESIS_CLOSE)
                break;
            else {
                create_syntax_error(ERR_EXPECTED_TOKEN_OR_TOKEN, t3, ',', ')');
                break;
            }

            node->update_location(t3);
        }
    }
    else 
    {
        node->update_location(t1);
        SAFE_CONSUME();
    }

    return node;
}

auto Parser::parse_constant() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::CONSTANT);

    const Token &t = SAFE_CONSUME();
    if (not t.is_constant_value()) 
    {
        create_syntax_error(ERR_EXPECTED_CONSTANT_VAL, t);
        return node;
    }
    
    node->set_token(t);
    
    return node;
}

auto Parser::parse_construct_call() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::CONSTRUCT_CALL);

    const Token &t = SAFE_CONSUME();
    if (t.type != TokenType::NEW_KWD) 
    {
        create_syntax_error(ERR_EXPECTED_TOKEN, t, "new");
        return node;
    }

    node->add_child(parse_identifier()); 
    CHECK_SYNTAX_ERROR();

    node->add_child(parse_argument_list()); 
    CHECK_SYNTAX_ERROR();

    return node;
}

auto Parser::parse_type() -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::DATA_TYPE);

    const Token &t = SAFE_CONSUME();
    if (not t.is_data_type()) 
    {
        create_syntax_error(ERR_EXPECTED_DATA_TYPE, t);
        return node;
    }
    
    node->set_token(t);
    
    return node;
}

auto Parser::parse_type(const Token &t) -> std::shared_ptr<ASTNode>
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::DATA_TYPE);
    node->set_token(t);
    return node;
}

} // namespace NCSC
