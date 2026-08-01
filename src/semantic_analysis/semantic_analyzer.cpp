#include "semantic_analysis/semantic_analyzer.hpp"

#include "parsing/ast_node/all_ast_nodes.hpp"


namespace NCSC
{
    
using namespace SemanticAnalysis;

SemanticAnalyzer::SemanticAnalyzer(std::shared_ptr<ASTNode> root, std::shared_ptr<ScriptSource> script_source)
    : root_node_{root}
    , script_source_{script_source}
{}

auto SemanticAnalyzer::do_analysis() -> std::shared_ptr<ASTNode>
{
    init_root_scope();

    first_pass();
    second_pass();
    third_pass();
    fourth_pass();

    return root_node_;
}

auto SemanticAnalyzer::first_pass() -> void
{
    const auto &declaration_body = root_node_->children()[0];
    for (const auto &declaration : declaration_body->children())
    {
        ASTNodeType decl_type = declaration->type();

        switch (decl_type)
        {
            case ASTNodeType::FUNCTION_DECLARATION:
            {
                auto func_decl = std::dynamic_pointer_cast<Parsing::FuncDeclASTNode>(declaration);
                const auto &name_node = func_decl->children()[0];
                const std::string &function_name = name_node->token().value;

                if (is_symbol_defined_elsewhere(name_node))
                    return;

                DeclData decl_data{};
                decl_data.decl_node = func_decl;
                decl_data.type = DeclData::Type::FUNCTION;

                func_decl->func_name = function_name;

                const auto &param_node = func_decl->children()[1];
                const auto &params = param_node->children();

                if (params.size() > 0)
                    func_decl->func_params.reserve(params.size() / 2);

                for (std::size_t i = 0; i < params.size(); i += 2)
                {
                    const auto &name_node = params[i];
                    const auto &type_node = params[i + 1];
                
                    const std::string &param_name = name_node->token().value;
                    ValueType param_type = value_type_from_node(type_node); 
                    
                    func_decl->func_params.push_back({ param_name, param_type });
                }

                const auto &return_node = func_decl->children()[2];
                ValueType return_type = value_type_from_node(return_node);
                func_decl->func_return_type = return_type;

                curr_scope_->add_declaration(function_name, decl_data);

                continue;
            }
            // Global variable
            case ASTNodeType::VARIABLE_DECLARATION:
            {
                auto var_decl = std::dynamic_pointer_cast<Parsing::VarDeclASTNode>(declaration);
                const auto &name_node = declaration->children()[0];
                const std::string &var_name = name_node->token().value;

                if (is_symbol_defined_elsewhere(name_node))
                    return;

                DeclData decl_data{};
                decl_data.decl_node = var_decl;
                decl_data.type = DeclData::Type::VARIABLE;

                var_decl->var_name = var_name;

                const auto &type_node = declaration->children()[1];
                ValueType var_type = value_type_from_node(type_node);

                var_decl->var_type = var_type;

                curr_scope_->add_declaration(var_name, decl_data);

                break;
            }
            case ASTNodeType::OBJ_DECLARATION:
            {
                auto obj_decl = std::dynamic_pointer_cast<Parsing::ObjDeclASTNode>(declaration);
                const auto &name_node = declaration->children()[0];
                const std::string &var_name = name_node->token().value;

                if (is_symbol_defined_elsewhere(name_node))
                    return;

                DeclData decl_data{};
                decl_data.decl_node = obj_decl;
                decl_data.type = DeclData::Type::OBJECT;

                obj_decl->obj_name = var_name;

                isize_t idx = curr_scope_->add_declaration(var_name, decl_data);
                
                obj_decl->obj_type = make_object_vtype(idx);

                break;
            }
        }
    }
}

auto SemanticAnalyzer::second_pass() -> void
{

}

auto SemanticAnalyzer::third_pass() -> void
{

}

auto SemanticAnalyzer::fourth_pass() -> void
{

}

auto SemanticAnalyzer::init_root_scope() -> void
{
    root_scope_ = std::make_shared<Scope>();
    curr_scope_ = root_scope_;
}

auto SemanticAnalyzer::enter_new_scope() -> void
{
    auto new_scope = std::make_shared<Scope>();
    new_scope->set_parent(curr_scope_);
    
    curr_scope_ = new_scope;
}

auto SemanticAnalyzer::exit_scope() -> void
{
    curr_scope_ = curr_scope_->get_parent();
}

auto SemanticAnalyzer::is_symbol_defined_elsewhere(const std::shared_ptr<ASTNode> &identifer) -> bool
{
    const std::string &name = identifer->token().value;
    auto decl_data = curr_scope_->get_declaration(name);
    if (decl_data)
    {
        error(ERR_ALREADY_DEFINED, identifer->location(), name);
        error(INFO_DEFINED_HERE, decl_data->decl_node->location(), name);

        return true;
    }

    return false;
}

auto SemanticAnalyzer::value_type_from_node(const std::shared_ptr<ASTNode> &type_node) -> ValueType
{
    if (type_node->type() != ASTNodeType::TOKEN and type_node->type() != ASTNodeType::DATA_TYPE)
        return ValueType::ERROR_TYPE;

    const Token &tok = type_node->token();
    switch (tok.type) 
    {
        case TokenType::VOID_KWD:       return ValueType::VOID;

        case TokenType::INT8_KWD:       return ValueType::INT8;
        case TokenType::INT16_KWD:      return ValueType::INT16;
        case TokenType::INT32_KWD:      return ValueType::INT32;
        case TokenType::INT64_KWD:      return ValueType::INT64;

        case TokenType::UINT8_KWD:      return ValueType::UINT8;
        case TokenType::UINT16_KWD:     return ValueType::UINT16;
        case TokenType::UINT32_KWD:     return ValueType::UINT32;
        case TokenType::UINT64_KWD:     return ValueType::UINT64;
        
        case TokenType::FLOAT32_KWD:    return ValueType::FLOAT32;
        case TokenType::FLOAT64_KWD:    return ValueType::FLOAT64;
        
        case TokenType::BOOL_KWD:       return ValueType::BOOL;

        default:                        break;
    }

    auto decl_data = curr_scope_->get_declaration(tok.value);
    if (decl_data and decl_data->type == DeclData::Type::OBJECT)
    {
        auto obj_node = std::dynamic_pointer_cast<Parsing::ObjDeclASTNode>(decl_data->decl_node);
        return obj_node->obj_type;
    }

    error(ERR_NOT_A_TYPE, type_node->location(), tok.to_string());

    return ValueType::ERROR_TYPE;
}

} // namespace NCSC
