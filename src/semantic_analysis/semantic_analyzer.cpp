#include "semantic_analysis/semantic_analyzer.hpp"


namespace NCSC
{
    
using namespace SemanticAnalysis;

SemanticAnalyzer::SemanticAnalyzer(const ASTNode &root, std::shared_ptr<ScriptSource> script_source)
    : root_node_{root}
    , script_source_{script_source}
{}

auto SemanticAnalyzer::do_analysis() -> const ASTNode &
{
    root_scope_ = std::make_shared<Scope>();
    curr_scope_ = root_scope_;

    first_pass();
    second_pass();
    third_pass();
    fourth_pass();

    return root_node_;
}

auto SemanticAnalyzer::first_pass() -> void
{
    const auto &declaration_body = root_node_.children()[0];
    for (const auto &declaration : declaration_body.children())
    {
        ASTNodeType decl_type = declaration.type();

        switch (decl_type)
        {
            case ASTNodeType::FUNCTION_DECLARATION:
            {
                const auto &name_node = declaration.children()[0];
                const std::string &function_name = name_node.token().value;

                if (is_symbol_defined_elsewhere(name_node))
                    return;

                auto function_data = std::make_shared<FunctionDeclData>();
                function_data->name = function_name;
                function_data->decl_node = &name_node;

                const auto &param_node = declaration.children()[1];
                const auto &params = param_node.children();

                if (params.size() > 0)
                    function_data->params.reserve(params.size() / 2);

                for (std::size_t i = 0; i < params.size(); i += 2)
                {
                    const auto &name_node = params[i];
                    const auto &type_node = params[i + 1];
                
                    const std::string &param_name = name_node.token().value;
                    ValueType param_type = value_type_from_node(type_node); 

                    VarDeclData param{};
                    param.name = param_name;
                    param.type = param_type;
                    
                    function_data->params.push_back(param);
                }

                const auto &return_node = declaration.children()[2];
                ValueType return_type = value_type_from_node(return_node);
                function_data->return_type = return_type;

                curr_scope_->add_declaration(function_name, function_data);

                continue;
            }
            // Global variable
            case ASTNodeType::VARIABLE_DECLARATION:
               break;
            case ASTNodeType::OBJ_DECLARATION:
                break;
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

auto SemanticAnalyzer::is_symbol_defined_elsewhere(const ASTNode &identifer) -> bool
{
    const std::string &name = identifer.token().value;
    auto decl_data = curr_scope_->get_declaration(name);
    if (decl_data)
    {
        error(ERR_ALREADY_DEFINED, identifer.location(), name);
        error(INFO_DEFINED_HERE, decl_data->decl_node->location(), name);

        return true;
    }

    return false;
}

auto SemanticAnalyzer::value_type_from_node(const ASTNode &type_node) -> ValueType
{
    if (type_node.type() != ASTNodeType::TOKEN and type_node.type() != ASTNodeType::DATA_TYPE)
        return ValueType::ERROR_TYPE;

    const Token &tok = type_node.token();
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
    if (decl_data->type == DeclData::Type::OBJECT)
    {
        auto obj_decl_data = std::dynamic_pointer_cast<ObjectDeclData>(decl_data);
        return obj_decl_data->type;
    }

    error(ERR_NOT_A_TYPE, type_node.location(), tok.to_string());

    return ValueType::ERROR_TYPE;
}

} // namespace NCSC
