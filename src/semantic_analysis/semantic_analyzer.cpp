#include "semantic_analysis/semantic_analyzer.hpp"

#include "parsing/ast_node/all_ast_nodes.hpp"
#include "semantic_analyzer.hpp"


namespace NCSC
{
    
using namespace SemanticAnalysis;

SemanticAnalyzer::SemanticAnalyzer(
    TypeErased<ASTNode> root, 
    PtrRef<ScriptSource> script_source, 
    PtrRef<ModuleContext> module_ctxt)   
    : root_node_{root}
    , script_source_{script_source}
{
    module_data_->type_table.insert(BULTIN_VTYPE_NAMES.begin(), BULTIN_VTYPE_NAMES.end());;
}

auto SemanticAnalyzer::do_analysis() -> PtrRef<ModuleData>
{
    module_data_ = make_ptr_ref<ModuleData>();
    module_data_->root_node = root_node_;

    init_root_scope();

    if (!first_pass())
        return nullptr;
    
    if (!second_pass())
        return nullptr;
    
    if (!third_pass())
        return nullptr;
    
    if (!fourth_pass())
        return nullptr;
    
    return module_data_;
}

auto SemanticAnalyzer::has_analysis_errors() const -> bool
{
    return not analysis_errors_.empty();
}

auto SemanticAnalyzer::get_analysis_errors() const -> const std::vector<Error> &
{
    return analysis_errors_;
}

auto SemanticAnalyzer::first_pass() -> bool
{
    for (const auto &child : root_node_->children())
    {
        if (child->type() != ASTNodeType::IMPORT_STMT)
            continue;

        if (not module_ctxt_)
        {
            error(ERR_NO_MODULE_CTXT, child->location());
            return false;
        }

        auto scoped_id = child->children()[0].dynamic_ptr_cast<Parsing::ScopedIdentifierASTNode>();
        const Parsing::ScopedPath &scoped_path = scoped_id->path;
        
        if (not module_ctxt_->has_module(scoped_path))
        {
            error(ERR_NO_MODULE_NAMED, child->location(), scoped_path.to_string());
            return false;
        }

        // The module was already imported
        if (auto module_data = module_ctxt_->get_module_data(scoped_path))
        {
            module_data_->imported_modules.push_back(module_data);
            continue;
        }

        auto errors = module_ctxt_->set_module_imported(scoped_path);
        if (not errors.empty())
        {
            analysis_errors_.insert(analysis_errors_.end(),
                std::make_move_iterator(errors.begin()),
                std::make_move_iterator(errors.end())
            );
            return false;
        }

        auto module_data = module_ctxt_->get_module_data(scoped_path);
        module_data_->imported_modules.push_back(module_data);
    }

    return true;
}

auto SemanticAnalyzer::second_pass() -> bool
{
    const auto &declaration_body = root_node_->children()[0];

    // First collect data about the objects so later functions or 
    // members that reference them as types are valid
    for (const auto &declaration : declaration_body->children())
    {
        switch (declaration->type())
        {
            case ASTNodeType::OBJ_DECLARATION:
            {
                auto obj_decl = declaration.dynamic_ptr_cast<Parsing::ObjDeclASTNode>();
                const auto &name_node = declaration->children()[0];
                const std::string &obj_name = name_node->token().value;

                if (is_symbol_defined_elsewhere(name_node))
                    return false;
                
                DeclData decl_data{};
                decl_data.decl_node = obj_decl;
                decl_data.type = DeclData::Type::OBJECT;

                obj_decl->obj_name = obj_name;

                isize_t idx = curr_scope_->add_declaration(obj_name, decl_data);
                
                obj_decl->obj_type = make_object_vtype(idx);

                module_data_->type_table.emplace(obj_decl->obj_name, obj_decl->obj_type);
                
                continue;
            }

            default:
                continue;
        }
    }

    // Then, with the object data that was resolved previously, 
    // collect member classes, functions, and variables in each object, do so recursively for member objects
    for (const auto &declaration : declaration_body->children())
    {
        if (declaration->type() != ASTNodeType::OBJ_DECLARATION)
            continue;

        auto obj_decl = declaration.dynamic_ptr_cast<Parsing::ObjDeclASTNode>();
        
    }

    for (const auto &declaration : declaration_body->children())
    {
        ASTNodeType decl_type = declaration->type();

        switch (decl_type)
        {
            case ASTNodeType::FUNCTION_DECLARATION:
            {
                auto func_decl = declaration.dynamic_ptr_cast<Parsing::FuncDeclASTNode>();
                const auto &name_node = func_decl->children()[0];
                const std::string &function_name = name_node->token().value;

                if (is_symbol_defined_elsewhere(name_node))
                    return false;

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
                auto var_decl = declaration.dynamic_ptr_cast<Parsing::VarDeclASTNode>();
                const auto &name_node = declaration->children()[0];
                const std::string &var_name = name_node->token().value;

                if (is_symbol_defined_elsewhere(name_node))
                    return false;

                DeclData decl_data{};
                decl_data.decl_node = var_decl;
                decl_data.type = DeclData::Type::VARIABLE;

                var_decl->var_name = var_name;

                const auto &type_node = declaration->children()[1];
                ValueType var_type = value_type_from_node(type_node);

                var_decl->var_type = var_type;

                curr_scope_->add_declaration(var_name, decl_data);

                continue;
            }
        }
    }
    return false;
}

auto SemanticAnalyzer::third_pass() -> bool
{
    return false;
}

auto SemanticAnalyzer::fourth_pass() -> bool
{
    return false;
}

auto SemanticAnalyzer::init_root_scope() -> void
{
    root_scope_ = PtrRef<Scope>::make();
    curr_scope_ = root_scope_;
}

auto SemanticAnalyzer::enter_new_scope() -> void
{
    auto new_scope = PtrRef<Scope>::make();
    new_scope->set_parent(curr_scope_);
    
    curr_scope_ = new_scope;
}

auto SemanticAnalyzer::exit_scope() -> void
{
    curr_scope_ = curr_scope_->get_parent();
}

auto SemanticAnalyzer::is_symbol_defined_elsewhere(const TypeErased<ASTNode> &identifer) -> bool
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

auto SemanticAnalyzer::value_type_from_node(const TypeErased<ASTNode> &type_node) -> ValueType
{
    // The type's name is contained in a token
    bool is_token = 
        type_node->type() == ASTNodeType::TOKEN or 
        type_node->type() == ASTNodeType::DATA_TYPE and
        type_node->type() != ASTNodeType::SCOPED_IDENTIFIER;

    if (not is_token and type_node->type() != ASTNodeType::SCOPED_IDENTIFIER)
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

    Parsing::ScopedPath scoped_path{};
    if (is_token)
    {
        scoped_path.base_name = tok.value;
    }
    else
    {
        auto scoped_id = type_node.dynamic_ptr_cast<Parsing::ScopedIdentifierASTNode>();
        scoped_path = scoped_id->path;
    }

    auto it = module_data_->type_table.find(scoped_path);
    if (it != module_data_->type_table.end())
        return it->second;

    error(ERR_NOT_A_TYPE, type_node->location(), scoped_path.to_string());

    return ValueType::ERROR_TYPE;
}

} // namespace NCSC
