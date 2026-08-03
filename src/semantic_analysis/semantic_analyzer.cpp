#include "semantic_analysis/semantic_analyzer.hpp"

#include <ranges>

#include "parsing/ast_node/all_ast_nodes.hpp"


namespace NCSC
{
    
using namespace SemanticAnalysis;

SemanticAnalyzer::SemanticAnalyzer(
    TypeErased<ASTNode> root,
    PtrRef<ScriptSource> script_source,
    ModuleContext *module_ctx)
    : root_node_{root}
    , script_source_{script_source}
    , module_ctx_{module_ctx}
{
    module_data_ = make_ptr_ref<ModuleData>();
    module_data_->root_node = root_node_;
    module_data_->type_table.insert(BULTIN_VTYPE_NAMES.begin(), BULTIN_VTYPE_NAMES.end());;
}

auto SemanticAnalyzer::do_analysis() -> PtrRef<ModuleData>
{
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
        if (child->type() == ASTNodeType::MODULE_DEF)
        {
            auto scoped_id = child->children()[0].dynamic_ptr_cast<Parsing::ScopedIdentifierASTNode>();

            module_data_->is_module = true;
            module_data_->path = scoped_id->path;

            break;
        }
        else if (child->type() == ASTNodeType::IMPORT_STMT)
        {
            if (not module_ctx_)
            {
                error(ERR_NO_MODULE_CTXT, child->location());
                return false;
            }

            auto scoped_id = child->children()[0].dynamic_ptr_cast<Parsing::ScopedIdentifierASTNode>();
            const Parsing::ScopedPath &scoped_path = scoped_id->path;
            
            if (not module_ctx_->has_module(scoped_path))
            {
                error(ERR_NO_MODULE_NAMED, child->location(), scoped_path.to_string());
                return false;
            }

            // The module was already imported
            if (auto module_data = module_ctx_->get_module_data(scoped_path))
            {
                module_data_->imported_modules.push_back(module_data);
                continue;
            }

            auto errors = module_ctx_->set_module_imported(scoped_path);
            if (not errors.empty())
            {
                analysis_errors_.insert(analysis_errors_.end(),
                    std::make_move_iterator(errors.begin()),
                    std::make_move_iterator(errors.end())
                );
                return false;
            }

            auto module_data = module_ctx_->get_module_data(scoped_path);
            module_data_->imported_modules.push_back(module_data);

            break;
        }
        else if (child->type() == ASTNodeType::USING_STMT)
        {
            if (not module_ctx_)
            {
                error(ERR_NO_MODULE_CTXT, child->location());
                return false;
            }

            auto using_stmt = child.dynamic_ptr_cast<Parsing::UsingStmtASTNode>();
            auto scoped_id = child->children()[0].dynamic_ptr_cast<Parsing::ScopedIdentifierASTNode>();
            auto scoped_path = scoped_id->path;

            if (using_stmt->is_type_alias)
            {
                const DeclData *resolved = module_data_->find_imported_symbol(scoped_path);

                if (not resolved)
                {
                    error(ERR_SYMBOL_NOT_IMPORTED, scoped_id->location(), scoped_path.to_string());
                    return false;
                }

                const std::string &local_name = scoped_path.base_name;
                if (root_scope_->using_aliases.contains(local_name))
                {
                    error(ERR_USING_CONFLICT, scoped_id->location(), scoped_path.to_string());
                    return false;
                }

                root_scope_->using_aliases[local_name] = *resolved;
            }
            else
            {
                const auto &module_data = module_data_->find_imported_module(scoped_path);
                if (not module_data)
                {
                    error(ERR_MODULE_NOT_IMPORTED, scoped_id->location(), scoped_path.to_string());
                    return false;
                }

                root_scope_->used_modules.push_back(module_data);
            }
        }
    }

    return true;
}

auto SemanticAnalyzer::second_pass() -> bool
{
    const auto &declaration_body = root_node_->children().back();

    std::unordered_set<isize_t> obj_indices{};

    // First collect data about the objects so later functions or 
    // members that reference them as types are valid
    for (isize_t i = 0; i < declaration_body->children().size(); i++)
    {
        const auto &declaration = declaration_body->children()[i];
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
                decl_data.decl_type = DeclarationType::OBJECT;

                obj_decl->name = obj_name;
                decl_data.name = obj_name;

                isize_t idx = add_global_declaration(obj_name, decl_data);
                
                obj_decl->obj_type = make_object_vtype(idx);
                decl_data.type = obj_decl->obj_type;

                module_data_->type_table.emplace(obj_name, obj_decl->obj_type);
                
                obj_indices.insert(i);

                continue;
            }

            default:
                continue;
        }
    }

    // Then, with the object data that was resolved previously, 
    // collect member classes, functions, and variables in each object
    for (auto obj_idx : obj_indices)
    {
        const auto &declaration = declaration_body->children()[obj_idx];
        if (declaration->type() != ASTNodeType::OBJ_DECLARATION)
            continue;

        auto obj_decl = declaration.dynamic_ptr_cast<Parsing::ObjDeclASTNode>();
        first_pass_decl_body(obj_decl->children()[1], obj_decl);
    }

    // Collect global declarations
    first_pass_decl_body(declaration_body, nullptr);
    
    // Collect the module's export data
    for (const auto &declaration : declaration_body->children())
    {
        if (declaration->type() != ASTNodeType::EXPORT_DECL)
            continue;

        for (const auto &exported_symbol : declaration->children())
        {
            if (not module_data_->is_module)
            {
                error(ERR_CANT_EXPORT, declaration->location());
                return false;
            }

            const std::string &symbol_name = exported_symbol->token().value;

            Parsing::ScopedPath scoped{};
            scoped.base_name = symbol_name;

            auto declaration = get_declaration(symbol_name, exported_symbol->location());
            if (not declaration)
            {
                error(ERR_SYMBOL_NOT_DEFINED, exported_symbol->location(), symbol_name);
                return false;
            }

            module_data_->exported_symbols.push_back(*declaration);
        }
    }
    
    return true;
}

auto SemanticAnalyzer::third_pass() -> bool
{
    return true;
}

auto SemanticAnalyzer::fourth_pass() -> bool
{
    return true;
}

auto SemanticAnalyzer::first_pass_decl_body(
    const TypeErased<ASTNode> &decl_body, 
    const TypeErased<Parsing::ObjDeclASTNode> &obj_decl) -> bool
{
    bool is_in_object = obj_decl != nullptr;

    for (const auto &declaration : decl_body->children())
    {
        ASTNodeType decl_type = declaration->type();

        switch (decl_type)
        {
            // Global function or method
            case ASTNodeType::FUNCTION_DECLARATION:
            {
                auto func_decl = declaration.dynamic_ptr_cast<Parsing::FuncDeclASTNode>();

                DeclData decl_data{};

                isize_t child_idx = 0;

                if (is_in_object)
                {
                    const auto &access_mod_node = func_decl->children()[child_idx++];
                    if (access_mod_node->token().type == TokenType::PUBLIC_KWD)
                        decl_data.access_mod = AccessModifier::PUBLIC;
                    else
                        decl_data.access_mod = AccessModifier::PRIVATE;
                }

                const auto &name_node = func_decl->children()[child_idx++];
                const std::string &function_name = name_node->token().value;

                if (is_in_object)
                {
                    auto method_it = obj_decl->obj_methods.find(function_name); 
                    if (method_it != obj_decl->obj_methods.end())
                    {
                        error(ERR_ALREADY_DEFINED, name_node->location(), function_name);
                        error(INFO_DEFINED_HERE, method_it->second.decl_node->location(), function_name);

                        return false;
                    }
                }
                else if (is_symbol_defined_elsewhere(name_node))
                {
                    return false;
                }

                decl_data.decl_node = func_decl;
                decl_data.decl_type = DeclarationType::FUNCTION;

                func_decl->name = function_name;
                decl_data.name = function_name;

                const auto &param_node = func_decl->children()[child_idx++];
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

                const auto &return_node = func_decl->children()[child_idx++];
                ValueType return_type = value_type_from_node(return_node);
                func_decl->func_return_type = return_type;
                decl_data.type = return_type;

                if (is_in_object)
                {
                    decl_data.idx = obj_decl->decl_indices.func_idx++;
                    obj_decl->obj_methods.emplace(function_name, decl_data);
                }
                else
                {
                    add_global_declaration(function_name, decl_data);
                }

                continue;
            }
            // Global or member variable
            case ASTNodeType::VARIABLE_DECLARATION:
            {
                auto var_decl = declaration.dynamic_ptr_cast<Parsing::VarDeclASTNode>();

                DeclData decl_data{};

                isize_t child_idx = 0;

                if (is_in_object)
                {
                    const auto &access_mod_node = var_decl->children()[child_idx++];
                    if (access_mod_node->token().type == TokenType::PUBLIC_KWD)
                        decl_data.access_mod = AccessModifier::PUBLIC;
                    else
                        decl_data.access_mod = AccessModifier::PRIVATE;
                }

                const auto &name_node = declaration->children()[child_idx++];
                const std::string &var_name = name_node->token().value;

                if (is_in_object)
                {
                    auto method_it = obj_decl->obj_members.find(var_name); 
                    if (method_it != obj_decl->obj_members.end())
                    {
                        error(ERR_ALREADY_DEFINED, name_node->location(), var_name);
                        error(INFO_DEFINED_HERE, method_it->second.decl_node->location(), var_name);

                        return false;
                    }
                }
                else if (is_symbol_defined_elsewhere(name_node))
                {
                    return false;
                }

                decl_data.decl_node = var_decl;
                decl_data.decl_type = DeclarationType::VARIABLE;

                var_decl->name = var_name;
                decl_data.name = var_name;

                const auto &type_node = declaration->children()[child_idx++];
                ValueType var_type = value_type_from_node(type_node);

                var_decl->var_type = var_type;
                decl_data.type = var_type;

                if (is_in_object)
                {
                    decl_data.idx = obj_decl->decl_indices.var_idx++;
                    obj_decl->obj_members.emplace(var_name, decl_data);
                }
                else
                {
                    add_global_declaration(var_name, decl_data);
                }

                continue;
            }
        }
    }

    return true;
}

auto SemanticAnalyzer::init_root_scope(const DeclIndices &decl_indices) -> void
{
    root_scope_ = PtrRef<Scope>::make(decl_indices);
    curr_scope_ = root_scope_;

    module_data_->root_scope = root_scope_;
}

auto SemanticAnalyzer::init_root_scope() -> void
{
    if (not root_scope_)
    {
        if (module_ctx_)
            init_root_scope(module_ctx_->decl_indices_);
        else
            init_root_scope(DeclIndices{ 0, 0, 0 });
    }
}

auto SemanticAnalyzer::enter_new_scope() -> void
{
    auto new_scope = PtrRef<Scope>::make(DeclIndices{});
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
    auto decl_candidates = curr_scope_->get_declaration(name);
    if (not decl_candidates.empty())
    {
        error(ERR_ALREADY_DEFINED, identifer->location(), name);

        if (decl_candidates.size() <= 1)
        {
            if (not decl_candidates[0].first)
                error(INFO_DEFINED_HERE, decl_candidates[0].second->decl_node->location(), name);
            else
                error(INFO_DEFINED_IN_MODULE, Location{}, decl_candidates[0].first->path.to_string());
        }

        return true;
    }

    return false;
}

auto SemanticAnalyzer::value_type_from_node(const TypeErased<ASTNode> &type_node) -> ValueType
{
    // The type's name is contained in a token
    bool is_token = 
        (type_node->type() == ASTNodeType::TOKEN or 
        type_node->type() == ASTNodeType::DATA_TYPE) and
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

    ValueType vtype = module_data_->search_type(scoped_path);
    if (vtype != ValueType::ERROR_TYPE)
        return vtype;

    auto decl_data = get_declaration(scoped_path.base_name, type_node->location());
    if (decl_data and decl_data->decl_type == DeclarationType::OBJECT)
        return decl_data->type;

    error(ERR_NOT_A_TYPE, type_node->location(), scoped_path.to_string());

    return ValueType::ERROR_TYPE;
}

auto SemanticAnalyzer::get_declaration(const std::string &name, const Location &err_location) -> const DeclData *
{
    auto declaration_candidates = curr_scope_->get_declaration(name);
    if (declaration_candidates.empty())
    {
        error(ERR_SYMBOL_NOT_DEFINED, err_location, name);
        return nullptr;
    }
    else if (declaration_candidates.size() > 1)
    {
        std::vector<std::string> ambiguous_modules;
        for (const auto &[module_data, _] : declaration_candidates)
            ambiguous_modules.push_back(module_data->path.to_string());

        auto ambiguous_modules_str = ambiguous_modules | std::views::join_with(std::string(", "));

        error(ERR_AMBIGUOUS_SYMBOL, err_location, name, ambiguous_modules_str);

        return nullptr;
    }

    return declaration_candidates[0].second;
}

auto SemanticAnalyzer::add_global_declaration(const std::string &name, DeclData &data) -> isize_t
{
    if (module_ctx_)
        module_ctx_->global_symbol_declared(data.decl_type);
    
    return root_scope_->add_declaration(name, data);
}

} // namespace NCSC
