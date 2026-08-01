#include "semantic_analysis/module_context.hpp"

#include <fstream>

#include "lexing/lexer.hpp"
#include "parsing/parser.hpp"
#include "semantic_analysis/semantic_analyzer.hpp"
#include "parsing/ast_node/scoped_identifier_ast_node.hpp"


namespace NCSC
{
    
using namespace SemanticAnalysis;

auto ModuleContext::create() -> PtrRef<ModuleContext>
{
    return PtrRef{new ModuleContext{}};
}

auto ModuleContext::add_import_folder(const std::filesystem::path &path) -> void
{
    if (not std::filesystem::is_directory(path))
        return;

    for (auto const &dir_entry : std::filesystem::recursive_directory_iterator{path})
    {
        if (dir_entry.is_directory())
            continue;

        const auto &file_path = dir_entry.path();
        if (file_path.extension() != "ncsc")
            return;

        add_import_file(file_path);
    }
}

auto ModuleContext::add_import_file(const std::filesystem::path &path) -> bool
{
    if (path.extension() != "ncsc")
        return false;

    std::ifstream file(path);

    if (not file.good())
        return false;
    
    std::string first_line;

    do
    {
        std::getline(file, first_line);
    } while (first_line.empty());
    
    auto tokens = tokenize(first_line);
    
    Parser parser{std::move(tokens)};
    auto root_node = parser.parse();

    if (parser.has_syntax_errors())
        return false;

    auto import_node = root_node->children()[0];
    if (import_node->type() != ASTNodeType::MODULE_DEF)
        return false;

    auto scoped_id = import_node->children()[0].dynamic_ptr_cast<Parsing::ScopedIdentifierASTNode>();
    discovered_modules_.emplace(scoped_id->path, path);

    return true;
}

auto ModuleContext::add_module(const std::string &file_contents, const std::filesystem::path &file_path) -> std::vector<Error>
{
    auto script_source = ScriptSource::from_contents(file_contents);
    script_source->file_path = file_path;

    std::vector<Token> tokens = tokenize(script_source);
    Parser parser{std::move(tokens), script_source};

    TypeErased<ASTNode> root = parser.parse();
    if (parser.has_syntax_errors())
        return parser.get_syntax_errors();

    auto import_node = root->children()[0];
    if (import_node->type() != ASTNodeType::MODULE_DEF)
    {
        auto err_info = ErrorInfo::create("Module", "M3", "INTERNAL: Can't add '{}' as a module because it doesn't have an @module statement");
        return { Error{err_info, err_info->get_formatted(file_path.string()), nullptr} };
    }

    auto scoped_id = import_node->children()[0].dynamic_ptr_cast<Parsing::ScopedIdentifierASTNode>();
    Parsing::ScopedPath scope_path = scoped_id->path;

    SemanticAnalyzer semantic_analyzer{root, script_source, PtrRef{this}};
    auto module_data = semantic_analyzer.do_analysis();
    module_data->path = scope_path;

    if (semantic_analyzer.has_analysis_errors())
        return semantic_analyzer.get_analysis_errors();

    imported_modules_.emplace(scope_path, module_data);
}

auto ModuleContext::has_module(const Parsing::ScopedPath &path) const -> bool
{
    return discovered_modules_.contains(path);
}

auto ModuleContext::get_module_path(const Parsing::ScopedPath &path) const -> std::filesystem::path
{
    auto it = discovered_modules_.find(path);
    if (it == discovered_modules_.end())
        return std::filesystem::path{};
    
    return it->second;
}

auto ModuleContext::get_module_data(const Parsing::ScopedPath &path) const -> PtrRef<SemanticAnalysis::ModuleData>
{
    auto it = imported_modules_.find(path);
    if (it == imported_modules_.end())
        return nullptr;
    
    return it->second;
}

auto ModuleContext::set_module_imported(const Parsing::ScopedPath &path) -> std::vector<Error>
{
    auto it = discovered_modules_.find(path);
    if (it == discovered_modules_.end())
    {
        auto err_info = ErrorInfo::create("Module", "M1", "INTERNAL: Can't find module '{}'");
        return { Error{err_info, err_info->get_formatted(path.to_string()), nullptr} };
    }

    const std::filesystem::path &file_path = it->second;
    std::ifstream file_stream{file_path};

    if (not file_stream.is_open())
    {
        auto err_info = ErrorInfo::create("Module", "M2", "INTERNAL: Can't open file '{}'");
        return { Error{err_info, err_info->get_formatted(file_path.string()), nullptr} };
    }

    std::string file_contents{};
    while (std::getline(file_stream, file_contents))
        file_contents += '\n';

    if (file_contents.back() == '\n')
        file_contents.pop_back();

    add_module(file_contents, file_path);

    return {};
}

auto ModuleContext::clear_imported_modules() -> void
{
    imported_modules_.clear();
}

} // namespace NCSC
