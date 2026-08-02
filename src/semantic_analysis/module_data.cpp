#include "semantic_analysis/module_data.hpp"

#include "utils/vector_utils.hpp"


namespace NCSC::SemanticAnalysis
{
    
auto ModuleData::find_exported_symbol(const std::string &symbol_name) const -> const DeclData *
{
    return Utils::find_named(symbol_name, exported_symbols);
}

auto ModuleData::search_local_type(const Parsing::ScopedPath &path) const -> ValueType
{
    auto it = type_table.find(path);
    if (it != type_table.end())
        return it->second;

    return ValueType::ERROR_TYPE;
}

auto ModuleData::search_type(const Parsing::ScopedPath &path) const -> ValueType
{
    auto local = search_local_type(path);

    if (local != ValueType::ERROR_TYPE)
        return local;

    for (const auto &imported_module : imported_modules)
    {
        const auto &imported_path = imported_module->path;
        // Remove common namespaces
        auto relative_path = path.remove(imported_path);

        // Are we in the right module
        if (not relative_path.scope_path.empty())
            continue;

        return imported_module->search_type(relative_path);
    }

    return ValueType::ERROR_TYPE;
}

    
} // namespace NCSC::SemanticAnalysis
