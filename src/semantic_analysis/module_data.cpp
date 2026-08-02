#include "semantic_analysis/module_data.hpp"

#include "utils/vector_utils.hpp"


namespace NCSC::SemanticAnalysis
{
    
auto ModuleData::find_exported_symbol(const std::string &symbol_name) const -> const DeclData *
{
    return Utils::find_named(symbol_name, exported_symbols);
}

auto ModuleData::find_imported_symbol(const Parsing::ScopedPath &symbol_path) const -> const DeclData *
{
    for (const auto &imported_module : imported_modules)
    {
        const auto &imported_path = imported_module->path;
        // Remove common namespaces
        auto relative_path = symbol_path.remove(imported_path);

        // Are we in the right module
        if (not relative_path.scope_path.empty())
            continue;

        const auto &symbol_name = relative_path.scope_path.empty() ? relative_path.base_name : relative_path.scope_path[0];
        return imported_module->find_exported_symbol(symbol_name);
    }

    return nullptr;
}
 
auto ModuleData::find_imported_module(const Parsing::ScopedPath &module_path) const -> PtrRef<ModuleData>
{
    for (const auto &imported_module : imported_modules)
    {
        if (imported_module->path == module_path)
            return imported_module;
    }

    return nullptr;
}

auto ModuleData::search_local_type(const Parsing::ScopedPath &type_path) const -> ValueType
{
    auto it = type_table.find(type_path);
    if (it != type_table.end())
        return it->second;

    return ValueType::ERROR_TYPE;
}

auto ModuleData::search_type(const Parsing::ScopedPath &type_path) const -> ValueType
{
    auto local = search_local_type(type_path);

    if (local != ValueType::ERROR_TYPE)
        return local;

    for (const auto &imported_module : imported_modules)
    {
        const auto &imported_path = imported_module->path;
        // Remove common namespaces
        auto relative_path = type_path.remove(imported_path);

        // Are we in the right module
        if (not relative_path.scope_path.empty())
            continue;

        return imported_module->search_local_type(relative_path);
    }

    return ValueType::ERROR_TYPE;
}

    
} // namespace NCSC::SemanticAnalysis
