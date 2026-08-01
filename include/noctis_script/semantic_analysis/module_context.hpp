#pragma once
#include <filesystem>
#include <unordered_map>

#include "../parsing/scoped_path.hpp"
#include "../error.hpp"
#include "module_data.hpp"


namespace NCSC
{
    
// If using this for more than one script, call clear_imported_modules 
// before the semantic analysis of a different script
class ModuleContext
{
public:
    ModuleContext() = default;

    auto add_import_folder(const std::filesystem::path &path) -> void;
    auto add_import_file(const std::filesystem::path &path) -> bool;

    // file_path: At least a file name
    // All required modules of this module must've added before calling this function
    auto add_module(const std::string &file_contents, const std::filesystem::path &file_path) -> std::vector<Error>;

    auto has_module(const Parsing::ScopedPath &path) const -> bool;
    auto get_module_path(const Parsing::ScopedPath &path) const -> std::filesystem::path;
    auto get_module_data(const Parsing::ScopedPath &path) const -> PtrRef<SemanticAnalysis::ModuleData>;

    auto set_module_imported(const Parsing::ScopedPath &path) -> std::vector<Error>;

    auto clear_imported_modules() -> void;

private:
    friend class SemanticAnalyzer;

    std::unordered_map<
        Parsing::ScopedPath,
        std::filesystem::path>                  discovered_modules_;

    std::unordered_map<
        Parsing::ScopedPath,
        PtrRef<SemanticAnalysis::ModuleData>>   imported_modules_;
};

} // namespace NCSC
