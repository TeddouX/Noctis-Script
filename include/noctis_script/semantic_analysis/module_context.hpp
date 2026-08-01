#pragma once
#include <filesystem>
#include <unordered_map>

#include "../parsing/scoped_path.hpp"


namespace NCSC
{
    
class ModuleContext
{
public:
    ModuleContext() = default;

    auto add_import_folder(const std::filesystem::path &path) -> void;
    auto add_import_file(const std::filesystem::path &path) -> void;

    auto has_module(const Parsing::ScopedPath &path) -> bool;

private:
    std::unordered_map<std::string, std::filesystem::path> modules_;
};

} // namespace NCSC
