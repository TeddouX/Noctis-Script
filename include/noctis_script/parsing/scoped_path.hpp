#pragma once
#include <vector>
#include <string>


namespace NCSC::Parsing
{
    
struct ScopedPath
{
    std::string                 base_name;
    std::vector<std::string>    scope_path;

    ScopedPath() = default;

    ScopedPath(const std::string &name)
        : base_name{name}
    {}

    auto operator==(const ScopedPath &other) const -> bool
    {
        return base_name == other.base_name and
            scope_path == other.scope_path;
    }

    auto merge(const ScopedPath &other) -> void;
    auto to_string() const -> std::string;
};

} // namespace NCSC::Parsing

namespace std
{
    template <>
    struct hash<NCSC::Parsing::ScopedPath> 
    {
        std::size_t operator()(const NCSC::Parsing::ScopedPath& obj) const noexcept 
        {
            return std::hash<std::string>{}(obj.to_string());
        }
    };
}
