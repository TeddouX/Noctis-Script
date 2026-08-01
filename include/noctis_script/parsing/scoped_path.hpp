#pragma once
#include <vector>
#include <string>


namespace NCSC::Parsing
{
    
struct ScopedPath
{
    std::string                 base_name;
    std::vector<std::string>    scope_path;

    auto to_string() const -> std::string
    {
        std::string str{};
        
        for (const auto &scope : scope_path)
            str += scope + "::";
        
        return str + base_name;
    }
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
