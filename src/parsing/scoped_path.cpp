#include "parsing/scoped_path.hpp"


namespace NCSC::Parsing
{

auto ScopedPath::merge(const ScopedPath &other) -> void
{
    if (other.scope_path.empty())
        return;

    if (scope_path.empty())
    {
        scope_path = other.scope_path;
        return;
    }

    if (scope_path.size() > other.scope_path.size())
        return;

    for (std::size_t i = 0; i < other.scope_path.size(); i++)
    {
        if (i >= scope_path.size())
        {
            scope_path.push_back(other.scope_path[i]);
            continue;
        }

        if (scope_path[i] != other.scope_path[i])
            break;
    }
}

auto ScopedPath::remove(const ScopedPath &other) const -> ScopedPath
{
    std::size_t common = 0;
    while (common < scope_path.size() &&
           common < other.scope_path.size() &&
           scope_path[common] == other.scope_path[common])
    {
        common++;
    }

    if (scope_path[common] == other.base_name)
        common++;

    ScopedPath removed{};
    removed.scope_path.assign(scope_path.begin() + common, scope_path.end());
    removed.base_name = this->base_name;
    
    return removed;
}
    
auto ScopedPath::to_string() const -> std::string
{
    std::string str{};
    
    for (const auto &scope : scope_path)
        str += scope + "::";
    
    return str + base_name;
}

} // namespace NCSC::Parsing
