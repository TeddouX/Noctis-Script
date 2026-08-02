#pragma once
#include <optional>
#include <vector>
#include <string>
#include <concepts>

#include "ncsc.hpp"


namespace NCSC::Utils
{

template <typename T>
concept Named = requires { 
    { T::name } -> std::convertible_to<const std::string &>; 
};


// -1 if nothing is found
template <Named _NamedT>
[[nodiscard]]
inline auto find_named_idx(const std::string &name, const std::vector<_NamedT> &vec) -> isize_t
{
    for (std::size_t i = 0; i < vec.size(); i++) 
    {
        const _NamedT &el = vec[i];
        if (el.name == name)
            return i;
    }

    return -1;
}


// nullptr if nothing is found
template <Named _NamedT>
[[nodiscard]]
inline auto find_named(const std::string &name, const std::vector<_NamedT> &vec) -> const _NamedT *
{
    isize_t idx = find_named_idx(name, vec);
    if (idx < 0)
        return nullptr;
    return &vec[idx];
}

} // namespace NCSC::Utils
