#include "compiler/bytecode.hpp"

namespace NCSC
{

auto Bytecode::script_source() const -> const std::shared_ptr<ScriptSource> &
{
    return src_;
}

auto Bytecode::has_dbg_info() const -> bool
{
    return has_dbg_info_;
}

auto Bytecode::bytes() const -> const std::vector<byte_t> &
{
    return bytes_;
}

auto Bytecode::location_at(std::size_t byte_idx) const -> const Location &
{
    if (location_entries_.empty())
        return {};

    // Get first offset that is bigger that byte_idx
    auto it = std::upper_bound(
        location_entries_.begin(),
        location_entries_.end(),
        byte_idx,
        [](std::size_t value, const LocationEntry &entry) 
        {
            return value < entry.offset;
        }
    );

    if (it == location_entries_.end())
        return location_entries_.back().location;
    else if (it == location_entries_.begin())
        return it->location;
    else
        return std::prev(it)->location;
}

} // namespace NCSC
