// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <ncsc/bytecode.hpp>

namespace NCSC
{
    
const Location &Bytecode::getLocationAt(size_t byteIdx) const {
    if (locationEntries_.empty())
        return {};

    auto it = std::upper_bound(
        locationEntries_.begin(),
        locationEntries_.end(),
        byteIdx,
        [](size_t value, const LocationEntry& entry) {
            return value < entry.offset;
        }
    );

    if (it == locationEntries_.end())
        return {};
    else if (it == locationEntries_.begin())
        return it->loc;
    
    return std::prev(it)->loc;
}

} // namespace NCSC
