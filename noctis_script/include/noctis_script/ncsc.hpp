#pragma once
#include <cstdint>


namespace NCSC
{
    
using isize_t   = std::int64_t;
using usize_t   = std::uint64_t;

using byte_t    = std::uint8_t;
using word_t    = std::uint16_t;
using dword_t   = std::uint32_t;
using qword_t   = std::uint32_t;

using float32_t = float;
using float64_t = double;

using index_word_t = dword_t;
constexpr index_word_t INVALID_INDEX = -1; 

} // namespace NCSC
