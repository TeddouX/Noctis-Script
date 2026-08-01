#pragma once
#include <cstdint>

// From https://github.com/google/googletest/blob/main/googletest/include/gtest/gtest_prod.h
#define FRIEND_TEST(test_case_name, test_name) \
    friend class test_case_name##_##test_name##_Test

namespace NCSC
{
    
using isize_t   = std::int64_t;
using usize_t   = std::uint64_t;

using byte_t    = std::uint8_t;
using word_t    = std::uint16_t;
using dword_t   = std::uint32_t;
using qword_t   = std::uint64_t;

using float32_t = float;
using float64_t = double;

using index_word_t = dword_t;
constexpr index_word_t INVALID_INDEX = -1; 

} // namespace NCSC
