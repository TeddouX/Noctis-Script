#pragma once
#include "types.hpp"
#include "ptr_ref.hpp"

// From https://github.com/google/googletest/blob/main/googletest/include/gtest/gtest_prod.h
#define FRIEND_TEST(test_case_name, test_name) \
    friend class test_case_name##_##test_name##_Test
