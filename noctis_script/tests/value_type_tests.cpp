// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <gtest/gtest.h>
#include <ncsc/value_type.hpp>

using namespace NCSC;

TEST(ValueTypeTests, CanPromoteIsFalseOnBiggerIntToSmallerInt) {
    ASSERT_FALSE(canPromoteType(ValueType::INT64, ValueType::INT32));
    ASSERT_FALSE(canPromoteType(ValueType::INT32, ValueType::INT16));
    ASSERT_FALSE(canPromoteType(ValueType::INT16, ValueType::INT8));
}

TEST(ValueTypeTests, CanPromoteIsFalseOnBiggerUIntToSmallerUInt) {
    ASSERT_FALSE(canPromoteType(ValueType::UINT64, ValueType::UINT32));
    ASSERT_FALSE(canPromoteType(ValueType::UINT32, ValueType::UINT16));
    ASSERT_FALSE(canPromoteType(ValueType::UINT16, ValueType::UINT8));
}

TEST(ValueTypeTests, CanPromoteIsFalseOnFloatToInt) {
    ASSERT_FALSE(canPromoteType(ValueType::FLOAT32, ValueType::INT32));
    ASSERT_FALSE(canPromoteType(ValueType::FLOAT64, ValueType::INT64));
}

// TEST(ValueTypeTests, CanPromoteIsFalseOnInt64ToFloat32) {
//     ASSERT_FALSE(canPromoteType(ValueType::INT64, ValueType::FLOAT32));
// }

TEST(ValueTypeTests, PromotionSmallerIntToBiggerIntWorks) {
    ASSERT_EQ(promoteType(ValueType::INT8, ValueType::INT16), ValueType::INT16);
    ASSERT_EQ(promoteType(ValueType::INT16, ValueType::INT32), ValueType::INT32);
    ASSERT_EQ(promoteType(ValueType::INT32, ValueType::INT64), ValueType::INT64);
}

TEST(ValueTypeTests, PromotionSmallerUIntToBiggerUIntWorks) {
    ASSERT_EQ(promoteType(ValueType::UINT8, ValueType::UINT16), ValueType::UINT16);
    ASSERT_EQ(promoteType(ValueType::UINT16, ValueType::UINT32), ValueType::UINT32);
    ASSERT_EQ(promoteType(ValueType::UINT32, ValueType::UINT64), ValueType::UINT64);
}

TEST(ValeTypeTests, PromotionIntToUIntWorks) {
    ASSERT_EQ(promoteType(ValueType::INT8, ValueType::UINT8), ValueType::UINT8);
    ASSERT_EQ(promoteType(ValueType::INT16, ValueType::UINT16), ValueType::UINT16);
    ASSERT_EQ(promoteType(ValueType::INT32, ValueType::UINT32), ValueType::UINT32);
    ASSERT_EQ(promoteType(ValueType::INT64, ValueType::UINT64), ValueType::UINT64);
}

TEST(ValueTypeTests, PromotionIntToFloat32Works) {
    ASSERT_EQ(promoteType(ValueType::INT8, ValueType::FLOAT32), ValueType::FLOAT32);
    ASSERT_EQ(promoteType(ValueType::INT16, ValueType::FLOAT32), ValueType::FLOAT32);
    ASSERT_EQ(promoteType(ValueType::INT32, ValueType::FLOAT32), ValueType::FLOAT32);
}

TEST(ValueTypeTests, PromotionIntToFloat64Works) {
    ASSERT_EQ(promoteType(ValueType::INT8, ValueType::FLOAT64), ValueType::FLOAT64);
    ASSERT_EQ(promoteType(ValueType::INT16, ValueType::FLOAT64), ValueType::FLOAT64);
    ASSERT_EQ(promoteType(ValueType::INT32, ValueType::FLOAT64), ValueType::FLOAT64);
    ASSERT_EQ(promoteType(ValueType::INT64, ValueType::FLOAT64), ValueType::FLOAT64);
}
