#include <gtest/gtest.h>
#include <ncsc/lexer.hpp>

TEST(LexerTests, IntegerIsTokenizedAsInteger) {
    auto tok = NCSC::Lexer("1").tokenizeAll()[0];

    ASSERT_EQ(tok.type, NCSC::TokenType::INT_CONSTANT);
    ASSERT_EQ(tok.val, "1");
}

TEST(LexerTests, FloatIsTokenizedAsFloat) {
    auto tok = NCSC::Lexer("1.").tokenizeAll()[0];
    ASSERT_EQ(tok.type, NCSC::TokenType::FLOAT_CONSTANT);
    ASSERT_EQ(tok.val, "1.0");
}

TEST(LexerTests, PointIsTokenizedAsPoint) {
    auto tok = NCSC::Lexer(".").tokenizeAll()[0];
    ASSERT_EQ(tok.type, NCSC::TokenType::POINT);
    ASSERT_EQ(tok.val, ".");
}
