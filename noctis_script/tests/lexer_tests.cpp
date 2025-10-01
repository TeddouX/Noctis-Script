#include <gtest/gtest.h>
#include <ncsc/lexer.h>

TEST(LexerTests, IntegerIsTokenizedAsInteger) {
    auto tok = NCSC::Lexer("1").tokenizeAll()[0];

    ASSERT_EQ(tok.type, NCSC::TokenType::INT);
    ASSERT_EQ(tok.val, "1");
}

TEST(LexerTests, FloatIsTokenizedAsFloat) {
    auto tok = NCSC::Lexer("1.").tokenizeAll()[0];
    ASSERT_EQ(tok.type, NCSC::TokenType::FLOAT);
    ASSERT_EQ(tok.val, "1.0");
}

TEST(LexerTests, PointIsTokenizedAsPoint) {
    auto tok = NCSC::Lexer(".").tokenizeAll()[0];
    ASSERT_EQ(tok.type, NCSC::TokenType::POINT);
    ASSERT_EQ(tok.val, ".");
}
