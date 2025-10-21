// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <gtest/gtest.h>
#include <ncsc/lexer.hpp>

using namespace NCSC;

auto getTokens(const std::string &src) {
    return Lexer(ScriptSource::fromSource(src)).tokenizeAll();
}

TEST(LexerTests, EndOfFileIsAdded) {
    auto tokens = getTokens("a; b; c + a");

    ASSERT_EQ(tokens.back().type, TokenType::END_OF_FILE);
}

TEST(LexerTests, WhitespacesAreCleared) {
    auto tokens = getTokens("   a   \t\t     b\tc\n\nd \n e ");

    ASSERT_EQ(tokens.size(), 6);
    ASSERT_EQ(tokens[0].val, "a");
    ASSERT_EQ(tokens[1].val, "b");
    ASSERT_EQ(tokens[2].val, "c");
    ASSERT_EQ(tokens[3].val, "d");
    ASSERT_EQ(tokens[4].val, "e");
}

TEST(LexerTests, SingleCommentsAreIgnored) {
    auto tokens = getTokens("// HELLLOLOOOOODSQDQKDLQkdlKQS //\nInt /// bro\n Int");

    ASSERT_EQ(tokens.size(), 3);
    ASSERT_EQ(tokens[0].type, TokenType::INT32_KWD);
    ASSERT_EQ(tokens[1].type, TokenType::INT32_KWD);
}

TEST(LexerTests, IntLiteral) {
    auto tokens = getTokens("123456789123456748923123");

    ASSERT_EQ(tokens.size(), 2);
    Token intTok = tokens[0];
    ASSERT_EQ(intTok.val, "123456789123456748923123");
    ASSERT_EQ(intTok.line, 1);
    ASSERT_EQ(intTok.col, 1);
    ASSERT_EQ(intTok.type, TokenType::INT_CONSTANT);
}

TEST(LexerTests, FloatLiteral) {
    auto tokens = getTokens("1548615641015348412.101231223132123");

    ASSERT_EQ(tokens.size(), 2);
    Token intTok = tokens[0];
    ASSERT_EQ(intTok.val, "1548615641015348412.101231223132123");
    ASSERT_EQ(intTok.line, 1);
    ASSERT_EQ(intTok.col, 1);
    ASSERT_EQ(intTok.type, TokenType::FLOAT_CONSTANT);
}

TEST(LexerTests, KeywordWithMultipleDefinitions) {
    auto tokens = getTokens("Int Int32");

    ASSERT_EQ(tokens.size(), 3);
    ASSERT_EQ(tokens[0].type, TokenType::INT32_KWD);
    ASSERT_EQ(tokens[1].type, TokenType::INT32_KWD);
}

TEST(LexerTests, Identifier) {
    auto tokens = getTokens("_a1123123azaaa");

    ASSERT_EQ(tokens.size(), 2);
    ASSERT_EQ(tokens[0].val, "_a1123123azaaa");
    ASSERT_EQ(tokens[0].type, TokenType::ID);
}

TEST(LexerTests, AssignmentOperators) {
    auto tokens = getTokens("= += -= *= /=");

    ASSERT_EQ(tokens.size(), 6);
    ASSERT_EQ(tokens[0].type, TokenType::EQUAL);
    ASSERT_EQ(tokens[1].type, TokenType::PLUS_EQUAL);
    ASSERT_EQ(tokens[2].type, TokenType::MINUS_EQUAL);
    ASSERT_EQ(tokens[3].type, TokenType::STAR_EQUAL);
    ASSERT_EQ(tokens[4].type, TokenType::SLASH_EQUAL);
}

TEST(LexerTests, BinaryOperators) {
    auto tokens = getTokens("+ - / *");

    ASSERT_EQ(tokens.size(), 5);
    ASSERT_EQ(tokens[0].type, TokenType::PLUS);
    ASSERT_EQ(tokens[1].type, TokenType::MINUS);
    ASSERT_EQ(tokens[2].type, TokenType::SLASH);
    ASSERT_EQ(tokens[3].type, TokenType::STAR);
}
