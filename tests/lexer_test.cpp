#include <gtest/gtest.h>
#include <print>

#include "noctis_script/lexer/lexer.hpp"


using namespace NCSC;


TEST(LexerTest, TokenizesIDsCorrectly) 
{
    const std::vector<std::string> test_ids = 
    {
        "hjkdlqhjkfdbnvc",
        "hjkdlqh123407895",
        "a1",
        "_a",
        "_",
        "_a_a_a_a_a_a",
        "_1_dsq_dsq",
    };

    for (const auto &test_id : test_ids) 
    {
        auto tokens = tokenize(test_id);

        ASSERT_EQ(tokens[0].type, TokenType::ID);
        ASSERT_EQ(tokens[0].value, test_id);
    }
}

TEST(LexerTest, TokenizesIntsCorrectly) 
{
    const std::vector<std::string> test_numbers = 
    {
        "123456789",
        "9999",
    };

    for (const auto &test_number : test_numbers) 
    {
        auto tokens = tokenize(test_number);

        ASSERT_EQ(tokens[0].type, TokenType::INT_CONSTANT);
        ASSERT_EQ(tokens[0].value, test_number);
    }
}

TEST(LexerTest, TokenizesFloatsCorrectly) 
{
    const std::vector<std::string> test_numbers = 
    {
        "123.1234864",
        "7845564132.14468678",
    };

    for (const auto &test_number : test_numbers) 
    {
        auto tokens = tokenize(test_number);

        ASSERT_EQ(tokens[0].type, TokenType::FLOAT_CONSTANT);
        ASSERT_EQ(tokens[0].value, test_number);
    }
}

TEST(LexerTest, AddsTrailingZeroesToFloats)
{
    auto tokens = tokenize(".1");
    ASSERT_EQ(tokens[0].type, TokenType::FLOAT_CONSTANT);
    ASSERT_EQ(tokens[0].value, "0.1");

    tokens = tokenize("1.");
    ASSERT_EQ(tokens[0].type, TokenType::FLOAT_CONSTANT);
    ASSERT_EQ(tokens[0].value, "1.0");
}

TEST(LexerTest, TokenizesEverything)
{
    for (const auto &[tok_ty, tok_str] : TOKENS_TO_STRING)
    {
        auto tokens = tokenize(tok_str);

        TokenType replaced_type = tok_ty;
        auto it = REPLACED_TOKENS.find(tok_ty);
        if (it != REPLACED_TOKENS.end())
            replaced_type = it->second;

        ASSERT_EQ(tokens[0].type, replaced_type);
    }
}

TEST(LexerTest, TokenizesWithCorrectColumn)
{
    auto tokens = tokenize("a b");
    ASSERT_EQ(tokens[0].location.column, 1);
    ASSERT_EQ(tokens[0].location.line, 1);

    ASSERT_EQ(tokens[1].location.column, 3);
    ASSERT_EQ(tokens[1].location.line, 1);
}

TEST(LexerTest, TokenizesWithCorrectLine)
{
    auto tokens = tokenize("a b\nc\nd e");
    ASSERT_EQ(tokens[0].location.column, 1);
    ASSERT_EQ(tokens[0].location.line, 1);

    ASSERT_EQ(tokens[1].location.column, 3);
    ASSERT_EQ(tokens[1].location.line, 1);

    ASSERT_EQ(tokens[2].location.column, 1);
    ASSERT_EQ(tokens[2].location.line, 2);

    ASSERT_EQ(tokens[3].location.column, 1);
    ASSERT_EQ(tokens[3].location.line, 3);

    ASSERT_EQ(tokens[4].location.column, 3);
    ASSERT_EQ(tokens[4].location.line, 3);
}

TEST(LexerTest, TokenizesAllReservedTokens)
{
    for (const auto &[tok_str, tok_ty] : RESERVED_TOKENS) 
    {
        auto tokens = tokenize(tok_str);

        TokenType replaced_type = tok_ty;
        auto it = REPLACED_TOKENS.find(tok_ty);
        if (it != REPLACED_TOKENS.end())
            replaced_type = it->second;

        ASSERT_EQ(tokens[0].type, replaced_type);
    }
}
