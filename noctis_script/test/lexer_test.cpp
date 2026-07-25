#include <gtest/gtest.h>
#include <print>

#include "noctis_script/lexer.hpp"


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

TEST(LexerTest, TokenizesArithmeticOperators)
{
    auto tokens = tokenize("+ - * /");

    ASSERT_EQ(tokens[0].type, TokenType::PLUS);
    ASSERT_EQ(tokens[1].type, TokenType::MINUS);
    ASSERT_EQ(tokens[2].type, TokenType::STAR);
    ASSERT_EQ(tokens[3].type, TokenType::SLASH);
}

TEST(LexerTest, TokenizesArithmeticAssignOperators)
{
    auto tokens = tokenize("+= -= *= /=");

    for (auto token : tokens) std::println(std::cerr, "{}", (int)token.type);

    ASSERT_EQ(tokens[0].type, TokenType::PLUS_EQUAL);
    ASSERT_EQ(tokens[1].type, TokenType::MINUS_EQUAL);
    ASSERT_EQ(tokens[2].type, TokenType::STAR_EQUAL);
    ASSERT_EQ(tokens[3].type, TokenType::SLASH_EQUAL);
}

TEST(LexerTest, TokenizesIncAndDecOperators)
{
    auto tokens = tokenize("++ --");

    ASSERT_EQ(tokens[0].type, TokenType::PLUS_PLUS);
    ASSERT_EQ(tokens[1].type, TokenType::MINUS_MINUS);
}
