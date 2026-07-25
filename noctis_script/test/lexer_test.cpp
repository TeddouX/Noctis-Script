#include <gtest/gtest.h>

#include "noctis_script/lexer.hpp"

using namespace NCSC;


TEST(LexerTest, TokenizesIDsCorrectly) {
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
