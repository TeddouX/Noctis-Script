#include <gtest/gtest.h>

#include <print>

#include "noctis_script/script/script_source.hpp"


using namespace NCSC;


TEST(ScriptSourceTest, SplitsLinesCorrectly)
{
    auto script_src = ScriptSource::from_contents(
R"(a
b
c
d
)"
    );

    ASSERT_EQ(script_src->get_lines().size(), 5);
    ASSERT_EQ(script_src->get_line(1), "a");
    ASSERT_EQ(script_src->get_line(3), "c");
    ASSERT_EQ(script_src->get_line(5), "");
}
