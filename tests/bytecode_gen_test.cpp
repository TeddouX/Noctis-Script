#include <gtest/gtest.h>
#include <print>

#include "noctis_script/bytecode_gen/bytecode_gen.hpp"


using namespace NCSC;


TEST(BytecodeGenTest, temp) 
{
    auto script_src = ScriptSource::from_contents(
R"(func main() -> void
{
    var bla: int = 1;
    bla++;
}
)"
    );
    BytecodeGenerator bc_gen{};
    bc_gen.compile_script(script_src);

    for (const auto &err : bc_gen.generation_errors())
        std::println("{}", err.get_error_message_with_source());

    ASSERT_TRUE(false);
}