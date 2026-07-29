#include <gtest/gtest.h>
#include <print>

#include "noctis_script/bytecode_gen/bytecode_gen.hpp"


using namespace NCSC;


TEST(BytecodeGenTest, temp) 
{
    auto script_src = ScriptSource::from_contents(
R"(func main(arg1: int, arg2: float) -> void
{
}
)"
    );
    BytecodeGenerator bc_gen{};
    bc_gen.compile_script(script_src);
}