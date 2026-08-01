#include <gtest/gtest.h>

#include "noctis_script/lexing/lexer.hpp"
#include "noctis_script/parsing/parser.hpp"
#include "noctis_script/semantic_analysis/semantic_analyzer.hpp"

using namespace NCSC;


TEST(SemanticAnalyzerTest, UpdatesPositionWithTokenCorrectly) 
{
    auto script_src = ScriptSource::from_contents(
R"(func main(arg1: int, arg2: float) -> bool
{}
)");
    Parser parser{tokenize(script_src)};
    auto root_node = parser.parse();
    SemanticAnalyzer analyzer{root_node, script_src};
    analyzer.do_analysis();
}
