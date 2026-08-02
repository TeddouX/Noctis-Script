#include <gtest/gtest.h>

#include <print>

#include "noctis_script/lexing/lexer.hpp"
#include "noctis_script/parsing/parser.hpp"
#include "noctis_script/parsing/ast_node/all_ast_nodes.hpp"
#include "noctis_script/semantic_analysis/semantic_analyzer.hpp"

namespace NCSC
{
    
using namespace SemanticAnalysis;

TEST(SemanticAnalyzerTest, FirstPassCorrectlyImportModules)
{
    ModuleContext module_context{};
    auto errs = module_context.add_module("@module testing;", "testing.ncsc");
    ASSERT_TRUE(errs.empty());

    auto script_src = ScriptSource::from_contents(
R"(@import testing;
)");

    Parser parser{tokenize(script_src), script_src};
    auto root_node = parser.parse();
    ASSERT_FALSE(parser.has_syntax_errors());

    SemanticAnalyzer analyzer{root_node, script_src, &module_context};
    analyzer.init_root_scope();
    analyzer.first_pass();
    ASSERT_FALSE(analyzer.has_analysis_errors());
    
    auto module_data = analyzer.module_data_;
    
    ASSERT_EQ(module_data->imported_modules.size(), 1);
    ASSERT_EQ(module_data->imported_modules[0]->path.base_name, "testing");
}

TEST(SemanticAnalyzerTest, SecondPassCorrectlyUsesModulesExportedSymbols)
{
    ModuleContext module_context{};
    auto errs = module_context.add_module(R"(@module std::math;
obj Vec3 {}
func some_function() {}
export
{
    Vec3,
    some_function
}
)", "testing.ncsc");
    ASSERT_TRUE(errs.empty());

    auto script_src = ScriptSource::from_contents(
R"(@import std::math
@using std::math::Vec3
func main() -> Vec45 {}
obj Vec45 {}
)");

    Parser parser{tokenize(script_src), script_src};
    auto root_node = parser.parse();
    std::println(std::cerr, "{}", root_node->ast_string());
    for (const auto &err: parser.get_syntax_errors())
        std::println(std::cerr, "{}", err.get_error_message_with_source());
    ASSERT_FALSE(parser.has_syntax_errors());

    SemanticAnalyzer analyzer{root_node, script_src, &module_context};
    analyzer.init_root_scope();
    analyzer.first_pass();
    analyzer.second_pass();
    ASSERT_FALSE(analyzer.has_analysis_errors());
    
    auto module_data = analyzer.module_data_;
}

TEST(SemanticAnalyzerTest, SecondPassFunctionCorrectData) 
{
    auto script_src = ScriptSource::from_contents(
R"(func main(arg1: int, arg2: float) -> bool {}
)");
    Parser parser{tokenize(script_src), script_src};
    auto root_node = parser.parse();
    SemanticAnalyzer analyzer{root_node, script_src};
    analyzer.init_root_scope();
    analyzer.first_pass();
    analyzer.second_pass();
    
    auto decl_data = analyzer.get_declaration("main", Location{});

    ASSERT_TRUE(decl_data != nullptr);
    ASSERT_EQ(decl_data->decl_type, DeclData::Type::FUNCTION);
    
    auto func_decl = decl_data->decl_node.dynamic_ptr_cast<Parsing::FuncDeclASTNode>();
    ASSERT_EQ(func_decl->func_name, "main");
    ASSERT_EQ(func_decl->func_params.size(), 2);
    
    const auto &[param1_name, param1_type] = func_decl->func_params[0];
    ASSERT_EQ(param1_name, "arg1");
    ASSERT_EQ(param1_type, ValueType::INT32);

    const auto &[param2_name, param2_type] = func_decl->func_params[1];
    ASSERT_EQ(param2_name, "arg2");
    ASSERT_EQ(param2_type, ValueType::FLOAT32);

    ASSERT_EQ(func_decl->func_return_type, ValueType::BOOL);
}

TEST(SemanticAnalyzerTest, SecondPassGlobalVarCorrectData)
{
    auto script_src = ScriptSource::from_contents(
R"(var bla: uint16 = 10;
)");
    Parser parser{tokenize(script_src), script_src};
    auto root_node = parser.parse();
    SemanticAnalyzer analyzer{root_node, script_src};
    analyzer.first_pass();
    analyzer.init_root_scope();
    analyzer.second_pass();

    auto decl_data = analyzer.get_declaration("bla", Location{});

    ASSERT_TRUE(decl_data != nullptr);
    ASSERT_EQ(decl_data->decl_type, DeclData::Type::VARIABLE);
    
    auto var_decl = decl_data->decl_node.dynamic_ptr_cast<Parsing::VarDeclASTNode>();
    ASSERT_EQ(var_decl->var_name, "bla");
    ASSERT_EQ(var_decl->var_type, ValueType::UINT16);
}

TEST(SemanticAnalyzerTest, SecondPassObjectCorrectData)
{
    auto script_src = ScriptSource::from_contents(
R"(obj Vec3 {}
)");
    Parser parser{tokenize(script_src), script_src};
    auto root_node = parser.parse();
    SemanticAnalyzer analyzer{root_node, script_src};
    analyzer.init_root_scope();
    analyzer.first_pass();
    analyzer.second_pass();

    auto decl_data = analyzer.get_declaration("Vec3", Location{});

    ASSERT_TRUE(decl_data != nullptr);
    ASSERT_EQ(decl_data->decl_type, DeclData::Type::OBJECT);
    
    auto obj_decl = decl_data->decl_node.dynamic_ptr_cast<Parsing::ObjDeclASTNode>();
    ASSERT_EQ(obj_decl->obj_name, "Vec3");
    ASSERT_EQ(obj_decl->obj_type, vtype_set_mask((ValueType)0, ValueType::OBJ_MASK));
}

TEST(SemanticAnalyzerTest, SecondPassCorrectIndices)
{
    auto script_src = ScriptSource::from_contents(
R"(
func func_0() {}
obj obj_0 {}
var var_0: int;
func func_1() {}
obj obj_1 {}
var var_1: int;
func func_2() {}
var var_2: int;
)");
    Parser parser{tokenize(script_src), script_src};
    auto root_node = parser.parse();
    SemanticAnalyzer analyzer{root_node, script_src};
    analyzer.init_root_scope();
    analyzer.first_pass();
    analyzer.second_pass();

    auto func_0_data = analyzer.get_declaration("func_0", Location{});
    ASSERT_TRUE(func_0_data != nullptr);
    ASSERT_EQ(func_0_data->decl_type, DeclData::Type::FUNCTION);
    ASSERT_EQ(func_0_data->idx, 0);

    auto func_1_data = analyzer.get_declaration("func_1", Location{});
    ASSERT_TRUE(func_1_data != nullptr);
    ASSERT_EQ(func_1_data->decl_type, DeclData::Type::FUNCTION);
    ASSERT_EQ(func_1_data->idx, 1);

    auto func_2_data = analyzer.get_declaration("func_2", Location{});
    ASSERT_TRUE(func_2_data != nullptr);
    ASSERT_EQ(func_2_data->decl_type, DeclData::Type::FUNCTION);
    ASSERT_EQ(func_2_data->idx, 2);

    auto obj_0_data = analyzer.get_declaration("obj_0", Location{});
    ASSERT_TRUE(obj_0_data != nullptr);
    ASSERT_EQ(obj_0_data->decl_type, DeclData::Type::OBJECT);
    ASSERT_EQ(obj_0_data->idx, 0);

    auto obj_1_data = analyzer.get_declaration("obj_1", Location{});
    ASSERT_TRUE(obj_1_data != nullptr);
    ASSERT_EQ(obj_1_data->decl_type, DeclData::Type::OBJECT);
    ASSERT_EQ(obj_1_data->idx, 1);

    auto var_0_data = analyzer.get_declaration("var_0", Location{});
    ASSERT_TRUE(var_0_data != nullptr);
    ASSERT_EQ(var_0_data->decl_type, DeclData::Type::VARIABLE);
    ASSERT_EQ(var_0_data->idx, 0);

    auto var_1_data = analyzer.get_declaration("var_1", Location{});
    ASSERT_TRUE(var_1_data != nullptr);
    ASSERT_EQ(var_1_data->decl_type, DeclData::Type::VARIABLE);
    ASSERT_EQ(var_1_data->idx, 1);

    auto var_2_data = analyzer.get_declaration("var_2", Location{});
    ASSERT_TRUE(var_2_data != nullptr);
    ASSERT_EQ(var_2_data->decl_type, DeclData::Type::VARIABLE);
    ASSERT_EQ(var_2_data->idx, 2);
}

} // namespace NCSC
