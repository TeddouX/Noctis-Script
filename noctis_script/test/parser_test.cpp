#include <gtest/gtest.h>

#include <print>

#include "noctis_script/parser/parser.hpp"
#include "noctis_script/lexer/lexer.hpp"


using namespace NCSC;


TEST(ParserTest, ParsesSimpleVariableDeclaration) 
{
    auto tokens = tokenize("var bla: int;");
    Parser parser{tokens};
    ASTNode root_node = parser.parse();

    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <VARIABLE_DECLARATION>
        ├── <IDENTIFIER>(bla)
        └── <DATA_TYPE>(int32))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    EXPECT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesSimpleVariableDeclarationWithIntConstantAssignment) 
{
    auto tokens = tokenize("var bla: int8 = 1;");
    Parser parser{tokens};
    ASTNode root_node = parser.parse();

    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <VARIABLE_DECLARATION>
        ├── <IDENTIFIER>(bla)
        ├── <DATA_TYPE>(int8)
        └── <EXPRESSION>
            └── <EXPRESSION_TERM>
                └── <EXPRESSION_VALUE>
                    └── <CONSTANT>(1))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    EXPECT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesSimpleVariableDeclarationWithFloatConstantAssignment) 
{
    auto tokens = tokenize("var bla: float = 1.0;");
    Parser parser{tokens};
    ASTNode root_node = parser.parse();

    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <VARIABLE_DECLARATION>
        ├── <IDENTIFIER>(bla)
        ├── <DATA_TYPE>(float32)
        └── <EXPRESSION>
            └── <EXPRESSION_TERM>
                └── <EXPRESSION_VALUE>
                    └── <CONSTANT>(1.0))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    EXPECT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesSimpleVariableDeclarationWithIntExpressionAssignment) 
{
    auto tokens = tokenize("var bla: int = 1 + 1 * (3 / 2);");
    Parser parser{tokens};
    ASTNode root_node = parser.parse();

    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <VARIABLE_DECLARATION>
        ├── <IDENTIFIER>(bla)
        ├── <DATA_TYPE>(int32)
        └── <EXPRESSION>
            └── <BINOP>(+)
                ├── <EXPRESSION_TERM>
                │   └── <EXPRESSION_VALUE>
                │       └── <CONSTANT>(1)
                └── <BINOP>(*)
                    ├── <EXPRESSION_TERM>
                    │   └── <EXPRESSION_VALUE>
                    │       └── <CONSTANT>(1)
                    └── <EXPRESSION_TERM>
                        └── <EXPRESSION_VALUE>
                            └── <EXPRESSION>
                                └── <BINOP>(/)
                                    ├── <EXPRESSION_TERM>
                                    │   └── <EXPRESSION_VALUE>
                                    │       └── <CONSTANT>(3)
                                    └── <EXPRESSION_TERM>
                                        └── <EXPRESSION_VALUE>
                                            └── <CONSTANT>(2))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    EXPECT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesEmptyFunction)
{
    Parser parser{tokenize("func main() -> void {}")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>)";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionWithParams)
{
    Parser parser{tokenize("func main(a: int, b: float) -> void {}")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        │   ├── <IDENTIFIER>(a)
        │   ├── <DATA_TYPE>(int32)
        │   ├── <IDENTIFIER>(b)
        │   └── <DATA_TYPE>(float32)
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>)";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithVariableDeclaration)
{
    Parser parser{tokenize("func main() -> void { var a: int = 0; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>
            └── <VARIABLE_DECLARATION>
                ├── <IDENTIFIER>(a)
                ├── <DATA_TYPE>(int32)
                └── <EXPRESSION>
                    └── <EXPRESSION_TERM>
                        └── <EXPRESSION_VALUE>
                            └── <CONSTANT>(0))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithVariableDeclarationAndAssignment)
{
    Parser parser{tokenize("func main() -> void { var a: int = 0; a = 0; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>
            ├── <VARIABLE_DECLARATION>
            │   ├── <IDENTIFIER>(a)
            │   ├── <DATA_TYPE>(int32)
            │   └── <EXPRESSION>
            │       └── <EXPRESSION_TERM>
            │           └── <EXPRESSION_VALUE>
            │               └── <CONSTANT>(0)
            └── <ASSIGNMENT>
                ├── <EXPRESSION_TERM>
                │   └── <EXPRESSION_VALUE>
                │       └── <IDENTIFIER>(a)
                ├── <BINOP>(=)
                └── <EXPRESSION>
                    └── <EXPRESSION_TERM>
                        └── <EXPRESSION_VALUE>
                            └── <CONSTANT>(0))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithMemberAccess)
{
    Parser parser{tokenize("func main() -> void { a.b.cccc.d; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>
            └── <ASSIGNMENT>
                └── <EXPRESSION_TERM>
                    ├── <EXPRESSION_VALUE>
                    │   └── <IDENTIFIER>(a)
                    └── <EXPRESSION_POSTOP>
                        ├── <IDENTIFIER>(b)
                        ├── <IDENTIFIER>(cccc)
                        └── <IDENTIFIER>(d))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithMemberMethodCall)
{
    Parser parser{tokenize("func main() -> void { a.b.cccc.method(); }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>
            └── <ASSIGNMENT>
                └── <EXPRESSION_TERM>
                    ├── <EXPRESSION_VALUE>
                    │   └── <IDENTIFIER>(a)
                    └── <EXPRESSION_POSTOP>
                        ├── <IDENTIFIER>(b)
                        ├── <IDENTIFIER>(cccc)
                        └── <FUNCTION_CALL>
                            ├── <IDENTIFIER>(method)
                            └── <ARGUMENT_LIST>)";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithMemberAssignment)
{
    Parser parser{tokenize("func main() -> void { a.b.cccc.d = 12; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>
            └── <ASSIGNMENT>
                ├── <EXPRESSION_TERM>
                │   ├── <EXPRESSION_VALUE>
                │   │   └── <IDENTIFIER>(a)
                │   └── <EXPRESSION_POSTOP>
                │       ├── <IDENTIFIER>(b)
                │       ├── <IDENTIFIER>(cccc)
                │       └── <IDENTIFIER>(d)
                ├── <BINOP>(=)
                └── <EXPRESSION>
                    └── <EXPRESSION_TERM>
                        └── <EXPRESSION_VALUE>
                            └── <CONSTANT>(12))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithSimpleIf)
{
    Parser parser{tokenize("func main() -> void { if b > 10 { a = false; } }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>
            └── <IF_STATEMENT>
                ├── <EXPRESSION>
                │   └── <BINOP>(>)
                │       ├── <EXPRESSION_TERM>
                │       │   └── <EXPRESSION_VALUE>
                │       │       └── <IDENTIFIER>(b)
                │       └── <EXPRESSION_TERM>
                │           └── <EXPRESSION_VALUE>
                │               └── <CONSTANT>(10)
                └── <STATEMENT_BLOCK>
                    └── <ASSIGNMENT>
                        ├── <EXPRESSION_TERM>
                        │   └── <EXPRESSION_VALUE>
                        │       └── <IDENTIFIER>(a)
                        ├── <BINOP>(=)
                        └── <EXPRESSION>
                            └── <EXPRESSION_TERM>
                                └── <EXPRESSION_VALUE>
                                    └── <CONSTANT>(false))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithIfAndElse)
{
    Parser parser{tokenize("func main() -> void { if b > 10 { a = false; } else { a = true; } }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>
            └── <IF_STATEMENT>
                ├── <EXPRESSION>
                │   └── <BINOP>(>)
                │       ├── <EXPRESSION_TERM>
                │       │   └── <EXPRESSION_VALUE>
                │       │       └── <IDENTIFIER>(b)
                │       └── <EXPRESSION_TERM>
                │           └── <EXPRESSION_VALUE>
                │               └── <CONSTANT>(10)
                ├── <STATEMENT_BLOCK>
                │   └── <ASSIGNMENT>
                │       ├── <EXPRESSION_TERM>
                │       │   └── <EXPRESSION_VALUE>
                │       │       └── <IDENTIFIER>(a)
                │       ├── <BINOP>(=)
                │       └── <EXPRESSION>
                │           └── <EXPRESSION_TERM>
                │               └── <EXPRESSION_VALUE>
                │                   └── <CONSTANT>(false)
                └── <ELSE_BRANCH>
                    └── <STATEMENT_BLOCK>
                        └── <ASSIGNMENT>
                            ├── <EXPRESSION_TERM>
                            │   └── <EXPRESSION_VALUE>
                            │       └── <IDENTIFIER>(a)
                            ├── <BINOP>(=)
                            └── <EXPRESSION>
                                └── <EXPRESSION_TERM>
                                    └── <EXPRESSION_VALUE>
                                        └── <CONSTANT>(true))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithIfElifAndElse)
{
    Parser parser{tokenize("func main() -> void { if b > 10 { a = false; } elif b < 5 { a = true; } else { c = false; } }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>
            └── <IF_STATEMENT>
                ├── <EXPRESSION>
                │   └── <BINOP>(>)
                │       ├── <EXPRESSION_TERM>
                │       │   └── <EXPRESSION_VALUE>
                │       │       └── <IDENTIFIER>(b)
                │       └── <EXPRESSION_TERM>
                │           └── <EXPRESSION_VALUE>
                │               └── <CONSTANT>(10)
                ├── <STATEMENT_BLOCK>
                │   └── <ASSIGNMENT>
                │       ├── <EXPRESSION_TERM>
                │       │   └── <EXPRESSION_VALUE>
                │       │       └── <IDENTIFIER>(a)
                │       ├── <BINOP>(=)
                │       └── <EXPRESSION>
                │           └── <EXPRESSION_TERM>
                │               └── <EXPRESSION_VALUE>
                │                   └── <CONSTANT>(false)
                ├── <ELIF_BRANCH>
                │   ├── <EXPRESSION>
                │   │   └── <BINOP>(<)
                │   │       ├── <EXPRESSION_TERM>
                │   │       │   └── <EXPRESSION_VALUE>
                │   │       │       └── <IDENTIFIER>(b)
                │   │       └── <EXPRESSION_TERM>
                │   │           └── <EXPRESSION_VALUE>
                │   │               └── <CONSTANT>(5)
                │   └── <STATEMENT_BLOCK>
                │       └── <ASSIGNMENT>
                │           ├── <EXPRESSION_TERM>
                │           │   └── <EXPRESSION_VALUE>
                │           │       └── <IDENTIFIER>(a)
                │           ├── <BINOP>(=)
                │           └── <EXPRESSION>
                │               └── <EXPRESSION_TERM>
                │                   └── <EXPRESSION_VALUE>
                │                       └── <CONSTANT>(true)
                └── <ELSE_BRANCH>
                    └── <STATEMENT_BLOCK>
                        └── <ASSIGNMENT>
                            ├── <EXPRESSION_TERM>
                            │   └── <EXPRESSION_VALUE>
                            │       └── <IDENTIFIER>(c)
                            ├── <BINOP>(=)
                            └── <EXPRESSION>
                                └── <EXPRESSION_TERM>
                                    └── <EXPRESSION_VALUE>
                                        └── <CONSTANT>(false))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithEmptyReturn)
{
    Parser parser{tokenize("func main() -> void { return; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(void)
        └── <STATEMENT_BLOCK>
            └── <RETURN_STATEMENT>)";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithReturnedValue)
{
    Parser parser{tokenize("func main() -> int { return 23; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <FUNCTION_DECLARATION>
        ├── <IDENTIFIER>(main)
        ├── <PARAMETER_LIST>
        ├── <DATA_TYPE>(int32)
        └── <STATEMENT_BLOCK>
            └── <RETURN_STATEMENT>
                └── <EXPRESSION>
                    └── <EXPRESSION_TERM>
                        └── <EXPRESSION_VALUE>
                            └── <CONSTANT>(23))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesEmptyObject)
{
    Parser parser{tokenize("obj Vec3 {}")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <OBJ_DECLARATION>
        ├── <IDENTIFIER>(Vec3)
        └── <DECLARATION_BODY>)";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesObjectWithMemberDeclaration)
{
    Parser parser{tokenize("obj Vec3 { public var a: int = 0; private var b: float = 10; var c: bool = false; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <OBJ_DECLARATION>
        ├── <IDENTIFIER>(Vec3)
        └── <DECLARATION_BODY>
            ├── <VARIABLE_DECLARATION>
            │   ├── <TOKEN>(public)
            │   ├── <IDENTIFIER>(a)
            │   ├── <DATA_TYPE>(int32)
            │   └── <EXPRESSION>
            │       └── <EXPRESSION_TERM>
            │           └── <EXPRESSION_VALUE>
            │               └── <CONSTANT>(0)
            ├── <VARIABLE_DECLARATION>
            │   ├── <TOKEN>(private)
            │   ├── <IDENTIFIER>(b)
            │   ├── <DATA_TYPE>(float32)
            │   └── <EXPRESSION>
            │       └── <EXPRESSION_TERM>
            │           └── <EXPRESSION_VALUE>
            │               └── <CONSTANT>(10)
            └── <VARIABLE_DECLARATION>
                ├── <TOKEN>(private)
                ├── <IDENTIFIER>(c)
                ├── <DATA_TYPE>(bool)
                └── <EXPRESSION>
                    └── <EXPRESSION_TERM>
                        └── <EXPRESSION_VALUE>
                            └── <CONSTANT>(false))";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesObjectWithMethodDeclaration)
{
    Parser parser{tokenize("obj Vec3 { public func a() {} private func b() {} func c() {} }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <OBJ_DECLARATION>
        ├── <IDENTIFIER>(Vec3)
        └── <DECLARATION_BODY>
            ├── <FUNCTION_DECLARATION>
            │   ├── <TOKEN>(public)
            │   ├── <IDENTIFIER>(a)
            │   ├── <PARAMETER_LIST>
            │   ├── <TOKEN>(void)
            │   └── <STATEMENT_BLOCK>
            ├── <FUNCTION_DECLARATION>
            │   ├── <TOKEN>(private)
            │   ├── <IDENTIFIER>(b)
            │   ├── <PARAMETER_LIST>
            │   ├── <TOKEN>(void)
            │   └── <STATEMENT_BLOCK>
            └── <FUNCTION_DECLARATION>
                ├── <IDENTIFIER>(c)
                ├── <PARAMETER_LIST>
                ├── <TOKEN>(void)
                └── <STATEMENT_BLOCK>)";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesObjectWithObjectDeclaration)
{
    Parser parser{tokenize("obj Vec3 { obj Vec2 {} }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <DECLARATION_BODY>
    └── <OBJ_DECLARATION>
        ├── <IDENTIFIER>(Vec3)
        └── <DECLARATION_BODY>
            └── <OBJ_DECLARATION>
                ├── <IDENTIFIER>(Vec2)
                └── <DECLARATION_BODY>)";

    ASSERT_TRUE(not parser.has_syntax_errors());
    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParserThrowsS1)
{
    Parser parser{tokenize("func main( -> {}")};
    ASTNode root_node = parser.parse();
    
    ASSERT_TRUE(parser.has_syntax_errors());
    ASSERT_EQ(parser.get_syntax_errors().size(), 1);
    ASSERT_EQ(parser.get_syntax_errors()[0].get_error_info()->err_code, "S1");
}

TEST(ParserTest, ParserThrowsS2)
{
    Parser parser{tokenize("func main(a: int b) -> {}")};
    ASTNode root_node = parser.parse();

    ASSERT_TRUE(parser.has_syntax_errors());
    ASSERT_EQ(parser.get_syntax_errors().size(), 1);
    ASSERT_EQ(parser.get_syntax_errors()[0].get_error_info()->err_code, "S2");
}

TEST(ParserTest, ParserThrowsS3)
{
    Parser parser{tokenize("func main(")};
    ASTNode root_node = parser.parse();

    ASSERT_TRUE(parser.has_syntax_errors());
    ASSERT_EQ(parser.get_syntax_errors().size(), 1);
    ASSERT_EQ(parser.get_syntax_errors()[0].get_error_info()->err_code, "S3");
}

TEST(ParserTest, ParserThrowsS4)
{
    Parser parser{tokenize("obj Vec3 { public; }")};
    ASTNode root_node = parser.parse();

    ASSERT_TRUE(parser.has_syntax_errors());
    ASSERT_EQ(parser.get_syntax_errors().size(), 1);
    ASSERT_EQ(parser.get_syntax_errors()[0].get_error_info()->err_code, "S4");
}

TEST(ParserTest, ParserThrowsS5)
{
    Parser parser{tokenize("var a: int = 0 a")};
    ASTNode root_node = parser.parse();

    ASSERT_TRUE(parser.has_syntax_errors());
    ASSERT_EQ(parser.get_syntax_errors().size(), 1);
    ASSERT_EQ(parser.get_syntax_errors()[0].get_error_info()->err_code, "S5");
}

TEST(ParserTest, ParserThrowsS6)
{
    // Unreachable
    ASSERT_TRUE(true);
}

TEST(ParserTest, ParserThrowsS7)
{
    Parser parser{tokenize("var a: int = =")};
    ASTNode root_node = parser.parse();

    ASSERT_TRUE(parser.has_syntax_errors());
    ASSERT_EQ(parser.get_syntax_errors().size(), 1);
    ASSERT_EQ(parser.get_syntax_errors()[0].get_error_info()->err_code, "S7");
}

TEST(ParserTest, ParserThrowsS8)
{
    Parser parser{tokenize("func ()")};
    ASTNode root_node = parser.parse();

    ASSERT_TRUE(parser.has_syntax_errors());
    ASSERT_EQ(parser.get_syntax_errors().size(), 1);
    ASSERT_EQ(parser.get_syntax_errors()[0].get_error_info()->err_code, "S8");
}

TEST(ParserTest, ParserThrowsS9)
{
    // Unreachable
    ASSERT_TRUE(true);
}

TEST(ParserTest, ParserThrowsS10)
{
    Parser parser{tokenize("func main() -> {}")};
    ASTNode root_node = parser.parse();

    ASSERT_TRUE(parser.has_syntax_errors());
    ASSERT_EQ(parser.get_syntax_errors().size(), 1);
    ASSERT_EQ(parser.get_syntax_errors()[0].get_error_info()->err_code, "S10");
}

TEST(ParserTest, ParserThrowsS11)
{
    Parser parser{tokenize("=")};
    ASTNode root_node = parser.parse();

    ASSERT_TRUE(parser.has_syntax_errors());
    ASSERT_EQ(parser.get_syntax_errors().size(), 1);
    ASSERT_EQ(parser.get_syntax_errors()[0].get_error_info()->err_code, "S11");
}

TEST(ParserTest, Temp)
{
    std::shared_ptr<ScriptSource> script_source = ScriptSource::from_contents(
R"(func main() {
    int a a;
}
)"
    );

    Parser parser{tokenize(script_source), script_source};
    parser.parse();

    ASSERT_TRUE(false);
}
