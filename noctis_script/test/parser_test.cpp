#include <gtest/gtest.h>

#include <print>

#include "noctis_script/parser/parser.hpp"
#include "noctis_script/lexer/lexer.hpp"


using namespace NCSC;


TEST(ParserTest, ParsesSimpleVariableDeclaration) 
{
    auto tokens = tokenize("let bla: int;");
    Parser parser{tokens};
    ASTNode root_node = parser.parse();

    const std::string expected_tree = 
R"(<ROOT>
└── <VARIABLE_DECLARATION>
    ├── <IDENTIFIER>(bla)
    └── <DATA_TYPE>(int32))";

    EXPECT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesSimpleVariableDeclarationWithIntConstantAssignment) 
{
    auto tokens = tokenize("let bla: int8 = 1;");
    Parser parser{tokens};
    ASTNode root_node = parser.parse();

    const std::string expected_tree = 
R"(<ROOT>
└── <VARIABLE_DECLARATION>
    ├── <IDENTIFIER>(bla)
    ├── <DATA_TYPE>(int8)
    └── <EXPRESSION>
        └── <EXPRESSION_TERM>
            └── <EXPRESSION_VALUE>
                └── <CONSTANT>(1))";

    EXPECT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesSimpleVariableDeclarationWithFloatConstantAssignment) 
{
    auto tokens = tokenize("let bla: float = 1.0;");
    Parser parser{tokens};
    ASTNode root_node = parser.parse();

    const std::string expected_tree = 
R"(<ROOT>
└── <VARIABLE_DECLARATION>
    ├── <IDENTIFIER>(bla)
    ├── <DATA_TYPE>(float32)
    └── <EXPRESSION>
        └── <EXPRESSION_TERM>
            └── <EXPRESSION_VALUE>
                └── <CONSTANT>(1.0))";

    EXPECT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesSimpleVariableDeclarationWithIntExpressionAssignment) 
{
    auto tokens = tokenize("let bla: int = 1 + 1 * (3 / 2);");
    Parser parser{tokens};
    ASTNode root_node = parser.parse();

    const std::string expected_tree = 
R"(<ROOT>
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

    EXPECT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesEmptyFunction)
{
    Parser parser{tokenize("func main() -> void {}")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <FUNCTION_DECLARATION>
    ├── <IDENTIFIER>(main)
    ├── <PARAMETER_LIST>
    ├── <DATA_TYPE>(void)
    └── <STATEMENT_BLOCK>)";

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionWithParams)
{
    Parser parser{tokenize("func main(a: int, b: float) -> void {}")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <FUNCTION_DECLARATION>
    ├── <IDENTIFIER>(main)
    ├── <PARAMETER_LIST>
    │   ├── <IDENTIFIER>(a)
    │   ├── <DATA_TYPE>(int32)
    │   ├── <IDENTIFIER>(b)
    │   └── <DATA_TYPE>(float32)
    ├── <DATA_TYPE>(void)
    └── <STATEMENT_BLOCK>)";

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithVariableDeclaration)
{
    Parser parser{tokenize("func main() -> void { let a: int = 0; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
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

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithVariableDeclarationAndAssignment)
{
    Parser parser{tokenize("func main() -> void { let a: int = 0; a = 0; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
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

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithMemberAccess)
{
    Parser parser{tokenize("func main() -> void { a.b.cccc.d; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
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

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithMemberMethodCall)
{
    Parser parser{tokenize("func main() -> void { a.b.cccc.method(); }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
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

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithMemberAssignment)
{
    Parser parser{tokenize("func main() -> void { a.b.cccc.d = 12; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
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

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithSimpleIf)
{
    Parser parser{tokenize("func main() -> void { if b > 10 { a = false; } }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
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

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithIfAndElse)
{
    Parser parser{tokenize("func main() -> void { if b > 10 { a = false; } else { a = true; } }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
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

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithIfElifAndElse)
{
    Parser parser{tokenize("func main() -> void { if b > 10 { a = false; } elif b < 5 { a = true; } else { c = false; } }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
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

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithEmptyReturn)
{
    Parser parser{tokenize("func main() -> void { return; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
└── <FUNCTION_DECLARATION>
    ├── <IDENTIFIER>(main)
    ├── <PARAMETER_LIST>
    ├── <DATA_TYPE>(void)
    └── <STATEMENT_BLOCK>
        └── <RETURN_STATEMENT>)";

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}

TEST(ParserTest, ParsesFunctionBodyWithReturnedValue)
{
    Parser parser{tokenize("func main() -> int { return 23; }")};
    ASTNode root_node = parser.parse();
    const std::string expected_tree = 
R"(<ROOT>
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

    ASSERT_EQ(root_node.ast_string(), expected_tree);
}
