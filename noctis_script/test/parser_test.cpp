#include <gtest/gtest.h>

#include <print>

#include "noctis_script/parser.hpp"
#include "noctis_script/lexer.hpp"


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
    └── <DATA_TYPE>(int))";

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
    ├── <DATA_TYPE>(float)
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
    ├── <DATA_TYPE>(int)
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
