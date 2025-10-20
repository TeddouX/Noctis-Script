// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <gtest/gtest.h>
#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>

using namespace NCSC;

auto parse(const std::string &code) {
    auto tokens = Lexer(code).tokenizeAll();
    return Parser(tokens).parseAll();
}

TEST(ParserTests, ParsesVariableDeclaration) {
    auto rootNode = parse("Int a;");

    ASSERT_TRUE(rootNode.hasChildren());
    ASSERT_EQ(rootNode.children[0].type, ScriptNodeType::VARIABLE_DECLARATION);
}

TEST(ParserTests, VariableDeclarationTypeIsParsed) {
    auto rootNode = parse("UInt64 a;");

    ASSERT_TRUE(rootNode.hasChildren());
    ASSERT_TRUE(rootNode.children[0].hasChildren());
    ASSERT_EQ(rootNode.children[0].children[0].type, ScriptNodeType::DATA_TYPE);
}

TEST(ParserTests, VariableDeclarationIdentifierIsParsed) {
    auto rootNode = parse("Double jdkqsljdlqsjdmjqmLDJKLMQS;");

    ASSERT_TRUE(rootNode.hasChildren());
    ASSERT_TRUE(rootNode.children[0].hasChildren());
    ASSERT_EQ(rootNode.children[0].children[1].type, ScriptNodeType::IDENTIFIER);
}

TEST(ParserTests, VariableDeclarationWithAssignmentIsParsed) {
    auto rootNode = parse("Double a = 1.00010;");

    ASSERT_TRUE(rootNode.hasChildren());
    ASSERT_TRUE(rootNode.children[0].hasChildren());
    ASSERT_EQ(rootNode.children[0].children.size(), 3);
    ASSERT_EQ(rootNode.children[0].children[2].type, ScriptNodeType::EXPRESSION);
}

TEST(ParserTests, CanParseTwoStatementsSeparatedBySemicolon) {
    auto rootNode = parse("Double a; Int b;");

    ASSERT_TRUE(rootNode.hasChildren());
    ASSERT_EQ(rootNode.children.size(), 2);
    ASSERT_EQ(rootNode.children[0].type, ScriptNodeType::VARIABLE_DECLARATION);
    ASSERT_EQ(rootNode.children[1].type, ScriptNodeType::VARIABLE_DECLARATION);
}
