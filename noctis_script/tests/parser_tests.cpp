// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2025, TeddouX (https://github.com/TeddouX/)
#include <gtest/gtest.h>
#include <ncsc/lexer.hpp>
#include <ncsc/parser.hpp>

using namespace NCSC;

auto parse(const std::string &code) {
    auto src = ScriptSource::fromSource(code); 
    auto tokens = Lexer(src).tokenizeAll();
    return Parser(tokens, src).parseAll();
}

TEST(ParserTests, ParsesVariableDeclaration) {
    auto rootNode = parse("Int a;");

    ASSERT_TRUE(rootNode.hasChildren());
    ASSERT_EQ(rootNode.getChild(0).getType(), ScriptNodeType::VARIABLE_DECLARATION);
}

TEST(ParserTests, VariableDeclarationTypeIsParsed) {
    auto rootNode = parse("UInt64 a;");

    ASSERT_TRUE(rootNode.hasChildren());
    ASSERT_TRUE(rootNode.getChild(0).hasChildren());
    ASSERT_EQ(rootNode.getChild(0).getChild(0).getType(), ScriptNodeType::DATA_TYPE);
}

TEST(ParserTests, VariableDeclarationIdentifierIsParsed) {
    auto rootNode = parse("Double jdkqsljdlqsjdmjqmLDJKLMQS;");

    ASSERT_TRUE(rootNode.hasChildren());
    ASSERT_TRUE(rootNode.getChild(0).hasChildren());
    ASSERT_EQ(rootNode.getChild(0).getChild(1).getType(), ScriptNodeType::IDENTIFIER);
}

TEST(ParserTests, VariableDeclarationWithAssignmentIsParsed) {
    auto rootNode = parse("Double a = 1.00010;");

    ASSERT_TRUE(rootNode.hasChildren());
    ASSERT_TRUE(rootNode.getChild(0).hasChildren());
    ASSERT_EQ(rootNode.getChild(0).getNumChildren(), 3);
    ASSERT_EQ(rootNode.getChild(0).getChild(2).getType(), ScriptNodeType::EXPRESSION);
}

TEST(ParserTests, CanParseTwoStatementsSeparatedBySemicolon) {
    auto rootNode = parse("Double a; Int b;");

    ASSERT_TRUE(rootNode.hasChildren());
    ASSERT_EQ(rootNode.getNumChildren(), 2);
    ASSERT_EQ(rootNode.getChild(0).getType(), ScriptNodeType::VARIABLE_DECLARATION);
    ASSERT_EQ(rootNode.getChild(1).getType(), ScriptNodeType::VARIABLE_DECLARATION);
}
