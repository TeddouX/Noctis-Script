#include <gtest/gtest.h>

#include "noctis_script/parsing/ast_node.hpp"

using namespace NCSC;


TEST(ASTNodeTest, UpdatesPositionWithTokenCorrectly) 
{
    auto node = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    Token tok{TokenType::ID, "abc"};
    tok.location = Location{12, 12, 15, 17};

    node->set_token(tok);

    const Location &loc = node->location();
    ASSERT_EQ(loc.line, 12);
    ASSERT_EQ(loc.line_end, 12);
    ASSERT_EQ(loc.column, 15);
    ASSERT_EQ(loc.column_end, 17);
}

TEST(ASTNodeTest, UpdatesPositionColumnBackwardsWithOtherNode) 
{
    auto root = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    auto first = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    auto second = std::make_shared<ASTNode>(ASTNodeType::ROOT);

    first->set_location(Location{2, 2, 15, 18});
    second->set_location(Location{2, 2, 12, 16});

    root->add_child(first);
    root->add_child(second);

    const Location &loc = root->location();
    ASSERT_EQ(loc.line, 2);
    ASSERT_EQ(loc.line_end, 2);
    ASSERT_EQ(loc.column, 12);
    ASSERT_EQ(loc.column_end, 18);
}

TEST(ASTNodeTest, UpdatesPositionLineBackwardsWithOtherNode) 
{
    auto root = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    auto first = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    auto second = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    
    first->set_location(Location{2, 2, 15, 22});
    second->set_location(Location{1, 1, 10, 16});

    root->add_child(first);
    root->add_child(second);

    const Location &loc = root->location();
    ASSERT_EQ(loc.line, 1);
    ASSERT_EQ(loc.line_end, 2);
    ASSERT_EQ(loc.column, 10);
    ASSERT_EQ(loc.column_end, 22);
}

TEST(ASTNodeTest, UpdatesPositionColumnForwardsWithOtherNode) 
{
    auto root = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    auto first = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    auto second = std::make_shared<ASTNode>(ASTNodeType::ROOT);

    first->set_location(Location{2, 2, 12, 16});
    second->set_location(Location{2, 2, 15, 18});

    root->add_child(first);
    root->add_child(second);

    const Location &loc = root->location();
    ASSERT_EQ(loc.line, 2);
    ASSERT_EQ(loc.line_end, 2);
    ASSERT_EQ(loc.column, 12);
    ASSERT_EQ(loc.column_end, 18);
}

TEST(ASTNodeTest, UpdatesPositionLineForwardsWithOtherNode) 
{
    auto root = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    auto first = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    auto second = std::make_shared<ASTNode>(ASTNodeType::ROOT);
    
    first->set_location(Location{1, 1, 10, 16});
    second->set_location(Location{2, 2, 15, 22});

    root->add_child(first);
    root->add_child(second);

    const Location &loc = root->location();
    ASSERT_EQ(loc.line, 1);
    ASSERT_EQ(loc.line_end, 2);
    ASSERT_EQ(loc.column, 10);
    ASSERT_EQ(loc.column_end, 22);
}
