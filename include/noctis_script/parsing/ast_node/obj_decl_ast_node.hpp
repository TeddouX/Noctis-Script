#pragma once
#include "../ast_node.hpp"
#include "../../semantic_analysis/sa_decl_data.hpp"
#include "../../semantic_analysis/sa_decl_indices.hpp"
#include "func_decl_ast_node.hpp"
#include "var_decl_ast_node.hpp"


namespace NCSC::Parsing
{

class ObjDeclASTNode : public ASTNode
{
public:
    explicit ObjDeclASTNode()
        : ASTNode{ASTNodeType::OBJ_DECLARATION}
    {}

    SemanticAnalysis::DeclIndices                               decl_indices;

    SemanticAnalysis::ValueType                                 obj_type;
    std::string                                                 name;

    std::unordered_map<std::string, SemanticAnalysis::DeclData> obj_objects;
    std::unordered_map<std::string, SemanticAnalysis::DeclData> obj_methods;
    std::unordered_map<std::string, SemanticAnalysis::DeclData> obj_members;
};

} // namespace NCSC::Parsing