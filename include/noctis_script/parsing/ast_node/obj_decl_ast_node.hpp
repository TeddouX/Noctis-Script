#pragma once
#include "../ast_node.hpp"
#include "../../semantic_analysis/sa_value_type.hpp"
#include "func_decl_ast_node.hpp"
#include "var_decl_ast_node.hpp"


namespace NCSC::Parsing
{

class ObjDeclASTNode : public ASTNode
{
public:
    explicit ObjDeclASTNode(ASTNodeType type)
        : ASTNode{type}
    {}

    SemanticAnalysis::ValueType                 obj_type;
    std::string                                 obj_name;
    std::vector<TypeErased<ObjDeclASTNode>>     obj_objects;
    std::vector<TypeErased<FuncDeclASTNode>>    obj_methods;
    std::vector<TypeErased<VarDeclASTNode>>     obj_members;
};

} // namespace NCSC::Parsing