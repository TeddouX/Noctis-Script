#pragma once
#include "../ast_node.hpp"
#include "../../semantic_analysis/sa_value_type.hpp"


namespace NCSC::Parsing
{

class FuncDeclASTNode : public ASTNode
{
public:
    FuncDeclASTNode()
        : ASTNode{ASTNodeType::FUNCTION_DECLARATION}
    {}
    
    bool                                                                parser_is_method = false;
    bool                                                                parser_is_op_override = false;
    TokenType                                                           parser_operator_overriden = TokenType::INVALID;

    std::vector<std::pair<std::string, SemanticAnalysis::ValueType>>    func_params;
    SemanticAnalysis::ValueType                                         func_return_type;
    std::string                                                         name;
};

} // namespace NCSC::Parsing
