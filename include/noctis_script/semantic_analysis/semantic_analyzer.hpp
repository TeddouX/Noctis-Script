#pragma once
#include <memory>
#include <variant>

#include "sa_scope.hpp"
#include "sa_value_type.hpp"
#include "../parsing/ast_node.hpp"
#include "../error.hpp"

namespace NCSC
{

class SemanticAnalyzer
{
public:
    SemanticAnalyzer(std::shared_ptr<ASTNode> root, std::shared_ptr<ScriptSource> script_source = nullptr);

    auto do_analysis() -> std::shared_ptr<ASTNode>;

private:
    std::shared_ptr<ASTNode>                    root_node_;
    std::shared_ptr<SemanticAnalysis::Scope>    root_scope_;
    std::shared_ptr<SemanticAnalysis::Scope>    curr_scope_;

    std::shared_ptr<ScriptSource>               script_source_;

    std::vector<Error>                          analysis_errors_;

    template <typename... _Args>
    auto error(const std::shared_ptr<ErrorInfo> &err_info, const Location &location, _Args&&... args) -> void
    {
        std::string err_message = err_info->get_formatted(std::forward<_Args>(args)...);
        Error err{err_info, err_message, script_source_, location};

        analysis_errors_.push_back(err);
    }

    // Collect declarations
    auto first_pass() -> void;
    // Resolve names
    auto second_pass() -> void;
    // Type checking
    auto third_pass() -> void;
    // Control flow analysis
    auto fourth_pass() -> void;

    auto enter_new_scope() -> void;
    auto exit_scope() -> void;
    
    auto is_symbol_defined_elsewhere(const std::shared_ptr<ASTNode> &identifer) -> bool;
    auto value_type_from_node(const std::shared_ptr<ASTNode> &type_node) -> SemanticAnalysis::ValueType;

    FRIEND_TEST(SemanticAnalyzerTest, UpdatesPositionWithTokenCorrectly);

    inline static auto ERR_NOT_A_TYPE       {ErrorInfo::create("Semantic Analysis", "SA1", "'{}' is not a type.")};
    inline static auto ERR_ALREADY_DEFINED  {ErrorInfo::create("Semantic Analysis", "SA2", "'{}' was already defined somewhere else.")};
  
    inline static auto INFO_DEFINED_HERE    {ErrorInfo::create("Semantic Analysis", "SAI1",  "'{}' defined here:", ErrorLevel::INFO)};
};

} // namespace NCSC
