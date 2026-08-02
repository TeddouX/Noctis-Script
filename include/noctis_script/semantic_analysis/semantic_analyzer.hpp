#pragma once
#include <memory>
#include <variant>

#include "sa_scope.hpp"
#include "sa_value_type.hpp"
#include "../parsing/ast_node.hpp"
#include "../parsing/scoped_path.hpp"
#include "../error.hpp"
#include "module_context.hpp"

namespace NCSC
{

class SemanticAnalyzer
{
public:
    SemanticAnalyzer(
        TypeErased<ASTNode> root, 
        PtrRef<ScriptSource> script_source = nullptr,
        ModuleContext *module_ctx = nullptr
    );

    auto do_analysis() -> PtrRef<SemanticAnalysis::ModuleData>;
    // INTERNAL: do not use unless you know what you're doing
    auto init_root_scope() -> void;
    // INTERNAL: do not use unless you know what you're doing
    auto init_root_scope(const SemanticAnalysis::DeclIndices &decl_indices) -> void;

    auto has_analysis_errors() const -> bool;
    auto get_analysis_errors() const -> const std::vector<Error> &;

private:
    TypeErased<ASTNode>                     root_node_;
    PtrRef<SemanticAnalysis::Scope>         root_scope_;
    PtrRef<SemanticAnalysis::Scope>         curr_scope_;
    PtrRef<SemanticAnalysis::ModuleData>    module_data_;

    PtrRef<ScriptSource>                    script_source_;
    ModuleContext                          *module_ctx_;

    std::vector<Error>                      analysis_errors_;

    template <typename... _Args>
    auto error(const PtrRef<ErrorInfo> &err_info, const Location &location, _Args&&... args) -> void
    {
        std::string err_message = err_info->get_formatted(std::forward<_Args>(args)...);
        Error err{err_info, err_message, script_source_, location};

        analysis_errors_.push_back(err);
    }

    // Handle module imports
    auto first_pass() -> bool;
    // Collect declarations, resolve names, collect export data 
    auto second_pass() -> bool;
    // Type checking
    auto third_pass() -> bool;
    // Control flow analysis
    auto fourth_pass() -> bool;

    auto enter_new_scope() -> void;
    auto exit_scope() -> void;
    
    auto is_symbol_defined_elsewhere(const TypeErased<ASTNode> &identifer) -> bool;
    auto value_type_from_node(const TypeErased<ASTNode> &type_node) -> SemanticAnalysis::ValueType;

    auto get_declaration(const std::string &name, const Location &err_location) -> const SemanticAnalysis::DeclData *;
    auto add_global_declaration(const std::string &name, SemanticAnalysis::DeclData &data) -> isize_t;

    FRIEND_TEST(SemanticAnalyzerTest, FirstPassCorrectlyImportModules);
    FRIEND_TEST(SemanticAnalyzerTest, SecondPassCorrectlyUsesModulesExportedSymbols);
    FRIEND_TEST(SemanticAnalyzerTest, SecondPassFunctionCorrectData);
    FRIEND_TEST(SemanticAnalyzerTest, SecondPassGlobalVarCorrectData);
    FRIEND_TEST(SemanticAnalyzerTest, SecondPassObjectCorrectData);
    FRIEND_TEST(SemanticAnalyzerTest, SecondPassCorrectIndices);

    inline static auto ERR_NOT_A_TYPE           {ErrorInfo::create("Semantic Analysis", "SA1",  "'{}' is not a type.")};
    inline static auto ERR_ALREADY_DEFINED      {ErrorInfo::create("Semantic Analysis", "SA2",  "'{}' was already defined somewhere else.")};
    inline static auto ERR_NO_MODULE_CTXT       {ErrorInfo::create("Semantic Analysis", "SA3",  "INTERNAL: Can't resolve imports because no module context was given")};
    inline static auto ERR_NO_MODULE_NAMED      {ErrorInfo::create("Semantic Analysis", "SA4",  "Can't find module '{}' (check spelling ?)")};
    inline static auto ERR_SYMBOL_NOT_DEFINED   {ErrorInfo::create("Semantic Analysis", "SA5",  "'{}' was not defined in this module (check spelling ?)")};
    inline static auto ERR_CANT_EXPORT          {ErrorInfo::create("Semantic Analysis", "SA6",  "Can't export symbols because the script was defined as a module")};
    inline static auto ERR_SYMBOL_NOT_IMPORTED  {ErrorInfo::create("Semantic Analysis", "SA7",  "'{}' was not imported (import the module that defines it or check spelling)")};
    inline static auto ERR_USING_CONFLICT       {ErrorInfo::create("Semantic Analysis", "SA8",  "An alias already exists for '{}' (remove this one)")};
    inline static auto ERR_MODULE_NOT_IMPORTED  {ErrorInfo::create("Semantic Analysis", "SA9",  "Module '{}' was not imported (import the module or check spelling)")};
    inline static auto ERR_AMBIGUOUS_SYMBOL     {ErrorInfo::create("Semantic Analysis", "SA10", "'{}' is ambiguous between {}")};
  
    inline static auto INFO_DEFINED_HERE        {ErrorInfo::create("Semantic Analysis", "SAI1",  "'{}' defined here:", ErrorLevel::INFO)};
    inline static auto INFO_DEFINED_IN_MODULE   {ErrorInfo::create("Semantic Analysis", "SAI2",  "'{}' defined in module '{}'", ErrorLevel::INFO)};
};

} // namespace NCSC
