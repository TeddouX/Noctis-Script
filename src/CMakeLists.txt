set(NCSC_SOURCES
    # "bytecode_gen/bytecode_gen.cpp"
    "bytecode_gen/bytecode.cpp"
    "bytecode_gen/scope.cpp"
    "bytecode_gen/value_type.cpp"
    "lexing/lexer.cpp"
    "lexing/token.cpp"
    "parsing/ast_node.cpp"
    "parsing/parser.cpp"
    "parsing/scoped_path.cpp"
    "script/script_source.cpp"
    "semantic_analysis/module_context.cpp"
    "semantic_analysis/sa_scope.cpp"
    "semantic_analysis/sa_value_type.cpp"
    "semantic_analysis/semantic_analyzer.cpp"
    "error.cpp"
)

if(NCSC_BUILD_SHARED)
    add_library(noctis_script SHARED ${NCSC_SOURCES})
else()
    add_library(noctis_script STATIC ${NCSC_SOURCES})
endif()

add_library(noctis_script::noctis_script ALIAS noctis_script)

target_include_directories(noctis_script
    PUBLIC
        ${CMAKE_SOURCE_DIR}/include
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_SOURCE_DIR}/include/noctis_script
)
