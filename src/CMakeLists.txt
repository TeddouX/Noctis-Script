set(NCSC_SOURCES
    "bytecode_gen/bytecode.cpp"
    "bytecode_gen/scope.cpp"
    "bytecode_gen/scope.cpp"
    "bytecode_gen/vm_bytecode_gen.cpp"
    "lexer/lexer.cpp"
    "lexer/token.cpp"
    "parser/ast_node.cpp"
    "parser/parser.cpp"
    "script/script_source.cpp"
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
