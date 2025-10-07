cmake_minimum_required(VERSION 3.16)
project(NoctisScript)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(NCSC_SOURCES
    # -------------------
    #      Headers
    # -------------------
    "include/ncsc/compiler.hpp"
    "include/ncsc/function.hpp"
    "include/ncsc/lexer.hpp"
    "include/ncsc/ncsc.hpp"
    "include/ncsc/parser.hpp"
    "include/ncsc/script_node.hpp"
    "include/ncsc/syntax_error.hpp"
    "include/ncsc/token.hpp"
    "include/ncsc/type_info.hpp"

    # -------------------
    #    Source files
    # -------------------
    "src/compiler.cpp"
    "src/function.cpp"
    "src/lexer.cpp"
    "src/parser.cpp"
    "src/script_node.cpp"
    "src/syntax_error.cpp"
    "src/token.cpp"
    "src/type_info.cpp"
)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${NCSC_SOURCES})

add_library(NoctisScript SHARED ${NCSC_SOURCES})

target_compile_definitions(NoctisScript 
    PRIVATE
        NCSC_BUILD
)

target_include_directories(NoctisScript 
    PUBLIC 
        "include"
)
