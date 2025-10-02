cmake_minimum_required(VERSION 3.16)
project(NoctisScript)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(NCSC_SOURCES
    # -------------------
    #      Headers
    # -------------------
    "include/ncsc/lexer.hpp"
    "include/ncsc/ncsc.hpp"
    "include/ncsc/parser.hpp"
    "include/ncsc/script_node.hpp"
    "include/ncsc/token.hpp"

    # -------------------
    #    Source files
    # -------------------
    "src/lexer.cpp"
    "src/parser.cpp"
    "src/script_node.cpp"
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




include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(NCSCTests
    "tests/lexer_tests.cpp"
)

target_link_libraries(NCSCTests
    NoctisScript
    GTest::gtest_main
)

add_custom_command(TARGET NCSCTests POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        $<TARGET_FILE:NoctisScript>
        $<TARGET_FILE_DIR:NCSCTests>
)

include(GoogleTest)
gtest_discover_tests(NCSCTests)
