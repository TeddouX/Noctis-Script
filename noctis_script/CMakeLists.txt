cmake_minimum_required(VERSION 4.4)
project(NoctisScript)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(NCSC_INCLUDE_DIR "include/noctis_script")

set(NCSC_SOURCES
    # -------------------
    #      Headers
    # -------------------
    "${NCSC_INCLUDE_DIR}/ast_node.hpp"
    "${NCSC_INCLUDE_DIR}/lexer.hpp"
    "${NCSC_INCLUDE_DIR}/parser.hpp"
    "${NCSC_INCLUDE_DIR}/token.hpp"


    # -------------------
    #    Source files
    # -------------------
    "src/ast_node.cpp"
    "src/lexer.cpp"
    "src/parser.cpp"
    "src/token.cpp"
)

add_library(NoctisScript SHARED ${NCSC_SOURCES})

target_compile_definitions(NoctisScript 
    PRIVATE
        NCSC_BUILD
)

target_include_directories(NoctisScript 
    PUBLIC 
        "include"
    PRIVATE
        "include/noctis_script"
)



include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(NCSCTests
    "test/ast_node_test.cpp"
    "test/lexer_test.cpp"
)

target_link_libraries(NCSCTests
    NoctisScript
    GTest::gtest_main
)

include(GoogleTest)
gtest_discover_tests(NCSCTests)
