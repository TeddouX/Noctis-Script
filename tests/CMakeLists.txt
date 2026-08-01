include(FetchContent)
FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

add_executable(noctis_script_tests
    "unit_tests/ast_node_test.cpp"
    "unit_tests/lexer_test.cpp"
    "unit_tests/parser_test.cpp"
    "unit_tests/script_source_test.cpp"
    "unit_tests/semantic_analyzer_test.cpp"
)

target_link_libraries(noctis_script_tests
    noctis_script::noctis_script
    GTest::gtest_main
)

include(GoogleTest)
gtest_discover_tests(noctis_script_tests)
