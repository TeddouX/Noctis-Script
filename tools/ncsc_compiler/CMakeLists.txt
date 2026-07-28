cmake_minimum_required(VERSION 4.4)
project(noctis_script_compiler
    VERSION 0.0.1
    LANGUAGES CXX
)

add_executable(noctis_script_compiler main.cpp)
set_target_properties(noctis_script_compiler PROPERTIES OUTPUT_NAME "ncsc_compiler")
target_link_libraries(noctis_script_compiler PRIVATE noctis_script::noctis_script)