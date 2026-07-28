cmake_minimum_required(VERSION 4.4)
project(noctis_script_cli
    VERSION 0.0.1
    LANGUAGES CXX
)

add_executable(noctis_script_cli main.cpp)
set_target_properties(noctis_script_cli PROPERTIES OUTPUT_NAME "ncsc")
target_link_libraries(noctis_script_cli PRIVATE noctis_script::noctis_script)
