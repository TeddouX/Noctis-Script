cmake_minimum_required(VERSION 3.8)
project(test)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(test "main.cpp")
target_link_libraries(test NoctisScript)

if (MSVC)
    set_property(TARGET test PROPERTY CMAKE_VS_DEBUGGER_WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
endif()

add_custom_command(TARGET test POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        $<TARGET_FILE:NoctisScript>
        $<TARGET_FILE_DIR:test>
)
