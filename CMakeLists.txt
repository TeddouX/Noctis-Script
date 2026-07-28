cmake_minimum_required(VERSION 4.4)
project(noctis_script
    VERSION 0.0.1
    LANGUAGES CXX
)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(NCSC_BUILD_CLI       "Build the noctis script CLI"           OFF)
option(NCSC_BUILD_COMPILER  "Build the noctis script compiler"      OFF)
option(NCSC_BUILD_TESTS     "Build the test suite"                  OFF)
option(NCSC_BUILD_SHARED    "Build libnoctisscript as a shared lib" OFF)

# Core code
add_subdirectory(src)

if(NCSC_BUILD_CLI)
    add_subdirectory(cli)
endif()

if(NCSC_BUILD_COMPILER)
    add_subdirectory(tools/ncsc_compiler)
endif()

if(NCSC_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()
