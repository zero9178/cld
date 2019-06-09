cmake_minimum_required(VERSION 3.10)
project(OpenCLParser)
list(APPEND CMAKE_PREFIX_PATH C:/MGPKG/LLVM-7.0.0)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD 99)

find_package(LLVM REQUIRED)
find_package(Threads REQUIRED)

if (MINGW)
    add_compile_options("-Wa,-mbig-obj")
endif ()

if (UNIX AND DEFINED SANITIZER)
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=${SANITIZER}")
endif ()

if (WIN32)
    add_compile_definitions(_WIN32_WINNT=0x0600 WINVER=0x0600)
endif ()

option(BUILD_TESTS "Build tests" ON)

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
include_directories(SYSTEM 3rdParty)
include_directories(src)
message(STATUS ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
if (NOT MSVC)
    list(APPEND CMAKE_CXX_FLAGS "-pedantic -Wall -Wextra -fdiagnostics-color=always")
    option(COVERAGE "Compile with coverage" OFF)

    if (COVERAGE)
        message(STATUS "Compiling with Coverage")
        link_libraries(gcov)
        add_compile_options(--coverage)
    endif ()
else ()
    add_compile_options(/w)
endif ()

llvm_map_components_to_libnames(llvm_libs Core ExecutionEngine MC MCJIT Object OrcJit Option RuntimeDyld Support native)

add_subdirectory(src/CompilerCore)

add_executable(OpenCLParser src/main.cpp)
target_link_libraries(OpenCLParser CompilerCore Preprocessor)
target_compile_definitions(OpenCLParser PUBLIC __STDC_FORMAT_MACROS)

add_executable(DemoC src/input.c)

if (${BUILD_TESTS})
    add_subdirectory(src/Tests)
endif ()
