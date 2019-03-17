cmake_minimum_required(VERSION 3.13)
project(OpenCLParser)
list(APPEND CMAKE_PREFIX_PATH C:/MGPKG/LLVM-7.0.0)
list(APPEND CMAKE_PREFIX_PATH C:/MGPKG/gtest-1.8.0)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD 99)

find_package(LLVM REQUIRED)

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
message(STATUS ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
list(APPEND CMAKE_CXX_FLAGS "-pedantic -Wall -Wextra -fdiagnostics-color=always")

llvm_map_components_to_libnames(llvm_libs Core ExecutionEngine MC MCJIT Object OrcJit Option RuntimeDyld Support native)

add_library(CompilerCore src/CompilerCore/Lexer.cpp src/CompilerCore/Lexer.hpp src/CompilerCore/buildTree.cpp src/CompilerCore/Parser.hpp src/CompilerCore/Parser.cpp src/CompilerCore/Codegen.cpp)
target_link_libraries(CompilerCore PUBLIC ${llvm_libs})

add_executable(OpenCLParser src/main.cpp)
target_link_libraries(OpenCLParser CompilerCore)
target_compile_definitions(OpenCLParser PUBLIC __STDC_FORMAT_MACROS)

add_executable(DemoC src/input.c)

add_subdirectory(src/Tests)
