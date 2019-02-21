cmake_minimum_required(VERSION 3.13)
project(OpenCLParser)
list(APPEND CMAKE_PREFIX_PATH C:/MGPKG/LLVM-7.0.0)
set(CMAKE_CXX_STANDARD 17)
find_package(LLVM REQUIRED)

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
message(STATUS ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
list(APPEND CMAKE_CXX_FLAGS "-pedantic -Wall -Wextra -fdiagnostics-color=always")

llvm_map_components_to_libnames(llvm_libs Core ExecutionEngine MC MCJIT Object OrcJit Option RuntimeDyld Support native)

add_executable(OpenCLParser src/main.cpp src/Lexer.cpp src/Lexer.hpp src/buildTree.cpp src/Parser.hpp src/Parser.cpp src/Codegen.cpp)
target_link_libraries(OpenCLParser ${llvm_libs})
target_compile_definitions(OpenCLParser PUBLIC __STDC_FORMAT_MACROS)
