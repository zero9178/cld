cmake_minimum_required(VERSION 3.13)
project(OpenCLParser)
list(APPEND CMAKE_PREFIX_PATH C:/MGPKG/LLVM-7.0.0)
set(CMAKE_CXX_STANDARD 17)
find_package(LLVM REQUIRED)

include_directories(${LLVM_INCLUDE_DIRS})
message(STATUS ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs Core ExecutionEngine MC MCJIT Object OrcJit Option RuntimeDyld Support native)

add_executable(OpenCLParser src/main.cpp src/Lexer.cpp src/Lexer.hpp)
target_link_libraries(OpenCLParser ${llvm_libs})
target_compile_definitions(OpenCLParser PUBLIC __STDC_FORMAT_MACROS)
target_compile_options(OpenCLParser PUBLIC -Wall)
