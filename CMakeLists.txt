cmake_minimum_required(VERSION 3.10)
project(OpenCLParser)
list(APPEND CMAKE_PREFIX_PATH C:/MGPKG/LLVM-7.0.0)
list(APPEND CMAKE_PREFIX_PATH C:/MGPKG/gtest-1.8.0)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD 99)

find_package(LLVM REQUIRED)
find_package(Threads REQUIRED)

if(UNIX)
    set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=undefined")
    set (CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=undefined")
endif()

option(BUILD_TESTS "Build tests" ON)

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
message(STATUS ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
list(APPEND CMAKE_CXX_FLAGS "-pedantic -Wall -Wextra -fdiagnostics-color=always")

llvm_map_components_to_libnames(llvm_libs Core ExecutionEngine MC MCJIT Object OrcJit Option RuntimeDyld Support native)

add_library(Lexer src/CompilerCore/Lexer.cpp  src/CompilerCore/Preprocessor.cpp)

add_library(CompilerCore src/CompilerCore/Parser.cpp src/CompilerCore/Syntax.cpp src/CompilerCore/Codegen.cpp)
target_link_libraries(CompilerCore PUBLIC ${llvm_libs} Threads::Threads Lexer)

add_executable(OpenCLParser src/main.cpp)
target_link_libraries(OpenCLParser CompilerCore)
target_compile_definitions(OpenCLParser PUBLIC __STDC_FORMAT_MACROS)

add_executable(DemoC src/input.c)

if(${BUILD_TESTS})
    add_subdirectory(src/Tests)
endif()

if(UNIX)
    add_executable(LexerTest src/Tests/LinuxLexerTest.cpp)
    target_link_libraries(LexerTest PUBLIC Lexer)
    target_include_directories(LexerTest PRIVATE src)
endif()
