cmake_minimum_required(VERSION 3.10)
project(OpenCLParser)
list(APPEND CMAKE_PREFIX_PATH C:/MGPKG/LLVM-7.0.0)
list(APPEND CMAKE_PREFIX_PATH C:/MGPKG/gtest-1.8.0)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD 99)

find_package(LLVM REQUIRED)
find_package(Threads REQUIRED)

if(MINGW)
    add_compile_options("-Wa,-mbig-obj")
endif()

if(UNIX)
    set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=undefined")
    set (CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=undefined")
endif()

option(BUILD_TESTS "Build tests" ON)

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
include_directories(src)
include_directories(3rdParty)
message(STATUS ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
list(APPEND CMAKE_CXX_FLAGS "-pedantic -Wall -Wextra -fdiagnostics-color=always")

llvm_map_components_to_libnames(llvm_libs Core ExecutionEngine MC MCJIT Object OrcJit Option RuntimeDyld Support native)

add_subdirectory(src/CompilerCore)

add_executable(OpenCLParser src/main.cpp)
target_link_libraries(OpenCLParser CompilerCore Preprocessor)
target_compile_definitions(OpenCLParser PUBLIC __STDC_FORMAT_MACROS)

add_executable(DemoC src/input.c)

if(${BUILD_TESTS})
    add_subdirectory(src/Tests)
endif()
