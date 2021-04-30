cmake_minimum_required(VERSION 3.11)

if (POLICY CMP0091)
    cmake_policy(SET CMP0091 NEW)
endif ()
if (POLICY CMP0092)
    cmake_policy(SET CMP0092 NEW)
endif ()

cmake_policy(SET CMP0069 NEW)

set(DEFAULT_CLD_USE_EXCEPTIONS ON)
if (MSVC AND CLD_FUZZER)
    set(DEFAULT_CLD_USE_EXCEPTIONS OFF)
endif ()


project(cld LANGUAGES CXX VERSION 1.0.0)
set(CMAKE_CXX_STANDARD 17)
option(CLD_BUILD_TESTS "Build tests" ON)
option(CLD_FUZZER "Build compiler with libfuzzer instrumentation and fuzzers" OFF)
option(CLD_ENABLE_ASSERTS "Enable asserts in Release mode" ${CLD_FUZZER})
option(CLD_USE_EXCEPTIONS "Use exceptions" "${DEFAULT_CLD_USE_EXCEPTIONS}")
option(CLD_COVERAGE "Compile with coverage" OFF)
option(CLD_DEFAULT_TARGET "Default compiler target" "")

include(GNUInstallDirs)

if (CLD_ENABLE_ASSERTS)
    add_compile_definitions(CLD_USE_ASSERTS)
endif ()

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" AND MSVC AND (CLD_COVERAGE OR CLD_FUZZER OR CLD_SANITIZER))
    execute_process(
            COMMAND ${CMAKE_CXX_COMPILER} /clang:-print-libgcc-file-name /clang:--rtlib=compiler-rt
            OUTPUT_VARIABLE clang_compiler_rt_file
            ERROR_VARIABLE clang_cl_stderr
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_STRIP_TRAILING_WHITESPACE
            RESULT_VARIABLE clang_cl_exit_code)
    if (NOT "${clang_cl_exit_code}" STREQUAL "0")
        message(FATAL_ERROR
                "Unable to invoke clang-cl to find resource dir: ${clang_cl_stderr}")
    endif ()
    file(TO_CMAKE_PATH "${clang_compiler_rt_file}" clang_compiler_rt_file)
    get_filename_component(clang_runtime_dir "${clang_compiler_rt_file}" DIRECTORY)
    message(STATUS "Clang-cl runtimes found in ${clang_runtime_dir}")
    link_directories(${clang_runtime_dir})
endif ()

if (DEFINED CLD_SANITIZER)
    if (MSVC)
        set(DEFAULT_CLD_USE_EXCEPTIONS OFF)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Oy- -fsanitize=${CLD_SANITIZER} -fno-sanitize-recover=all")
        link_libraries(clang_rt.asan.lib)
        link_libraries(clang_rt.asan_cxx.lib)
        link_libraries(clang_rt.asan-preinit.lib)
    else ()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-omit-frame-pointer -fsanitize=${CLD_SANITIZER} -fno-sanitize-recover=all")
    endif ()
endif ()


if (WIN32)
    if (NOT MSVC)
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--stack,8000000")
    else ()
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /STACK:8000000")
    endif ()
    add_compile_definitions(WIN32_LEAN_AND_MEAN NOMINMAX)
endif ()


find_package(LLVM REQUIRED 12)

include_directories(SYSTEM 3rdParty)
include_directories(src)
message(STATUS ${LLVM_INCLUDE_DIRS})
add_definitions("${LLVM_DEFINITIONS}")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
add_compile_definitions(CTRE_STRING_IS_UTF8)

if (NOT LLVM_ENABLE_RTTI)
    if (MSVC)
        string(REGEX REPLACE "/GR" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /GR-")
    else ()
        string(REGEX REPLACE "-frtti" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
    endif ()
endif ()

if (CLD_COVERAGE)
    message(STATUS "Compiling with Coverage")
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
        add_compile_options(--coverage)
        if (WIN32)
            link_libraries(gcov)
        endif ()
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        add_compile_options(-fprofile-instr-generate -fcoverage-mapping)
        if (NOT MSVC)
            add_link_options(-fprofile-instr-generate)
        endif ()
    else ()
        message(ERROR "Unknown coverage implementation")
    endif ()
endif ()

if (CLD_USE_EXCEPTIONS)
    if (MSVC)
        add_compile_options(/EHsc)
    endif ()
else ()
    if (MSVC)
        add_compile_options(/EHs-c-)
        add_compile_definitions(_HAS_EXCEPTIONS=0)
    else ()
        add_compile_options(-fno-exceptions)
    endif ()
endif ()

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    add_compile_options(-Wno-assume -Wno-nullability-extension -Wno-nullability-completeness -Wno-unknown-pragmas)
endif ()
if (NOT MSVC)
    find_package(Threads REQUIRED)
    link_libraries(Threads::Threads)
    add_compile_options(-pedantic -Wall -Wextra -Wno-unknown-pragmas -Wnon-virtual-dtor)
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        add_compile_options(-fcolor-diagnostics)
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
        add_compile_options(-fdiagnostics-color=always)
        if (WIN32)
            if (NOT ${CMAKE_CXX_FLAGS} MATCHES ".*[ \t\r\n]-flto[^a-zA-Z_].*")
                add_compile_options(-Wa,-mbig-obj)
            endif ()
        endif ()
    endif ()
else ()
    add_compile_options(/bigobj /permissive- /W4 /wd4068 /w34716 /Zc:__cplusplus /utf-8)
    add_compile_definitions(_CRT_SECURE_NO_WARNINGS _SILENCE_CXX17_ITERATOR_BASE_CLASS_DEPRECATION_WARNING)
    #Taken from HandleLLVMOptions.cmake
    add_compile_options(# Disabled warnings.
            -wd4141 # Suppress ''modifier' : used more than once' (because of __forceinline combined with inline)
            -wd4146 # Suppress 'unary minus operator applied to unsigned type, result still unsigned'
            -wd4244 # Suppress ''argument' : conversion from 'type1' to 'type2', possible loss of data'
            -wd4267 # Suppress ''var' : conversion from 'size_t' to 'type', possible loss of data'
            -wd4291 # Suppress ''declaration' : no matching operator delete found; memory will not be freed if initialization throws an exception'
            -wd4345 # Suppress 'behavior change: an object of POD type constructed with an initializer of the form () will be default-initialized'
            -wd4351 # Suppress 'new behavior: elements of array 'array' will be default initialized'
            -wd4456 # Suppress 'declaration of 'var' hides local variable'
            -wd4457 # Suppress 'declaration of 'var' hides function parameter'
            -wd4458 # Suppress 'declaration of 'var' hides class member'
            -wd4459 # Suppress 'declaration of 'var' hides global declaration'
            -wd4503 # Suppress ''identifier' : decorated name length exceeded, name was truncated'
            -wd4624 # Suppress ''derived class' : destructor could not be generated because a base class destructor is inaccessible'
            -wd4722 # Suppress 'function' : destructor never returns, potential memory leak
            -wd4100 # Suppress 'unreferenced formal parameter'
            -wd4127 # Suppress 'conditional expression is constant'
            -wd4512 # Suppress 'assignment operator could not be generated'
            -wd4505 # Suppress 'unreferenced local function has been removed'
            -wd4610 # Suppress '<class> can never be instantiated'
            -wd4510 # Suppress 'default constructor could not be generated'
            -wd4702 # Suppress 'unreachable code'
            -wd4245 # Suppress ''conversion' : conversion from 'type1' to 'type2', signed/unsigned mismatch'
            -wd4706 # Suppress 'assignment within conditional expression'
            -wd4310 # Suppress 'cast truncates constant value'
            -wd4701 # Suppress 'potentially uninitialized local variable'
            -wd4703 # Suppress 'potentially uninitialized local pointer variable'
            -wd4389 # Suppress 'signed/unsigned mismatch'
            -wd4611 # Suppress 'interaction between '_setjmp' and C++ object destruction is non-portable'
            -wd4805 # Suppress 'unsafe mix of type <type> and type <type> in operation'
            -wd4204 # Suppress 'nonstandard extension used : non-constant aggregate initializer'
            -wd4577 # Suppress 'noexcept used with no exception handling mode specified; termination on exception is not guaranteed'
            -wd4091 # Suppress 'typedef: ignored on left of '' when no variable is declared'
            # C4592 is disabled because of false positives in Visual Studio 2015
            # Update 1. Re-evaluate the usefulness of this diagnostic with Update 2.
            -wd4592 # Suppress ''var': symbol will be dynamically initialized (implementation limitation)
            -wd4319 # Suppress ''operator' : zero extending 'type' to 'type' of greater size'
            # C4709 is disabled because of a bug with Visual Studio 2017 as of
            # v15.8.8. Re-evaluate the usefulness of this diagnostic when the bug
            # is fixed.
            -wd4709 # Suppress comma operator within array index expression
            -wd4848

            # Ideally, we'd like this warning to be enabled, but even MSVC 2019 doesn't
            # support the 'aligned' attribute in the way that clang sources requires (for
            # any code that uses the LLVM_ALIGNAS macro), so this is must be disabled to
            # avoid unwanted alignment warnings.
            -wd4324 # Suppress 'structure was padded due to __declspec(align())'

            # Promoted warnings.
            -w14062 # Promote 'enumerator in switch of enum is not handled' to level 1 warning.

            # Promoted warnings to errors.
            -we4238 # Promote 'nonstandard extension used : class rvalue used as lvalue' to error.
            )
    if (NOT "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        add_compile_options(/Zc:preprocessor)
    endif ()
endif ()

if (${CLD_BUILD_TESTS})
    enable_testing()
    if (CLD_FUZZER AND "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" AND NOT MINGW)
        add_compile_definitions(CLD_IN_FUZZER)
        if (WIN32)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=fuzzer-no-link,address -fno-sanitize-recover=all")
            link_libraries(clang_rt.asan.lib)
            link_libraries(clang_rt.asan_cxx.lib)
            link_libraries(clang_rt.asan-preinit.lib)
            link_libraries(clang_rt.fuzzer.lib)
        else ()
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-omit-frame-pointer -fsanitize=fuzzer-no-link,undefined,address -fno-sanitize-recover=all")
        endif ()
        add_subdirectory(src/cld/Fuzzer)
    endif ()
    add_subdirectory(src/cld/Tests)
endif ()

add_subdirectory(src/cld/Support)
add_subdirectory(src/cld/Frontend)
add_subdirectory(src/cld/LLVMBackend)
add_subdirectory(src/cld/cldMain)

add_executable(cld src/cld/main.cpp)
target_link_libraries(cld cldMain)

install(TARGETS cld cldMain Support Frontend LLVMBackend
        EXPORT cldConfig
        RUNTIME DESTINATION ${CMAKE_INSTALL_DIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        )

export(TARGETS cld cldMain Support Frontend LLVMBackend NAMESPACE cld:: FILE "${CMAKE_CURRENT_BINARY_DIR}/cldConfig.cmake")

install(DIRECTORY src/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} FILES_MATCHING PATTERN "*.hpp" PATTERN "*.def" PATTERN "cld/Tests" EXCLUDE PATTERN "cld/Fuzzer" EXCLUDE)

install(EXPORT cldConfig DESTINATION "${CMAKE_INSTALL_DATADIR}/cld/cmake" NAMESPACE cld::)
