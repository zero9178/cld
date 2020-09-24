
add_executable(CompilerTests lexer.cpp parser.cpp main.cpp semantics.cpp constantevaluator.cpp preprocessor.cpp preprocessor_parser.cpp diagnostics.cpp TestConfig.cpp commandline.cpp)
target_link_libraries(CompilerTests Frontend)
target_include_directories(CompilerTests PRIVATE ..)
set_source_files_properties(main.cpp PROPERTIES COMPILE_DEFINITIONS CATCH_CONFIG_RUNNER SKIP_PRECOMPILE_HEADERS ON)
if (CLD_USE_PCH)
    target_precompile_headers(CompilerTests PRIVATE <cld/Frontend/Compiler/ErrorMessages.hpp> <catch.hpp>)
endif ()

add_executable(fuzzerCrashRunner fuzzerCrashRunner.cpp)
target_link_libraries(fuzzerCrashRunner Frontend)

add_executable(csmithRunner csmithRunner.cpp)
target_link_libraries(csmithRunner Frontend)
