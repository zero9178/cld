
add_executable(FrontendTests lexer_tests.cpp parser_tests.cpp main.cpp semantics_tests.cpp constantevaluator_tests.cpp preprocessor_tests.cpp preprocessor_parser_tests.cpp diagnostics_tests.cpp)
target_link_libraries(FrontendTests Frontend testCommon)
set_source_files_properties(main.cpp PROPERTIES COMPILE_DEFINITIONS CATCH_CONFIG_RUNNER SKIP_PRECOMPILE_HEADERS ON)
if (CLD_USE_PCH)
    target_precompile_headers(FrontendTests PRIVATE <cld/Frontend/Compiler/ErrorMessages.hpp> <catch.hpp>)
endif ()
catch_discover_tests(FrontendTests)

add_executable(fuzzerCrashRunner fuzzerCrashRunner.cpp)
target_link_libraries(fuzzerCrashRunner Frontend)

add_executable(csmithRunner csmithRunner.cpp)
target_link_libraries(csmithRunner Frontend)
