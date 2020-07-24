
add_executable(CompilerTests lexer.cpp parser.cpp main.cpp semantics.cpp constantevaluator.cpp preprocessor.cpp preprocessor_parser.cpp diagnostics.cpp TestConfig.cpp)
target_link_libraries(CompilerTests Frontend)
target_include_directories(CompilerTests PRIVATE ..)
target_compile_definitions(CompilerTests PRIVATE CATCH_CONFIG_ENABLE_ALL_STRINGMAKERS)
if (MSVC AND ("${CLD_SANITIZER}" MATCHES ".*address.*"))
    target_compile_definitions(CompilerTests PRIVATE CATCH_CONFIG_NO_WINDOWS_SEH)
endif ()
set_source_files_properties(main.cpp PROPERTIES COMPILE_DEFINITIONS CATCH_CONFIG_RUNNER)

add_executable(fuzzerCrashRunner fuzzerCrashRunner.cpp)
target_link_libraries(fuzzerCrashRunner Frontend)
