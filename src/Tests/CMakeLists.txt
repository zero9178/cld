
add_executable(CompilerTests lexer.cpp parser.cpp main.cpp semantics.cpp constantevaluator.cpp preprocessor.cpp preprocessor_parser.cpp)
target_link_libraries(CompilerTests Frontend)
target_include_directories(CompilerTests PRIVATE ..)
target_compile_definitions(CompilerTests PRIVATE CATCH_CONFIG_ENABLE_ALL_STRINGMAKERS)
