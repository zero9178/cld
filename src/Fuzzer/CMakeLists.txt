
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/ParserCorpus)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/ParserLogs)

add_executable(parserFuzzer parserFuzzer.cpp)
target_link_libraries(parserFuzzer Frontend)
target_compile_options(parserFuzzer PRIVATE -fno-omit-frame-pointer -fsanitize=fuzzer,undefined,address,leak)
target_link_options(parserFuzzer PRIVATE -fsanitize=fuzzer,undefined,address,leak)

file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/LexerCorpus)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/LexerLogs)

add_executable(lexerFuzzer lexerFuzzer.cpp)
target_link_libraries(lexerFuzzer Frontend)
target_compile_options(lexerFuzzer PRIVATE -fno-omit-frame-pointer -fsanitize=fuzzer,undefined,address,leak)
target_link_options(lexerFuzzer PRIVATE -fsanitize=fuzzer,undefined,address,leak)

file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/PPLexerCorpus)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/PPLexerLogs)

add_executable(ppLexerFuzzer ppLexerFuzzer.cpp)
target_link_libraries(ppLexerFuzzer Frontend)
target_compile_options(ppLexerFuzzer PRIVATE -fno-omit-frame-pointer -fsanitize=fuzzer,undefined,address,leak)
target_link_options(ppLexerFuzzer PRIVATE -fsanitize=fuzzer,undefined,address,leak)
