
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/ParserCorpus)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/ParserLogs)

add_executable(parserFuzzer parserFuzzer.cpp)
target_link_libraries(parserFuzzer Frontend)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/LexerCorpus)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/LexerLogs)

add_executable(lexerFuzzer lexerFuzzer.cpp)
target_link_libraries(lexerFuzzer Frontend)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/PPLexerCorpus)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/PPLexerLogs)

add_executable(ppLexerFuzzer ppLexerFuzzer.cpp)
target_link_libraries(ppLexerFuzzer Frontend)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/PPParserCorpus)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/PPParserLogs)

add_executable(ppParserFuzzer ppParserFuzzer.cpp)
target_link_libraries(ppParserFuzzer Frontend)
