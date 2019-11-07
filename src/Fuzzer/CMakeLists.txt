
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-omit-frame-pointer -fsanitize=fuzzer,undefined")

file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/Corpus)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/Logs)

add_executable(parserFuzzer parserFuzzer.cpp)
target_link_libraries(parserFuzzer Frontend)
