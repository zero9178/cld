
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-omit-frame-pointer -fsanitize=fuzzer,undefined")

add_executable(parserFuzzer parserFuzzer.cpp)
target_link_libraries(parserFuzzer Frontend)
