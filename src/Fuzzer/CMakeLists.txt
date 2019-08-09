
if (WIN32)
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=undefined")
    link_libraries(clang_rt.fuzzer-x86_64)
    link_libraries(clang_rt.stats-x86_64)
else ()
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=undefined,fuzzer,address")
endif ()

add_executable(parserFuzzer parserFuzzer.cpp)
target_link_libraries(parserFuzzer Frontend)
