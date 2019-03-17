find_package(GTest REQUIRED)

add_executable(CompilerTests lexer.cpp)
target_link_libraries(CompilerTests CompilerCore GTest::Main)
target_include_directories(CompilerTests PRIVATE ..)
