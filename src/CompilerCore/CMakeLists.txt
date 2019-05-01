
add_library(Preprocessor Preprocessor/Preprocessor.cpp)
target_link_libraries(Preprocessor Parser)
add_library(Lexer C/Lexer.cpp)
add_library(Parser C/Parser.cpp C/Syntax.cpp C/ConstantEvaluator.cpp C/Expected.hpp C/FailureReason.cpp C/FailureReason.hpp C/Representations.cpp C/Representations.hpp)
target_link_libraries(Parser Lexer)

add_library(CompilerCore C/Codegen.cpp)
target_link_libraries(CompilerCore PUBLIC ${llvm_libs} Threads::Threads Parser)
