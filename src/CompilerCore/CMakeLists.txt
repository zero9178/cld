
add_library(Frontend Preprocessor/Preprocessor.cpp C/Lexer.cpp C/Parser.cpp C/Syntax.cpp C/ConstantEvaluator.cpp C/Semantics.cpp C/SemanticAnalysis.cpp C/Message.cpp C/ParserDeclarations.cpp C/ParserExpressions.cpp C/ParserUtil.cpp)

add_library(CompilerCore C/Codegen.cpp)
target_link_libraries(CompilerCore PUBLIC ${llvm_libs} Frontend)
