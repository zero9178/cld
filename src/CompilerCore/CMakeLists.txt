
add_library(Frontend Preprocessor/Preprocessor.cpp C/Lexer.cpp C/Parser.cpp C/Syntax.cpp C/ConstantEvaluator.cpp C/Semantics.cpp C/SemanticAnalysis.cpp C/Message.cpp C/ParserDeclarations.cpp C/ParserExpressions.cpp C/ParserUtil.cpp C/SourceObject.cpp C/SourceObject.hpp ../../3rdParty/termcolor.cpp C/LanguageOptions.cpp C/LanguageOptions.hpp)
target_link_libraries(Frontend PRIVATE ${llvm_libs})

add_library(CompilerCore C/Codegen.cpp)
target_link_libraries(CompilerCore PUBLIC Frontend ${llvm_libs})
