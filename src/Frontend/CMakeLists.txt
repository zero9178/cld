
add_subdirectory(Common)

add_library(Frontend Preprocessor/Preprocessor.cpp Compiler/Lexer.cpp Compiler/Parser.cpp Compiler/Syntax.cpp Compiler/ConstantEvaluator.cpp Compiler/Semantics.cpp Compiler/SemanticAnalysis.cpp Compiler/Message.cpp Compiler/ParserDeclarations.cpp Compiler/ParserExpressions.cpp Compiler/ParserUtil.cpp Compiler/SourceObject.cpp Compiler/SourceObject.hpp Compiler/LanguageOptions.cpp Compiler/LanguageOptions.hpp Preprocessor/Syntax.cpp Preprocessor/Syntax.hpp Preprocessor/Parser.cpp Compiler/Diagnostic.cpp)
target_link_libraries(Frontend PUBLIC Support)
