
llvm_map_components_to_libnames(llvm_support Support)
add_library(Support Common/Text.cpp Common/Util.cpp)
target_link_libraries(Support PRIVATE ${llvm_support})

add_library(Frontend Preprocessor/Preprocessor.cpp C/Lexer.cpp C/Parser.cpp C/Syntax.cpp C/ConstantEvaluator.cpp C/Semantics.cpp C/SemanticAnalysis.cpp C/Message.cpp C/ParserDeclarations.cpp C/ParserExpressions.cpp C/ParserUtil.cpp C/SourceObject.cpp C/SourceObject.hpp C/LanguageOptions.cpp C/LanguageOptions.hpp Preprocessor/Syntax.cpp Preprocessor/Syntax.hpp Preprocessor/Parser.cpp)
target_link_libraries(Frontend PUBLIC Support)

add_library(CompilerCore C/Codegen.cpp)
target_link_libraries(CompilerCore PUBLIC Frontend ${llvm_libs})
