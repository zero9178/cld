
add_subdirectory(Common)

add_library(Frontend Preprocessor/Preprocessor.cpp Compiler/Lexer.cpp Compiler/Parser.cpp Compiler/Syntax.cpp Compiler/ConstValue.cpp Compiler/Semantics.cpp Compiler/SemanticAnalysis.cpp Compiler/Message.cpp Compiler/ParserDeclarations.cpp Compiler/ParserExpressions.cpp Compiler/ParserUtil.cpp Compiler/SourceObject.cpp Compiler/SourceObject.hpp Compiler/LanguageOptions.cpp Compiler/LanguageOptions.hpp Preprocessor/Syntax.cpp Preprocessor/Syntax.hpp Preprocessor/Parser.cpp Compiler/Diagnostic.cpp Compiler/SemanticAnalysisExpressions.cpp Compiler/SemanticAnalysisTypes.cpp)
target_link_libraries(Frontend PUBLIC Support)
if (CLD_USE_PCH)
    target_precompile_headers(Frontend PUBLIC <vector> <variant> <string> <optional> <ctre.hpp> <unordered_map> <map> <memory> <algorithm> <llvm/ADT/APSInt.h> <llvm/ADT/APFloat.h> <string_view> <tuple> <Frontend/Compiler/ErrorMessages.hpp> <Frontend/Common/Filesystem.hpp>)
endif ()
