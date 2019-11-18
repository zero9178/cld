#ifndef OPENCLPARSER_PREPROCESSOR_HPP
#define OPENCLPARSER_PREPROCESSOR_HPP

#include <llvm/Support/raw_ostream.h>

#include <vector>

namespace OpenCL
{
    class SourceObject;

    namespace Lexer
    {
        class Token;
    }

    namespace PP
    {
        std::vector<Lexer::Token> preprocess(const std::vector<Lexer::Token>& tokens,
                                             llvm::raw_ostream* reporter = &llvm::errs());
    }
} // namespace OpenCL

#endif // OPENCLPARSER_PREPROCESSOR_HPP
