#include <llvm/ADT/ScopeExit.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Timer.h>

#include <CompilerCore/C/Lexer.hpp>
#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Preprocessor/Parser.hpp>

#include <iostream>

int main(int argc, char** argv)
{
    if (argc < 2)
    {
        llvm::errs() << "Missing mode\n";
        return -1;
    }
    std::string mode = argv[1];
    if (argc < 3)
    {
        llvm::errs() << "Missing file\n";
        return -1;
    }
    std::string filename = argv[2];
    for (int i = 3; i < argc; i++)
    {
        filename += ' ';
        filename += argv[i];
    }
    std::uint64_t size;
    auto error = llvm::sys::fs::file_size(filename, size);
    if (error != std::error_code())
    {
        llvm::errs() << "Could not open or find file\n";
        return -1;
    }
    auto result = llvm::sys::fs::openNativeFileForRead(filename);
    if (!result)
    {
        llvm::errs() << "Could not open or find file\n";
        return -1;
    }
    auto scopeExit = llvm::make_scope_exit([&] { llvm::sys::fs::closeFile(*result); });
    std::string input(size, ' ');
    std::size_t read = 0;
    error = llvm::sys::fs::readNativeFile(*result, llvm::MutableArrayRef(input.data(), input.size()), &read);
    if (error != std::error_code())
    {
        llvm::errs() << "Could not read file\n";
        return -1;
    }
    assert(read == input.size());

    llvm::Timer timer(mode, "Time it took for the " + mode + " to finish");
    llvm::TimeRegion region(timer);
    if (mode == "lexer")
    {
        auto tokens = cld::Lexer::tokenize(input, cld::LanguageOptions::native(), &llvm::nulls());
        cld::Lexer::toCTokens(tokens, &llvm::nulls());
    }
    else if (mode == "parser")
    {
        bool errors = false;
        auto tokens = cld::Lexer::tokenize(input, cld::LanguageOptions::native(), &llvm::nulls(), &errors);
        if (errors || tokens.data().empty())
        {
            return 0;
        }
        auto ctokens = cld::Lexer::toCTokens(tokens, &llvm::nulls(), &errors);
        if (errors || ctokens.data().empty())
        {
            return 0;
        }

        cld::Parser::Context context(ctokens, &llvm::nulls());
        context.setBracketMax(64);
        auto begin = ctokens.data().cbegin();
        parseTranslationUnit(begin, ctokens.data().cend(), context);
    }
    else if (mode == "pplexer")
    {
        cld::Lexer::tokenize(input, cld::LanguageOptions::native(), &llvm::nulls());
    }
    else if (mode == "ppparser")
    {
        bool errors;
        auto tokens = cld::Lexer::tokenize(input, cld::LanguageOptions::native(), &llvm::nulls(), &errors);
        if (errors || tokens.data().empty())
        {
            return 0;
        }
        cld::PP::buildTree(tokens, &llvm::nulls());
    }
    else
    {
        llvm::errs() << "Invalid mode: " << mode << '\n';
        return -1;
    }
}
