
#include <llvm/Support/Timer.h>

#include <cld/Frontend/Compiler/Lexer.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/SemanticAnalysis.hpp>
#include <cld/Frontend/Compiler/SourceObject.hpp>
#include <cld/Frontend/Preprocessor/Parser.hpp>
#include <cld/Frontend/Preprocessor/Preprocessor.hpp>
#include <cld/Support/Filesystem.hpp>
#include <cld/Support/ScopeExit.hpp>

int main(int argc, char** argv)
{
#ifdef _MSC_VER
    _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);
    _CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_ERROR, _CRTDBG_FILE_STDERR);
#endif
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
    cld::fs::ifstream file(filename, std::ios_base::in | std::ios_base::ate | std::ios_base::binary);
    if (!file.is_open())
    {
        llvm::errs() << "Couldn't open file\n";
        return -1;
    }
    std::uint64_t size = file.tellg();
    file.seekg(0);
    std::string input(size, '\0');
    file.read(input.data(), size);

    filename = (cld::fs::current_path() / filename).u8string();

    llvm::Timer timer(mode, "Time it took for the " + mode + " to finish");
    llvm::TimeRegion region(timer);
    cld::LanguageOptions options = cld::LanguageOptions::native();
    if (mode == "lexer")
    {
        bool errors = false;
        auto tokens = cld::Lexer::tokenize(std::move(input), &options, &llvm::nulls(), &errors, filename);
        if (errors)
        {
            return 0;
        }
        cld::Lexer::toCTokens(tokens, &llvm::nulls());
    }
    else if (mode == "parser")
    {
        bool errors = false;
        auto tokens = cld::Lexer::tokenize(std::move(input), &options, &llvm::nulls(), &errors, filename);
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
        const auto* begin = std::as_const(ctokens).data().data();
        parseTranslationUnit(begin, ctokens.data().data() + ctokens.data().size(), context);
    }
    else if (mode == "pplexer")
    {
        cld::Lexer::tokenize(input, &options, &llvm::nulls(), nullptr, filename);
    }
    else if (mode == "ppparser")
    {
        bool errors;
        auto tokens = cld::Lexer::tokenize(std::move(input), &options, &llvm::nulls(), &errors, filename);
        if (errors || tokens.data().empty())
        {
            return 0;
        }
        cld::PP::buildTree(tokens, &llvm::nulls());
    }
    else if (mode == "sema")
    {
        bool errors = false;
        auto pptokens = cld::Lexer::tokenize(std::move(input), &options, &llvm::errs(), &errors, filename);
        if (errors)
        {
            return 0;
        }
        cld::PP::Options ppOptions;
        for (int i = 3; i < argc; i++)
        {
            ppOptions.includeDirectories.push_back(argv[i]);
        }
        pptokens = cld::PP::preprocess(std::move(pptokens), ppOptions, &llvm::errs(), &errors);
        if (errors)
        {
            return 0;
        }
        auto ctokens = cld::Lexer::toCTokens(pptokens, &llvm::errs(), &errors);
        if (errors)
        {
            return 0;
        }
        auto tree = cld::Parser::buildTree(ctokens, &llvm::errs(), &errors);
        if (errors)
        {
            return 0;
        }
        cld::Semantics::SemanticAnalysis analysis(ctokens);
        analysis.visit(tree);
        return 0;
    }
    else if (mode == "csmith")
    {
        options.enabledWarnings.erase("macro-redefined");
        bool errors = false;
        auto pptokens = cld::Lexer::tokenize(std::move(input), &options, &llvm::errs(), &errors, filename);
        if (errors)
        {
            return -1;
        }
        cld::PP::Options ppOptions;
        ppOptions.additionalMacros.emplace_back("CSMITH_MINIMAL", "");
        ppOptions.additionalMacros.emplace_back("STANDALONE", "");
        for (int i = 3; i < argc; i++)
        {
            ppOptions.includeDirectories.push_back(argv[i]);
        }
        pptokens = cld::PP::preprocess(std::move(pptokens), ppOptions, &llvm::errs(), &errors);
        if (errors)
        {
            return -1;
        }
        auto ctokens = cld::Lexer::toCTokens(pptokens, &llvm::errs(), &errors);
        if (errors)
        {
            return -1;
        }
        auto tree = cld::Parser::buildTree(ctokens, &llvm::errs(), &errors);
        if (errors)
        {
            return -1;
        }

        cld::Semantics::SemanticAnalysis analysis(ctokens, &llvm::errs(), &errors);
        analysis.visit(tree);
        if (errors)
        {
            return -1;
        }
        return 0;
    }
    else
    {
        llvm::errs() << "Invalid mode: " << mode << '\n';
        return -1;
    }
}
