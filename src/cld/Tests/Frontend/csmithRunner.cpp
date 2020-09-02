#include <cld/Common/Filesystem.hpp>
#include <cld/Frontend/Compiler/LanguageOptions.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/Program.hpp>
#include <cld/Frontend/Preprocessor/Preprocessor.hpp>

#include <cstdlib>
#include <iostream>

std::string findCSmith()
{
    if (std::getenv("CSMITH"))
    {
        return std::getenv("CSMITH");
    }
    std::string_view path = std::getenv("PATH");
    while (!path.empty())
    {
        auto dirEnd = path.find(
#ifndef _WIN32
            ':'
#else
            ';'
#endif
        );
        auto dir = path.substr(0, dirEnd);
        if (cld::fs::exists(cld::fs::path(dir)
                            / "csmith"
#ifdef _WIN32
                              ".exe"
#endif
                            ))
        {
            return (cld::fs::path(dir)
                    / "csmith"
#ifdef _WIN32
                      ".exe"
#endif
                    )
                .string();
        }
        if (dirEnd == path.npos)
        {
            break;
        }
        path = path.substr(dirEnd + 1);
    }
    return "";
}

int main()
{
    auto csmith = findCSmith();
    while (true)
    {
        std::cout.flush();
        auto ret = std::system((csmith + " --no-packed-struct > test.c").c_str());
        if (ret != 0)
        {
            std::cerr << "Executing csmith failed with error code " << ret;
            return -1;
        }
        std::cout.flush();
        cld::fs::ifstream file("test.c", std::ios_base::binary | std::ios_base::ate | std::ios_base::in);
        if (!file.is_open())
        {
            std::cerr << "Failed to open test.c";
            return -1;
        }

        std::size_t size = file.tellg();
        file.seekg(0);
        std::string input(size, '\0');
        file.read(input.data(), size);
        file.close();

        auto options = cld::LanguageOptions::native();
        if (!std::getenv("CSMITH_INC"))
        {
            std::cerr << "No directory for csmith headers set";
            return -1;
        }
        options.includeDirectories.emplace_back(std::getenv("CSMITH_INC"));
        options.additionalMacros.emplace_back("CSMITH_MINIMAL", "");
        options.additionalMacros.emplace_back("STANDALONE", "");
        options.disabledWarnings.insert("macro-redefined");
        bool errors = false;
        auto pptokens = cld::Lexer::tokenize(std::move(input), options, &llvm::errs(), &errors,
                                             (cld::fs::current_path() / "test.c").u8string());
        if (errors)
        {
            std::cerr << "Lexer failed";
            return -1;
        }
        pptokens = cld::PP::preprocess(std::move(pptokens), &llvm::errs(), &errors);
        if (errors)
        {
            std::cerr << "Preprocessor failed";
            return -1;
        }
        auto ctokens = cld::Lexer::toCTokens(pptokens, &llvm::errs(), &errors);
        if (errors)
        {
            std::cerr << "PP Token to C Tokens failed";
            return -1;
        }
        auto tree = cld::Parser::buildTree(ctokens, &llvm::errs(), &errors);
        if (errors)
        {
            std::cerr << "Parser failed";
            return -1;
        }
        cld::Semantics::analyse(tree, std::move(ctokens), &llvm::errs(), &errors);
    }
}
