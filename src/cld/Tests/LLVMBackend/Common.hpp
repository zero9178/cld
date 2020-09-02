#pragma once

#include <catch.hpp>

#include <llvm/IR/Module.h>

#include <cld/Common/function_ref.hpp>
#include <cld/Frontend/Compiler/Lexer.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/Program.hpp>
#include <cld/Frontend/Compiler/Semantics.hpp>
#include <cld/Frontend/Preprocessor/Preprocessor.hpp>

#define generateProgram(str)                                                                                 \
    [](std::string source) {                                                                                 \
        bool errors = false;                                                                                 \
        auto pptokens =                                                                                      \
            cld::Lexer::tokenize(std::move(source), cld::LanguageOptions::native(), &llvm::errs(), &errors); \
        REQUIRE_FALSE(errors);                                                                               \
        pptokens = cld::PP::preprocess(std::move(pptokens), &llvm::errs(), &errors);                         \
        REQUIRE_FALSE(errors);                                                                               \
        auto ctokens = cld::Lexer::toCTokens(pptokens, &llvm::errs(), &errors);                              \
        REQUIRE_FALSE(errors);                                                                               \
        auto tree = cld::Parser::buildTree(ctokens, &llvm::errs(), &errors);                                 \
        REQUIRE_FALSE(errors);                                                                               \
        auto program = cld::Semantics::analyse(tree, std::move(ctokens), &llvm::errs(), &errors);            \
        REQUIRE_FALSE(errors);                                                                               \
        return program;                                                                                      \
    }(str)

#define generateProgramWithOptions(str, opt)                                                      \
    [](std::string source, const cld::LanguageOptions& options) {                                 \
        bool errors = false;                                                                      \
        auto pptokens = cld::Lexer::tokenize(std::move(source), options, &llvm::errs(), &errors); \
        REQUIRE_FALSE(errors);                                                                    \
        pptokens = cld::PP::preprocess(std::move(pptokens), &llvm::errs(), &errors);              \
        REQUIRE_FALSE(errors);                                                                    \
        auto ctokens = cld::Lexer::toCTokens(pptokens, &llvm::errs(), &errors);                   \
        REQUIRE_FALSE(errors);                                                                    \
        auto tree = cld::Parser::buildTree(ctokens, &llvm::errs(), &errors);                      \
        REQUIRE_FALSE(errors);                                                                    \
        auto program = cld::Semantics::analyse(tree, std::move(ctokens), &llvm::errs(), &errors); \
        REQUIRE_FALSE(errors);                                                                    \
        return program;                                                                           \
    }(str, cld::LanguageOptions::fromTriple(opt))

namespace cld::Tests
{
namespace details
{
void computeAndGet(std::unique_ptr<llvm::Module>&& module, std::string_view functionName,
                   cld::function_ref<void(std::uintptr_t)> symbolCallback);

template <class F>
struct return_type;

template <class R, class... A>
struct return_type<R(A...)>
{
    typedef R type;
};

} // namespace details

template <class Fn, class... Args>
auto computeInJIT(std::unique_ptr<llvm::Module>&& module, std::string_view functionName, Args&&... args)
{
    if constexpr (std::is_void_v<typename details::return_type<Fn>::type>)
    {
        details::computeAndGet(std::move(module), functionName, [&](std::uintptr_t symbol) {
            reinterpret_cast<Fn*>(symbol)(std::forward<Args&&>(args)...);
        });
    }
    else
    {
        typename details::return_type<Fn>::type result;
        details::computeAndGet(std::move(module), functionName, [&](std::uintptr_t symbol) {
            result = reinterpret_cast<Fn*>(symbol)(std::forward<Args&&>(args)...);
        });
        return result;
    }
}

class ContainsIR : public Catch::MatcherBase<llvm::Module>
{
    std::string m_pattern;

public:
    explicit ContainsIR(std::string pattern) : m_pattern(std::move(pattern)) {}

    bool match(const llvm::Module&) const override
    {
        return false;
    }

protected:
    std::string describe() const override
    {
        return std::string();
    }
};

inline std::string printModule(const llvm::Module& arg)
{
    std::string s;
    llvm::raw_string_ostream ss(s);
    arg.print(ss, nullptr);
    ss.flush();
    return s;
}

} // namespace cld::Tests

using namespace cld::Tests;

namespace Catch
{
template <>
struct StringMaker<llvm::Module>
{
    static std::string convert(const llvm::Module& module)
    {
        std::string s;
        {
            llvm::raw_string_ostream ss(s);
            module.print(ss, nullptr);
        }
        return "'" + s + "'";
    }
};

} // namespace Catch
