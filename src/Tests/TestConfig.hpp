#pragma once

#include "catch.hpp"

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APSInt.h>

#include <Frontend/Common/Text.hpp>
#include <Frontend/Compiler/Diagnostic.hpp>
#include <Frontend/Compiler/LanguageOptions.hpp>
#include <Frontend/Compiler/Semantics.hpp>
#include <Frontend/Preprocessor/Preprocessor.hpp>

#include <numeric>

namespace cld::Tests
{
const inline auto x64windowsGnu = LanguageOptions{cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 80, 8};
const inline auto x86windowsGnu = LanguageOptions{cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 80, 4};
const inline auto x64windowsMsvc = LanguageOptions{cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 64, 8};
const inline auto x86windowsMsvc = LanguageOptions{cld::LanguageOptions::C99, 1, true, 2, false, 2, 4, 4, 64, 4};
const inline auto x64linux = LanguageOptions{cld::LanguageOptions::C99, 1, true, 4, true, 2, 4, 8, 80, 8};
const inline auto x86linux = LanguageOptions{cld::LanguageOptions::C99, 1, true, 4, true, 2, 4, 4, 80, 4};

namespace detail
{
template <class Diagnostic, class... Args>
std::string args(const Diagnostic&, Args&&... args)
{
    std::array<std::string, sizeof...(Args)> strArgs = {{cld::to_string(args)...}};
    std::string result;
    std::u32string_view stringView = Diagnostic::getFormat();
    for (auto& iter : ctre::range<cld::detail::DIAG_ARG_PATTERN>(stringView))
    {
        auto view = iter.view();
        const char32_t* start = stringView.data();
        result.resize(result.size() + 4 * (view.data() - stringView.data()));
        char* resStart = result.data() + result.size() - 4 * (view.data() - stringView.data());
        auto ret = llvm::ConvertUTF32toUTF8(
            reinterpret_cast<const llvm::UTF32**>(&start), reinterpret_cast<const llvm::UTF32*>(view.data()),
            reinterpret_cast<llvm::UTF8**>(&resStart), reinterpret_cast<llvm::UTF8*>(result.data() + result.size()),
            llvm::strictConversion);
        (void)ret;
        CLD_ASSERT(ret == llvm::conversionOK);
        result.resize(resStart - result.data());
        stringView.remove_prefix(view.data() + view.size() - stringView.data());
        auto mod = iter.get<1>().view();
        auto index = iter.get<2>().view().back() - '0';
        if (mod != U"s")
        {
            result += strArgs[index];
        }
        else
        {
            if (strArgs[index] != "1")
            {
                result += "s";
            }
        }
    }
    const char32_t* start = stringView.data();
    result.resize(result.size() + 4 * stringView.size());
    char* resStart = result.data() + result.size() - 4 * stringView.size();
    auto ret =
        llvm::ConvertUTF32toUTF8(reinterpret_cast<const llvm::UTF32**>(&start),
                                 reinterpret_cast<const llvm::UTF32*>(stringView.data() + stringView.size()),
                                 reinterpret_cast<llvm::UTF8**>(&resStart),
                                 reinterpret_cast<llvm::UTF8*>(result.data() + result.size()), llvm::strictConversion);
    (void)ret;
    CLD_ASSERT(ret == llvm::conversionOK);
    result.resize(resStart - result.data());
    return result;
}
} // namespace detail

struct ProducesError : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesError(const std::string& comparator)
        : ContainsMatcher(Catch::Matchers::StdString::CasedString("error: " + comparator, Catch::CaseSensitive::Yes))
    {
    }

    template <class Diagnostic, class... Args>
    ProducesError(const Diagnostic& diagnostic, Args&&... args)
        : ProducesError(detail::args(diagnostic, std::forward<Args>(args)...))
    {
    }
};

struct ProducesNoErrors : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesNoErrors() : ContainsMatcher(Catch::Matchers::StdString::CasedString("error: ", Catch::CaseSensitive::Yes))
    {
    }

    bool match(const std::string& source) const override
    {
        return !ContainsMatcher::match(source);
    }

    std::string describe() const override
    {
        return "Produces no errors";
    }
};

struct ProducesNote : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesNote(const std::string& comparator)
        : ContainsMatcher(Catch::Matchers::StdString::CasedString("note: " + comparator, Catch::CaseSensitive::Yes))
    {
    }

    template <class Diagnostic, class... Args>
    ProducesNote(const Diagnostic& diagnostic, Args&&... args)
        : ProducesNote(detail::args(diagnostic, std::forward<Args>(args)...))
    {
    }
};

struct ProducesNoNotes : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesNoNotes() : ContainsMatcher(Catch::Matchers::StdString::CasedString("note: ", Catch::CaseSensitive::Yes)) {}

    bool match(const std::string& source) const override
    {
        return !ContainsMatcher::match(source);
    }

    std::string describe() const override
    {
        return "Produces no notes";
    }
};

struct ProducesWarning : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesWarning(const std::string& comparator)
        : ContainsMatcher(Catch::Matchers::StdString::CasedString("warning: " + comparator, Catch::CaseSensitive::Yes))
    {
    }

    template <class Diagnostic, class... Args>
    ProducesWarning(const Diagnostic& diagnostic, Args&&... args)
        : ProducesWarning(detail::args(diagnostic, std::forward<Args>(args)...))
    {
    }
};

struct ProducesNoWarnings : Catch::Matchers::StdString::ContainsMatcher
{
    ProducesNoWarnings()
        : ContainsMatcher(Catch::Matchers::StdString::CasedString("warning: ", Catch::CaseSensitive::Yes))
    {
    }

    bool match(const std::string& source) const override
    {
        return !ContainsMatcher::match(source);
    }

    std::string describe() const override
    {
        return "Produces no warnings";
    }
};

#define ProducesNothing() ProducesNoErrors() && ProducesNoNotes() && ProducesNoWarnings()

// Matcher that works almost like == of two strings except it doesn't care of any leading whitespaces per line
struct ProducesLines : Catch::MatcherBase<std::string_view>
{
    ProducesLines(std::string_view text)
    {
        std::size_t result = 0;
        while (true)
        {
            auto newline = text.find('\n', result);
            if (newline == std::string_view::npos)
            {
                m_linesTrimmed.emplace_back(trimEnd(text.substr(result)));
                break;
            }
            m_linesTrimmed.emplace_back(trimEnd(text.substr(result, newline - result)));
            result = newline + 1;
        }
    }

    std::string describe() const override
    {
        return "lines match \""
               + std::accumulate(m_linesTrimmed.begin(), m_linesTrimmed.end(), std::string(),
                                 [first = true](const std::string& lhs, const std::string& rhs) mutable {
                                     if (!first)
                                     {
                                         return lhs + '\n' + rhs;
                                     }
                                     else
                                     {
                                         first = false;
                                         return rhs;
                                     }
                                 })
               + '"';
    }

    bool match(const std::string_view& source) const override
    {
        std::size_t i = 0;
        std::size_t result = 0;
        while (i < m_linesTrimmed.size())
        {
            auto newline = source.find('\n', result);
            if (newline == std::string_view::npos)
            {
                return trimEnd(source.substr(result)) == m_linesTrimmed[i] && i + 1 == m_linesTrimmed.size();
            }
            if (trimEnd(source.substr(result, newline - result)) != m_linesTrimmed[i])
            {
                return false;
            }
            result = newline + 1;
            i++;
        }
        auto leftOver = source.substr(result);
        return std::all_of(leftOver.begin(), leftOver.end(), [](unsigned char c) { return std::isspace(c & 0x7F); });
    }

private:
    static std::string_view trimEnd(std::string_view text)
    {
        // ASCII should hopefully suffice for our tests... Otherwise I will have to properly parse it to UTF32
        // and use one of the whitespace sets of the Lexer to match whitespace characters
        auto end =
            std::find_if(std::make_reverse_iterator(text.end()), std::make_reverse_iterator(text.begin()), [](char c) {
                return c != ' ' && c != '\t';
            }).base();
        return text.substr(0, end - text.begin());
    }

    std::vector<std::string> m_linesTrimmed;
};

struct FileScope
{
    std::string m_path;

public:
    FileScope(std::string_view path) : m_path(cld::to_string(path)) {}

    ~FileScope();

    FileScope(const FileScope&) = delete;
    FileScope& operator=(const FileScope&) = delete;
    FileScope(FileScope&&) = delete;
    FileScope& operator=(FileScope&&) = delete;
};

[[nodiscard]] FileScope createInclude(std::string_view path, std::string_view content);

} // namespace cld::Tests

// IT'S ONLY FOR TESTS I SWEAR. It's in a namepspace for mangling
using namespace cld::Tests;

namespace Catch
{
template <>
struct StringMaker<llvm::APSInt>
{
    static std::string convert(const llvm::APSInt& value)
    {
        return "APSInt{" + std::to_string(value.getBitWidth()) + " bits, " + value.toString(10) + "}";
    }
};

template <>
struct StringMaker<llvm::APInt>
{
    static std::string convert(const llvm::APSInt& value)
    {
        return "APInt{" + std::to_string(value.getBitWidth()) + " bits, " + value.toString(10) + "}";
    }
};

template <>
struct StringMaker<cld::Semantics::Type>
{
    static std::string convert(const cld::Semantics::Type& type)
    {
        return '\'' + cld::to_string(type.getName()) + '\''
               + (type.isTypedef() ? "(aka '" + cld::to_string(type.getTypeName()) + "')" : "");
    }
};

template <>
struct StringMaker<cld::PPSourceObject>
{
    static std::string convert(const cld::PPSourceObject& sourceObject)
    {
        return "'"
               + cld::PP::reconstruct(sourceObject.data().data(),
                                      sourceObject.data().data() + sourceObject.data().size(), sourceObject)
               + "'";
    }
};
} // namespace Catch
