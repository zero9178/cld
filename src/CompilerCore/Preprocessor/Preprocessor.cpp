#include "Preprocessor.hpp"

#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/ScopeExit.h>

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Text.hpp>

#include <ctime>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Parser.hpp"

namespace
{
constexpr std::array PREDEFINED_MACRO_NAMES = {"__DATE__",          "__FILE__",
                                               "__LINE__",          "__STDC__",
                                               "__STDC_HOSTED__",   "__STDC_MB_MIGHT_NEQ_WC__",
                                               "__STDC_VERSION__",  "__TIME__",
                                               "__STC_IEC_559__",   "__STDC_IEC_559_COMPLEX__",
                                               "__STDC_ISO_10646__"};

template <class T, class Iterator, class F>
void insertWithTransform(std::vector<T>& vector, Iterator begin, Iterator end, F function)
{
    if constexpr (std::is_default_constructible_v<T>)
    {
        auto prevSize = vector.size();
        vector.resize(prevSize + std::distance(begin, end));
        std::transform(begin, end, vector.begin() + prevSize, function);
    }
    else
    {
        vector.reserve(vector.size() + std::distance(begin, end));
        std::transform(begin, end, std::back_inserter(vector), function);
    }
}

class Preprocessor final : private cld::SourceInterface
{
    llvm::raw_ostream* m_report;
    const cld::LanguageOptions& m_options;
    std::uint64_t m_currentOffset = 0;
    std::uint64_t m_macroID = 0;
    std::uint64_t m_currentFile = 0;
    std::vector<cld::Lexer::PPToken> m_result;
    std::vector<cld::Source::Substitution> m_substitutions;
    std::vector<std::uint64_t> m_ppStarts{0};
    struct Macro
    {
        cld::Lexer::PPTokenIterator identifierPos;
        std::optional<std::vector<std::string_view>> identifierList;
        bool hasEllipse;
        std::vector<cld::Lexer::PPToken> replacement;
    };
    std::unordered_map<std::string_view, Macro> m_defines;
    std::vector<std::unordered_set<std::string_view>> m_disabledMacros;
    std::vector<cld::Source::File> m_files;
    bool m_errorsOccured = false;
    bool m_visitingScratchPad = true;

    void pushNewline()
    {
        if (m_visitingScratchPad)
        {
            return;
        }
        m_ppStarts.push_back(++m_currentOffset);
    }

    void pushLine(llvm::ArrayRef<cld::Lexer::PPToken> tokens)
    {
        if (tokens.empty() || m_visitingScratchPad)
        {
            pushNewline();
            return;
        }
        auto base = tokens.front().getPPOffset();
        insertWithTransform(m_result, tokens.begin(), tokens.end(), [this, base](const cld::Lexer::PPToken& token) {
            return token.copy({}, {}, {}, {}, m_currentOffset + token.getPPOffset() - base);
        });
        m_currentOffset = m_result.back().getPPOffset() + m_result.back().getCharSpaceLength();
        pushNewline();
    }

    void log(std::vector<cld::Message> messages)
    {
        for (auto& iter : messages)
        {
            if (iter.getSeverity() == cld::Message::Error)
            {
                m_errorsOccured = true;
            }
            if (m_report)
            {
                iter.print(*m_report, *this);
            }
        }
    }

    static bool equal(const cld::PP::DefineDirective& lhs, const Macro& rhs)
    {
        if (lhs.identifierList != rhs.identifierList)
        {
            return false;
        }
        if (lhs.hasEllipse != rhs.hasEllipse)
        {
            return false;
        }
        return std::equal(lhs.replacementBegin, lhs.replacementEnd, rhs.replacement.begin(), rhs.replacement.end(),
                          [](const cld::Lexer::PPToken& lhs, const cld::Lexer::PPToken& rhs) {
                              if (lhs.getTokenType() != rhs.getTokenType())
                              {
                                  return false;
                              }
                              return lhs.getValue() == rhs.getValue();
                          });
    }

    struct OffsetSortedHash
    {
        std::size_t operator()(const cld::Lexer::PPToken* ptr) const
        {
            return std::hash<std::uint64_t>()(ptr->getOffset());
        }
    };

    struct OffsetSortedEqual
    {
        bool operator()(const cld::Lexer::PPToken* lhs, const cld::Lexer::PPToken* rhs) const
        {
            return lhs->getOffset() == rhs->getOffset();
        }
    };

    using OffsetSortedUnorderedSet =
        std::unordered_set<const cld::Lexer::PPToken*, OffsetSortedHash, OffsetSortedEqual>;

    std::vector<cld::Lexer::PPToken>
        argumentSubstitution(std::vector<cld::Lexer::PPToken>&& replacementList,
                             std::vector<llvm::ArrayRef<cld::Lexer::PPToken>>&& arguments,
                             const OffsetSortedUnorderedSet& argumentsInReplacement,
                             std::unordered_map<std::string_view, std::uint64_t> nameToIndex)
    {
        std::vector<std::vector<cld::Lexer::PPToken>> argumentsAfterSubstitution(arguments.size());
        for (auto& iter : argumentsInReplacement)
        {
            auto index = nameToIndex.find(iter->getValue());
            CLD_ASSERT(index != nameToIndex.end());
            if (arguments[index->second].empty())
            {
                continue;
            }
            auto end = std::as_const(arguments[index->second]).data() + arguments[index->second].size();
            auto ends = llvm::ArrayRef(end);
            std::vector<cld::Lexer::PPToken> result = macroSubstitute(arguments[index->second].data(), ends);
            arguments[index->second] = {};
            argumentsAfterSubstitution[index->second] = std::move(result);
        }
        std::int64_t delta = 0;
        std::vector<cld::Lexer::PPToken> result;
        auto start = replacementList.begin();
        for (auto iter = replacementList.begin(); iter != replacementList.end();)
        {
            if (iter->getTokenType() != cld::Lexer::TokenType::Identifier)
            {
                iter++;
                continue;
            }
            if (argumentsInReplacement.count(&*iter) == 0)
            {
                iter++;
                continue;
            }
            insertWithTransform(result, std::move_iterator(start), std::move_iterator(iter),
                                [delta](cld::Lexer::PPToken&& token) {
                                    return std::move(token).move({}, {}, {}, {}, token.getPPOffset() + delta);
                                });
            auto index = nameToIndex.find(iter->getValue());
            CLD_ASSERT(index != nameToIndex.end());
            std::uint64_t base = iter->getPPOffset() + delta;
            auto& temp = argumentsAfterSubstitution[index->second];
            std::uint64_t length = 0;
            if (!temp.empty())
            {
                auto firstOffset = temp.front().getPPOffset();
                length = temp.back().getPPOffset() + temp.back().getCharSpaceLength() - firstOffset;
                insertWithTransform(result, temp.begin(), temp.end(),
                                    [base, firstOffset](const cld::Lexer::PPToken& token) {
                                        return token.copy({}, {}, {}, {}, token.getPPOffset() - firstOffset + base);
                                    });
            }
            delta -= iter->getCharSpaceLength() - length;
            start = ++iter;
        }
        insertWithTransform(result, std::move_iterator(start), std::move_iterator(replacementList.end()),
                            [delta](cld::Lexer::PPToken&& token) {
                                return std::move(token).move({}, {}, {}, {}, token.getPPOffset() + delta);
                            });
        return result;
    }

    std::vector<cld::Lexer::PPToken> macroSubstitute(cld::Lexer::PPTokenIterator begin,
                                                     llvm::ArrayRef<const cld::Lexer::PPToken*>& ends,
                                                     cld::Lexer::PPTokenIterator posReference = nullptr,
                                                     const std::unordered_set<const cld::Lexer::PPToken*>& ignored = {})
    {
        std::int64_t delta = 0;
        std::vector<cld::Lexer::PPToken> output;
        auto start = begin;
        for (auto iter = begin; iter != ends.front();)
        {
            if (iter->getTokenType() != cld::Lexer::TokenType::Identifier || ignored.count(iter) != 0)
            {
                iter++;
                continue;
            }
            auto name = iter->getValue();
            if (name == "__FILE__" || name == "__LINE__")
            {
                auto pos = posReference != nullptr ? posReference : iter;
                m_defines.erase(name);
                m_visitingScratchPad = true;
                auto scope = llvm::make_scope_exit([this] { m_visitingScratchPad = false; });
                std::string source;
                if (name == "__FILE__")
                {
                    source =
                        "#define __FILE__ \"" + escapeString(m_files[(std::uint64_t)pos->getFileId()].path) + "\"\n";
                }
                else
                {
                    source = "#define __LINE__ " + std::to_string(pos->getLine(*this)) + "\n";
                }
                auto scratchPadPP =
                    cld::Lexer::tokenize(source, m_options, m_report, &m_errorsOccured, "<Scratch Pad>");
                CLD_ASSERT(!m_errorsOccured);
                include(std::move(scratchPadPP));
            }
            auto result = m_defines.find(name);
            if (result == m_defines.end()
                || ((std::uint64_t)iter->getMacroId() < m_disabledMacros.size()
                    && m_disabledMacros[(std::uint64_t)iter->getMacroId()].count(name) > 0))
            {
                iter++;
                continue;
            }
            if (result->second.identifierList
                && (iter + 1 == ends.front() || (iter + 1)->getTokenType() != cld::Lexer::TokenType::OpenParentheses))
            {
                iter++;
                continue;
            }
            insertWithTransform(output, start, iter, [delta](const cld::Lexer::PPToken& token) {
                return token.copy({}, {}, {}, {}, token.getPPOffset() + delta, {});
            });

            std::uint64_t base = iter->getPPOffset() + delta;
            if (!result->second.identifierList)
            {
                // Object like macro
                auto i = ++m_macroID;
                m_disabledMacros.resize(i + 1);
                m_disabledMacros[i].insert(name);
                if ((std::uint64_t)iter->getMacroId() != 0)
                {
                    m_disabledMacros[i].insert(m_disabledMacros[(std::uint64_t)iter->getMacroId()].begin(),
                                               m_disabledMacros[(std::uint64_t)iter->getMacroId()].end());
                }
                m_substitutions.push_back({*result->second.identifierPos});
                auto& temp = result->second.replacement; // macroSubstitute(.data(), repEnds, iter, i);
                std::uint64_t length = 0;
                if (!temp.empty())
                {
                    auto firstOffset = temp.front().getPPOffset();
                    length = temp.back().getPPOffset() + temp.back().getCharSpaceLength() - firstOffset;
                    insertWithTransform(output, temp.begin(), temp.end(),
                                        [base, firstOffset, i](const cld::Lexer::PPToken& token) {
                                            return token.copy({}, {}, {}, {}, token.getPPOffset() - firstOffset + base,
                                                              {}, cld::Lexer::MacroID(i));
                                        });
                }
                delta -= iter->getCharSpaceLength() - length;
                start = ++iter;
                continue;
            }

            // Function like macro
            auto namePos = iter;
            auto i = ++m_macroID;
            m_disabledMacros.resize(i + 1);
            m_disabledMacros[i].insert(name);
            if ((std::uint64_t)iter->getMacroId() != 0)
            {
                m_disabledMacros[i].insert(m_disabledMacros[(std::uint64_t)iter->getMacroId()].begin(),
                                           m_disabledMacros[(std::uint64_t)iter->getMacroId()].end());
            }
            iter += 2;
            std::vector<llvm::ArrayRef<cld::Lexer::PPToken>> arguments;
            auto argCount = result->second.identifierList->size();
            auto first = iter;
            const cld::Lexer::PPToken* varargStart = iter;
            while (true)
            {
                auto count = 0;
                const cld::Lexer::PPToken* closeParentheseOrComma;
                while (true)
                {
                    closeParentheseOrComma =
                        std::find_if(first, ends.front(), [&count](const cld::Lexer::PPToken& token) mutable -> bool {
                            if (token.getTokenType() == cld::Lexer::TokenType::CloseParentheses)
                            {
                                if (count == 0)
                                {
                                    return true;
                                }
                                count--;
                            }
                            else if (token.getTokenType() == cld::Lexer::TokenType::OpenParentheses)
                            {
                                count++;
                            }
                            else if (token.getTokenType() == cld::Lexer::TokenType::Comma && count == 0)
                            {
                                return true;
                            }
                            return false;
                        });
                    if (ends.size() > 1 && closeParentheseOrComma == ends.front())
                    {
                        // if ends has a size greater than one there are more tokens available in the next line
                        // that should be consumed in order to be able to find the closing parentheses for the macro
                        // call
                        ends = ends.drop_front();
                        continue;
                    }
                    break;
                }
                if (closeParentheseOrComma == ends.front())
                {
                    log({cld::Message::error(cld::Errors::Parser::EXPECTED_N.args(cld::to_string(count + 1) + " ')'"),
                                             cld::Message::after, closeParentheseOrComma - 1,
                                             {cld::InsertAfter(closeParentheseOrComma - 1, ")")}),
                         cld::Message::note(cld::Notes::TO_MATCH_N_HERE.args("'('"), namePos + 1,
                                            {cld::PointAt(namePos + 1)})});
                    return output;
                }
                if (!result->second.hasEllipse || arguments.size() != argCount)
                {
                    if (argCount != 0
                        || closeParentheseOrComma->getTokenType() != cld::Lexer::TokenType::CloseParentheses)
                    {
                        arguments.emplace_back(first, closeParentheseOrComma);
                    }
                    if (result->second.hasEllipse && arguments.size() == argCount)
                    {
                        if (closeParentheseOrComma->getTokenType() == cld::Lexer::TokenType::CloseParentheses)
                        {
                            varargStart = closeParentheseOrComma;
                        }
                        else
                        {
                            varargStart = closeParentheseOrComma + 1;
                        }
                    }
                }
                if (closeParentheseOrComma->getTokenType() == cld::Lexer::TokenType::CloseParentheses)
                {
                    iter = closeParentheseOrComma;
                    break;
                }
                first = closeParentheseOrComma + 1;
            }

            if (arguments.size() < argCount)
            {
                auto& format = result->second.hasEllipse ?
                                   cld::Errors::PP::NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_AT_LEAST_N_GOT_N :
                                   cld::Errors::PP::NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N;
                log({cld::Message::error(format.args('"' + cld::to_string(name) + '"', argCount, arguments.size()),
                                         namePos, iter + 1, {cld::Underline(namePos)}),
                     cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, result->second.identifierPos,
                                        {cld::Underline(result->second.identifierPos)})});
                break;
            }
            else if (arguments.size() > argCount && !result->second.hasEllipse)
            {
                auto firstRedundant = arguments.begin() + argCount;
                std::vector<cld::Modifier> modifiers = {cld::Underline(namePos)};
                modifiers.reserve(1 + firstRedundant->size());
                std::transform(
                    firstRedundant, arguments.end(), std::back_inserter(modifiers),
                    [](llvm::ArrayRef<cld::Lexer::PPToken> ref) { return cld::PointAt(ref.begin(), ref.end()); });
                log({cld::Message::error(cld::Errors::PP::TOO_MANY_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N.args(
                                             '"' + cld::to_string(name) + '"', argCount, arguments.size()),
                                         namePos, iter + 1, std::move(modifiers)),
                     cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, result->second.identifierPos,
                                        {cld::Underline(result->second.identifierPos)})});
                break;
            }

            if (result->second.hasEllipse)
            {
                arguments.emplace_back(varargStart, iter);
            }

            std::unordered_set<const cld::Lexer::PPToken*> argumentsInReplacement;
            for (auto& token : result->second.replacement)
            {
                if (token.getTokenType() != cld::Lexer::TokenType::Identifier)
                {
                    continue;
                }
                if (std::find(result->second.identifierList->begin(), result->second.identifierList->end(),
                              token.getValue())
                    != result->second.identifierList->end())
                {
                    argumentsInReplacement.insert(&token);
                }
                else if (result->second.hasEllipse && token.getValue() == "__VA_ARGS__")
                {
                    argumentsInReplacement.insert(&token);
                }
            }
            m_substitutions.push_back({*result->second.identifierPos});
            auto repEnd = std::as_const(result->second.replacement).data() + result->second.replacement.size();
            auto repEnds = llvm::ArrayRef(repEnd);
            auto ppToken = macroSubstitute(result->second.replacement.data(), repEnds, iter, argumentsInReplacement);
            std::unordered_map<std::string_view, std::uint64_t> nameToIndex;
            for (auto identifier = result->second.identifierList->begin();
                 identifier != result->second.identifierList->end(); identifier++)
            {
                nameToIndex.emplace(*identifier, identifier - result->second.identifierList->begin());
            }
            if (result->second.hasEllipse)
            {
                nameToIndex.emplace("__VA_ARGS__", arguments.size() - 1);
            }
            auto actualReplacement = argumentSubstitution(
                std::move(ppToken), std::move(arguments),
                {argumentsInReplacement.begin(), argumentsInReplacement.end()}, std::move(nameToIndex));
            std::uint64_t lengthOfReplacement = 0;
            if (!actualReplacement.empty())
            {
                auto firstOffset = actualReplacement.front().getPPOffset();
                lengthOfReplacement = actualReplacement.back().getPPOffset()
                                      + actualReplacement.back().getCharSpaceLength() - firstOffset;
                insertWithTransform(
                    output, std::move_iterator(actualReplacement.begin()), std::move_iterator(actualReplacement.end()),
                    [base, firstOffset](cld::Lexer::PPToken&& token) {
                        return std::move(token).move({}, {}, {}, {}, token.getPPOffset() - firstOffset + base);
                    });
            }

            auto lengthOfMacroCall = iter->getPPOffset() + iter->getCharSpaceLength() - namePos->getPPOffset();
            delta -= lengthOfMacroCall - lengthOfReplacement;
            start = ++iter;
        }
        insertWithTransform(output, start, ends.front(), [delta](const cld::Lexer::PPToken& token) {
            return token.copy({}, {}, {}, {}, token.getPPOffset() + delta);
        });
        return output;
    }

    std::string escapeString(std::string_view input)
    {
        // TODO:
        return {input.begin(), input.end()};
    }

    const cld::Lexer::TokenBase* inc(const cld::Lexer::TokenBase* ptr) const noexcept override
    {
        return static_cast<const cld::Lexer::PPToken*>(ptr) + 1;
    }

    const cld::Lexer::TokenBase* dec(const cld::Lexer::TokenBase* ptr) const noexcept override
    {
        return static_cast<const cld::Lexer::PPToken*>(ptr) - 1;
    }

    std::uint64_t getLineNumber(cld::Lexer::FileID fileID, std::uint64_t offset) const noexcept override
    {
        CLD_ASSERT((std::uint64_t)fileID < m_files.size());
        auto result = std::lower_bound(m_files[(std::uint64_t)fileID].starts.begin(),
                                       m_files[(std::uint64_t)fileID].starts.end(), offset);
        return std::distance(m_files[(std::uint64_t)fileID].starts.begin(), result) + (*result == offset ? 1 : 0);
    }

    std::uint64_t getLineStartOffset(cld::Lexer::FileID fileID, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT((std::uint64_t)fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[(std::uint64_t)fileID].starts.size());
        return m_files[(std::uint64_t)fileID].starts[line - 1];
    }

    std::uint64_t getLineEndOffset(cld::Lexer::FileID fileID, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT((std::uint64_t)fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[(std::uint64_t)fileID].starts.size());
        return line == m_files[(std::uint64_t)fileID].starts.size() ?
                   m_files[(std::uint64_t)fileID].ppTokens.back().getOffset()
                       + m_files[(std::uint64_t)fileID].ppTokens.back().getLength() :
                   m_files[(std::uint64_t)fileID].starts[line];
    }

    std::uint64_t getPPLineNumber(std::uint64_t offset) const noexcept override
    {
        auto result = std::lower_bound(m_ppStarts.begin(), m_ppStarts.end(), offset);
        return std::distance(m_ppStarts.begin(), result) + (*result == offset ? 1 : 0);
    }

    std::uint64_t getPPLineStartOffset(std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(line - 1 < m_ppStarts.size());
        return m_ppStarts[line - 1];
    }

    std::uint64_t getPPLineEndOffset(std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(line - 1 < m_ppStarts.size());
        return line == m_ppStarts.size() ? m_result.back().getPPOffset() + m_result.back().getLength() :
                                           m_ppStarts[line];
    }

    const std::vector<cld::Source::File>& getFiles() const noexcept override
    {
        return m_files;
    }

public:
    Preprocessor(llvm::raw_ostream* report, const cld::LanguageOptions& options) noexcept
        : m_report(report), m_options(options)
    {
        std::string scratchPadSource;
        const auto t = std::time(nullptr);
        const auto tm = std::localtime(&t);
        std::string result(30, ' ');
        // I would absolutely love to use %e here but that does not work on Windows pre UCRT.
        // Instead I use %d which sadly inserts a 0 in front which would be non compliant. Therefore there is a #
        // in front of the day so that using a string replace of #0 would remove the leading 0
        auto size = std::strftime(result.data(), result.size(), "%b #%d %Y", tm);
        CLD_ASSERT(size > 0);
        result.resize(size);
        auto pos = result.find("#0");
        if (pos != result.npos)
        {
            result.erase(pos, 2);
        }
        else
        {
            pos = result.find("#");
            CLD_ASSERT(pos != result.npos);
            result.erase(pos, 1);
        }

        scratchPadSource += "#define __DATE__ \"" + result + "\"\n";

        result.resize(30, ' ');
        size = std::strftime(result.data(), result.size(), "%H:%M:%S", tm);
        CLD_ASSERT(size > 0);
        result.resize(size);
        scratchPadSource += "#define __TIME__ \"" + result + "\"\n";

        scratchPadSource += "#define __STDC__ 1\n";
        scratchPadSource += "#define __STDC_HOSTED__ 0\n";
        scratchPadSource += "#define __STDC_MB_MIGHT_NEQ_WC__ 1\n";
        scratchPadSource += "#define __STDC_VERSION__ 199901L\n";

        auto scratchPadPP = cld::Lexer::tokenize(scratchPadSource, options, report, &m_errorsOccured, "<Scratch Pad>");
        CLD_ASSERT(!m_errorsOccured);
        include(std::move(scratchPadPP));
        m_visitingScratchPad = false;
    }

    Preprocessor(const Preprocessor&) = delete;
    Preprocessor& operator=(const Preprocessor&) = delete;
    Preprocessor(Preprocessor&&) noexcept = delete;
    Preprocessor& operator=(Preprocessor&&) noexcept = delete;

    std::vector<cld::Lexer::PPToken>& getResult() noexcept
    {
        return m_result;
    }

    std::vector<cld::Source::Substitution>& getSubstitutions() noexcept
    {
        return m_substitutions;
    }

    std::vector<std::uint64_t>& getPpStarts() noexcept
    {
        return m_ppStarts;
    }

    std::vector<cld::Source::File>& getFiles() noexcept
    {
        return m_files;
    }

    void include(cld::PPSourceObject&& sourceObject)
    {
        auto scope = llvm::make_scope_exit([prev = m_currentFile, this] { m_currentFile = prev; });
        CLD_ASSERT(sourceObject.getFiles().size() == 1);
        for (auto& iter : sourceObject.getFiles())
        {
            for (auto& iter2 : iter.ppTokens)
            {
                iter2 = std::move(iter2).move({}, {}, {}, {}, {}, cld::Lexer::FileID(m_files.size()));
            }
            m_files.push_back(
                {std::move(iter.path), std::move(iter.source), std::move(iter.starts), std::move(iter.ppTokens)});
        }
        m_currentFile = m_files.size() - 1;
        cld::PP::Context context(*this, m_report);
        auto begin = std::as_const(m_files).back().ppTokens.data();
        auto tree = parseFile(
            begin, std::as_const(m_files).back().ppTokens.data() + std::as_const(m_files).back().ppTokens.size(),
            context);
        if (context.getErrorCount() != 0)
        {
            m_errorsOccured = true;
            return;
        }
        visit(tree);
    }

    void visit(const cld::PP::File& file)
    {
        for (auto& iter : file.groups)
        {
            visit(iter);
        }
    }

    void visit(const cld::PP::Group& group)
    {
        for (auto& iter : group.groupPart)
        {
            visit(iter);
        }
    }

    void visit(const cld::PP::GroupPart& groupPart)
    {
        cld::match(groupPart, [this](auto&& value) { this->visit(value); });
    }

    void visit(const cld::PP::TextBlock& text)
    {
        std::vector<const cld::Lexer::PPToken*> ends(text.ends.size());
        CLD_ASSERT(ends.size() > 0);
        std::transform(text.ends.begin(), text.ends.end(), ends.begin(),
                       [&text](std::uint64_t index) { return text.tokens.data() + index; });
        auto ref = llvm::ArrayRef(ends);
        auto iter = text.tokens.data();
        while (!ref.empty())
        {
            m_currentOffset += iter->getOffset() - getLineStartOffset(iter->getFileId(), iter->getLine(*this));
            auto result = macroSubstitute(iter, ref);
            pushLine(result);
            iter = ref.front();
            ref = ref.drop_front();
        }
    }

    void visit(const cld::PP::IfSection& ifSection) {}

    void visit(const cld::PP::ControlLine& controlLine)
    {
        cld::match(controlLine.variant, [this](auto&& value) { this->visit(value); });
    }

    void visit(const cld::PP::NonDirective&) {}

    void visit(const cld::PP::ControlLine::IncludeTag& includeTag) {}

    void visit(const cld::PP::ControlLine::LineTag& lineTag) {}

    void visit(const cld::PP::ControlLine::ErrorTag& errorTag) {}

    void visit(const cld::PP::ControlLine::PragmaTag& pragmaTag) {}

    void visit(cld::Lexer::PPTokenIterator undef)
    {
        pushNewline();
        auto name = undef->getValue();
        if (std::any_of(PREDEFINED_MACRO_NAMES.begin(), PREDEFINED_MACRO_NAMES.end(),
                        [name](std::string_view value) { return value == name; }))
        {
            log({cld::Message::error(
                cld::Errors::PP::UNDEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED.args('\'' + cld::to_string(name) + '\''),
                undef, {cld::Underline(undef)})});
            return;
        }
        m_defines.erase(name);
    }

    void visit(const cld::PP::DefineDirective& defineDirective)
    {
        pushNewline();
        auto iter = !defineDirective.hasEllipse ?
                        std::find_if(defineDirective.replacementBegin, defineDirective.replacementEnd,
                                     [](const cld::Lexer::PPToken& token) {
                                         if (token.getTokenType() != cld::Lexer::TokenType::Identifier)
                                         {
                                             return false;
                                         }
                                         return token.getValue() == "__VA_ARGS__";
                                     }) :
                        defineDirective.replacementEnd;
        if (iter != defineDirective.replacementEnd)
        {
            log({cld::Message::error(cld::Errors::PP::VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST, iter,
                                     {cld::Underline(iter)})});
        }
        auto name = defineDirective.identifierPos->getValue();
        if (name == "defined")
        {
            log({cld::Message::error(cld::Errors::PP::DEFINED_CANNOT_BE_USED_AS_MACRO_NAME,
                                     defineDirective.identifierPos, {cld::Underline(defineDirective.identifierPos)})});
            return;
        }
        if (!m_visitingScratchPad
            && std::any_of(PREDEFINED_MACRO_NAMES.begin(), PREDEFINED_MACRO_NAMES.end(),
                           [name](std::string_view value) { return value == name; }))
        {
            log({cld::Message::error(
                cld::Errors::PP::DEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED.args('\'' + cld::to_string(name) + '\''),
                defineDirective.identifierPos, {cld::Underline(defineDirective.identifierPos)})});
            return;
        }
        auto [result, notADuplicate] =
            m_defines.insert({name,
                              {defineDirective.identifierPos,
                               defineDirective.identifierList,
                               defineDirective.hasEllipse,
                               {defineDirective.replacementBegin, defineDirective.replacementEnd}}});
        if (notADuplicate)
        {
            return;
        }
        if (equal(defineDirective, result->second))
        {
            log({cld::Message::warning(cld::Warnings::PP::N_REDEFINED.args('\'' + cld::to_string(name) + '\''),
                                       defineDirective.identifierPos, {cld::Underline(defineDirective.identifierPos)}),
                 cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, result->second.identifierPos,
                                    {cld::Underline(result->second.identifierPos)})});
            return;
        }
        log({cld::Message::error(cld::Errors::PP::REDEFINITION_OF_MACRO_N.args('\'' + cld::to_string(name) + '\''),
                                 defineDirective.identifierPos, {cld::Underline(defineDirective.identifierPos)}),
             cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, result->second.identifierPos,
                                {cld::Underline(result->second.identifierPos)})});
    }
};
} // namespace

cld::PPSourceObject cld::PP::preprocess(cld::PPSourceObject&& sourceObject, llvm::raw_ostream* reporter) noexcept
{
    auto options = sourceObject.getLanguageOptions();
    Preprocessor preprocessor(reporter, options);
    preprocessor.include(std::move(sourceObject));
    return PPSourceObject(std::move(preprocessor.getResult()), std::move(preprocessor.getFiles()), options,
                          std::move(preprocessor.getSubstitutions()), std::move(preprocessor.getPpStarts()));
}
