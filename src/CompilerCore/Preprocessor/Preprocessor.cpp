#include "Preprocessor.hpp"

#include <llvm/ADT/ScopeExit.h>

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/Common/Text.hpp>

#include <ctime>
#include <stack>
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

template <class T>
std::vector<T>& append(std::vector<T>& lhs, std::vector<T>&& rhs)
{
    lhs.reserve(lhs.size() + rhs.size());
    lhs.insert(lhs.end(), std::move_iterator(rhs.begin()), std::move_iterator(rhs.end()));
    return lhs;
}

class Preprocessor final : private cld::SourceInterface
{
    llvm::raw_ostream* m_report;
    const cld::LanguageOptions& m_options;
    std::uint64_t m_macroID = 0;
    std::uint64_t m_currentFile = 0;
    std::vector<cld::Lexer::PPToken> m_result;
    std::vector<cld::Source::Substitution> m_substitutions{1};
    std::vector<std::uint64_t> m_ppStarts{0};
    struct Macro
    {
        cld::Lexer::PPTokenIterator identifierPos;
        std::optional<std::vector<cld::Lexer::PPToken>> argumentList;
        bool hasEllipse;
        llvm::ArrayRef<cld::Lexer::PPToken> replacement;
    };
    std::unordered_map<std::string_view, Macro> m_defines;
    std::vector<std::unordered_set<std::string>> m_disabledMacros{1};
    std::vector<cld::Source::File> m_files;
    bool m_errorsOccured = false;
    bool m_visitingScratchPad = true;

    void pushLine(llvm::ArrayRef<cld::Lexer::PPToken> tokens)
    {
        if (m_visitingScratchPad)
        {
            return;
        }
        m_result.insert(m_result.end(), tokens.begin(), tokens.end());
        m_result.insert(m_result.end(),
                        cld::Lexer::PPToken(cld::Lexer::TokenType::Newline, 0, 0, 0, 0, cld::Lexer::FileID(0)));
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
        if (lhs.argumentList.has_value() != rhs.argumentList.has_value())
        {
            return false;
        }
        if (lhs.argumentList
            && !std::equal(lhs.argumentList->begin(), lhs.argumentList->end(), rhs.argumentList->begin(),
                           rhs.argumentList->end(), [](const cld::Lexer::PPToken& lhs, const cld::Lexer::PPToken& rhs) {
                               return lhs.getValue() == rhs.getValue();
                           }))
        {
            return false;
        }
        if (lhs.hasEllipse != rhs.hasEllipse)
        {
            return false;
        }
        return std::equal(lhs.replacement.begin(), lhs.replacement.end(), rhs.replacement.begin(),
                          rhs.replacement.end(), [](const cld::Lexer::PPToken& lhs, const cld::Lexer::PPToken& rhs) {
                              if (lhs.getTokenType() != rhs.getTokenType())
                              {
                                  return false;
                              }
                              return lhs.getValue() == rhs.getValue();
                          });
    }

    struct OffsetHash
    {
        std::size_t operator()(const cld::Lexer::PPToken* ptr) const noexcept
        {
            return std::hash<std::uint64_t>()(ptr->getOffset())
                   ^ (std::hash<cld::Lexer::FileID>()(ptr->getFileId()) << 1);
        }
    };

    struct OffsetEqual
    {
        bool operator()(const cld::Lexer::PPToken* lhs, const cld::Lexer::PPToken* rhs) const noexcept
        {
            return std::tuple(lhs->getOffset(), (std::uint64_t)lhs->getFileId())
                   == std::tuple(rhs->getOffset(), (std::uint64_t)rhs->getFileId());
        }
    };

    using TokenSet = std::unordered_set<const cld::Lexer::PPToken*, OffsetHash, OffsetEqual>;

    std::vector<cld::Lexer::PPToken>
        argumentSubstitution(std::uint64_t parentID, const std::vector<cld::Lexer::PPToken>& identifierList,
                             std::vector<cld::Lexer::PPToken>&& replacementList,
                             std::vector<llvm::ArrayRef<cld::Lexer::PPToken>>&& arguments,
                             TokenSet&& argumentsInReplacement,
                             std::unordered_map<std::string_view, std::uint64_t> nameToIndex)
    {
        std::vector<cld::Lexer::PPToken> result;
        auto start = replacementList.begin();
        auto idStart = m_macroID + 1;
        for (auto iter = replacementList.begin(); iter != replacementList.end();)
        {
            if (iter->getTokenType() != cld::Lexer::TokenType::Identifier
                && iter->getTokenType() != cld::Lexer::TokenType::Pound)
            {
                iter++;
                continue;
            }
            bool stringify = false;
            if (iter->getTokenType() == cld::Lexer::TokenType::Pound)
            {
                stringify = true;
                iter++;
            }
            if (iter == replacementList.end() || argumentsInReplacement.count(&*iter) == 0)
            {
                if (stringify)
                {
                    // TODO: Error
                }
                else
                {
                    iter++;
                }
                continue;
            }
            result.insert(result.end(), std::move_iterator(start), std::move_iterator(iter - (stringify ? 1 : 0)));
            auto index = nameToIndex.find(iter->getValue());
            CLD_ASSERT(index != nameToIndex.end());
            std::vector<cld::Lexer::PPToken> copy = arguments[index->second];
            for (auto tokenIter = copy.begin(); tokenIter != copy.end(); tokenIter++)
            {
                if (tokenIter->getTokenType() == cld::Lexer::TokenType::Newline)
                {
                    if (tokenIter + 1 != copy.end())
                    {
                        (tokenIter + 1)->setLeadingWhitespace(true);
                    }
                    tokenIter = copy.erase(tokenIter);
                }
            }
            auto i = ++m_macroID;
            m_disabledMacros.push_back({});
            for (auto& token : copy)
            {
                token.setMacroId(cld::Lexer::MacroID(i));
            }
            auto prevSize = result.size();
            if (!stringify)
            {
                macroSubstitute(std::move(copy), [&result, iter](auto&& vector) {
                    if (!vector.empty())
                    {
                        vector[0].setLeadingWhitespace(iter->hasLeadingWhitespace());
                    }
                    append(result, std::move(vector));
                });
            }
            else
            {
                std::string text = "\"";
                bool first = true;
                for (auto& token : copy)
                {
                    if (!first && token.hasLeadingWhitespace())
                    {
                        text += ' ';
                    }
                    first = false;
                    if (token.getTokenType() == cld::Lexer::TokenType::StringLiteral
                        || token.getTokenType() == cld::Lexer::TokenType::Literal)
                    {
                        auto temp = cld::Lexer::normalizeSpelling(token.getRepresentation(*this));
                        for (auto character : temp)
                        {
                            switch (character)
                            {
                                case '"': text += "\\\""; break;
                                case '\\': text += "\\\\"; break;
                                default: text += character; break;
                            }
                        }
                    }
                    else
                    {
                        text += cld::Lexer::normalizeSpelling(token.getRepresentation(*this));
                    }
                }
                text += '\"';
                auto scratchPadPP = cld::Lexer::tokenize(text, m_options, m_report, &m_errorsOccured, "<Strings>");
                CLD_ASSERT(!m_errorsOccured);
                CLD_ASSERT(scratchPadPP.getFiles().size() == 1);
                CLD_ASSERT(scratchPadPP.getFiles()[0].ppTokens.size() == 2);
                CLD_ASSERT(scratchPadPP.getFiles()[0].ppTokens[1].getTokenType() == cld::Lexer::TokenType::Newline);
                auto file = scratchPadPP.getFiles()[0];
                file.ppTokens[0].setFileId(cld::Lexer::FileID(m_files.size()));
                file.ppTokens[0].setMacroId(cld::Lexer::MacroID(i));
                result.insert(result.end(), file.ppTokens[0]);
                m_files.push_back(std::move(file));
            }
            m_substitutions.push_back(
                {{identifierList[index->second].getOffset(), identifierList[index->second].getLength(),
                  identifierList[index->second].getFileId(), identifierList[index->second].getMacroId()},
                 {iter->getOffset(), iter->getLength(), iter->getFileId(), cld::Lexer::MacroID(parentID)},
                 prevSize == result.size()});
            start = ++iter;
            if (prevSize == result.size() && start != replacementList.end() && !start->hasLeadingWhitespace())
            {
                start->setLeadingWhitespace((iter - 1)->hasLeadingWhitespace());
            }
        }
        for (auto i = idStart; i <= m_macroID; i++)
        {
            m_disabledMacros[i].insert(m_disabledMacros[parentID].begin(), m_disabledMacros[parentID].end());
        }
        result.insert(result.end(), std::move_iterator(start), std::move_iterator(replacementList.end()));
        return result;
    }

    void macroSubstitute(std::vector<cld::Lexer::PPToken>&& tokens,
                         llvm::function_ref<void(std::vector<cld::Lexer::PPToken>&&)> lineOutput)
    {
        if (tokens.empty())
        {
            return;
        }
        std::vector<cld::Lexer::PPToken> output;
        auto line = tokens.front().getLine(*this);
        std::string_view file = m_files[(std::uint64_t)tokens.front().getFileId()].path;
        auto* start = tokens.data();
        for (auto* iter = tokens.data(); iter != tokens.data() + tokens.size();)
        {
            if (iter->getTokenType() == cld::Lexer::TokenType::Newline)
            {
                line++;
                iter++;
                if (iter == tokens.data() + tokens.size())
                {
                    break;
                }
                output.insert(output.end(), std::move_iterator(start), std::move_iterator(iter - 1));
                start = iter;
                lineOutput(std::move(output));
                output.clear();
                continue;
            }
            else if (iter->getTokenType() != cld::Lexer::TokenType::Identifier)
            {
                iter++;
                continue;
            }

            auto name = iter->getValue();
            if (name == "__FILE__" || name == "__LINE__")
            {
                m_defines.erase(name);
                m_visitingScratchPad = true;
                auto scope = llvm::make_scope_exit([this] { m_visitingScratchPad = false; });
                std::string source;
                if (name == "__FILE__")
                {
                    source = "#define __FILE__ \"" + escapeString(file) + "\"\n";
                }
                else
                {
                    source = "#define __LINE__ " + std::to_string(line) + "\n";
                }
                auto scratchPadPP =
                    cld::Lexer::tokenize(source, m_options, m_report, &m_errorsOccured, "<Scratch Pad>");
                CLD_ASSERT(!m_errorsOccured);
                include(std::move(scratchPadPP));
            }
            auto result = m_defines.find(name);
            if (result == m_defines.end()
                || ((std::uint64_t)iter->getMacroId() < m_disabledMacros.size()
                    && m_disabledMacros[(std::uint64_t)iter->getMacroId()].count(cld::to_string(name)) > 0))
            {
                iter++;
                continue;
            }
            if (result->second.argumentList)
            {
                auto* maybeOpenParenth =
                    std::find_if(iter + 1, tokens.data() + tokens.size(), [](const cld::Lexer::PPToken& token) {
                        return token.getTokenType() != cld::Lexer::TokenType::Newline;
                    });
                if (maybeOpenParenth == tokens.data() + tokens.size()
                    || maybeOpenParenth->getTokenType() != cld::Lexer::TokenType::OpenParentheses)
                {
                    iter++;
                    continue;
                }
            }

            output.insert(output.end(), std::move_iterator(start), std::move_iterator(iter));
            if (!result->second.argumentList)
            {
                // Object like macro
                auto i = ++m_macroID;
                m_disabledMacros.push_back({cld::to_string(name)});
                m_disabledMacros[i].insert(m_disabledMacros[(std::uint64_t)iter->getMacroId()].begin(),
                                           m_disabledMacros[(std::uint64_t)iter->getMacroId()].end());
                std::vector<cld::Lexer::PPToken> temp = result->second.replacement;
                m_substitutions.push_back(
                    {{result->second.identifierPos->getOffset(), result->second.identifierPos->getLength(),
                      result->second.identifierPos->getFileId(), result->second.identifierPos->getMacroId()},
                     {iter->getOffset(), iter->getLength(), iter->getFileId(), iter->getMacroId()},
                     temp.empty()});
                if (temp.empty())
                {
                    start = ++iter;
                    if (start != tokens.data() + tokens.size() && !start->hasLeadingWhitespace())
                    {
                        start->setLeadingWhitespace((iter - 1)->hasLeadingWhitespace());
                    }
                    continue;
                }

                temp[0].setLeadingWhitespace(iter->hasLeadingWhitespace());
                for (auto& iter2 : temp)
                {
                    iter2.setMacroId(cld::Lexer::MacroID(i));
                }
                temp.insert(temp.end(), std::move_iterator(iter + 1),
                            std::move_iterator(tokens.data() + tokens.size()));
                tokens = std::move(temp);
                start = iter = tokens.data();
                continue;
            }

            // Function like macro
            auto* namePos = iter;
            auto i = ++m_macroID;
            m_disabledMacros.push_back({cld::to_string(name)});
            m_disabledMacros[i].insert(m_disabledMacros[(std::uint64_t)iter->getMacroId()].begin(),
                                       m_disabledMacros[(std::uint64_t)iter->getMacroId()].end());
            iter = std::find_if(iter + 1, tokens.data() + tokens.size(),
                                [&line](const cld::Lexer::PPToken& token) {
                                    if (token.getTokenType() == cld::Lexer::TokenType::Newline)
                                    {
                                        line++;
                                        return false;
                                    }
                                    return true;
                                })
                   + 1;
            std::vector<llvm::ArrayRef<cld::Lexer::PPToken>> arguments;
            const auto identifierCount = result->second.argumentList->size() - (result->second.hasEllipse ? 1 : 0);
            auto* first = iter;
            const cld::Lexer::PPToken* varargStart = iter;
            while (true)
            {
                auto count = 0;
                auto* closeParentheseOrComma =
                    std::find_if(first, tokens.data() + tokens.size(),
                                 [&count, &line](const cld::Lexer::PPToken& token) mutable -> bool {
                                     if (token.getTokenType() == cld::Lexer::TokenType::Newline)
                                     {
                                         line++;
                                         return false;
                                     }
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
                if (closeParentheseOrComma == tokens.data() + tokens.size())
                {
                    log({cld::Message::error(cld::Errors::Parser::EXPECTED_N.args(cld::to_string(count + 1) + " ')'"),
                                             cld::Message::after, closeParentheseOrComma - 1,
                                             {cld::InsertAfter(closeParentheseOrComma - 1, ")")}),
                         cld::Message::note(cld::Notes::TO_MATCH_N_HERE.args("'('"), namePos + 1,
                                            {cld::PointAt(namePos + 1)})});
                    return;
                }
                if (!result->second.hasEllipse || arguments.size() != identifierCount)
                {
                    if (identifierCount != 0
                        || closeParentheseOrComma->getTokenType() != cld::Lexer::TokenType::CloseParentheses)
                    {
                        arguments.emplace_back(first, closeParentheseOrComma);
                    }
                    if (result->second.hasEllipse && arguments.size() == identifierCount)
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

            if (arguments.size() < identifierCount)
            {
                auto& format = result->second.hasEllipse ?
                                   cld::Errors::PP::NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_AT_LEAST_N_GOT_N :
                                   cld::Errors::PP::NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N;
                log({cld::Message::error(
                         format.args('"' + cld::to_string(name) + '"', identifierCount, arguments.size()), namePos,
                         iter + 1, {cld::Underline(namePos)}),
                     cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, result->second.identifierPos,
                                        {cld::Underline(result->second.identifierPos)})});
                break;
            }
            else if (arguments.size() > identifierCount && !result->second.hasEllipse)
            {
                auto firstRedundant = arguments.begin() + identifierCount;
                std::vector<cld::Modifier> modifiers = {cld::Underline(namePos)};
                modifiers.reserve(1 + firstRedundant->size());
                std::transform(
                    firstRedundant, arguments.end(), std::back_inserter(modifiers),
                    [](llvm::ArrayRef<cld::Lexer::PPToken> ref) { return cld::PointAt(ref.begin(), ref.end()); });
                log({cld::Message::error(cld::Errors::PP::TOO_MANY_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N.args(
                                             '"' + cld::to_string(name) + '"', identifierCount, arguments.size()),
                                         namePos, iter + 1, std::move(modifiers)),
                     cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, result->second.identifierPos,
                                        {cld::Underline(result->second.identifierPos)})});
                break;
            }

            if (result->second.hasEllipse)
            {
                arguments.emplace_back(varargStart, iter);
            }

            std::vector<cld::Lexer::PPToken> ppToken = result->second.replacement;
            std::unordered_map<std::string_view, std::uint64_t> nameToIndex;
            for (auto identifier = result->second.argumentList->begin();
                 identifier != result->second.argumentList->end(); identifier++)
            {
                nameToIndex.emplace(identifier->getValue(), identifier - result->second.argumentList->begin());
            }
            if (result->second.hasEllipse)
            {
                nameToIndex.emplace("__VA_ARGS__", arguments.size() - 1);
            }
            TokenSet argumentsInReplacement;
            for (auto& token : result->second.replacement)
            {
                if (token.getTokenType() != cld::Lexer::TokenType::Identifier)
                {
                    continue;
                }
                if (std::find_if(
                        result->second.argumentList->begin(), result->second.argumentList->end(),
                        [&token](const cld::Lexer::PPToken& value) { return value.getValue() == token.getValue(); })
                    != result->second.argumentList->end())
                {
                    argumentsInReplacement.insert(&token);
                }
                else if (result->second.hasEllipse && token.getValue() == "__VA_ARGS__")
                {
                    argumentsInReplacement.insert(&token);
                }
            }
            m_substitutions.push_back(
                {{result->second.identifierPos->getOffset(), result->second.identifierPos->getLength(),
                  result->second.identifierPos->getFileId(), result->second.identifierPos->getMacroId()},
                 {namePos->getOffset(), iter->getOffset() + iter->getLength() - namePos->getOffset(),
                  namePos->getFileId(), namePos->getMacroId()},
                 false});
            auto index = m_substitutions.size() - 1;
            auto actualReplacement =
                argumentSubstitution(i, *result->second.argumentList, std::move(ppToken), std::move(arguments),
                                     std::move(argumentsInReplacement), std::move(nameToIndex));
            if (actualReplacement.empty())
            {
                m_substitutions[index].empty = true;
                start = ++iter;
                continue;
            }

            for (auto& iter2 : actualReplacement)
            {
                if (iter2.isMacroInserted())
                {
                    continue;
                }
                iter2.setMacroId(cld::Lexer::MacroID(i));
            }
            actualReplacement.insert(actualReplacement.end(), std::move_iterator(iter + 1),
                                     std::move_iterator(tokens.data() + tokens.size()));
            tokens = std::move(actualReplacement);
            start = iter = tokens.data();
        }
        output.insert(output.end(), std::move_iterator(start), std::move_iterator(tokens.data() + tokens.size()));
        lineOutput(std::move(output));
    }

    std::string escapeString(std::string_view input)
    {
        std::string result;
        result.reserve(input.size());
        for (auto& iter : input)
        {
            switch (iter)
            {
                case '"': result += "\\\""; break;
                case '\\': result += "\\\\"; break;
                case '?': result += "\\?"; break;
                default: result += iter;
            }
        }
        return {input.begin(), input.end()};
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

    const std::vector<cld::Source::File>& getFiles() const noexcept override
    {
        return m_files;
    }

    const std::vector<cld::Source::Substitution>& getSubstitutions() const noexcept override
    {
        return m_substitutions;
    }

public:
    Preprocessor(llvm::raw_ostream* report, const cld::LanguageOptions& options) noexcept
        : m_report(report), m_options(options)
    {
        std::string scratchPadSource;
        const auto t = std::time(nullptr);
        const auto* tm = std::localtime(&t);
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
                iter2.setFileId(cld::Lexer::FileID(m_files.size()));
            }
            m_files.push_back(
                {std::move(iter.path), std::move(iter.source), std::move(iter.starts), std::move(iter.ppTokens)});
        }
        m_currentFile = m_files.size() - 1;
        cld::PP::Context context(*this, m_report);
        const auto* begin = std::as_const(m_files).back().ppTokens.data();
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
        macroSubstitute(text.tokens, [this](auto&& tokens) { pushLine(tokens); });
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
        const auto* iter = !defineDirective.hasEllipse ?
                               std::find_if(defineDirective.replacement.begin(), defineDirective.replacement.end(),
                                            [](const cld::Lexer::PPToken& token) {
                                                if (token.getTokenType() != cld::Lexer::TokenType::Identifier)
                                                {
                                                    return false;
                                                }
                                                return token.getValue() == "__VA_ARGS__";
                                            }) :
                               defineDirective.replacement.end();
        if (iter != defineDirective.replacement.end())
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
                               defineDirective.argumentList,
                               defineDirective.hasEllipse,
                               {defineDirective.replacement.begin(), defineDirective.replacement.end()}}});
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

std::string cld::PP::reconstruct(const cld::Lexer::PPToken* begin, const cld::Lexer::PPToken* end,
                                 const SourceInterface& sourceInterface) noexcept
{
    if (begin == end)
    {
        return {};
    }
    std::string result = Lexer::normalizeSpelling(begin->getRepresentation(sourceInterface));
    const auto* prev = begin++;
    while (begin != end)
    {
        if (begin->getTokenType() == cld::Lexer::TokenType::Newline)
        {
            result += '\n';
        }
        else if (begin->hasLeadingWhitespace()
                 || Lexer::needsWhitespaceInBetween(prev->getTokenType(), begin->getTokenType()))
        {
            result += " " + Lexer::normalizeSpelling(begin->getRepresentation(sourceInterface));
        }
        else
        {
            result += Lexer::normalizeSpelling(begin->getRepresentation(sourceInterface));
        }
        prev = begin++;
    }
    return result;
}
