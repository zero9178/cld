#include "Preprocessor.hpp"

#include <cld/Frontend/Compiler/ErrorMessages.hpp>
#include <cld/Frontend/Compiler/Parser.hpp>
#include <cld/Frontend/Compiler/SemanticAnalysis.hpp>
#include <cld/Support/Filesystem.hpp>
#include <cld/Support/ScopeExit.hpp>
#include <cld/Support/Text.hpp>

#include <ctime>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Parser.hpp"

namespace
{
constexpr std::array PREDEFINED_MACRO_NAMES = {"__DATE__",         "__FILE__",        "__LINE__",
                                               "__STDC__",         "__STDC_HOSTED__", "__STDC_MB_MIGHT_NEQ_WC__",
                                               "__STDC_VERSION__", "__TIME__"};

template <class T>
std::vector<T>& append(std::vector<T>& lhs, std::vector<T>&& rhs)
{
    lhs.insert(lhs.end(), std::move_iterator(rhs.begin()), std::move_iterator(rhs.end()));
    return lhs;
}

class Preprocessor final : private cld::PPSourceInterface
{
    llvm::raw_ostream* m_reporter;
    const cld::LanguageOptions& m_languageOptions;
    const cld::PP::Options& m_ppOptions;
    std::uint32_t m_macroID = 0;
    std::uint32_t m_currentFile = 0;
    std::vector<cld::Lexer::PPToken> m_result;
    std::vector<cld::Source::PPRecord> m_substitutions{1};
    struct Macro
    {
        cld::Lexer::PPTokenIterator identifierPos;
        std::optional<std::vector<cld::Lexer::PPToken>> argumentList;
        bool hasEllipse;
        llvm::ArrayRef<cld::Lexer::PPToken> replacement;
    };
    std::unordered_map<std::string_view, Macro> m_defines;

    std::vector<std::unordered_set<cld::Lexer::PPTokenIterator>> m_disabledMacros{1};
    std::vector<cld::Source::File> m_files;
    std::vector<std::vector<cld::Lexer::PPToken>> m_fileTokens;
    std::vector<cld::Lexer::IntervalMap> m_intervalMaps;
    bool m_errorsOccurred = false;
    bool m_visitingScratchPad = true;

    struct IncludeGuardOpt
    {
        std::vector<std::pair<std::string_view, bool>> neededMacroValues;
    };

    std::unordered_map<std::string, IncludeGuardOpt> m_includeGuardOptCache;

    void pushLine(std::vector<cld::Lexer::PPToken>&& tokens)
    {
        if (m_visitingScratchPad)
        {
            return;
        }
        append(m_result, std::move(tokens));
    }

    bool log(const cld::Message& message)
    {
        if (m_reporter)
        {
            *m_reporter << message;
        }
        if (message.getSeverity() == cld::Severity::Error)
        {
            m_errorsOccurred = true;
        }
        return message.getSeverity() != cld::Severity::None;
    }

    static bool equal(const Macro& lhs, const Macro& rhs)
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
            return cld::hashCombine(ptr->getOffset(), ptr->getFileId());
        }
    };

    struct OffsetEqual
    {
        bool operator()(const cld::Lexer::PPToken* lhs, const cld::Lexer::PPToken* rhs) const noexcept
        {
            return std::tuple(lhs->getOffset(), lhs->getFileId()) == std::tuple(rhs->getOffset(), rhs->getFileId());
        }
    };

    using TokenSet = std::unordered_set<const cld::Lexer::PPToken*, OffsetHash, OffsetEqual>;

    std::vector<cld::Lexer::PPToken> argumentSubstitution(std::uint32_t parentID,
                                                          const std::vector<cld::Lexer::PPToken>& identifierList,
                                                          std::vector<cld::Lexer::PPToken>&& replacementList,
                                                          std::vector<llvm::ArrayRef<cld::Lexer::PPToken>>&& arguments,
                                                          TokenSet&& argumentsInReplacement,
                                                          std::unordered_map<std::string_view, std::size_t> nameToIndex)
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
                CLD_ASSERT(!stringify);
                iter++;
                continue;
            }
            result.insert(result.end(), std::move_iterator(start), std::move_iterator(iter - (stringify ? 1 : 0)));
            // Token pasting is done if iter is either the right
            bool doublePoundToTheLeft = !result.empty()
                                        && result.back().getTokenType() == cld::Lexer::TokenType::DoublePound
                                        && !std::holds_alternative<cld::Source::TokenConcatenation>(
                                            m_substitutions[result.back().getMacroId()]);
            // Or left hand side of the operator
            bool doublePoundToTheRight =
                iter + 1 != replacementList.end() && (iter + 1)->getTokenType() == cld::Lexer::TokenType::DoublePound
                && !std::holds_alternative<cld::Source::TokenConcatenation>(m_substitutions[(iter + 1)->getMacroId()]);
            // But not when the argument is being stringified (then the string needs to be pasted)
            bool inTokenPasting = (doublePoundToTheLeft || doublePoundToTheRight) && !stringify;
            auto index = nameToIndex.find(iter->getValue());
            CLD_ASSERT(index != nameToIndex.end());
            std::vector<cld::Lexer::PPToken> copy = arguments[index->second];
            for (auto tokenIter = copy.begin(); tokenIter != copy.end();)
            {
                if (tokenIter->getTokenType() != cld::Lexer::TokenType::Newline)
                {
                    tokenIter++;
                    continue;
                }
                if (tokenIter + 1 != copy.end())
                {
                    (tokenIter + 1)->setLeadingWhitespace(true);
                }
                tokenIter = copy.erase(tokenIter);
                if (tokenIter != copy.end())
                {
                    tokenIter++;
                }
            }
            auto i = ++m_macroID;
            m_disabledMacros.push_back({});
            if (!copy.empty())
            {
                m_disabledMacros.back().insert(m_disabledMacros[copy.front().getMacroId()].begin(),
                                               m_disabledMacros[copy.front().getMacroId()].end());
            }
            m_substitutions.push_back({});
            CLD_ASSERT(i == m_substitutions.size() - 1);
            for (auto& token : copy)
            {
                token.setMacroId(i);
            }
            auto prevSize = result.size();
            if (!stringify)
            {
                if (!inTokenPasting)
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
                    if (!copy.empty())
                    {
                        copy[0].setLeadingWhitespace(iter->hasLeadingWhitespace());
                    }
                    append(result, std::move(copy));
                    if (result.size() == prevSize && doublePoundToTheLeft)
                    {
                        // If it's empty and the ## was to our left delete it by poping the token
                        result.pop_back();
                    }
                }
                auto temp = *iter;
                temp.setMacroId(parentID);
                m_substitutions[i] = cld::Source::Substitution{
                    identifierList[index->second], std::move(temp), {}, prevSize == result.size()};
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
                        continue;
                    }
                    text += cld::Lexer::normalizeSpelling(token.getRepresentation(*this));
                }
                text += '\"';
                bool errorsOccurred = false;
                auto scratchPadPP =
                    cld::Lexer::tokenize(std::move(text), &m_languageOptions, m_reporter, &errorsOccurred, "<Strings>");
                CLD_ASSERT(!errorsOccurred);
                CLD_ASSERT(scratchPadPP.getFiles().size() == 1);
                CLD_ASSERT(scratchPadPP.data().size() == 1);
                scratchPadPP.data()[0].setFileId(m_files.size());
                scratchPadPP.data()[0].setMacroId(i);
                result.insert(result.end(), scratchPadPP.data()[0]);
                auto file = scratchPadPP.getFiles()[0];
                m_files.push_back(std::move(file));
                m_substitutions[i] = cld::Source::Stringification{std::move(copy), *iter};
            }
            // If we did token pasting and it was empty and the ## is to the right "delete" it by setting the start
            // iterator to one past it as not to include it when inserting. Don't do that when there was also a ##
            // to the left. Otherwise x ## y ## z with y being the empty expansion would result in x z instead of
            // x ## z
            if (prevSize == result.size() && inTokenPasting && doublePoundToTheRight && !doublePoundToTheLeft)
            {
                iter++;
                // There must be a token after the ##
                CLD_ASSERT(iter + 1 != replacementList.end());
                if (!(iter + 1)->hasLeadingWhitespace())
                {
                    (iter + 1)->setLeadingWhitespace(iter->hasLeadingWhitespace()
                                                     || (iter - 1)->hasLeadingWhitespace());
                }
            }
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

    template <class F>
    static TokenSet filterTokens(llvm::ArrayRef<cld::Lexer::PPToken> tokens, cld::Lexer::TokenType type, F&& hasValueFn)
    {
        TokenSet result;
        for (auto& token : tokens)
        {
            if (token.getTokenType() != type)
            {
                continue;
            }
            if (hasValueFn(token.getValue()))
            {
                result.insert(&token);
            }
        }
        return result;
    }

    std::optional<std::vector<llvm::ArrayRef<cld::Lexer::PPToken>>>
        gatherArguments(const cld::Lexer::PPToken*& begin, const cld::Lexer::PPToken* end,
                        const cld::Lexer::PPToken& namePos, const Macro& macro, std::uint64_t& line)
    {
        std::vector<llvm::ArrayRef<cld::Lexer::PPToken>> arguments;
        const auto identifierCount = macro.argumentList->size() - (macro.hasEllipse ? 1 : 0);
        auto* first = begin;
        const cld::Lexer::PPToken* varargStart = begin;
        while (true)
        {
            auto count = 0;
            auto* closeParenthesesOrComma =
                std::find_if(first, end, [&count, &line](const cld::Lexer::PPToken& token) mutable -> bool {
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
            if (closeParenthesesOrComma == end)
            {
                // There can be infinitely many newlines in between the name and the (
                auto* openParentheses = std::find_if(&namePos, end, [](const cld::Lexer::PPToken& token) {
                    return token.getTokenType() == cld::Lexer::TokenType::OpenParentheses;
                });
                log(cld::Errors::Parser::EXPECTED_N.args(*(closeParenthesesOrComma - 1), *this,
                                                         cld::Lexer::TokenType::CloseParentheses,
                                                         *(closeParenthesesOrComma - 1)));
                log(cld::Notes::TO_MATCH_N_HERE.args(*openParentheses, *this, *openParentheses));
                return {};
            }
            if (!macro.hasEllipse || arguments.size() != identifierCount)
            {
                if (identifierCount != 0
                    || closeParenthesesOrComma->getTokenType() != cld::Lexer::TokenType::CloseParentheses)
                {
                    arguments.emplace_back(first, closeParenthesesOrComma);
                }
                if (macro.hasEllipse && arguments.size() == identifierCount)
                {
                    if (closeParenthesesOrComma->getTokenType() == cld::Lexer::TokenType::CloseParentheses)
                    {
                        varargStart = closeParenthesesOrComma;
                    }
                    else
                    {
                        varargStart = closeParenthesesOrComma + 1;
                    }
                }
            }
            if (closeParenthesesOrComma->getTokenType() == cld::Lexer::TokenType::CloseParentheses)
            {
                begin = closeParenthesesOrComma;
                break;
            }
            first = closeParenthesesOrComma + 1;
        }

        if (arguments.size() < identifierCount)
        {
            if (macro.hasEllipse)
            {
                log(cld::Errors::PP::NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_AT_LEAST_N_GOT_N.args(
                    std::forward_as_tuple(namePos, *(begin + 1)), *this, namePos, identifierCount, arguments.size()));
            }
            else
            {
                log(cld::Errors::PP::NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N.args(
                    std::forward_as_tuple(namePos, *(begin + 1)), *this, namePos, identifierCount, arguments.size()));
            }
            log(cld::Notes::PREVIOUSLY_DECLARED_HERE.args(*macro.identifierPos, *this, *macro.identifierPos));
            return {};
        }
        if (arguments.size() > identifierCount && !macro.hasEllipse)
        {
            auto firstRedundant = arguments.begin() + identifierCount;
            log(cld::Errors::PP::TOO_MANY_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N.args(
                namePos, *this, namePos, identifierCount, arguments.size(),
                std::forward_as_tuple(firstRedundant->front(), arguments.back().back())));
            log(cld::Notes::PREVIOUSLY_DECLARED_HERE.args(*macro.identifierPos, *this, *macro.identifierPos));
            return {};
        }

        if (macro.hasEllipse)
        {
            arguments.emplace_back(varargStart, begin);
        }
        return arguments;
    }

    void evaluateConcats(std::uint32_t parentID, std::vector<cld::Lexer::PPToken>& tokens,
                         std::optional<TokenSet> concatOperators = {})
    {
        for (auto iter = tokens.begin(); iter != tokens.end(); iter++)
        {
            if (iter->getTokenType() != cld::Lexer::TokenType::DoublePound
                || std::holds_alternative<cld::Source::TokenConcatenation>(m_substitutions[iter->getMacroId()])
                || (concatOperators && concatOperators->count(&*iter) == 0))
            {
                continue;
            }
            CLD_ASSERT(iter != tokens.begin());
            auto& lhs = *(iter - 1);
            auto leadingWhitespace = lhs.hasLeadingWhitespace();
            CLD_ASSERT(iter + 1 != tokens.end());
            auto& rhs = *(iter + 1);
            std::string text;
            {
                auto lhsView = lhs.getRepresentation(*this);
                auto rhsView = rhs.getRepresentation(*this);
                text.resize(lhsView.size() + rhsView.size());
                std::memcpy(text.data(), lhsView.data(), lhsView.size());
                std::memcpy(text.data() + lhsView.size(), rhsView.data(), rhsView.size());
            }
            bool errors = false;
            auto scratchPadPP =
                cld::Lexer::tokenize(std::move(text), &m_languageOptions, &llvm::nulls(), &errors, "<Pastings>");
            CLD_ASSERT(scratchPadPP.getFiles().size() == 1);
            if (errors || scratchPadPP.data().size() != 1)
            {
                if (log(cld::Warnings::PP::TOKEN_CONCATENATION_RESULTING_IN_AN_INVALID_TOKEN_IS_UB.args(
                        *iter, *this, lhs, *iter, rhs)))
                {
                    log(cld::Notes::PP::WHEN_CONCATENATING_N_AND_N.args(*iter, *this, lhs, *iter, rhs));
                }
                iter = tokens.erase(iter - 1, iter + 2);
                if (iter == tokens.end())
                {
                    break;
                }
                continue;
            }
            CLD_ASSERT(scratchPadPP.data().size() == 1);
            auto i = ++m_macroID;
            m_disabledMacros.push_back({});
            m_disabledMacros[i].insert(m_disabledMacros[parentID].begin(), m_disabledMacros[parentID].end());
            m_substitutions.push_back(cld::Source::TokenConcatenation{lhs, rhs});
            scratchPadPP.data()[0].setFileId(m_files.size());
            scratchPadPP.data()[0].setMacroId(i);
            iter = tokens.erase(iter - 1, iter + 1);
            *iter = scratchPadPP.data()[0];
            iter->setLeadingWhitespace(leadingWhitespace);
            m_files.push_back(std::move(scratchPadPP.getFiles()[0]));
        }
    }

    template <class F>
    void macroSubstitute(std::vector<cld::Lexer::PPToken>&& tokens, F lineOutput, bool inIfExpression = false)
    {
        if (tokens.empty())
        {
            return;
        }
        std::vector<cld::Lexer::PPToken> output;
        auto line = tokens.front().getLine(*this);
        auto fileId = tokens.front().getFileId();
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
            if (iter->getTokenType() != cld::Lexer::TokenType::Identifier)
            {
                iter++;
                continue;
            }

            auto name = iter->getValue();
            if (name == "__FILE__" || name == "__LINE__")
            {
                m_defines.erase(name);
                m_visitingScratchPad = true;
                auto scope = cld::ScopeExit([this] { m_visitingScratchPad = false; });
                const auto& range = m_files[fileId].lineAndFileMapping;
                auto map = std::upper_bound(range.begin(), range.end(), line,
                                            [](auto line, const auto& tuple) { return line < std::get<0>(tuple); });
                if (!range.empty() && map == range.end())
                {
                    map--;
                }
                std::string source;
                if (name == "__FILE__")
                {
                    std::string_view file;
                    if (map == range.end() || !std::get<1>(*map))
                    {
                        file = m_files[fileId].path;
                    }
                    else
                    {
                        file = *std::get<1>(*map);
                    }
                    source = "#define __FILE__ \"" + escapeString(file) + "\"\n";
                }
                else
                {
                    std::uint64_t printedLine;
                    if (map != range.end())
                    {
                        printedLine = std::get<2>(*map) + line - std::get<0>(*map);
                    }
                    else
                    {
                        printedLine = line;
                    }
                    source = "#define __LINE__ " + std::to_string(printedLine) + "\n";
                }
                bool errorsOccurred = false;
                auto scratchPadPP = cld::Lexer::tokenize(std::move(source), &m_languageOptions, m_reporter,
                                                         &errorsOccurred, "<Scratch Pad>");
                CLD_ASSERT(!errorsOccurred);
                include(std::move(scratchPadPP));
            }
            else if (inIfExpression && name == "defined")
            {
                iter++;
                if (iter != tokens.data() + tokens.size())
                {
                    if (iter->getTokenType() == cld::Lexer::TokenType::Identifier)
                    {
                        iter++;
                    }
                    else if (iter->getTokenType() == cld::Lexer::TokenType::OpenParentheses
                             && (iter + 1) != tokens.data() + tokens.size()
                             && (iter + 1)->getTokenType() == cld::Lexer::TokenType::Identifier)
                    {
                        iter += 2;
                    }
                }
                continue;
            }
            auto result = m_defines.find(name);
            if (result == m_defines.end()
                || (iter->getMacroId() < m_disabledMacros.size()
                    && m_disabledMacros[iter->getMacroId()].count(result->second.identifierPos) > 0))
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
                m_disabledMacros.push_back({result->second.identifierPos});
                m_disabledMacros[i].insert(m_disabledMacros[iter->getMacroId()].begin(),
                                           m_disabledMacros[iter->getMacroId()].end());
                std::vector<cld::Lexer::PPToken> temp = result->second.replacement;
                m_substitutions.push_back(
                    cld::Source::Substitution{*result->second.identifierPos, *iter, {}, temp.empty()});
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
                    iter2.setMacroId(i);
                }
                evaluateConcats(i, temp);
                temp.insert(temp.end(), std::move_iterator(iter + 1),
                            std::move_iterator(tokens.data() + tokens.size()));
                tokens = std::move(temp);
                start = iter = tokens.data();
                continue;
            }

            // Function like macro
            auto* namePos = iter;
            auto i = ++m_macroID;
            m_disabledMacros.push_back({result->second.identifierPos});
            m_disabledMacros[i].insert(m_disabledMacros[iter->getMacroId()].begin(),
                                       m_disabledMacros[iter->getMacroId()].end());
            m_substitutions.push_back({});
            CLD_ASSERT(i == m_substitutions.size() - 1);
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
            auto arguments = gatherArguments(const_cast<const cld::Lexer::PPToken*&>(iter),
                                             tokens.data() + tokens.size(), *namePos, result->second, line);
            if (!arguments)
            {
                break;
            }

            std::unordered_map<std::string_view, std::uint64_t> nameToIndex;
            for (auto identifier = result->second.argumentList->begin();
                 identifier != result->second.argumentList->end(); identifier++)
            {
                nameToIndex.emplace(identifier->getValue(), identifier - result->second.argumentList->begin());
            }
            if (result->second.hasEllipse)
            {
                nameToIndex.emplace("__VA_ARGS__", arguments->size() - 1);
            }

            auto argumentsInReplacement =
                filterTokens(result->second.replacement, cld::Lexer::TokenType::Identifier,
                             [&nameToIndex](std::string_view value) { return nameToIndex.count(value); });
            m_substitutions[i] = cld::Source::Substitution{*result->second.identifierPos, *namePos, *iter, false};

            auto concatOps = filterTokens(result->second.replacement, cld::Lexer::TokenType::DoublePound,
                                          [](auto&&) { return true; });
            std::vector<cld::Lexer::PPToken> ppToken = result->second.replacement;
            auto actualReplacement =
                argumentSubstitution(i, *result->second.argumentList, std::move(ppToken), std::move(*arguments),
                                     std::move(argumentsInReplacement), std::move(nameToIndex));
            if (actualReplacement.empty())
            {
                cld::get<cld::Source::Substitution>(m_substitutions[i]).empty = true;
                start = ++iter;
                continue;
            }

            for (auto& iter2 : actualReplacement)
            {
                if (iter2.isMacroInserted())
                {
                    continue;
                }
                iter2.setMacroId(i);
            }
            evaluateConcats(i, actualReplacement, concatOps);
            actualReplacement.insert(actualReplacement.end(), std::move_iterator(iter + 1),
                                     std::move_iterator(tokens.data() + tokens.size()));
            tokens = std::move(actualReplacement);
            start = iter = tokens.data();
        }
        output.insert(output.end(), std::move_iterator(start), std::move_iterator(tokens.data() + tokens.size()));
        lineOutput(std::move(output));
    }

    static std::string escapeString(std::string_view input)
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

    std::optional<Macro> parseDefineDirective(const cld::PP::DefineDirective& defineDirective)
    {
        if (defineDirective.tokens.empty())
        {
            log(cld::Errors::Parser::EXPECTED_N_AFTER_N.args(
                *defineDirective.defineToken, *this, cld::Lexer::TokenType::Identifier, *defineDirective.defineToken));
            return {};
        }
        if (defineDirective.tokens[0].getTokenType() != cld::Lexer::TokenType::Identifier)
        {
            log(cld::Errors::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                defineDirective.tokens[0], *this, cld::Lexer::TokenType::Identifier, defineDirective.tokens[0]));
            return {};
        }
        const auto* identifierPos = &defineDirective.tokens[0];
        const auto* curr = identifierPos + 1;
        const auto* end = defineDirective.tokens.end();
        if (curr == end)
        {
            return Macro{identifierPos, std::nullopt, false, {}};
        }

        if (curr->getTokenType() != cld::Lexer::TokenType::OpenParentheses
            || curr->getOffset() != identifierPos->getOffset() + identifierPos->getLength())
        {
            if (curr->getTokenType() != cld::Lexer::TokenType::Newline
                && curr->getOffset() == identifierPos->getOffset() + identifierPos->getLength())
            {
                log(cld::Errors::PP::WHITESPACE_REQUIRED_AFTER_OBJECT_MACRO_DEFINITION.args(*curr, *this, *curr));
            }
            return Macro{identifierPos, std::nullopt, false, {curr, end}};
        }

        // We are a function like macro from here on
        const auto* openP = curr++;
        if (curr == end)
        {
            log(cld::Errors::Parser::EXPECTED_N.args(*openP, *this, cld::Lexer::TokenType::CloseParentheses, *openP));
            log(cld::Notes::TO_MATCH_N_HERE.args(*openP, *this, *openP));
            return {};
        }

        switch (curr->getTokenType())
        {
            case cld::Lexer::TokenType::CloseParentheses:
            {
                curr++;
                return Macro{identifierPos, {std::vector<cld::Lexer::PPToken>{}}, false, {curr, end}};
            }
            case cld::Lexer::TokenType::Ellipse:
            {
                auto argumentList = std::vector{*curr};
                curr++;
                if (curr == end)
                {
                    log(cld::Errors::Parser::EXPECTED_N_AFTER_N.args(
                        *(curr - 1), *this, cld::Lexer::TokenType::CloseParentheses, *(curr - 1)));
                    log(cld::Notes::TO_MATCH_N_HERE.args(*openP, *this, *openP));
                    return {};
                }
                if (curr->getTokenType() != cld::Lexer::TokenType::CloseParentheses)
                {
                    log(cld::Errors::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                        *curr, *this, cld::Lexer::TokenType::CloseParentheses, *curr));
                    log(cld::Notes::TO_MATCH_N_HERE.args(*openP, *this, *openP));
                    return {};
                }
                return Macro{identifierPos, std::move(argumentList), true, {curr + 1, end}};
            }
            default: break;
        }

        if (curr->getTokenType() != cld::Lexer::TokenType::Identifier)
        {
            log(cld::Errors::Parser::EXPECTED_N_INSTEAD_OF_N.args(*curr, *this, cld::Lexer::TokenType::Identifier,
                                                                  *curr));
            return {};
        }
        std::vector<cld::Lexer::PPToken> argumentList = {*curr};
        curr++;
        bool ellipse = false;
        while (curr != end && curr->getTokenType() == cld::Lexer::TokenType::Comma)
        {
            curr++;
            if (curr != end && curr->getTokenType() == cld::Lexer::TokenType::Ellipse)
            {
                ellipse = true;
                argumentList.push_back(*curr);
                curr++;
                break;
            }
            if (curr == end)
            {
                log(cld::Errors::Parser::EXPECTED_N.args(*(curr - 1), *this, cld::Lexer::TokenType::Identifier,
                                                         *(curr - 1)));
                return {};
            }
            if (curr->getTokenType() != cld::Lexer::TokenType::Identifier)
            {
                log(cld::Errors::Parser::EXPECTED_N_INSTEAD_OF_N.args(*curr, *this, cld::Lexer::TokenType::Identifier,
                                                                      *curr));
                return {};
            }
            auto string = curr->getValue();
            auto result =
                std::find_if(argumentList.begin(), argumentList.end(),
                             [string](const cld::Lexer::PPToken& token) { return string == token.getValue(); });
            if (result == argumentList.end())
            {
                argumentList.push_back(*(curr++));
                continue;
            }
            log(cld::Errors::PP::REDEFINITION_OF_MACRO_PARAMETER_N.args(*curr, *this, *curr));
            log(cld::Notes::PREVIOUSLY_DECLARED_HERE.args(*result, *this, *result));
            curr++;
        }
        if (curr == end)
        {
            log(cld::Errors::Parser::EXPECTED_N.args(*(curr - 1), *this, cld::Lexer::TokenType::CloseParentheses,
                                                     *(curr - 1)));
            log(cld::Notes::TO_MATCH_N_HERE.args(*openP, *this, *openP));
            return {};
        }
        if (curr->getTokenType() != cld::Lexer::TokenType::CloseParentheses)
        {
            log(cld::Errors::Parser::EXPECTED_N_INSTEAD_OF_N.args(*curr, *this, cld::Lexer::TokenType::CloseParentheses,
                                                                  *curr));
            log(cld::Notes::TO_MATCH_N_HERE.args(*openP, *this, *openP));
            return {};
        }
        curr++;
        return Macro{identifierPos, std::move(argumentList), ellipse, {curr, end}};
    }

    std::optional<bool> evaluateExpression(cld::Lexer::PPTokenIterator ifToken,
                                           llvm::ArrayRef<cld::Lexer::PPToken> tokens)
    {
        std::vector<cld::Lexer::PPToken> result;
        macroSubstitute(
            tokens,
            [&result](auto&& tokens) {
                result.insert(result.end(), std::move_iterator(tokens.begin()), std::move_iterator(tokens.end()));
            },
            true);
        bool errorsOccurred = false;
        auto ctokens =
            cld::Lexer::toCTokens(result.data(), result.data() + result.size(), *this, m_reporter, &errorsOccurred);
        if (errorsOccurred)
        {
            m_errorsOccurred = true;
            return {};
        }
        for (auto iter = ctokens.begin(); iter != ctokens.end(); iter++)
        {
            if (iter->getTokenType() == cld::Lexer::TokenType::Literal)
            {
                switch (iter->getType())
                {
                    case cld::Lexer::CToken::Type::Int:
                    case cld::Lexer::CToken::Type::Long:
                    case cld::Lexer::CToken::Type::LongLong: iter->setType(cld::Lexer::CToken::Type::LongLong); break;
                    case cld::Lexer::CToken::Type::UnsignedInt:
                    case cld::Lexer::CToken::Type::UnsignedLong:
                    case cld::Lexer::CToken::Type::UnsignedLongLong:
                        iter->setType(cld::Lexer::CToken::Type::UnsignedLongLong);
                        break;
                    default: break;
                }
                if (!std::holds_alternative<llvm::APSInt>(iter->getValue()))
                {
                    continue;
                }
                auto integer = cld::get<llvm::APSInt>(iter->getValue());
                integer = integer.extOrTrunc(64);
                iter->setValue(integer);
            }
            else if (cld::Lexer::isText(iter->getTokenType()))
            {
                if (iter->getTokenType() == cld::Lexer::TokenType::Identifier && iter->getText() == "defined")
                {
                    // Neither defined nor the identifier following it (with optionally an opening parentheses
                    // in between) should be converted to 0
                    if (iter + 1 == ctokens.end())
                    {
                        continue;
                    }
                    if ((iter + 1)->getTokenType() == cld::Lexer::TokenType::Identifier)
                    {
                        iter++;
                    }
                    else if ((iter + 1)->getTokenType() == cld::Lexer::TokenType::OpenParentheses)
                    {
                        if (iter + 2 != ctokens.end()
                            && (iter + 2)->getTokenType() == cld::Lexer::TokenType::Identifier)
                        {
                            iter += 2;
                        }
                    }
                    continue;
                }
                bool hasLeadingWhitespace = iter->hasLeadingWhitespace();
                *iter = cld::Lexer::CToken(cld::Lexer::TokenType::Literal, iter->getOffset(), iter->getLength(),
                                           iter->getFileId(), iter->getMacroId(), llvm::APSInt(64, false),
                                           cld::Lexer::CToken::Type::LongLong);
                iter->setLeadingWhitespace(hasLeadingWhitespace);
            }
        }
        if (ctokens.empty())
        {
            if (ifToken->getValue() == "if")
            {
                CLD_ASSERT(!tokens.empty());
                log(cld::Errors::PP::EXPECTED_AN_EXPRESSION_AFTER_IF.args(tokens, *this, tokens));
            }
            else
            {
                if (tokens.empty())
                {
                    log(cld::Errors::PP::EXPECTED_AN_EXPRESSION_AFTER_ELIF.args(*ifToken, *this, *ifToken));
                }
                else
                {
                    log(cld::Errors::PP::EXPECTED_AN_EXPRESSION_AFTER_ELIF_2.args(tokens, *this, tokens));
                }
            }
            return {};
        }
        const auto* begin = std::as_const(ctokens).data();
        auto context = cld::Parser::Context(*this, m_reporter, true);
        auto tree = cld::Parser::parseConditionalExpression(begin, ctokens.data() + ctokens.size(), context);
        if (context.getCurrentErrorCount() != 0 || !tree)
        {
            m_errorsOccurred = true;
            return {};
        }
        cld::Semantics::SemanticAnalysis analysis(*this, m_reporter, &errorsOccurred, [this](std::string_view macro) {
            return macro == "__FILE__" || macro == "__LINE__" || m_defines.count(macro) != 0;
        });
        auto exp = analysis.visit(*tree);
        if (errorsOccurred)
        {
            m_errorsOccurred = true;
            return {};
        }
        auto value = analysis.evaluateConstantExpression(*exp);
        if (!value)
        {
            for (auto& iter : value.error())
            {
                log(iter);
            }
            return {};
        }
        return static_cast<bool>(*value);
    }

    std::uint64_t getLineNumber(std::uint32_t fileID, std::uint64_t offset) const noexcept override
    {
        CLD_ASSERT(fileID < m_files.size());
        auto result = std::lower_bound(m_files[fileID].starts.begin(), m_files[fileID].starts.end(), offset);
        return std::distance(m_files[fileID].starts.begin(), result) + (*result == offset ? 1 : 0);
    }

    std::uint64_t getLineStartOffset(std::uint32_t fileID, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[fileID].starts.size());
        return m_files[fileID].starts[line - 1];
    }

    [[nodiscard]] std::uint64_t getLineEndOffset(std::uint32_t fileID, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(fileID < m_files.size());
        CLD_ASSERT(line < m_files[fileID].starts.size());
        return m_files[fileID].starts[line] - 1;
    }

    llvm::ArrayRef<cld::Source::File> getFiles() const noexcept override
    {
        return m_files;
    }

    llvm::ArrayRef<cld::Source::PPRecord> getSubstitutions() const noexcept override
    {
        return m_substitutions;
    }

    const cld::LanguageOptions& getLanguageOptions() const noexcept override
    {
        return m_languageOptions;
    }

    llvm::ArrayRef<cld::Lexer::IntervalMap> getIntervalMaps() const noexcept override
    {
        return m_intervalMaps;
    }

public:
    Preprocessor(llvm::raw_ostream* report, const cld::PP::Options& ppOptions,
                 const cld::LanguageOptions& languageOptions) noexcept
        : m_reporter(report), m_languageOptions(languageOptions), m_ppOptions(ppOptions)
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
        scratchPadSource += "#define __STDC_HOSTED__ ";
        scratchPadSource += (languageOptions.freeStanding ? "0\n" : "1\n");
        scratchPadSource += "#define __STDC_MB_MIGHT_NEQ_WC__ 1\n";
        scratchPadSource += "#define __STDC_VERSION__ 199901L\n";
        for (auto& [name, def] : m_ppOptions.additionalMacros)
        {
            scratchPadSource += "#define " + name + " " + def + "\n";
        }

        bool errorsOccurred = false;
        auto scratchPadPP = cld::Lexer::tokenize(std::move(scratchPadSource), &languageOptions, report, &errorsOccurred,
                                                 "<Scratch Pad>");
        if (!errorsOccurred)
        {
            include(std::move(scratchPadPP));
        }
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

    llvm::ArrayRef<cld::Source::PPRecord> getSubstitutions() noexcept
    {
        return m_substitutions;
    }

    std::vector<cld::Source::File>& getFiles() noexcept
    {
        return m_files;
    }

    std::vector<cld::Lexer::IntervalMap>& getIntervalMaps() noexcept
    {
        return m_intervalMaps;
    }

    static std::optional<std::vector<const cld::PP::IfSection*>> isOnlyIfSections(const cld::PP::File& file)
    {
        // TODO: Maybe support includes if the includes themselves are one big if section as well?
        std::vector<const cld::PP::IfSection*> result;
        if (file.groups.size() != 1)
        {
            return std::nullopt;
        }
        for (auto& iter : file.groups[0].groupPart)
        {
            if (std::holds_alternative<cld::PP::NonDirective>(iter))
            {
                continue;
            }
            if (std::holds_alternative<cld::PP::IfSection>(iter))
            {
                result.push_back(&cld::get<cld::PP::IfSection>(iter));
                continue;
            }
            if (!std::holds_alternative<cld::PP::TextBlock>(iter))
            {
                return std::nullopt;
            }
            if (std::any_of(cld::get<cld::PP::TextBlock>(iter).tokens.begin(),
                            cld::get<cld::PP::TextBlock>(iter).tokens.end(), [](const cld::Lexer::PPToken& ppToken) {
                                return ppToken.getTokenType() != cld::Lexer::TokenType::Newline;
                            }))
            {
                return std::nullopt;
            }
        }
        return result;
    }

    void include(cld::PPSourceObject&& sourceObject,
                 std::optional<std::pair<std::uint32_t, std::uint64_t>> includePos = {})
    {
        auto scope = cld::ScopeExit([prev = m_currentFile, this] { m_currentFile = prev; });
        CLD_ASSERT(sourceObject.getFiles().size() == 1);
        auto path = cld::fs::u8path(sourceObject.getFiles()[0].path);
        for (auto& iter2 : sourceObject.data())
        {
            iter2.setFileId(m_files.size());
        }
        m_files.push_back({std::move(sourceObject.getFiles()[0].path),
                           std::move(sourceObject.getFiles()[0].source),
                           std::move(sourceObject.getFiles()[0].starts),
                           sourceObject.getFiles()[0].systemHeader,
                           includePos,
                           {}});
        m_intervalMaps.push_back(std::move(sourceObject.getIntervalMap()[0]));
        m_currentFile = m_files.size() - 1;
        cld::PP::Context context(*this, m_reporter);
        const auto* begin = std::as_const(sourceObject).data().data();
        auto tree = parseFile(
            begin, std::as_const(sourceObject).data().data() + std::as_const(sourceObject).data().size(), context);
        if (context.getErrorCount() != 0)
        {
            m_errorsOccurred = true;
            return;
        }
        if (auto ifSections = isOnlyIfSections(tree))
        {
            if (cld::fs::exists(path) && m_includeGuardOptCache.count(path.u8string()) == 0)
            {
                bool allSimpleIfs = true;
                std::vector<std::pair<std::string_view, bool>> neededMacros;
                for (auto& iter : *ifSections)
                {
                    if (iter->elifGroups.empty() && !iter->optionalElseGroup
                        && (std::holds_alternative<cld::PP::IfGroup::IfDefTag>(iter->ifGroup.ifs)
                            || std::holds_alternative<cld::PP::IfGroup::IfnDefTag>(iter->ifGroup.ifs)))
                    {
                        neededMacros.emplace_back(
                            cld::match(
                                iter->ifGroup.ifs,
                                [](const llvm::ArrayRef<cld::Lexer::PPToken>&) -> std::string_view { CLD_UNREACHABLE; },
                                [](const auto& value) { return value.identifier; }),
                            std::holds_alternative<cld::PP::IfGroup::IfDefTag>(iter->ifGroup.ifs));
                    }
                    else
                    {
                        allSimpleIfs = false;
                        break;
                    }
                }
                if (allSimpleIfs)
                {
                    m_includeGuardOptCache[path.u8string()] = {std::move(neededMacros)};
                }
            }
        }
        m_result.reserve(std::max(m_result.size() + sourceObject.data().size(), m_result.capacity()));
        visit(tree);
        m_fileTokens.push_back(std::move(sourceObject.data()));
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

    void visit(const cld::PP::UnknownDirective& unknownDirective)
    {
        log(cld::Errors::PP::N_IS_AN_INVALID_PREPROCESSOR_DIRECTIVE.args(*unknownDirective.identifier, *this,
                                                                         *unknownDirective.identifier));
    }

    void visit(const cld::PP::TextBlock& text)
    {
        macroSubstitute(text.tokens, cld::bind_front(&Preprocessor::pushLine, this));
    }

    void visit(const cld::PP::IfSection& ifSection)
    {
        auto included = cld::match(
            ifSection.ifGroup.ifs,
            [&](const cld::PP::IfGroup::IfnDefTag& ifnDefTag) -> std::optional<bool> {
                return m_defines.count(ifnDefTag.identifier) == 0;
            },
            [&](const cld::PP::IfGroup::IfDefTag& ifDefTag) -> std::optional<bool> {
                return m_defines.count(ifDefTag.identifier) != 0;
            },
            [&](llvm::ArrayRef<cld::Lexer::PPToken> tokens) -> std::optional<bool> {
                return evaluateExpression(ifSection.ifGroup.ifsToken, tokens);
            });
        if (!included)
        {
            return;
        }
        if (*included)
        {
            if (ifSection.ifGroup.optionalGroup)
            {
                visit(*ifSection.ifGroup.optionalGroup);
            }
            return;
        }
        for (auto& iter : ifSection.elifGroups)
        {
            included = evaluateExpression(iter.elifToken, iter.constantExpression);
            if (!included)
            {
                return;
            }
            if (*included)
            {
                if (iter.optionalGroup)
                {
                    visit(*iter.optionalGroup);
                }
                return;
            }
        }
        if (!ifSection.optionalElseGroup || !ifSection.optionalElseGroup->optionalGroup)
        {
            return;
        }
        visit(*ifSection.optionalElseGroup->optionalGroup);
    }

    void visit(const cld::PP::ControlLine& controlLine)
    {
        cld::match(controlLine.variant, [this](auto&& value) { this->visit(value); });
    }

    void visit(const cld::PP::NonDirective&) {}

    void visit(const cld::PP::ControlLine::IncludeTag& includeTag)
    {
        std::string path;
        bool isQuoted = false;
        if (includeTag.tokens.size() == 1
            && includeTag.tokens[0].getTokenType() == cld::Lexer::TokenType::StringLiteral)
        {
            isQuoted = includeTag.tokens[0].getRepresentation(*this)[0] == '"';
            path = includeTag.tokens[0].getValue();
        }
        else
        {
            std::vector<cld::Lexer::PPToken> result;
            macroSubstitute(includeTag.tokens, [&result](auto&& tokens) {
                result.insert(result.end(), std::move_iterator(tokens.begin()), std::move_iterator(tokens.end()));
            });
            if (result.empty())
            {
                if (includeTag.tokens.empty())
                {
                    log(cld::Errors::PP::EXPECTED_A_FILENAME_AFTER_INCLUDE.args(*includeTag.includeToken, *this,
                                                                                *includeTag.includeToken));
                }
                else
                {
                    log(cld::Errors::PP::EXPECTED_A_FILENAME_AFTER_INCLUDE_2.args(
                        *includeTag.includeToken, *this,
                        std::forward_as_tuple(includeTag.tokens.front(), includeTag.tokens.back())));
                }
                return;
            }
            if (result[0].getTokenType() != cld::Lexer::TokenType::LessThan
                && result[0].getTokenType() != cld::Lexer::TokenType::StringLiteral)
            {
                log(cld::Errors::PP::EXPECTED_A_FILENAME_AFTER_INCLUDE_2.args(
                    *includeTag.includeToken, *this, std::forward_as_tuple(result.front(), result.back())));
                return;
            }
            if (result[0].getTokenType() == cld::Lexer::TokenType::StringLiteral)
            {
                isQuoted = true;
                if (result.size() > 1)
                {
                    log(cld::Errors::PP::EXTRA_TOKENS_AFTER_INCLUDE.args(
                        *includeTag.includeToken, *this, std::forward_as_tuple(result[1], result.back())));
                }
                path = result[0].getValue();
            }
            else
            {
                auto iter = result.begin() + 1;
                for (; iter != result.end() && iter->getTokenType() != cld::Lexer::TokenType::GreaterThan; iter++)
                {
                    if (!path.empty() && iter->hasLeadingWhitespace())
                    {
                        path += ' ';
                    }
                    path += iter->getRepresentation(*this);
                }
                if (iter == result.end())
                {
                    log(cld::Errors::Lexer::UNTERMINATED_INCLUDE_DIRECTIVE.args(
                        *includeTag.includeToken, *this, std::forward_as_tuple(result.front(), result.back())));
                }
                else if (iter + 1 != result.end())
                {
                    log(cld::Errors::PP::EXTRA_TOKENS_AFTER_INCLUDE.args(
                        *includeTag.includeToken, *this, std::forward_as_tuple(result[1], result.back())));
                }
            }
        }

        bool systemHeader = false;
        cld::fs::ifstream result;
        cld::fs::path resultPath;
        std::vector<std::string> candidates;
        if (cld::fs::u8path(path).is_absolute())
        {
            result.open(cld::fs::u8path(path), std::ios_base::in | std::ios_base::binary | std::ios_base::ate);
            resultPath = cld::fs::u8path(path);
        }
        else
        {
            if (isQuoted || includeTag.includeToken->getValue() == "include_next")
            {
                if (includeTag.includeToken->getValue() != "include_next")
                {
                    auto dir = cld::fs::u8path(m_files[m_currentFile].path);
                    dir.remove_filename();
                    if (cld::fs::exists(dir))
                    {
                        candidates.push_back(dir.string());
                    }
                    else
                    {
                        // If we are not in a current file it's probably due to it being stdin or similar.
                        // For those cases add the current working directory to the include candidates
                        candidates.emplace_back(cld::fs::current_path().string());
                    }
                }
                candidates.insert(candidates.end(), m_ppOptions.includeQuoteDirectories.begin(),
                                  m_ppOptions.includeQuoteDirectories.end());
            }
            candidates.insert(candidates.end(), m_ppOptions.includeDirectories.begin(),
                              m_ppOptions.includeDirectories.end());
            candidates.insert(candidates.end(), m_ppOptions.systemDirectories.begin(),
                              m_ppOptions.systemDirectories.end());
            if (includeTag.includeToken->getValue() == "include_next")
            {
                auto dir = cld::fs::u8path(m_files[m_currentFile].path);
                dir.remove_filename();
                auto thisDir = std::find_if(candidates.begin(), candidates.end(),
                                            [&](const std::string& value)
                                            {
                                                std::error_code ec;
                                                return cld::fs::equivalent(cld::fs::u8path(value), dir, ec);
                                            });
                if (thisDir != candidates.end())
                {
                    candidates.erase(candidates.begin(), thisDir + 1);
                }
            }
            for (auto iter = candidates.begin(); iter != candidates.end(); iter++)
            {
                auto filename = cld::fs::u8path(*iter);
                filename /= cld::fs::u8path(path);
                result.open(filename, std::ios_base::in | std::ios_base::binary | std::ios_base::ate);
                if (!result.is_open())
                {
                    continue;
                }
                resultPath = std::move(filename);
                systemHeader = std::any_of(m_ppOptions.systemDirectories.begin(), m_ppOptions.systemDirectories.end(),
                                           [iter](std::string_view path1)
                                           {
                                               std::error_code ec1;
                                               return cld::fs::equivalent(cld::fs::u8path(path1), *iter, ec1);
                                           });
                if (!systemHeader && isQuoted && m_files[m_currentFile].systemHeader)
                {
                    auto dir1 = cld::fs::u8path(m_files[m_currentFile].path);
                    dir1.remove_filename();
                    systemHeader = *iter == dir1.u8string();
                }
                break;
            }
        }
        if (!result.is_open())
        {
            log(cld::Errors::PP::FILE_NOT_FOUND.args(
                includeTag.tokens.front(), *this, path,
                std::forward_as_tuple(includeTag.tokens.front(), includeTag.tokens.back())));
            return;
        }
        resultPath = cld::fs::absolute(resultPath);
        resultPath = resultPath.lexically_normal();
        auto cachedResult = m_includeGuardOptCache.find(resultPath.u8string());
        if (cachedResult != m_includeGuardOptCache.end()
            && std::all_of(cachedResult->second.neededMacroValues.begin(), cachedResult->second.neededMacroValues.end(),
                           [&](auto&& pair) { return m_defines.count(pair.first) != pair.second; }))
        {
            return;
        }

        std::size_t end = result.tellg();
        result.seekg(0);
        std::string text(end, '\0');
        result.read(text.data(), text.size());
        result.close();

        bool errors = false;
        auto newFile =
            cld::Lexer::tokenize(std::move(text), &m_languageOptions, m_reporter, &errors, resultPath.u8string());
        if (errors)
        {
            m_errorsOccurred = true;
            return;
        }
        if (systemHeader)
        {
            for (auto& iter : newFile.getFiles())
            {
                iter.systemHeader = true;
            }
        }
        include(std::move(newFile), std::pair{m_currentFile, includeTag.includeToken->getOffset()});
    }

    void visit(const cld::PP::ControlLine::LineTag& lineTag)
    {
        if (lineTag.tokens.empty())
        {
            log(cld::Errors::PP::EXPECTED_A_NUMBER_AFTER_LINE.args(*lineTag.lineToken, *this, *lineTag.lineToken));
            return;
        }
        std::vector<cld::Lexer::PPToken> result;
        std::uint64_t lineValue;
        if (lineTag.tokens[0].getTokenType() == cld::Lexer::TokenType::PPNumber
            && !(lineTag.tokens.size() != 1
                 && (lineTag.tokens.size() != 2
                     || lineTag.tokens[1].getTokenType() != cld::Lexer::TokenType::StringLiteral
                     || lineTag.tokens[1].getRepresentation(*this).front() != '"')))
        {
            result = lineTag.tokens;
        }
        else
        {
            macroSubstitute(lineTag.tokens, [&result](auto&& tokens) {
                result.insert(result.end(), std::move_iterator(tokens.begin()), std::move_iterator(tokens.end()));
            });
            if (result.empty())
            {
                log(cld::Errors::PP::EXPECTED_A_NUMBER_AFTER_LINE.args(*lineTag.lineToken, *this, *lineTag.lineToken));
                return;
            }
            if (result[0].getTokenType() != cld::Lexer::TokenType::PPNumber)
            {
                log(cld::Errors::PP::EXPECTED_A_NUMBER_AFTER_LINE.args(result[0], *this, result[0]));
                return;
            }
            if (result.size() > 1)
            {
                if (result[1].getTokenType() != cld::Lexer::TokenType::StringLiteral)
                {
                    log(cld::Errors::PP::EXPECTED_END_OF_LINE_OR_STRING_AFTER_NUMBER_IN_LINE.args(result[1], *this,
                                                                                                  result[1]));
                    return;
                }
                if (result[1].getRepresentation(*this).front() != '"')
                {
                    log(cld::Errors::PP::STRING_MUST_BE_NORMAL_IN_LINE_DIRECTIVE.args(result[1], *this, result[1]));
                    return;
                }
            }
            if (result.size() > 2)
            {
                log(cld::Errors::PP::EXTRA_TOKENS_AFTER_LINE.args(result[2], *this,
                                                                  std::forward_as_tuple(result[2], result.back())));
            }
        }
        CLD_ASSERT(result[0].getTokenType() == cld::Lexer::TokenType::PPNumber);
        llvm::APInt input;
        auto errors = llvm::StringRef(result[0].getValue().data(), result[0].getValue().size()).getAsInteger(10, input);
        if (errors)
        {
            log(cld::Errors::PP::NUMBER_MUST_BE_IN_DECIMAL_IN_LINE_DIRECTIVE.args(result[0], *this, result[0]));
            return;
        }
        if (input == 0)
        {
            log(cld::Errors::PP::NUMBER_MUST_NOT_BE_ZERO_IN_LINE_DIRECTIVE.args(result[0], *this, result[0]));
            return;
        }
        if (input.ugt(2147483647))
        {
            log(cld::Errors::PP::NUMBER_MUST_NOT_BE_GREATER_THAN_X_IN_LINE_DIRECTIVE.args(result[0], *this, result[0]));
            return;
        }
        lineValue = input.getZExtValue();
        std::optional<std::string> text;
        if (result.size() == 2)
        {
            CLD_ASSERT(result[1].getTokenType() == cld::Lexer::TokenType::StringLiteral);
            auto ctoken = cld::Lexer::parseStringLiteral(result[1], *this, m_reporter);
            if (!ctoken)
            {
                return;
            }
            text = cld::get<std::string>(std::move(ctoken->getValue()));
        }
        auto thisLine = lineTag.lineToken->getLine(*this);
        m_files[m_currentFile].lineAndFileMapping.emplace_back(thisLine + 1, std::move(text), lineValue);
    }

    void visit(const cld::PP::ControlLine::ErrorTag& errorTag)
    {
        log(cld::Errors::PP::ERROR_ENCOUNTERED.args(*errorTag.errorToken, *this, errorTag.tokens));
    }

    void visit(const cld::PP::ControlLine::PragmaTag&) {}

    void visit(cld::Lexer::PPTokenIterator undef)
    {
        auto name = undef->getValue();
        if (std::any_of(PREDEFINED_MACRO_NAMES.begin(), PREDEFINED_MACRO_NAMES.end(),
                        cld::bind_front(std::equal_to{}, name)))
        {
            log(cld::Errors::PP::UNDEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED.args(*undef, *this, *undef));
            return;
        }
        m_defines.erase(name);
    }

    void visit(const cld::PP::DefineDirective& defineDirective)
    {
        auto macro = parseDefineDirective(defineDirective);
        if (!macro)
        {
            return;
        }
        bool errors = false;
        if (!macro->hasEllipse)
        {
            const auto* iter = std::find_if(macro->replacement.begin(), macro->replacement.end(),
                                            [](const cld::Lexer::PPToken& token) {
                                                if (token.getTokenType() != cld::Lexer::TokenType::Identifier)
                                                {
                                                    return false;
                                                }
                                                return token.getValue() == "__VA_ARGS__";
                                            });
            if (iter != macro->replacement.end())
            {
                log(cld::Errors::PP::VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST.args(*iter, *this, *iter));
                errors = true;
            }
        }
        if (macro->argumentList)
        {
            for (const auto* iter = macro->replacement.begin(); iter != macro->replacement.end(); iter++)
            {
                if (iter->getTokenType() == cld::Lexer::TokenType::Pound)
                {
                    iter++;
                    if (iter == macro->replacement.end() || iter->getTokenType() != cld::Lexer::TokenType::Identifier
                        || (std::none_of(macro->argumentList->begin(),
                                         macro->argumentList->end() - (macro->hasEllipse ? 1 : 0),
                                         [&iter](const cld::Lexer::PPToken& token) {
                                             return token.getValue() == iter->getValue();
                                         })
                            && (!macro->hasEllipse || iter->getValue() != "__VA_ARGS__")))
                    {
                        log(cld::Errors::PP::EXPECTED_AN_ARGUMENT_AFTER_POUND.args(*iter, *this, *(iter - 1), *iter));
                        errors = true;
                    }
                }
            }
        }
        if (!macro->replacement.empty())
        {
            if (macro->replacement.front().getTokenType() == cld::Lexer::TokenType::DoublePound)
            {
                log(cld::Errors::PP::OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_BEGINNING_OF_REPLACEMENT_LIST.args(
                    macro->replacement.front(), *this, macro->replacement.front()));
                errors = true;
            }
            if (macro->replacement.size() > 1
                && macro->replacement.back().getTokenType() == cld::Lexer::TokenType::DoublePound)
            {
                log(cld::Errors::PP::OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_END_OF_REPLACEMENT_LIST.args(
                    macro->replacement.back(), *this, macro->replacement.back()));
                errors = true;
            }
        }
        auto name = macro->identifierPos->getValue();
        if (name == "defined")
        {
            log(cld::Errors::PP::DEFINED_CANNOT_BE_USED_AS_MACRO_NAME.args(*macro->identifierPos, *this,
                                                                           *macro->identifierPos));
            errors = true;
        }
        if (!m_visitingScratchPad
            && std::any_of(PREDEFINED_MACRO_NAMES.begin(), PREDEFINED_MACRO_NAMES.end(),
                           cld::bind_front(std::equal_to{}, name)))
        {
            log(cld::Errors::PP::DEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED.args(*macro->identifierPos, *this,
                                                                              *macro->identifierPos));
            errors = true;
        }
        if (errors)
        {
            return;
        }
        auto [result, notADuplicate] = m_defines.insert({macro->identifierPos->getValue(),
                                                         {macro->identifierPos,
                                                          macro->argumentList,
                                                          macro->hasEllipse,
                                                          {macro->replacement.begin(), macro->replacement.end()}}});
        if (notADuplicate)
        {
            return;
        }
        if (equal(*macro, result->second))
        {
            if (log(cld::Warnings::PP::N_REDEFINED.args(*macro->identifierPos, *this, *macro->identifierPos)))
            {
                log(cld::Notes::PREVIOUSLY_DECLARED_HERE.args(*result->second.identifierPos, *this,
                                                              *result->second.identifierPos));
            }
            return;
        }
        log(cld::Errors::PP::REDEFINITION_OF_MACRO_N.args(*macro->identifierPos, *this, *macro->identifierPos));
        log(cld::Notes::PREVIOUSLY_DECLARED_HERE.args(*result->second.identifierPos, *this,
                                                      *result->second.identifierPos));
    }

    bool errorsOccurred() const
    {
        return m_errorsOccurred;
    }
};
} // namespace

cld::PPSourceObject cld::PP::preprocess(cld::PPSourceObject&& sourceObject, const Options& options,
                                        llvm::raw_ostream* reporter, bool* errorsOccurred) noexcept
{
    auto& languageOptions = sourceObject.getLanguageOptions();
    Preprocessor preprocessor(reporter, options, languageOptions);
    preprocessor.include(std::move(sourceObject));
    if (errorsOccurred)
    {
        *errorsOccurred = preprocessor.errorsOccurred();
    }
    return PPSourceObject(std::move(preprocessor.getResult()), std::move(preprocessor.getFiles()), &languageOptions,
                          std::move(preprocessor.getSubstitutions()), {std::move(preprocessor.getIntervalMaps())});
}
