#include "Preprocessor.hpp"

#include <llvm/ADT/ScopeExit.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>

#include <Frontend/Compiler/ErrorMessages.hpp>
#include <Frontend/Common/Text.hpp>

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
    cld::Source::PPRecord m_substitutions{1};
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
        m_result.insert(m_result.end(), cld::Lexer::PPToken(cld::Lexer::TokenType::Newline, 0, 0, 0, 0, 0));
    }

    void log(std::vector<cld::Message> messages)
    {
        for (auto& iter : messages)
        {
            if (iter.getSeverity() == cld::Severity::Error)
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
            return std::hash<std::uint64_t>()(ptr->getOffset()) ^ (std::hash<std::uint32_t>()(ptr->getFileId()) << 1);
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
            for (auto tokenIter = copy.begin(); tokenIter != copy.end(); tokenIter++)
            {
                if (tokenIter->getTokenType() != cld::Lexer::TokenType::Newline)
                {
                    continue;
                }
                if (tokenIter + 1 != copy.end())
                {
                    (tokenIter + 1)->setLeadingWhitespace(true);
                }
                tokenIter = copy.erase(tokenIter);
            }
            auto i = ++m_macroID;
            m_disabledMacros.push_back({});
            m_substitutions.push_back({});
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
                auto scratchPadPP = cld::Lexer::tokenize(text, m_options, m_report, &m_errorsOccured, "<Strings>");
                CLD_ASSERT(!m_errorsOccured);
                CLD_ASSERT(scratchPadPP.getFiles().size() == 1);
                CLD_ASSERT(scratchPadPP.getFiles()[0].ppTokens.size() == 2);
                CLD_ASSERT(scratchPadPP.getFiles()[0].ppTokens[1].getTokenType() == cld::Lexer::TokenType::Newline);
                auto file = scratchPadPP.getFiles()[0];
                file.ppTokens[0].setFileId(m_files.size());
                file.ppTokens[0].setMacroId(i);
                result.insert(result.end(), file.ppTokens[0]);
                m_files.push_back(std::move(file));
                m_substitutions.push_back(cld::Source::Stringification{std::move(copy), *iter});
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
//TODO:                log({cld::Message::error(cld::Errors::Parser::EXPECTED_N.args(cld::to_string(count + 1) + " ')'"),
//                                         cld::Message::after, closeParenthesesOrComma - 1,
//                                         {cld::InsertAfter(closeParenthesesOrComma - 1, ")")}),
//                     cld::Message::note(cld::Notes::TO_MATCH_N_HERE.args("'('"), openParentheses,
//                                        {cld::PointAt(openParentheses)})});
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
//TODO:            auto& format = macro.hasEllipse ?
//                               cld::Errors::PP::NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_AT_LEAST_N_GOT_N :
//                               cld::Errors::PP::NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N;
//            log({cld::Message::error(
//                     format.args('"' + cld::to_string(namePos.getValue()) + '"', identifierCount, arguments.size()),
//                     &namePos, begin + 1, {cld::Underline(&namePos)}),
//                 cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, macro.identifierPos,
//                                    {cld::Underline(macro.identifierPos)})});
            return {};
        }
        else if (arguments.size() > identifierCount && !macro.hasEllipse)
        {
            auto firstRedundant = arguments.begin() + identifierCount;
//TODO:            std::vector<cld::Modifier> modifiers = {cld::Underline(&namePos)};
//            modifiers.reserve(1 + firstRedundant->size());
//            std::transform(
//                firstRedundant, arguments.end(), std::back_inserter(modifiers),
//                [](llvm::ArrayRef<cld::Lexer::PPToken> ref) { return cld::PointAt(ref.begin(), ref.end()); });
//            log({cld::Message::error(
//                     cld::Errors::PP::TOO_MANY_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N.args(
//                         '"' + cld::to_string(namePos.getValue()) + '"', identifierCount, arguments.size()),
//                     &namePos, begin + 1, std::move(modifiers)),
//                 cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, macro.identifierPos,
//                                    {cld::Underline(macro.identifierPos)})});
            return {};
        }

        if (macro.hasEllipse)
        {
            arguments.emplace_back(varargStart, begin);
        }
        return arguments;
    }

    void evaluateConcats(std::uint64_t parentID, std::vector<cld::Lexer::PPToken>& tokens,
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
            auto scratchPadPP = cld::Lexer::tokenize(text, m_options, &llvm::nulls(), &errors, "<Pastings>");
            CLD_ASSERT(scratchPadPP.getFiles().size() == 1);
            if (errors || scratchPadPP.getFiles()[0].ppTokens.size() != 2)
            {
//TODO:                log({cld::Message::warning(cld::Warnings::PP::TOKEN_CONCATENATION_RESULTING_IN_AN_INVALID_TOKEN_IS_UB,
//                                           &lhs, &rhs + 1,
//                                           {cld::Underline(&lhs), cld::Underline(&rhs), cld::PointAt(&*iter)}),
//                     cld::Message::note(cld::Notes::PP::WHEN_CONCATENATING_N_AND_N.args(
//                                            cld::Lexer::normalizeSpelling(lhs.getRepresentation(*this)),
//                                            cld::Lexer::normalizeSpelling(rhs.getRepresentation(*this))),
//                                        &lhs, &rhs + 1, {cld::Underline(&lhs), cld::Underline(&rhs)})});
                iter = tokens.erase(iter - 1, iter + 2);
                if (iter == tokens.end())
                {
                    break;
                }
                continue;
            }
            CLD_ASSERT(scratchPadPP.getFiles()[0].ppTokens.size() == 2);
            CLD_ASSERT(scratchPadPP.getFiles()[0].ppTokens[1].getTokenType() == cld::Lexer::TokenType::Newline);
            auto i = ++m_macroID;
            m_disabledMacros.push_back({});
            m_disabledMacros[i].insert(m_disabledMacros[parentID].begin(), m_disabledMacros[parentID].end());
            m_substitutions.push_back(cld::Source::TokenConcatenation{lhs, rhs});
            auto file = scratchPadPP.getFiles()[0];
            file.ppTokens[0].setFileId(m_files.size());
            file.ppTokens[0].setMacroId(i);
            iter = tokens.erase(iter - 1, iter + 1);
            *iter = file.ppTokens[0];
            iter->setLeadingWhitespace(leadingWhitespace);
            m_files.push_back(std::move(file));
        }
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
        std::string_view file = m_files[tokens.front().getFileId()].path;
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
                || (iter->getMacroId() < m_disabledMacros.size()
                    && m_disabledMacros[iter->getMacroId()].count(cld::to_string(name)) > 0))
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
            m_disabledMacros.push_back({cld::to_string(name)});
            m_disabledMacros[i].insert(m_disabledMacros[iter->getMacroId()].begin(),
                                       m_disabledMacros[iter->getMacroId()].end());
            m_substitutions.push_back({});
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

    std::uint64_t getLineEndOffset(std::uint32_t fileID, std::uint64_t line) const noexcept override
    {
        CLD_ASSERT(fileID < m_files.size());
        CLD_ASSERT(line - 1 < m_files[fileID].starts.size());
        return line == m_files[fileID].starts.size() ?
                   m_files[fileID].ppTokens.back().getOffset() + m_files[fileID].ppTokens.back().getLength() :
                   m_files[fileID].starts[line];
    }

    const std::vector<cld::Source::File>& getFiles() const noexcept override
    {
        return m_files;
    }

    const cld::Source::PPRecord& getSubstitutions() const noexcept override
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

    cld::Source::PPRecord& getSubstitutions() noexcept
    {
        return m_substitutions;
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
                iter2.setFileId(m_files.size());
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

    void visit(const cld::PP::ControlLine::IncludeTag& includeTag)
    {
        std::string path;
        bool isQuoted = false;
        if (includeTag.tokens.size() != 1
            || includeTag.tokens[0].getTokenType() != cld::Lexer::TokenType::StringLiteral)
        {
            std::vector<cld::Lexer::PPToken> result;
            macroSubstitute(includeTag.tokens, [&result](auto&& tokens) {
                result.insert(result.end(), std::move_iterator(tokens.begin()), std::move_iterator(tokens.end()));
            });
            if (result.empty())
            {
                if (includeTag.tokens.empty())
                {
//TODO:                    log({cld::Message::error(cld::Errors::PP::EXPECTED_A_FILENAME_AFTER_INCLUDE, cld::Message::after,
//                                             includeTag.includeToken, {cld::InsertAfter(includeTag.includeToken)})});
                }
                else
                {
//TODO:                    log({cld::Message::error(cld::Errors::PP::EXPECTED_A_FILENAME_AFTER_INCLUDE,
//                                             includeTag.includeToken, includeTag.tokens.end(),
//                                             {cld::Underline(includeTag.tokens.begin(), includeTag.tokens.end())})});
                }
                return;
            }
            if (result[0].getTokenType() != cld::Lexer::TokenType::LessThan
                && result[0].getTokenType() != cld::Lexer::TokenType::StringLiteral)
            {
//TODO:                log({cld::Message::error(cld::Errors::PP::EXPECTED_A_FILENAME_AFTER_INCLUDE, includeTag.includeToken,
//                                         &result.back() + 1, {cld::Underline(&result.front(), &result.back() + 1)})});
                return;
            }
            if (result[0].getTokenType() == cld::Lexer::TokenType::StringLiteral)
            {
                isQuoted = true;
                if (result.size() != 0)
                {
//TODO:                    log({cld::Message::error(cld::Errors::PP::EXTRA_TOKENS_AFTER_INCLUDE, includeTag.includeToken,
//                                             &result.back() + 1,
//                                             {cld::Underline(&result.front() + 1, &result.back() + 1)})});
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
//TODO:                    log({cld::Message::error(
//                        cld::Errors::Lexer::UNTERMINATED_N.args(cld::Errors::Lexer::INCLUDE_DIRECTIVE),
//                        includeTag.includeToken, &result.back() + 1,
//                        {cld::Underline(&result.front(), &result.back() + 1)})});
                }
                else if (iter + 1 != result.end())
                {
//TODO:                    log({cld::Message::error(cld::Errors::PP::EXTRA_TOKENS_AFTER_INCLUDE, includeTag.includeToken,
//                                             &result.back() + 1, {cld::Underline(&*iter + 1, &result.back() + 1)})});
                }
            }
        }
        else
        {
            isQuoted = includeTag.tokens[0].getRepresentation(*this)[0] == '"';
            path = includeTag.tokens[0].getValue();
        }
        std::vector<std::string> candidates;
        if (llvm::sys::path::is_absolute(path))
        {
            candidates.push_back("");
        }
        llvm::SmallString<50> dir(m_files[m_currentFile].path);
        llvm::sys::path::remove_filename(dir);
        if (llvm::sys::fs::exists(dir))
        {
            candidates.push_back(std::string(dir.begin(), dir.end()));
        }
        if (isQuoted)
        {
            candidates.insert(candidates.end(), m_options.includeQuoteDirectories.begin(),
                              m_options.includeQuoteDirectories.end());
        }
        candidates.insert(candidates.end(), m_options.includeDirectories.begin(), m_options.includeDirectories.end());
        for (const auto& candidate : candidates)
        {
            llvm::SmallString<50> filename;
            if (candidate.empty())
            {
                filename = path;
            }
            else if (candidate.back() == '/'
#if _WIN32
                     || candidate.back() == '\\'
#endif
            )
            {
                filename = candidate + path;
            }
            else
            {
                filename = candidate + '/' + path;
            }
            if (!llvm::sys::fs::exists(filename))
            {
                continue;
            }
            std::uint64_t size;
            auto error = llvm::sys::fs::file_size(filename, size);
            if (!error)
            {
                auto handle = llvm::sys::fs::openNativeFileForRead(filename);
                if (handle)
                {
                    auto scopeExit = llvm::make_scope_exit([&] { llvm::sys::fs::closeFile(*handle); });
                    llvm::sys::fs::mapped_file_region mapping(*handle, llvm::sys::fs::mapped_file_region::readonly,
                                                              size, 0, error);
                    if (!error)
                    {
                        std::string_view view(mapping.const_data(), mapping.size());
                        llvm::sys::path::remove_dots(filename, true);
                        auto newFile = cld::Lexer::tokenize(view, m_options, m_report, &m_errorsOccured,
                                                            {filename.data(), filename.size()});
                        if (m_errorsOccured)
                        {
                            return;
                        }
                        llvm::sys::fs::closeFile(*handle);
                        scopeExit.release();
                        include(std::move(newFile));
                        return;
                    }
                }
            }
            // TODO: Error about failure to open
            break;
        }
        // TODO: Error about failure to find
    }

    void visit(const cld::PP::ControlLine::LineTag& lineTag) {}

    void visit(const cld::PP::ControlLine::ErrorTag& errorTag) {}

    void visit(const cld::PP::ControlLine::PragmaTag& pragmaTag) {}

    void visit(cld::Lexer::PPTokenIterator undef)
    {
        auto name = undef->getValue();
        if (std::any_of(PREDEFINED_MACRO_NAMES.begin(), PREDEFINED_MACRO_NAMES.end(),
                        [name](std::string_view value) { return value == name; }))
        {
//TODO:            log({cld::Message::error(
//                cld::Errors::PP::UNDEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED.args('\'' + cld::to_string(name) + '\''),
//                undef, {cld::Underline(undef)})});
            return;
        }
        m_defines.erase(name);
    }

    void visit(const cld::PP::DefineDirective& defineDirective)
    {
        bool errors = false;
        if (!defineDirective.hasEllipse)
        {
            const auto* iter = std::find_if(defineDirective.replacement.begin(), defineDirective.replacement.end(),
                                            [](const cld::Lexer::PPToken& token) {
                                                if (token.getTokenType() != cld::Lexer::TokenType::Identifier)
                                                {
                                                    return false;
                                                }
                                                return token.getValue() == "__VA_ARGS__";
                                            });
            if (iter != defineDirective.replacement.end())
            {
//TODO:                log({cld::Message::error(cld::Errors::PP::VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST, iter,
//                                         {cld::Underline(iter)})});
                errors = true;
            }
        }
        if (defineDirective.argumentList)
        {
            for (const auto* iter = defineDirective.replacement.begin(); iter != defineDirective.replacement.end();
                 iter++)
            {
                if (iter->getTokenType() == cld::Lexer::TokenType::Pound)
                {
                    iter++;
                    if (iter == defineDirective.replacement.end()
                        || iter->getTokenType() != cld::Lexer::TokenType::Identifier
                        || (std::none_of(defineDirective.argumentList->begin(),
                                         defineDirective.argumentList->end() - (defineDirective.hasEllipse ? 1 : 0),
                                         [&iter](const cld::Lexer::PPToken& token) {
                                             return token.getValue() == iter->getValue();
                                         })
                            && (!defineDirective.hasEllipse || iter->getValue() != "__VA_ARGS__")))
                    {
//TODO:                        log({cld::Message::error(cld::Errors::PP::EXPECTED_AN_ARGUMENT_AFTER_POUND, iter,
//                                                 {cld::PointAt(iter - 1), cld::Underline(iter)})});
                        errors = true;
                    }
                }
            }
        }
        if (!defineDirective.replacement.empty())
        {
            if (defineDirective.replacement.front().getTokenType() == cld::Lexer::TokenType::DoublePound)
            {
//TODO:                log({cld::Message::error(
//                    cld::Errors::PP::OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_BEGINNING_OF_REPLACEMENT_LIST,
//                    &defineDirective.replacement.front(), {cld::Underline(&defineDirective.replacement.front())})});
                errors = true;
            }
            if (defineDirective.replacement.size() > 1
                && defineDirective.replacement.back().getTokenType() == cld::Lexer::TokenType::DoublePound)
            {
//TODO:                log({cld::Message::error(cld::Errors::PP::OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_END_OF_REPLACEMENT_LIST,
//                                         &defineDirective.replacement.back(),
//                                         {cld::Underline(&defineDirective.replacement.back())})});
                errors = true;
            }
        }
        auto name = defineDirective.identifierPos->getValue();
        if (name == "defined")
        {
//TODO:            log({cld::Message::error(cld::Errors::PP::DEFINED_CANNOT_BE_USED_AS_MACRO_NAME,
//                                     defineDirective.identifierPos, {cld::Underline(defineDirective.identifierPos)})});
            errors = true;
        }
        if (!m_visitingScratchPad
            && std::any_of(PREDEFINED_MACRO_NAMES.begin(), PREDEFINED_MACRO_NAMES.end(),
                           [name](std::string_view value) { return value == name; }))
        {
//TODO:            log({cld::Message::error(
//                cld::Errors::PP::DEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED.args('\'' + cld::to_string(name) + '\''),
//                defineDirective.identifierPos, {cld::Underline(defineDirective.identifierPos)})});
            errors = true;
        }
        if (errors)
        {
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
//TODO:            log({cld::Message::warning(cld::Warnings::PP::N_REDEFINED.args('\'' + cld::to_string(name) + '\''),
//                                       defineDirective.identifierPos, {cld::Underline(defineDirective.identifierPos)}),
//                 cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, result->second.identifierPos,
//                                    {cld::Underline(result->second.identifierPos)})});
            return;
        }
//TODO:        log({cld::Message::error(cld::Errors::PP::REDEFINITION_OF_MACRO_N.args('\'' + cld::to_string(name) + '\''),
//                                 defineDirective.identifierPos, {cld::Underline(defineDirective.identifierPos)}),
//             cld::Message::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, result->second.identifierPos,
//                                {cld::Underline(result->second.identifierPos)})});
    }
};
} // namespace

cld::PPSourceObject cld::PP::preprocess(cld::PPSourceObject&& sourceObject, llvm::raw_ostream* reporter) noexcept
{
    auto options = sourceObject.getLanguageOptions();
    Preprocessor preprocessor(reporter, options);
    preprocessor.include(std::move(sourceObject));
    return PPSourceObject(std::move(preprocessor.getResult()), std::move(preprocessor.getFiles()), options,
                          std::move(preprocessor.getSubstitutions()));
}
