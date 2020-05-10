#include "Preprocessor.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/SourceObject.hpp>

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

class Preprocessor final
{
    llvm::raw_ostream* m_report;
    const cld::PPSourceObject& m_sourceObject;
    const cld::LanguageOptions& m_options;
    std::uint64_t m_currentOffset = 0;
    std::uint64_t m_macroID = 0;
    std::vector<cld::Lexer::PPToken> m_result;
    std::vector<cld::Source::Substitution> m_substitutions;
    std::vector<std::uint64_t> m_ppStarts{0};
    struct Macro
    {
        std::optional<cld::Lexer::PPTokenIterator> identifierPos;
        std::optional<std::vector<std::string>> identifierList;
        bool hasEllipse;
        std::vector<cld::Lexer::PPToken> replacement;
    };
    std::unordered_map<std::string_view, Macro> m_defines;
    std::vector<std::unordered_set<std::string_view>> m_disabledMacros;
    bool m_errorsOccured = false;

    void pushNewline()
    {
        m_ppStarts.push_back(++m_currentOffset);
    }

    void pushTokens(llvm::ArrayRef<cld::Lexer::PPToken> tokens, std::uint64_t id = 0)
    {
        if (tokens.empty())
        {
            return;
        }
        m_result.reserve(m_result.size() + tokens.size());
        auto& front = tokens.front();
        auto base = front.getCharSpaceOffset();
        std::transform(tokens.begin(), tokens.end(), std::back_inserter(m_result),
                       [this, base, id](cld::Lexer::PPToken token) {
                           token.setPPOffset(m_currentOffset + token.getCharSpaceOffset() - base);
                           token.setMacroId(id);
                           return token;
                       });
        m_currentOffset = m_result.back().getPPOffset() + m_result.back().getCharSpaceLength();
    }

    void log(std::vector<cld::PPMessage> messages)
    {
        for (auto& iter : messages)
        {
            if (iter.getSeverity() == cld::PPMessage::Error)
            {
                m_errorsOccured = true;
            }
            if (m_report)
            {
                iter.print(*m_report, m_sourceObject);
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

    void macroSubstitute(llvm::ArrayRef<cld::Lexer::PPToken> tokens, std::uint64_t id = 0)
    {
        auto start = tokens.begin();
        for (auto iter = tokens.begin(); iter != tokens.end(); iter++)
        {
            if (iter->getTokenType() != cld::Lexer::TokenType::Identifier)
            {
                continue;
            }
            auto& name = iter->getValue();
            auto result = m_defines.find(name);
            if (result == m_defines.end() || (id < m_disabledMacros.size() && m_disabledMacros[id].count(name) > 0))
            {
                continue;
            }
            pushTokens({start, iter}, id);
            // Add the right amount of whitespace before the identifier that is about to be replaced
            if (iter != start)
            {
                m_currentOffset += iter->getCharSpaceOffset()
                                   - (m_result.back().getCharSpaceOffset() + m_result.back().getCharSpaceLength());
            }
            if (!result->second.identifierList)
            {
                auto i = ++m_macroID;
                m_disabledMacros.resize(i + 1);
                m_disabledMacros[i].insert(name);
                if (id != 0)
                {
                    m_disabledMacros[i].insert(m_disabledMacros[id].begin(), m_disabledMacros[id].end());
                }
                macroSubstitute(result->second.replacement, i);
                m_substitutions.resize(std::max(m_substitutions.size(), i + 1));
                m_substitutions[i] = cld::Source::Substitution{iter->getOffset(), iter->getLength()};
            }
            else
            {
                CLD_UNREACHABLE;
            }
            start = iter + 1;
            if (start != tokens.end())
            {
                m_currentOffset +=
                    start->getCharSpaceOffset() - (iter->getCharSpaceOffset() + iter->getCharSpaceLength());
            }
        }
        pushTokens({start, tokens.end()}, id);
    }

public:
    Preprocessor(llvm::raw_ostream* report, const cld::PPSourceObject& sourceObject,
                 const cld::LanguageOptions& options) noexcept
        : m_report(report), m_sourceObject(sourceObject), m_options(options)
    {
        //        auto time = std::time(nullptr);
        //        auto tm = std::localtime(&time);
        //        std::string date(30, ' ');
        //        auto size = std::strftime(date.data(), date.size(), "%b %e %Y", tm);
        //        CLD_ASSERT(size > 0);
        //        date.resize(size);
        //        auto token = cld::Lexer::Token::builtinToken(cld::Lexer::TokenType::StringLiteral, date, '"' + date +
        //        '"'); m_defines.insert({"__DATE__", {std::nullopt, std::nullopt, false, {token}}}); date.resize(30, '
        //        '); size = std::strftime(date.data(), date.size(), "%H:%M:%S", tm); CLD_ASSERT(size > 0);
        //        date.resize(size);
        //        token = cld::Lexer::Token::builtinToken(cld::Lexer::TokenType::StringLiteral, date, '"' + date + '"');
        //        m_defines.insert({"__TIME__", {std::nullopt, std::nullopt, false, {token}}});
        //
        //        token = cld::Lexer::Token::builtinToken(cld::Lexer::TokenType::PPNumber, "1", "1");
        //        m_defines.insert({"__STDC__", {std::nullopt, std::nullopt, false, {token}}});
        //        token = cld::Lexer::Token::builtinToken(cld::Lexer::TokenType::PPNumber, "0", "0");
        //        m_defines.insert({"__STDC_HOSTED__", {std::nullopt, std::nullopt, false, {token}}});
        //        token = cld::Lexer::Token::builtinToken(cld::Lexer::TokenType::PPNumber, "1", "1");
        //        m_defines.insert({"__STDC_MB_MIGHT_NEQ_WC__", {std::nullopt, std::nullopt, false, {token}}});
        //        token = cld::Lexer::Token::builtinToken(cld::Lexer::TokenType::PPNumber, "19901L", "19901L");
        //        m_defines.insert({"__STDC_VERSION__", {std::nullopt, std::nullopt, false, {token}}});
        //
        //        m_filesInUse.push_back(options.absoluteFilepath.empty() ? "<stdin>" : options.absoluteFilepath);
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

    void visit(const std::vector<cld::Lexer::PPToken>& text)
    {
        if (!text.empty())
        {
            m_currentOffset +=
                text.front().getOffset()
                - m_sourceObject.getLineStartOffset(text.front().getFileId(), text.front().getLine(m_sourceObject));
        }
        macroSubstitute(text);
        pushNewline();
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
        auto& name = undef->getValue();
        if (std::any_of(PREDEFINED_MACRO_NAMES.begin(), PREDEFINED_MACRO_NAMES.end(),
                        [&name](std::string_view value) { return value == name; }))
        {
            log({cld::PPMessage::error(
                cld::Errors::PP::UNDEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED.args('\'' + name + '\''), undef,
                {cld::Underline(undef)})});
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
            log({cld::PPMessage::error(cld::Errors::PP::VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST, iter,
                                       {cld::Underline(iter)})});
        }
        auto& name = defineDirective.identifierPos->getValue();
        if (name == "defined")
        {
            log({cld::PPMessage::error(cld::Errors::PP::DEFINED_CANNOT_BE_USED_AS_MACRO_NAME,
                                       defineDirective.identifierPos,
                                       {cld::Underline(defineDirective.identifierPos)})});
            return;
        }
        if (std::any_of(PREDEFINED_MACRO_NAMES.begin(), PREDEFINED_MACRO_NAMES.end(),
                        [&name](std::string_view value) { return value == name; }))
        {
            log({cld::PPMessage::error(
                cld::Errors::PP::DEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED.args('\'' + name + '\''),
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
        CLD_ASSERT(result->second.identifierPos);
        if (equal(defineDirective, result->second))
        {
            log({cld::PPMessage::warning(cld::Warnings::PP::N_REDEFINED.args('\'' + name + '\''),
                                         defineDirective.identifierPos,
                                         {cld::Underline(defineDirective.identifierPos)}),
                 cld::PPMessage::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, *result->second.identifierPos,
                                      {cld::Underline(*result->second.identifierPos)})});
            return;
        }
        log({cld::PPMessage::error(cld::Errors::PP::REDEFINITION_OF_MACRO_N.args('\'' + name + '\''),
                                   defineDirective.identifierPos, {cld::Underline(defineDirective.identifierPos)}),
             cld::PPMessage::note(cld::Notes::PREVIOUSLY_DECLARED_HERE, *result->second.identifierPos,
                                  {cld::Underline(*result->second.identifierPos)})});
    }
};
} // namespace

cld::PPSourceObject cld::PP::preprocess(const cld::PPSourceObject& sourceObject, llvm::raw_ostream* reporter) noexcept
{
    auto [tree, ok] = buildTree(sourceObject, reporter);
    if (!ok)
    {
        return cld::PPSourceObject(sourceObject);
    }
    Preprocessor preprocessor(reporter, sourceObject, sourceObject.getLanguageOptions());
    preprocessor.visit(tree);
    //    return PPSourceObject(std::move(preprocessor.getResult()), std::move(preprocessor.getSubstitutions()),
    //                          std::move(preprocessor.getPpStarts()));
}
