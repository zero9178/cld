#include "Parser.hpp"

#include <Frontend/Compiler/ErrorMessages.hpp>
#include <Frontend/Compiler/SourceObject.hpp>

namespace
{
bool expect(cld::Lexer::TokenType tokenType, cld::Lexer::PPTokenIterator& begin, cld::Lexer::PPTokenIterator end,
            cld::PP::Context& context, std::optional<cld::Message> additional = {})
{
    if (begin == end || begin->getTokenType() != tokenType)
    {
        if (begin == end || begin->getTokenType() == cld::Lexer::TokenType::Newline)
        {
            context.log(cld::Errors::Parser::EXPECTED_N.args(*(begin - 1), context.getSourceInterface(), tokenType,
                                                             *(begin - 1)));
        }
        else
        {
            context.log(cld::Errors::Parser::EXPECTED_N_INSTEAD_OF_N.args(*begin, context.getSourceInterface(),
                                                                          tokenType, *begin));
        }
        if (additional)
        {
            context.log(*additional);
        }
        return false;
    }
    begin++;
    return true;
}

cld::Lexer::PPTokenIterator findNewline(cld::Lexer::PPTokenIterator begin, cld::Lexer::PPTokenIterator end)
{
    return std::find_if(begin, end, [](const cld::Lexer::PPToken& token) {
        return token.getTokenType() == cld::Lexer::TokenType::Newline;
    });
}

void skipLine(cld::Lexer::PPTokenIterator& begin, cld::Lexer::PPTokenIterator end)
{
    begin = findNewline(begin, end);
    begin = begin == end ? begin : begin + 1;
}

template <std::size_t N>
void skipUntil(cld::Lexer::PPTokenIterator& begin, cld::Lexer::PPTokenIterator end,
               const std::array<cld::Lexer::TokenType, N>& tokens)
{
    begin = std::find_if(begin, end, [&tokens](const cld::Lexer::PPToken& token) {
        return std::any_of(tokens.begin(), tokens.end(),
                           [&token](auto&& value) { return token.getTokenType() == value; });
    });
}

} // namespace

cld::PP::Context::Context(const cld::SourceInterface& sourceInterface, llvm::raw_ostream* reporter)
    : m_sourceInterface(sourceInterface), m_reporter(reporter)
{
}

void cld::PP::Context::log(const Message& message)
{
    if (message.getSeverity() == Severity::Error)
    {
        m_errorCount++;
    }
    if (m_reporter)
    {
        *m_reporter << message;
    }
}

std::size_t cld::PP::Context::getErrorCount() const
{
    return m_errorCount;
}

const cld::SourceInterface& cld::PP::Context::getSourceInterface() const
{
    return m_sourceInterface;
}

std::pair<cld::PP::File, bool> cld::PP::buildTree(const PPSourceObject& sourceObject, llvm::raw_ostream* reporter)
{
    Context context(sourceObject, reporter);
    const auto* begin = sourceObject.data().data();
    return {parseFile(begin, sourceObject.data().data() + sourceObject.data().size(), context),
            context.getErrorCount() == 0};
}

cld::PP::File cld::PP::parseFile(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context)
{
    std::vector<Group> result;
    while (begin != end)
    {
        result.push_back(parseGroup(begin, end, context));
    }
    return File{std::move(result)};
}

cld::PP::Group cld::PP::parseGroup(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context,
                                   bool inIf)
{
    CLD_ASSERT(begin != end);
    std::vector<GroupPart> parts;
    do
    {
        if (begin->getTokenType() != Lexer::TokenType::Pound)
        {
            // TextBlock
            const auto* start = begin;
            while (begin != end && begin->getTokenType() != Lexer::TokenType::Pound)
            {
                const auto* eol = findNewline(begin, end);
                begin = eol == end ? eol : eol + 1;
            }
            parts.emplace_back(TextBlock{{start, begin == end ? begin : begin - 1}});
            continue;
        }
        begin++;
        if (begin == end || begin->getTokenType() == Lexer::TokenType::Newline)
        {
            continue;
        }
        if (begin->getTokenType() != Lexer::TokenType::Identifier)
        {
            const auto* eol = findNewline(begin, end);
            parts.emplace_back(NonDirective{begin, eol});
            begin = eol == end ? eol : eol + 1;
            continue;
        }
        auto value = begin->getValue();
        if (value == "if" || value == "ifdef" || value == "ifndef")
        {
            parts.emplace_back(parseIfSection(begin, end, context));
        }
        else if (value == "include" || value == "define" || value == "undef" || value == "line" || value == "error"
                 || value == "pragma")
        {
            parts.emplace_back(parseControlLine(begin, end, context));
        }
        else if (inIf && (value == "endif" || value == "elif" || value == "else"))
        {
            begin--;
            break;
        }
        else
        {
            parts.emplace_back(UnknownDirective{begin});
            skipLine(begin, end);
        }
    } while (begin != end);
    return Group{std::move(parts)};
}

cld::PP::ControlLine cld::PP::parseControlLine(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end,
                                               Context& context)
{
    CLD_ASSERT(begin != end && begin->getTokenType() == Lexer::TokenType::Identifier);
    auto value = begin->getValue();
    if (value == "include" || value == "line" || value == "error" || value == "pragma" || value == "define")
    {
        begin++;
        const auto* eol = findNewline(begin, end);
        expect(Lexer::TokenType::Newline, eol, end, context);
        const auto* lineStart = begin;
        begin = eol;
        if (value == "include")
        {
            return {ControlLine::IncludeTag{lineStart - 1, {lineStart, eol - 1}}};
        }
        else if (value == "line")
        {
            return {ControlLine::LineTag{lineStart - 1, {lineStart, eol - 1}}};
        }
        else if (value == "error")
        {
            return {ControlLine::ErrorTag{lineStart - 1, {lineStart, eol - 1}}};
        }
        else if (value == "pragma")
        {
            return {ControlLine::PragmaTag{{lineStart, eol - 1}}};
        }
        else if (value == "define")
        {
            return {DefineDirective{lineStart - 1, {lineStart, eol - 1}}};
        }
        CLD_UNREACHABLE;
    }
    else if (value == "undef")
    {
        begin++;
        if (!expect(Lexer::TokenType::Identifier, begin, end, context))
        {
            skipLine(begin, end);
            return {};
        }
        const auto* name = begin - 1;
        if (!expect(Lexer::TokenType::Newline, begin, end, context))
        {
            skipLine(begin, end);
            return {};
        }
        return {name};
    }
    CLD_UNREACHABLE;
}

cld::PP::IfSection cld::PP::parseIfSection(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context)
{
    const auto* ifPos = begin;
    auto ifGroup = parseIfGroup(begin, end, context);
    std::vector<ElIfGroup> elifGroups;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::Pound)
    {
        const auto* id = begin + 1;
        if (id == end || id->getTokenType() != Lexer::TokenType::Identifier)
        {
            break;
        }
        if (id->getValue() != "elif")
        {
            break;
        }
        begin++;
        elifGroups.push_back(parseElIfGroup(begin, end, context));
    }
    std::optional<ElseGroup> optionalElseGroup;
    if (begin != end && begin->getTokenType() == Lexer::TokenType::Pound)
    {
        const auto* id = begin + 1;
        if (id != end && id->getTokenType() == Lexer::TokenType::Identifier && id->getValue() == "else")
        {
            begin++;
            optionalElseGroup = parseElseGroup(begin, end, context);
        }
    }

    if (begin == end || begin->getTokenType() != Lexer::TokenType::Pound || begin + 1 == end
        || (begin + 1)->getTokenType() != Lexer::TokenType::Identifier || (begin + 1)->getValue() != "endif")
    {
        if (begin == end)
        {
            context.log(Errors::Parser::EXPECTED_ENDIF.args(diag::after(*(begin - 1)), context.getSourceInterface(),
                                                            *(begin - 1), std::string_view("#endif")));
        }
        else if (begin->getTokenType() == Lexer::TokenType::Pound && begin + 1 != end)
        {
            context.log(Errors::Parser::EXPECTED_ENDIF_INSTEAD_OF_N.args(*(begin + 1), context.getSourceInterface(),
                                                                         *(begin + 1)));
        }
        else
        {
            CLD_UNREACHABLE;
        }
        context.log(Notes::TO_MATCH_N_HERE.args(*ifPos, context.getSourceInterface(), *ifPos));
        skipLine(begin, end);
        return IfSection{std::move(ifGroup), std::move(elifGroups), std::move(optionalElseGroup)};
    }
    begin += 2;
    return IfSection{std::move(ifGroup), std::move(elifGroups), std::move(optionalElseGroup)};
}

cld::PP::ElseGroup cld::PP::parseElseGroup(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context)
{
    CLD_ASSERT(begin != end && begin->getTokenType() == Lexer::TokenType::Identifier && begin->getValue() == "else");
    begin++;
    if (!expect(Lexer::TokenType::Newline, begin, end, context))
    {
        skipLine(begin, end);
    }
    if (begin == end
        || (begin->getTokenType() == Lexer::TokenType::Pound && begin + 1 != end
            && (begin + 1)->getTokenType() == Lexer::TokenType::Identifier && (begin + 1)->getValue() == "endif"))
    {
        return ElseGroup{{}};
    }

    auto group = std::make_unique<Group>(parseGroup(begin, end, context, true));
    return ElseGroup{std::move(group)};
}

cld::PP::ElIfGroup cld::PP::parseElIfGroup(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context)
{
    CLD_ASSERT(begin != end && begin->getTokenType() == Lexer::TokenType::Identifier && begin->getValue() == "elif");
    const auto* elifToken = begin++;
    const auto* eol = findNewline(begin, end);
    auto vector = llvm::ArrayRef(begin, eol);
    begin = eol;
    expect(Lexer::TokenType::Newline, begin, end, context);
    if (begin == end
        || (begin->getTokenType() == Lexer::TokenType::Pound && (begin + 1) != end
            && (begin + 1)->getTokenType() == Lexer::TokenType::Identifier
            && ((begin + 1)->getValue() == "elif" || (begin + 1)->getValue() == "else"
                || (begin + 1)->getValue() == "endif")))
    {
        return ElIfGroup{elifToken, std::move(vector), {}};
    }

    auto group = std::make_unique<Group>(parseGroup(begin, end, context, true));
    return ElIfGroup{elifToken, std::move(vector), std::move(group)};
}

cld::PP::IfGroup cld::PP::parseIfGroup(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context)
{
    CLD_ASSERT(begin != end && begin->getTokenType() == Lexer::TokenType::Identifier);
    IfGroup::variant variant;
    auto value = begin->getValue();
    const auto* ifsToken = begin;
    if (value == "if")
    {
        begin++;
        const auto* eol = findNewline(begin, end);
        auto vector = llvm::ArrayRef(begin, eol);
        if (vector.empty())
        {
            context.log(
                Errors::Parser::EXPECTED_TOKENS_AFTER_N.args(*(begin - 1), context.getSourceInterface(), *(begin - 1)));
        }
        variant = vector;
        begin = eol == end ? eol : eol + 1;
    }
    else if (value == "ifdef")
    {
        begin++;
        if (!expect(Lexer::TokenType::Identifier, begin, end, context))
        {
            skipLine(begin, end);
        }
        else
        {
            variant = IfGroup::IfDefTag{(begin - 1)->getValue()};
            if (!expect(Lexer::TokenType::Newline, begin, end, context))
            {
                skipLine(begin, end);
            }
        }
    }
    else if (value == "ifndef")
    {
        begin++;
        if (!expect(Lexer::TokenType::Identifier, begin, end, context))
        {
            skipLine(begin, end);
        }
        else
        {
            variant = IfGroup::IfnDefTag{(begin - 1)->getValue()};
            if (!expect(Lexer::TokenType::Newline, begin, end, context))
            {
                skipLine(begin, end);
            }
        }
    }
    else
    {
        CLD_UNREACHABLE;
    }
    if (begin == end
        || (begin->getTokenType() == Lexer::TokenType::Pound && (begin + 1) != end
            && (begin + 1)->getTokenType() == Lexer::TokenType::Identifier
            && ((begin + 1)->getValue() == "elif" || (begin + 1)->getValue() == "else"
                || (begin + 1)->getValue() == "endif")))
    {
        return IfGroup{ifsToken, std::move(variant), {}};
    }

    auto group = std::make_unique<Group>(parseGroup(begin, end, context, true));
    return IfGroup{ifsToken, std::move(variant), std::move(group)};
}
