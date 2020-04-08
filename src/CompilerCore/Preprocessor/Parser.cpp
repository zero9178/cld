#include "Parser.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Util.hpp>

namespace
{
bool expect(cld::Lexer::TokenType tokenType, cld::Lexer::TokenIterator& begin, cld::Lexer::TokenIterator end,
            cld::PP::Context& context, std::vector<cld::Message>&& additional = {})
{
    if (begin == end || begin->getTokenType() != tokenType)
    {
        if (begin == end || begin->getTokenType() == cld::Lexer::TokenType::Newline)
        {
            context.log(
                {cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N.args(cld::Lexer::tokenName(tokenType)),
                                     begin - 1, cld::Modifier(begin - 1, begin, cld::Modifier::InsertAtEnd))});
        }
        else
        {
            context.log(
                {cld::Message::error(cld::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                         cld::Lexer::tokenName(tokenType), '\'' + begin->getRepresentation() + '\''),
                                     begin, cld::Modifier(begin, begin + 1, cld::Modifier::PointAtBeginning))});
        }
        context.log(additional);
        return false;
    }
    begin++;
    return true;
}

cld::Lexer::TokenIterator findNewline(cld::Lexer::TokenIterator begin, cld::Lexer::TokenIterator end)
{
    return std::find_if(begin, end, [](const cld::Lexer::Token& token) {
        return token.getTokenType() == cld::Lexer::TokenType::Newline;
    });
}

void skipLine(cld::Lexer::TokenIterator& begin, cld::Lexer::TokenIterator end)
{
    begin = findNewline(begin, end);
    begin = begin == end ? begin : begin + 1;
}

template <std::size_t N>
void skipUntil(cld::Lexer::TokenIterator& begin, cld::Lexer::TokenIterator end,
               const std::array<cld::Lexer::TokenType, N>& tokens)
{
    begin = std::find_if(begin, end, [&tokens](const cld::Lexer::Token& token) {
        return std::any_of(tokens.begin(), tokens.end(),
                           [&token](auto&& value) { return token.getTokenType() == value; });
    });
}

// template <std::size_t N>
// cld::Lexer::TokenIterator findDirectives(cld::Lexer::TokenIterator begin, cld::Lexer::TokenIterator end,
//                                         const std::array<const char*, N>& directives)
//{
//    for (auto iter = begin; iter != end; iter++)
//    {
//        if (iter + 1 == end || iter + 2 == end)
//        {
//            return end;
//        }
//        if (iter->getTokenType() == cld::Lexer::TokenType::Newline
//            && (iter + 1)->getTokenType() == cld::Lexer::TokenType::Pound
//            && (iter + 2)->getTokenType() == cld::Lexer::TokenType::Identifier
//            && std::any_of(
//                directives.begin(), directives.end(),
//                [&string = std::get<std::string>((iter + 2)->getValue())](auto&& sv) { return sv == string; }))
//        {
//            return iter + 1;
//        }
//    }
//    return end;
//}
} // namespace

cld::PP::Context::Context(const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter)
    : m_sourceObject(sourceObject), m_reporter(reporter)
{
}

void cld::PP::Context::log(std::vector<Message> messages)
{
    for (auto& iter : messages)
    {
        if (iter.getSeverity() == Message::Error)
        {
            m_errorCount++;
        }
        if (m_reporter)
        {
            iter.print(*m_reporter, m_sourceObject);
        }
    }
}

std::size_t cld::PP::Context::getErrorCount() const
{
    return m_errorCount;
}

std::pair<cld::PP::File, bool> cld::PP::buildTree(const SourceObject& sourceObject, llvm::raw_ostream* reporter)
{
    Context context(sourceObject, reporter);
    auto begin = sourceObject.data().cbegin();
    return {parseFile(begin, sourceObject.data().cend(), context), context.getErrorCount() == 0};
}

cld::PP::File cld::PP::parseFile(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    std::vector<Group> result;
    while (begin != end)
    {
        result.push_back(parseGroup(begin, end, context));
    }
    return File{std::move(result)};
}

cld::PP::Group cld::PP::parseGroup(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context, bool inIf)
{
    CLD_ASSERT(begin != end);
    std::vector<GroupPart> parts;
    do
    {
        if (begin->getTokenType() != Lexer::TokenType::Pound)
        {
            // TextLine
            auto eol = findNewline(begin, end);
            parts.emplace_back(std::vector(begin, eol));
            begin = eol == end ? eol : eol + 1;
            continue;
        }
        begin++;
        if (begin == end || begin->getTokenType() == Lexer::TokenType::Newline)
        {
            continue;
        }
        if (begin->getTokenType() != Lexer::TokenType::Identifier)
        {
            auto eol = findNewline(begin, end);
            parts.emplace_back(NonDirective{begin, eol});
            begin = eol == end ? eol : eol + 1;
            continue;
        }
        const auto& value = std::get<std::string>(begin->getValue());
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
            context.log(
                {Message::error(ErrorMessages::PP::N_IS_AN_INVALID_PREPROCESSOR_DIRECTIVE.args("'" + value + "'"),
                                begin, cld::Modifier(begin, begin + 1))});
            skipLine(begin, end);
        }
    } while (begin != end);
    return Group{std::move(parts)};
}

cld::PP::ControlLine cld::PP::parseControlLine(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    CLD_ASSERT(begin != end && begin->getTokenType() == Lexer::TokenType::Identifier);
    const auto& value = std::get<std::string>(begin->getValue());
    if (value == "include" || value == "line" || value == "error" || value == "pragma")
    {
        begin++;
        if (begin != end && begin->getTokenType() != Lexer::TokenType::Newline)
        {
            auto eol = findNewline(begin, end);
            expect(Lexer::TokenType::Newline, eol, end, context);
            auto start = begin;
            begin = eol;
            if (value == "include")
            {
                return {ControlLine::IncludeTag{start, eol - 1}};
            }
            else if (value == "line")
            {
                return {ControlLine::LineTag{start, eol - 1}};
            }
            else if (value == "error")
            {
                return {ControlLine::ErrorTag{start, eol - 1}};
            }
            else if (value == "pragma")
            {
                return {ControlLine::PragmaTag{start, eol - 1}};
            }
            CLD_UNREACHABLE;
        }

        if (value == "include" || value == "line")
        {
            context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("Tokens", "'" + value + "'"),
                                        begin - 1, Modifier(begin - 1, begin, Modifier::InsertAtEnd))});
            if (value == "include")
            {
                return {ControlLine::IncludeTag{}};
            }
            else if (value == "line")
            {
                return {ControlLine::LineTag{}};
            }
        }
        else
        {
            expect(Lexer::TokenType::Newline, begin, end, context);
            if (value == "error")
            {
                return {ControlLine::ErrorTag{}};
            }
            else if (value == "pragma")
            {
                return {ControlLine::PragmaTag{}};
            }
        }
        CLD_UNREACHABLE;
    }
    else if (value == "define")
    {
        return {parseDefineDirective(begin, end, context)};
    }
    else if (value == "undef")
    {
        begin++;
        if (!expect(Lexer::TokenType::Identifier, begin, end, context))
        {
            skipLine(begin, end);
            return {};
        }
        const auto& name = std::get<std::string>((begin - 1)->getValue());
        if (!expect(Lexer::TokenType::Newline, begin, end, context))
        {
            skipLine(begin, end);
            return {};
        }
        return {name};
    }
    CLD_UNREACHABLE;
}

cld::PP::DefineDirective cld::PP::parseDefineDirective(Lexer::TokenIterator& begin, Lexer::TokenIterator end,
                                                       Context& context)
{
    CLD_ASSERT(begin != end && begin->getTokenType() == Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "define");
    begin++;
    auto identifierPos = begin;
    if (begin->getTokenType() == Lexer::TokenType::DefinedKeyword)
    {
        context.log({Message::error(ErrorMessages::PP::DEFINED_CANNOT_BE_USED_AS_MACRO_NAME, identifierPos,
                                    Modifier(identifierPos, identifierPos + 1))});
        begin++;
    }
    else if (!expect(Lexer::TokenType::Identifier, begin, end, context))
    {
        skipLine(begin, end);
        return {};
    }
    if (begin == end)
    {
        expect(Lexer::TokenType::Newline, begin, end, context);
        return {identifierPos, {}, false, identifierPos, identifierPos};
    }

    if (begin->getTokenType() != Lexer::TokenType::OpenParentheses
        || begin->getOffset() != identifierPos->getOffset() + identifierPos->getLength())
    {
        if (begin->getTokenType() != Lexer::TokenType::Newline
            && begin->getOffset() == identifierPos->getOffset() + identifierPos->getLength())
        {
            context.log({Message::error(ErrorMessages::PP::WHITESPACE_REQUIRED_AFTER_OBJECT_MACRO_DEFINITION, begin,
                                        Modifier(begin, begin + 1))});
        }
        auto eol = findNewline(begin, end);
        auto define = DefineDirective{identifierPos, {}, false, begin, eol};
        begin = eol;
        expect(Lexer::TokenType::Newline, begin, end, context);
        return define;
    }

    auto openP = begin++;
    if (begin == end || begin->getTokenType() == Lexer::TokenType::Newline)
    {
        expect(Lexer::TokenType::CloseParentheses, begin, end, context,
               {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), openP,
                              Modifier(openP, openP + 1, Modifier::PointAtBeginning))});
        skipLine(begin, end);
        return DefineDirective{identifierPos, {std::vector<std::string>()}, false, identifierPos, identifierPos};
    }

    switch (begin->getTokenType())
    {
        case Lexer::TokenType::CloseParentheses:
        {
            begin++;
            auto eol = findNewline(begin, end);
            auto define = DefineDirective{identifierPos, {std::vector<std::string>()}, false, begin, eol};
            begin = eol;
            expect(Lexer::TokenType::Newline, begin, end, context);
            return define;
        }
        case Lexer::TokenType::Ellipse:
        {
            begin++;
            if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                        {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), openP,
                                       Modifier(openP, openP + 1, Modifier::PointAtBeginning))}))
            {
                skipLine(begin, end);
                return DefineDirective{identifierPos, {std::vector<std::string>()}, true, identifierPos, identifierPos};
            }
            auto eol = findNewline(begin, end);
            auto define = DefineDirective{identifierPos, {std::vector<std::string>()}, true, begin, eol};
            begin = eol;
            expect(Lexer::TokenType::Newline, begin, end, context);
            return define;
        }
        default: break;
    }

    if (!expect(Lexer::TokenType::Identifier, begin, end, context))
    {
        skipLine(begin, end);
        return DefineDirective{identifierPos, {std::vector<std::string>()}, false, identifierPos, identifierPos};
    }

    std::vector<Lexer::TokenIterator> identifierList = {begin - 1};
    bool ellipse = false;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::Comma)
    {
        begin++;
        if (begin == end || begin->getTokenType() == Lexer::TokenType::Identifier
            || begin->getTokenType() != Lexer::TokenType::Ellipse)
        {
            if (!expect(Lexer::TokenType::Identifier, begin, end, context))
            {
                skipUntil(begin, end, std::array{Lexer::TokenType::Newline, Lexer::TokenType::CloseParentheses});
                if (begin != end && begin->getTokenType() == Lexer::TokenType::CloseParentheses)
                {
                    break;
                }
                begin++;
                std::vector<std::string> result;
                result.reserve(identifierList.size());
                std::transform(identifierList.begin(), identifierList.end(), std::back_inserter(result),
                               [](Lexer::TokenIterator iter) { return std::get<std::string>(iter->getValue()); });
                return DefineDirective{identifierPos, std::move(result), false, identifierPos, identifierPos};
            }
            const auto& string = std::get<std::string>((begin - 1)->getValue());
            // Technically this is part of semantics and not the parser but I want to triggers such errors as early on
            // as possible so we don't need to keep iterators around longer than we need
            auto result =
                std::find_if(identifierList.begin(), identifierList.end(), [&string](Lexer::TokenIterator iter) {
                    return string == std::get<std::string>(iter->getValue());
                });
            if (result == identifierList.end())
            {
                identifierList.push_back(begin - 1);
                continue;
            }
            context.log({Message::error(ErrorMessages::PP::REDEFINITION_OF_MACRO_PARAMETER_N.args("'" + string + "'"),
                                        begin - 1, Modifier(begin - 1, begin)),
                         Message::note(Notes::PREVIOUSLY_DECLARED_HERE, *result, Modifier(*result, *result + 1))});
            continue;
        }

        if (begin->getTokenType() != Lexer::TokenType::Ellipse)
        {
            CLD_UNREACHABLE;
        }
        ellipse = true;
        begin++;
        break;
    }

    expect(Lexer::TokenType::CloseParentheses, begin, end, context,
           {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), openP,
                          Modifier(openP, openP + 1, Modifier::PointAtBeginning))});
    auto eol = findNewline(begin, end);
    std::vector<std::string> result;
    result.reserve(identifierList.size());
    std::transform(identifierList.begin(), identifierList.end(), std::back_inserter(result),
                   [](Lexer::TokenIterator iter) { return std::get<std::string>(iter->getValue()); });
    auto define = DefineDirective{identifierPos, std::move(result), ellipse, begin, eol};
    begin = eol;
    expect(Lexer::TokenType::Newline, begin, end, context);
    return define;
}

cld::PP::IfSection cld::PP::parseIfSection(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    auto ifPos = begin;
    auto ifGroup = parseIfGroup(begin, end, context);
    std::vector<ElIfGroup> elifGroups;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::Pound)
    {
        auto id = begin + 1;
        if (id == end || id->getTokenType() != Lexer::TokenType::Identifier)
        {
            break;
        }
        const auto& value = std::get<std::string>(id->getValue());
        if (value != "elif")
        {
            break;
        }
        begin++;
        elifGroups.push_back(parseElIfGroup(begin, end, context));
    }
    std::optional<ElseGroup> optionalElseGroup;
    if (begin != end && begin->getTokenType() == Lexer::TokenType::Pound)
    {
        auto id = begin + 1;
        if (id != end && id->getTokenType() == Lexer::TokenType::Identifier
            && std::get<std::string>(id->getValue()) == "else")
        {
            begin++;
            optionalElseGroup = parseElseGroup(begin, end, context);
        }
    }

    if (begin == end || begin->getTokenType() != Lexer::TokenType::Pound || begin + 1 == end
        || (begin + 1)->getTokenType() != Lexer::TokenType::Identifier
        || std::get<std::string>((begin + 1)->getValue()) != "endif")
    {
        auto additional = Message::note(Notes::TO_MATCH_N_HERE.args("'if'"), ifPos, Modifier(ifPos, ifPos + 1));
        if (begin == end)
        {
            context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("'#endif'"), end,
                                        Modifier(begin - 1, begin, Modifier::InsertAtEnd, "#endif")),
                         std::move(additional)});
        }
        else if (begin->getTokenType() == Lexer::TokenType::Pound && begin + 1 != end)
        {
            context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                            "'#endif'", "'#" + (begin + 1)->getRepresentation() + "'"),
                                        begin + 1, Modifier(begin, begin + 2)),
                         std::move(additional)});
        }
        else
        {
            CLD_UNREACHABLE;
        }
        skipLine(begin, end);
        return IfSection{std::move(ifGroup), std::move(elifGroups), std::move(optionalElseGroup)};
    }
    begin += 2;
    return IfSection{std::move(ifGroup), std::move(elifGroups), std::move(optionalElseGroup)};
}

cld::PP::ElseGroup cld::PP::parseElseGroup(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    CLD_ASSERT(begin != end && begin->getTokenType() == Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "else");
    begin++;
    expect(Lexer::TokenType::Newline, begin, end, context);
    if (begin == end
        || (begin->getTokenType() == Lexer::TokenType::Pound && begin + 1 != end
            && (begin + 1)->getTokenType() == Lexer::TokenType::Identifier
            && std::get<std::string>((begin + 1)->getValue()) == "endif"))
    {
        return ElseGroup{{}};
    }

    return ElseGroup{std::make_unique<Group>(parseGroup(begin, end, context, true))};
}

cld::PP::ElIfGroup cld::PP::parseElIfGroup(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    CLD_ASSERT(begin != end && begin->getTokenType() == Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "elif");
    begin++;
    auto eol = findNewline(begin, end);
    auto vector = std::vector(begin, eol);
    if (vector.empty())
    {
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("Tokens", "'elif'"), begin - 1,
                                    Modifier(begin - 1, begin, Modifier::InsertAtEnd))});
    }
    begin = eol;
    expect(Lexer::TokenType::Newline, begin, end, context);
    if (begin == end
        || (begin->getTokenType() == Lexer::TokenType::Pound && (begin + 1) != end
            && (begin + 1)->getTokenType() == Lexer::TokenType::Identifier
            && (std::get<std::string>((begin + 1)->getValue()) == "elif"
                || std::get<std::string>((begin + 1)->getValue()) == "else"
                || std::get<std::string>((begin + 1)->getValue()) == "endif")))
    {
        return ElIfGroup{std::move(vector), {}};
    }

    return ElIfGroup{std::move(vector), std::make_unique<Group>(parseGroup(begin, end, context, true))};
}

cld::PP::IfGroup cld::PP::parseIfGroup(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    CLD_ASSERT(begin != end && begin->getTokenType() == Lexer::TokenType::Identifier);
    IfGroup::variant variant;
    const auto& value = std::get<std::string>(begin->getValue());
    if (value == "if")
    {
        begin++;
        auto eol = findNewline(begin, end);
        auto vector = std::vector(begin, eol);
        if (vector.empty())
        {
            context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("Tokens", "'if'"), begin - 1,
                                        Modifier(begin - 1, begin, Modifier::InsertAtEnd))});
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
            variant = IfGroup::IfDefTag{std::get<std::string>((begin - 1)->getValue())};
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
            variant = IfGroup::IfnDefTag{std::get<std::string>((begin - 1)->getValue())};
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
            && (std::get<std::string>((begin + 1)->getValue()) == "elif"
                || std::get<std::string>((begin + 1)->getValue()) == "else"
                || std::get<std::string>((begin + 1)->getValue()) == "endif")))
    {
        return IfGroup{std::move(variant), {}};
    }

    return IfGroup{std::move(variant), std::make_unique<Group>(parseGroup(begin, end, context, true))};
}
