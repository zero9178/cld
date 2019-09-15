#include "Preprocessor.hpp"

#include <CompilerCore/C/ErrorMessages.hpp>
#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/C/Syntax.hpp>
#include <CompilerCore/Common/Util.hpp>

#include <cassert>

namespace
{
    struct Group;

    Group parseGroup(OpenCL::Parser::Tokens::const_iterator& begin, OpenCL::Parser::Tokens::const_iterator end,
                     const OpenCL::SourceObject& sourceObject, std::ostream* reporter);

    std::vector<OpenCL::Lexer::Token>::const_iterator
        findEOLWithOutBackslash(std::vector<OpenCL::Lexer::Token>::const_iterator begin,
                                std::vector<OpenCL::Lexer::Token>::const_iterator end)
    {
        auto result = begin;
        do
        {
            result = OpenCL::findEOL(result, end);
        } while ((result - 1)->getTokenType() == OpenCL::Lexer::TokenType::Backslash);
        return result;
    }

    /**
     * <IfGroup> ::= <TokenType::Pound> <TokenType::IfKeyword> <ConstantExpression> <NEWLINE> [ <Group> ]
     *             | <TokenType::Pound> <Identifier=ifdef> <TokenType::Identifier> <NEWLINE> [ <Group> ]
     *             | <TokenType::Pound> <Identifier=ifndef> <TokenType::Identifier> <NEWLINE> [ <Group> ]
     */
    struct IfGroup final
    {
        struct IfDefTag final
        {
            std::string identifier;
        };
        struct IfnDefTag final
        {
            std::string identifier;
        };
        std::variant<IfDefTag, IfnDefTag, OpenCL::Syntax::ConstantExpression> ifs;
        std::unique_ptr<Group> optionalGroup;
    };

    /**
     * <ElIfGroup> ::= <TokenType::Pound> <Identifier=elif> <ConstantExpression> <NEWLINE> [ <Group> ]
     */
    struct ElIfGroup final
    {
        OpenCL::Syntax::ConstantExpression constantExpression;
        std::unique_ptr<Group> optionalGroup;
    };

    /**
     * <ElseGroup> ::= <TokenType::Pound> <TokenType::ElseKeyword> <NEWLINE> [<Group>]
     */
    struct ElseGroup final
    {
        std::unique_ptr<Group> optionalGroup;
    };

    /**
     * <IfSection> ::= <IfGroup> {<ElIfGroup>} [<ElseGroup] <TokenType::Pound> <Identifier=endif> <NEWLINE>
     */
    struct IfSection final
    {
        IfGroup ifGroup;
        std::vector<ElIfGroup> elifGroups;
        std::optional<ElseGroup> optionalElseGroup;
    };

    /**
     * OpenParentheses must immediately follow the define keyword for the alternative to be valid
     * otherwise its just another token inside of <TOKENS>
     *
     * <DefineDirectives> ::= <TokenType::Pound> <Identifier=define> <TokenType::Identifier> [<TOKENS>] <NEWLINE>
     *                 | <TokenType::Pound> <Identifier=define> <TokenType::Identifier> <TokenType::OpenParentheses>
     *                   [ <TokenType::Identifier { <TokenType::Comma> <TokenType::Identifier> } ]
     *                   <TokenType::CloseParentheses> [<TOKENS>] <NEWLINE>
     *                 | <TokenType::Pound> <Identifier=define> <TokenType::Identifier> <TokenType::OpenParentheses>
     *                   <TokenType::Ellipse> <TokenType::CloseParentheses> [ <TOKENS> ] <NEWLINE>
     *                 | <TokenType::Pound> <Identifier=define> <TokenType::Identifier> <TokenType::OpenParentheses>
     *                   [ <TokenType::Identifier { <TokenType::Comma> <TokenType::Identifier> } ] <TokenType::Comma>
     *                   <TokenType::Ellipse> <TokenType::CloseParentheses> [ <TOKENS> ] <NEWLINE>
     *
     * <ControlLine> ::= <TokenType::Pound> <Identifier=include> <TOKENS> <NEWLINE>
     *                 | <DefineDirectives>
     *                 | <TokenType::Pound> <Identifier=undef> <TokenType::Identifier> <NEWLINE>
     *                 | <TokenType::Pound> <Identifier=line> <TOKENS> <NEWLINE>
     *                 | <TokenType::Pound> <Identifier=error> [<TOKENS>] <NEWLINE>
     *                 | <TokenType::Pound> <Identifier=pragma> [<TOKENS>] <NEWLINE>
     */
    struct ControlLine final
    {
        struct DefineDirective
        {
            std::string identifier;
            /**
             * Its an optional to differentiate between an empty identifier list and no identifier list
             */
            std::optional<std::vector<std::string>> identifierList;
            bool hasEllipse;
            std::vector<OpenCL::Lexer::Token> replacementList;
        };

        struct IncludeTag final
        {
            std::vector<OpenCL::Lexer::Token> tokens;
        };

        struct LineTag final
        {
            std::vector<OpenCL::Lexer::Token> tokens;
        };

        struct ErrorTag final
        {
            std::vector<OpenCL::Lexer::Token> tokens;
        };

        struct PragmaTag final
        {
            std::vector<OpenCL::Lexer::Token> tokens;
        };

        std::variant<IncludeTag, LineTag, ErrorTag, PragmaTag, std::string, DefineDirective> variant;
    };

    struct NonDirective
    {
        std::vector<OpenCL::Lexer::Token> tokens;
    };

    /**
     * <GroupPart> ::= <IfSection> | <ControlLine> | <TextLine> | <NonDirective>
     */
    using GroupPart = std::variant<IfSection, ControlLine, std::vector<OpenCL::Lexer::Token>, NonDirective>;

    /**
     * <Group> ::= <GroupPart> { <GroupPart> }
     */
    struct Group final
    {
        std::vector<GroupPart> groupPart;
    };

    /**
     * <File> := { <Group> }
     */
    struct File final
    {
        std::vector<Group> groups;
    };

    bool expect(OpenCL::Lexer::TokenType tokenType, OpenCL::Parser::Tokens::const_iterator begin,
                OpenCL::Parser::Tokens::const_iterator end, const OpenCL::SourceObject& sourceObject,
                std::ostream* reporter)
    {
        if (begin == end || begin->getTokenType() != tokenType)
        {
            if (reporter)
            {
                if (begin == end)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N.args(OpenCL::Lexer::tokenName(tokenType)),
                        sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                        OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
                }
                else
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(OpenCL::Lexer::tokenName(tokenType),
                                                                                    '\'' + begin->emitBack() + '\''),
                        sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                        OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
                }
            }
            return false;
        }
        return true;
    }

    IfGroup parseIfGroup(OpenCL::Parser::Tokens::const_iterator& begin, OpenCL::Parser::Tokens::const_iterator end,
                         const OpenCL::SourceObject& sourceObject, std::ostream* reporter)
    {
        IfGroup result{};
        assert(begin != end);
        if (begin->getTokenType() == OpenCL::Lexer::TokenType::IfKeyword)
        {
            begin++;
            OpenCL::Parser::Context context(sourceObject, reporter);
            auto expEnd = findEOLWithOutBackslash(begin, end);
            auto constantExpression = OpenCL::Parser::parseConditionalExpression(begin, expEnd, context);
            if (begin != expEnd)
            {
                if (reporter)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("newline", "expression"),
                        sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                        OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
                }
                return result;
            }
            result = IfGroup{std::move(constantExpression), nullptr};
        }
        else if (begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier)
        {
            const auto& value = std::get<std::string>(begin->getValue());
            begin++;
            if (!expect(OpenCL::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
            {
                return result;
            }
            const auto& identifier = std::get<std::string>(begin->getValue());
            begin++;
            // One may argue that if begin == end we technically don't have a newline either but id rather error later
            // and complain about not having an endif
            if (begin != end && begin->getLine() == (begin - 1)->getLine())
            {
                if (reporter)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args("newline",
                                                                                    '\'' + begin->emitBack() + '\''),
                        sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                        OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
                }
                return result;
            }
            if (identifier == "ifdef")
            {
                result = IfGroup{IfGroup::IfDefTag{value}, nullptr};
            }
            else if (identifier == "ifndef")
            {
                result = IfGroup{IfGroup::IfnDefTag{value}, nullptr};
            }
            else
            {
                OPENCL_UNREACHABLE;
            }
        }
        else
        {
            OPENCL_UNREACHABLE;
        }

        if (begin != end)
        {
            result.optionalGroup = std::make_unique<Group>(parseGroup(begin, end, sourceObject, reporter));
        }
        return result;
    }

    ElIfGroup parseElIfGroup(OpenCL::Parser::Tokens::const_iterator& begin, OpenCL::Parser::Tokens::const_iterator end,
                             const OpenCL::SourceObject& sourceObject, std::ostream* reporter)
    {
        assert(begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "elif");
        begin++;
        auto newline = findEOLWithOutBackslash(begin, end);
        OpenCL::Parser::Context context(sourceObject, reporter);
        auto constantExpression = OpenCL::Parser::parseConditionalExpression(begin, newline, context);
        ElIfGroup result{std::move(constantExpression), {}};
        if (begin != newline)
        {
            if (reporter)
            {
                *reporter << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("newline", "expression"),
                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                    OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
            }
            return result;
        }
        if (begin != end)
        {
            result.optionalGroup = std::make_unique<Group>(parseGroup(begin, end, sourceObject, reporter));
        }
        return result;
    }

    ElseGroup parseElseGroup(OpenCL::Parser::Tokens::const_iterator& begin, OpenCL::Parser::Tokens::const_iterator end,
                             const OpenCL::SourceObject& sourceObject, std::ostream* reporter)
    {
        assert(begin->getTokenType() == OpenCL::Lexer::TokenType::ElseKeyword);
        begin++;
        if (begin != end && begin->getLine() == (begin - 1)->getLine())
        {
            if (reporter)
            {
                *reporter << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N.args("newline"), sourceObject.getLineStart(begin),
                    sourceObject.getLineEnd(begin),
                    OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
            }
            return {{}};
        }
        ElseGroup result{};
        if (begin != end)
        {
            result.optionalGroup = std::make_unique<Group>(parseGroup(begin, end, sourceObject, reporter));
        }
        return result;
    }

    IfSection parseIfSection(OpenCL::Parser::Tokens::const_iterator& begin, OpenCL::Parser::Tokens::const_iterator end,
                             const OpenCL::SourceObject& sourceObject, std::ostream* reporter)
    {
        assert(begin != end);
        auto ifGroup = parseIfGroup(begin, end, sourceObject, reporter);
        if (begin == end)
        {
            if (reporter)
            {
                (*reporter) << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N.args("#endif"), sourceObject.getLineStart(begin),
                    sourceObject.getLineEnd(begin), OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
            }
            return {std::move(ifGroup), {}, {}};
        }
        else if (begin->getTokenType() != OpenCL::Lexer::TokenType::Pound)
        {
            if (reporter)
            {
                (*reporter) << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("#endif", '\'' + begin->emitBack() + '\''),
                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                    OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
            }
            return {std::move(ifGroup), {}, {}};
        }
        else
        {
            begin++;
        }

        std::vector<ElIfGroup> elifGroups;
        while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "elif")
        {
            elifGroups.push_back(parseElIfGroup(begin, end, sourceObject, reporter));
        }

        if (begin == end)
        {
            if (reporter)
            {
                (*reporter) << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N.args("#endif"), sourceObject.getLineStart(begin),
                    sourceObject.getLineEnd(begin), OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
            }
            return {std::move(ifGroup), std::move(elifGroups), {}};
        }
        else if (begin->getTokenType() != OpenCL::Lexer::TokenType::Pound)
        {
            if (reporter)
            {
                (*reporter) << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("#endif", '\'' + begin->emitBack() + '\''),
                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                    OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
            }
            return {std::move(ifGroup), std::move(elifGroups), {}};
        }
        else
        {
            begin++;
        }

        IfSection result{std::move(ifGroup), std::move(elifGroups), {}};
        if (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::ElseKeyword)
        {
            result.optionalElseGroup = parseElseGroup(begin, end, sourceObject, reporter);
        }

        if (begin == end)
        {
            if (reporter)
            {
                (*reporter) << OpenCL::Message::error(
                    OpenCL::ErrorMessages::Parser::EXPECTED_N.args("#endif"), sourceObject.getLineStart(begin),
                    sourceObject.getLineEnd(begin),
                    OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
            }
        }
        else
        {
            begin++;
            if (begin == end || begin->getTokenType() != OpenCL::Lexer::TokenType::Identifier
                || std::get<std::string>(begin->getValue()) != "endif")
            {
                if (reporter)
                {
                    if (begin == end)
                    {
                        (*reporter) << OpenCL::Message::error(
                            OpenCL::ErrorMessages::Parser::EXPECTED_N.args("#endif"), sourceObject.getLineStart(begin),
                            sourceObject.getLineEnd(begin),
                            OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
                    }
                    else
                    {
                        (*reporter) << OpenCL::Message::error(
                            OpenCL::ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("#endif",
                                                                                    '\'' + begin->emitBack() + '\''),
                            sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                            OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
                    }
                }
            }
        }

        return result;
    }

    ControlLine::DefineDirective parseDefineDirective(OpenCL::Parser::Tokens::const_iterator& begin,
                                                      OpenCL::Parser::Tokens::const_iterator end,
                                                      const OpenCL::SourceObject& sourceObject, std::ostream* reporter)
    {
        assert(begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "define");
        begin++;
        if (!expect(OpenCL::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
        {
            return ControlLine::DefineDirective{};
        }
        const auto& name = std::get<std::string>(begin->getValue());
        begin++;
        if (begin == end || begin->getTokenType() != OpenCL::Lexer::TokenType::OpenParentheses
            || begin->getLine() != (begin - 1)->getLine()
            || begin->getColumn() != (begin - 1)->getColumn() + (begin - 1)->getLength())
        {
            // No ( after the identifier or there's whitespace in between the identifier and the (
            auto eol = findEOLWithOutBackslash(begin, end);
            std::vector tokens(begin, eol);
            begin = eol;
            return ControlLine::DefineDirective{name, {}, false, std::move(tokens)};
        }
        else
        {
            begin++;
            bool hasEllipse = false;
            std::vector<std::string> identifierList;
            bool first = true;
            while (begin != end
                   && (first ? begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
                                   || begin->getTokenType() == OpenCL::Lexer::TokenType::Ellipse :
                               begin->getTokenType() == OpenCL::Lexer::TokenType::Comma))
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    begin++;
                }
                if (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier)
                {
                    identifierList.push_back(std::get<std::string>(begin->getValue()));
                }
                else if (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Ellipse)
                {
                    hasEllipse = true;
                    break;
                }
                else
                {
                    if (reporter)
                    {
                        if (begin == end)
                        {
                            *reporter << OpenCL::Message::error(
                                OpenCL::ErrorMessages::Parser::EXPECTED_N.args("identifier or '...'"),
                                sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                                OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
                        }
                        else
                        {
                            *reporter << OpenCL::Message::error(
                                OpenCL::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                    "identifier or '...'", '\'' + begin->emitBack() + '\''),
                                sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                                OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
                        }
                    }
                    return ControlLine::DefineDirective{name, std::move(identifierList), false, {}};
                }
            }
            if (expect(OpenCL::Lexer::TokenType::CloseParentheses, begin, end, sourceObject, reporter))
            {
                begin++;
            }
            auto newline = findEOLWithOutBackslash(begin, end);
            std::vector<OpenCL::Lexer::Token> token(begin, newline);
            begin = newline;
            return ControlLine::DefineDirective{name, std::move(identifierList), hasEllipse, std::move(token)};
        }
    }

    ControlLine parseControlLine(OpenCL::Parser::Tokens::const_iterator& begin,
                                 OpenCL::Parser::Tokens::const_iterator end, const OpenCL::SourceObject& sourceObject,
                                 std::ostream* reporter)
    {
        const auto& value = std::get<std::string>(begin->getValue());
        if (value == "define")
        {
            return ControlLine{parseDefineDirective(begin, end, sourceObject, reporter)};
        }
        else if (value == "undef")
        {
            begin++;
            if (!expect(OpenCL::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
            {
                return ControlLine{""};
            }
            const auto& name = std::get<std::string>(begin->getValue());
            begin++;
            if (begin != end && begin->getLine() == (begin - 1)->getLine())
            {
                if (reporter)
                {
                    if (begin == end)
                    {
                        *reporter << OpenCL::Message::error(
                            OpenCL::ErrorMessages::Parser::EXPECTED_N.args("newline"), sourceObject.getLineStart(begin),
                            sourceObject.getLineEnd(begin),
                            OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
                    }
                    else
                    {
                        *reporter << OpenCL::Message::error(
                            OpenCL::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                "newline", '\'' + begin->emitBack() + '\''),
                            sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                            OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
                    }
                }
            }
            return ControlLine{name};
        }
        else
        {
            begin++;
            auto newline = findEOLWithOutBackslash(begin, end);
            std::vector<OpenCL::Lexer::Token> tokens(begin, newline);
            begin = newline;
            if (value == "include")
            {
                if (tokens.empty() && reporter)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("tokens", "include"),
                        sourceObject.getLineStart(begin - 1), sourceObject.getLineEnd(begin - 1),
                        OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
                }
                return ControlLine{ControlLine::IncludeTag{std::move(tokens)}};
            }
            else if (value == "line")
            {
                if (tokens.empty() && reporter)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("tokens", "line"),
                        sourceObject.getLineStart(begin - 1), sourceObject.getLineEnd(begin - 1),
                        OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
                }
                return ControlLine{ControlLine::LineTag{std::move(tokens)}};
            }
            else if (value == "error")
            {
                return ControlLine{ControlLine::ErrorTag{std::move(tokens)}};
            }
            else if (value == "pragma")
            {
                return ControlLine{ControlLine::PragmaTag{std::move(tokens)}};
            }
            else
            {
                OPENCL_UNREACHABLE;
            }
        }
    }

    Group parseGroup(OpenCL::Parser::Tokens::const_iterator& begin, OpenCL::Parser::Tokens::const_iterator end,
                     const OpenCL::SourceObject& sourceObject, std::ostream* reporter)
    {
        if (begin == end)
        {
            if (reporter)
            {
                *reporter << OpenCL::Message::error(OpenCL::ErrorMessages::Parser::EXPECTED_N.args("Tokens"),
                                                    sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                                                    OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
            }
            return {};
        }

        std::vector<GroupPart> parts;
        do
        {
            if (begin->getTokenType() != OpenCL::Lexer::TokenType::Pound)
            {
                auto eol = findEOLWithOutBackslash(begin, end);
                parts.emplace_back(std::vector(begin, eol));
                begin = eol;
                continue;
            }
            begin++;
            if (begin == end || begin->getLine() != (begin - 1)->getLine())
            {
                continue;
            }
            switch (begin->getTokenType())
            {
                case OpenCL::Lexer::TokenType::Identifier:
                {
                    // Unknown little feature but you can put a case anywhere even inside of control flow
                    // One just need to avoid have any declarations above it hence the inlined value in the if below
                    if (std::get<std::string>(begin->getValue()) == "ifdef"
                        || std::get<std::string>(begin->getValue()) == "ifndef")
                    {
                        case OpenCL::Lexer::TokenType::IfKeyword:
                        {
                            parts.emplace_back(parseIfSection(begin, end, sourceObject, reporter));
                            continue;
                        }
                    }
                    if (std::get<std::string>(begin->getValue()) == "elif"
                        || std::get<std::string>(begin->getValue()) == "endif")
                    {
                        case OpenCL::Lexer::TokenType::ElseKeyword:
                        {
                            begin--;
                            goto End;
                        }
                    }
                    const auto& value = std::get<std::string>(begin->getValue());
                    if (value == "include" || value == "undef" || value == "line" || value == "error"
                        || value == "pragma" || value == "define")
                    {
                        parts.emplace_back(parseControlLine(begin, end, sourceObject, reporter));
                        continue;
                    }
                    [[fallthrough]];
                }
                default:
                {
                    auto eol = findEOLWithOutBackslash(begin, end);
                    parts.emplace_back(NonDirective{std::vector(begin, eol)});
                    begin = eol;
                    continue;
                }
            }
        } while (begin != end);
    End:
        return {std::move(parts)};
    }

    File parseFile(OpenCL::Parser::Tokens::const_iterator& begin, OpenCL::Parser::Tokens::const_iterator end,
                   const OpenCL::SourceObject& sourceObject, std::ostream* reporter)
    {
        std::vector<Group> groups;
        while (begin != end)
        {
            groups.push_back(parseGroup(begin, end, sourceObject, reporter));
        }
        return {std::move(groups)};
    }
} // namespace

OpenCL::SourceObject OpenCL::PP::preprocess(const SourceObject& sourceObject, std::ostream* reporter)
{
    auto begin = sourceObject.begin();
    auto file = parseFile(begin, sourceObject.end(), sourceObject, reporter);

    return SourceObject{{}};
}
