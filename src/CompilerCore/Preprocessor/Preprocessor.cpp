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

    struct State;

    std::vector<OpenCL::Lexer::Token> processGroup(OpenCL::SourceObject::const_iterator& begin,
                                                   OpenCL::SourceObject::const_iterator end,
                                                   const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                   State* state);

    std::vector<OpenCL::Lexer::Token>::const_iterator
        findEOLWithOutBackslash(OpenCL::SourceObject::const_iterator begin, OpenCL::SourceObject::const_iterator end)
    {
        auto result = begin;
        do
        {
            result = OpenCL::findEOL(result, end);
        } while ((result - 1)->getTokenType() == OpenCL::Lexer::TokenType::Backslash);
        return result;
    }

    std::vector<OpenCL::Lexer::Token> filterForNewlineAndBackslash(OpenCL::SourceObject::const_iterator begin,
                                                                   OpenCL::SourceObject::const_iterator end)
    {
        std::vector<OpenCL::Lexer::Token> result(begin, end);
        result.erase(std::remove_if(result.begin(), result.end(),
                                    [](const OpenCL::Lexer::Token& token) {
                                        return token.getTokenType() == OpenCL::Lexer::TokenType::Newline
                                               || token.getTokenType() == OpenCL::Lexer::TokenType::Backslash;
                                    }),
                     result.end());
        return result;
    }

    /**
     * <IfGroup> ::= <TokenType::Pound> <Identifier=if> <ConstantExpression> <TokenType::Newline> [ <Group> ]
     *             | <TokenType::Pound> <Identifier=ifdef> <TokenType::Identifier> <TokenType::Newline> [ <Group> ]
     *             | <TokenType::Pound> <Identifier=ifndef> <TokenType::Identifier> <TokenType::Newline> [ <Group> ]
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
     * <ElIfGroup> ::= <TokenType::Pound> <Identifier=elif> <ConstantExpression> <TokenType::Newline> [ <Group> ]
     */
    struct ElIfGroup final
    {
        OpenCL::Syntax::ConstantExpression constantExpression;
        std::unique_ptr<Group> optionalGroup;
    };

    /**
     * <ElseGroup> ::= <TokenType::Pound> <Identifier=else> <TokenType::Newline> [<Group>]
     */
    struct ElseGroup final
    {
        std::unique_ptr<Group> optionalGroup;
    };

    /**
     * <IfSection> ::= <IfGroup> {<ElIfGroup>} [<ElseGroup] <TokenType::Pound> <Identifier=endif> <TokenType::Newline>
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
     * <DefineDirectives> ::= <TokenType::Pound> <Identifier=define> <TokenType::Identifier> [<TOKENS>]
     * <TokenType::Newline> | <TokenType::Pound> <Identifier=define> <TokenType::Identifier>
     *                        <TokenType::OpenParentheses> [ <TokenType::Identifier { <TokenType::Comma>
     * <TokenType::Identifier> } ] <TokenType::CloseParentheses> [<TOKENS>] <TokenType::Newline> | <TokenType::Pound>
     * <Identifier=define> <TokenType::Identifier> <TokenType::OpenParentheses> <TokenType::Ellipse>
     * <TokenType::CloseParentheses> [ <TOKENS> ] <TokenType::Newline> | <TokenType::Pound> <Identifier=define>
     * <TokenType::Identifier> <TokenType::OpenParentheses> [ <TokenType::Identifier { <TokenType::Comma>
     * <TokenType::Identifier> } ] <TokenType::Comma> <TokenType::Ellipse> <TokenType::CloseParentheses> [ <TOKENS> ]
     * <TokenType::Newline>
     *
     * <ControlLine> ::= <TokenType::Pound> <Identifier=include> <TOKENS> <TokenType::Newline>
     *                 | <DefineDirectives>
     *                 | <TokenType::Pound> <Identifier=undef> <TokenType::Identifier> <TokenType::Newline>
     *                 | <TokenType::Pound> <Identifier=line> <TOKENS> <TokenType::Newline>
     *                 | <TokenType::Pound> <Identifier=error> [<TOKENS>] <TokenType::Newline>
     *                 | <TokenType::Pound> <Identifier=pragma> [<TOKENS>] <TokenType::Newline>
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

    bool expect(OpenCL::Lexer::TokenType tokenType, OpenCL::SourceObject::const_iterator begin,
                OpenCL::SourceObject::const_iterator end, const OpenCL::SourceObject& sourceObject,
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

    struct State
    {
        std::unordered_map<std::string, ControlLine::DefineDirective> defines;
    };

    std::vector<OpenCL::Lexer::Token> processIfGroup(OpenCL::SourceObject::const_iterator& begin,
                                                     OpenCL::SourceObject::const_iterator end,
                                                     const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                     State* state)
    {
        IfGroup result{};
        assert(begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier);
        const auto& value = std::get<std::string>(begin->getValue());
        assert(value == "if" || value == "ifdef" || value == "ifndef");
        begin++;
        if (value == "if")
        {
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
        else
        {
            if (!expect(OpenCL::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
            {
                return result;
            }
            const auto& identifier = std::get<std::string>(begin->getValue());
            begin++;
            if (begin != end && !expect(OpenCL::Lexer::TokenType::Newline, begin, end, sourceObject, reporter))
            {
                return result;
            }
            if (value == "ifdef")
            {
                result = IfGroup{IfGroup::IfDefTag{identifier}, nullptr};
            }
            else if (value == "ifndef")
            {
                result = IfGroup{IfGroup::IfnDefTag{identifier}, nullptr};
            }
            else
            {
                OPENCL_UNREACHABLE;
            }
        }

        if (begin != end)
        {
            result.optionalGroup = std::make_unique<Group>(processGroup(begin, end, sourceObject, reporter));
        }
        return result;
    }

    std::vector<OpenCL::Lexer::Token> processElIfGroup(OpenCL::SourceObject::const_iterator& begin,
                                                       OpenCL::SourceObject::const_iterator end,
                                                       const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                       State* state)
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
            result.optionalGroup = std::make_unique<Group>(processGroup(begin, end, sourceObject, reporter));
        }
        return result;
    }

    std::vector<OpenCL::Lexer::Token> processElseGroup(OpenCL::SourceObject::const_iterator& begin,
                                                       OpenCL::SourceObject::const_iterator end,
                                                       const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                       State* state)
    {
        assert(begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
               && std::get<std::string>(begin->getValue()) == "else");
        begin++;
        if (!expect(OpenCL::Lexer::TokenType::Newline, begin, end, sourceObject, reporter))
        {
            return {{}};
        }
        ElseGroup result{};
        if (begin != end)
        {
            result.optionalGroup = std::make_unique<Group>(processGroup(begin, end, sourceObject, reporter));
        }
        return result;
    }

    std::vector<OpenCL::Lexer::Token> processIfSection(OpenCL::SourceObject::const_iterator& begin,
                                                       OpenCL::SourceObject::const_iterator end,
                                                       const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                       State* state)
    {
        assert(begin != end);
        auto ifGroup = processIfGroup(begin, end, sourceObject, reporter);
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
            elifGroups.push_back(processElIfGroup(begin, end, sourceObject, reporter));
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
        if (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier
            && std::get<std::string>(begin->getValue()) == "else")
        {
            result.optionalElseGroup = processElseGroup(begin, end, sourceObject, reporter);
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

    std::vector<OpenCL::Lexer::Token> processDefineDirective(OpenCL::SourceObject::const_iterator& begin,
                                                             OpenCL::SourceObject::const_iterator end,
                                                             const OpenCL::SourceObject& sourceObject,
                                                             std::ostream* reporter, State* state)
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
            std::vector<OpenCL::Lexer::Token> tokens(begin, eol);
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

    std::vector<OpenCL::Lexer::Token> processControlLine(OpenCL::SourceObject::const_iterator& begin,
                                                         OpenCL::SourceObject::const_iterator end,
                                                         const OpenCL::SourceObject& sourceObject,
                                                         std::ostream* reporter, State* state)
    {
        const auto& value = std::get<std::string>(begin->getValue());
        if (value == "define")
        {
            return processDefineDirective(begin, end, sourceObject, reporter, state);
        }
        else if (value == "undef")
        {
            begin++;
            if (!expect(OpenCL::Lexer::TokenType::Identifier, begin, end, sourceObject, reporter))
            {
                return {};
            }
            if (state)
            {
                state->defines.erase(std::get<std::string>(begin->getValue()));
            }
            begin++;
            if (begin != end)
            {
                expect(OpenCL::Lexer::TokenType::Newline, begin, end, sourceObject, reporter);
            }
            return {};
        }
        else
        {
            begin++;
            auto newline = findEOLWithOutBackslash(begin, end);
            auto tokens = filterForNewlineAndBackslash(begin, newline);
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
                // TODO: Process tokens to open file, tokenize it and call processFile with it
                return {};
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
                return {};
            }
            else if (value == "error")
            {
                if (reporter)
                {
                    // TODO reporting
                }
                return {};
            }
            else if (value == "pragma")
            {
                return {};
            }
            else
            {
                OPENCL_UNREACHABLE;
            }
        }
    }

    std::vector<OpenCL::Lexer::Token> processGroup(OpenCL::SourceObject::const_iterator& begin,
                                                   OpenCL::SourceObject::const_iterator end,
                                                   const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                   State* state)
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

        std::vector<OpenCL::Lexer::Token> result;
        do
        {
            if (begin->getTokenType() != OpenCL::Lexer::TokenType::Pound)
            {
                auto eol = findEOLWithOutBackslash(begin, end);
                auto vector = filterForNewlineAndBackslash(begin, eol);
                result.insert(result.end(), vector.begin(), vector.end());
                begin = eol;
                continue;
            }
            begin++;
            if (begin == end || begin->getTokenType() == OpenCL::Lexer::TokenType::Newline)
            {
                continue;
            }
            if (begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier)
            {
                const auto& value = std::get<std::string>(begin->getValue());
                if (value == "elif" || value == "endif" || value == "else")
                {
                    begin--;
                    break;
                }
                if (value == "ifdef" || value == "ifndef" || value == "if")
                {
                    auto vector = processIfSection(begin, end, sourceObject, reporter, state);
                    result.insert(result.end(), vector.begin(), vector.end());
                    continue;
                }
                if (value == "include" || value == "undef" || value == "line" || value == "error" || value == "pragma"
                    || value == "define")
                {
                    auto vector = processControlLine(begin, end, sourceObject, reporter, state);
                    result.insert(result.end(), vector.begin(), vector.end());
                    continue;
                }
            }
            auto eol = findEOLWithOutBackslash(begin, end);
            auto vector = filterForNewlineAndBackslash(begin, eol);
            result.insert(result.end(), vector.begin(), vector.end());
            begin = eol;
        } while (begin != end);
        return result;
    }

    std::vector<OpenCL::Lexer::Token> processFile(OpenCL::SourceObject::const_iterator& begin,
                                                  OpenCL::SourceObject::const_iterator end,
                                                  const OpenCL::SourceObject& sourceObject, std::ostream* reporter,
                                                  State* state)
    {
        std::vector<OpenCL::Lexer::Token> result;
        while (begin != end)
        {
            auto vector = processGroup(begin, end, sourceObject, reporter, state);
            result.insert(result.end(), vector.begin(), vector.end());
        }
        return result;
    }
} // namespace

OpenCL::SourceObject OpenCL::PP::preprocess(const SourceObject& sourceObject, std::ostream* reporter)
{
    auto begin = sourceObject.begin();
    State state;
    auto file = processFile(begin, sourceObject.end(), sourceObject, reporter, &state);

    return SourceObject{{}};
}
