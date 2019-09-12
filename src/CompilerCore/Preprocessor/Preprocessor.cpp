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
     *                 | <TokenType::Pound> <NEWLINE>
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
            begin = expEnd;
            result = IfGroup{std::move(constantExpression), nullptr};
        }
        else if (begin->getTokenType() == OpenCL::Lexer::TokenType::Identifier)
        {
            const auto& value = std::get<std::string>(begin->getValue());
            begin++;
            if (begin == end)
            {
                if (reporter)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N.args("identifier"), sourceObject.getLineStart(begin),
                        sourceObject.getLineEnd(begin),
                        OpenCL::Modifier(begin - 1, begin, OpenCL::Modifier::InsertAtEnd));
                }
                return result;
            }
            else if (begin->getTokenType() != OpenCL::Lexer::TokenType::Identifier)
            {
                if (reporter)
                {
                    *reporter << OpenCL::Message::error(
                        OpenCL::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args("identifier",
                                                                                    '\'' + begin->emitBack() + '\''),
                        sourceObject.getLineStart(begin), sourceObject.getLineEnd(begin),
                        OpenCL::Modifier(begin, begin + 1, OpenCL::Modifier::PointAtBeginning));
                }
                return result;
            }
            const auto& identifier = std::get<std::string>(begin->getValue());
            begin++;
            // One may argue that if begin == end we technically dont have a newline either but id rather error later
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

    IfSection parseIfSection(OpenCL::Parser::Tokens::const_iterator& begin, OpenCL::Parser::Tokens::const_iterator end,
                             const OpenCL::SourceObject& sourceObject, std::ostream* reporter)
    {
        assert(begin != end);
        auto ifGroup = parseIfGroup(begin, end, sourceObject, reporter);

        return {};
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
                    const auto& value = std::get<std::string>(begin->getValue());
                }
                default:
                {
                }
            }
        } while (begin != end);
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
