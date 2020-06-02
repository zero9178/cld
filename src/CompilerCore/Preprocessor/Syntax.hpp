#pragma once

#include <CompilerCore/C/Lexer.hpp>

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace cld::PP
{
struct Group;

/**
 * <IfGroup> ::= <TokenType::Pound> <Identifier=if> <TOKENS> <TokenType::Newline> [ <Group> ]
 *             | <TokenType::Pound> <Identifier=ifdef> <TokenType::Identifier> <TokenType::Newline> [ <Group> ]
 *             | <TokenType::Pound> <Identifier=ifndef> <TokenType::Identifier> <TokenType::Newline> [ <Group> ]
 */
struct IfGroup final
{
    struct IfnDefTag final
    {
        std::string_view identifier;
    };
    struct IfDefTag final
    {
        std::string_view identifier;
    };
    using variant = std::variant<IfnDefTag, IfDefTag, std::vector<Lexer::PPToken>>;
    variant ifs;
    std::unique_ptr<Group> optionalGroup;
};

/**
 * <ElIfGroup> ::= <TokenType::Pound> <Identifier=elif> <TOKENS> <TokenType::Newline> [ <Group> ]
 */
struct ElIfGroup final
{
    std::vector<Lexer::PPToken> constantExpression;
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
 * <IfSection> ::= <IfGroup> {<ElIfGroup>} [<ElseGroup>] <TokenType::Pound> <Identifier=endif> <TokenType::Newline>
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
 *  <DefineDirectives> ::= <TokenType::Pound> <Identifier=define> <TokenType::Identifier> [<TOKENS>]
 *                           <TokenType::Newline>
 *                       | <TokenType::Pound> <Identifier=define> <TokenType::Identifier> <TokenType::OpenParentheses>
 *                           [ <TokenType::Identifier { <TokenType::Comma> <TokenType::Identifier> } ]
 *                           <TokenType::CloseParentheses> [<TOKENS>] <TokenType::Newline>
 *                       | <TokenType::Pound> <Identifier=define> <TokenType::Identifier> <TokenType::OpenParentheses>
 *                         <TokenType::Ellipse> <TokenType::CloseParentheses> [ <TOKENS> ] <TokenType::Newline>
 *                       | <TokenType::Pound> <Identifier=define> <TokenType::Identifier> <TokenType::OpenParentheses>
 *                         [ <TokenType::Identifier { <TokenType::Comma> <TokenType::Identifier> } ] <TokenType::Comma>
 *                         <TokenType::Ellipse> <TokenType::CloseParentheses> [ <TOKENS> ] <TokenType::Newline>
 */
struct DefineDirective final
{
    Lexer::PPTokenIterator identifierPos;
    /**
     * Its an optional to differentiate between an empty identifier list and no identifier list
     */
    std::optional<std::vector<std::string_view>> identifierList;
    bool hasEllipse;
    Lexer::PPTokenIterator replacementBegin;
    Lexer::PPTokenIterator replacementEnd;
};

/**
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
    struct IncludeTag final
    {
        Lexer::PPTokenIterator begin;
        Lexer::PPTokenIterator end;
    };

    struct LineTag final
    {
        Lexer::PPTokenIterator begin;
        Lexer::PPTokenIterator end;
    };

    struct ErrorTag final
    {
        Lexer::PPTokenIterator begin;
        Lexer::PPTokenIterator end;
    };

    struct PragmaTag final
    {
        Lexer::PPTokenIterator begin;
        Lexer::PPTokenIterator end;
    };

    std::variant<IncludeTag, LineTag, ErrorTag, PragmaTag, Lexer::PPTokenIterator, DefineDirective> variant;
};

/**
 * <NonDirective> ::= <TokenType::Pound> <TOKEN> { <TOKEN> } <TokenType::Newline>
 */
struct NonDirective final
{
    Lexer::PPTokenIterator begin;
    Lexer::PPTokenIterator end;
};

/**
 * <TextBlock> ::= <TOKEN> <TokenType::Newline> { <TOKEN> <TokenType::Newline> }
 */
struct TextBlock final
{
    std::vector<Lexer::PPToken> tokens;
    std::vector<std::uint64_t> ends;
};

/**
 * <GroupPart> ::= <IfSection> | <ControlLine> | <TextLine> | <NonDirective>
 */
using GroupPart = std::variant<IfSection, ControlLine, TextBlock, NonDirective>;

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

} // namespace cld::PP
