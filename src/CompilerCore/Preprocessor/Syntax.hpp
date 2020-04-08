#pragma once

#include <CompilerCore/C/Syntax.hpp>

#include <memory>
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
        std::string identifier;
    };
    struct IfDefTag final
    {
        std::string identifier;
    };
    using variant = std::variant<IfnDefTag, IfDefTag, std::vector<Lexer::Token>>;
    variant ifs;
    std::unique_ptr<Group> optionalGroup;
};

/**
 * <ElIfGroup> ::= <TokenType::Pound> <Identifier=elif> <TOKENS> <TokenType::Newline> [ <Group> ]
 */
struct ElIfGroup final
{
    std::vector<Lexer::Token> constantExpression;
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
    Lexer::TokenIterator identifierPos;
    /**
     * Its an optional to differentiate between an empty identifier list and no identifier list
     */
    std::optional<std::vector<std::string>> identifierList;
    bool hasEllipse;
    Lexer::TokenIterator replacementBegin;
    Lexer::TokenIterator replacementEnd;
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
        Lexer::TokenIterator begin;
        Lexer::TokenIterator end;
    };

    struct LineTag final
    {
        Lexer::TokenIterator begin;
        Lexer::TokenIterator end;
    };

    struct ErrorTag final
    {
        Lexer::TokenIterator begin;
        Lexer::TokenIterator end;
    };

    struct PragmaTag final
    {
        Lexer::TokenIterator begin;
        Lexer::TokenIterator end;
    };

    std::variant<IncludeTag, LineTag, ErrorTag, PragmaTag, std::string, DefineDirective> variant;
};

/**
 * <NonDirective> ::= <TokenType::Pound> <TOKEN> { <TOKEN> } <TokenType::Newline>
 */
struct NonDirective final
{
    Lexer::TokenIterator begin;
    Lexer::TokenIterator end;
};

/**
 * <GroupPart> ::= <IfSection> | <ControlLine> | <TextLine> | <NonDirective>
 */
using GroupPart = std::variant<IfSection, ControlLine, std::vector<Lexer::Token>, NonDirective>;

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
