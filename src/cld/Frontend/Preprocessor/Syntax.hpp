#pragma once

#include <cld/Frontend/Compiler/Lexer.hpp>

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
    using variant = std::variant<IfnDefTag, IfDefTag, llvm::ArrayRef<Lexer::PPToken>>;
    Lexer::PPTokenIterator ifsToken;
    variant ifs;
    std::unique_ptr<Group> optionalGroup;
};

/**
 * <ElIfGroup> ::= <TokenType::Pound> <Identifier=elif> <TOKENS> <TokenType::Newline> [ <Group> ]
 */
struct ElIfGroup final
{
    Lexer::PPTokenIterator elifToken;
    llvm::ArrayRef<Lexer::PPToken> constantExpression;
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
 * Due to conditional exclusion it must only be fully parsed in the semantics stage
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
    Lexer::PPTokenIterator defineToken;
    llvm::ArrayRef<Lexer::PPToken> tokens;
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
        Lexer::PPTokenIterator includeToken;
        llvm::ArrayRef<Lexer::PPToken> tokens;
    };

    struct LineTag final
    {
        Lexer::PPTokenIterator lineToken;
        llvm::ArrayRef<Lexer::PPToken> tokens;
    };

    struct ErrorTag final
    {
        Lexer::PPTokenIterator errorToken;
        llvm::ArrayRef<Lexer::PPToken> tokens;
    };

    struct PragmaTag final
    {
        llvm::ArrayRef<Lexer::PPToken> tokens;
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
    llvm::ArrayRef<Lexer::PPToken> tokens;
};

struct UnknownDirective
{
    Lexer::PPTokenIterator identifier;
};

/**
 * <GroupPart> ::= <IfSection> | <ControlLine> | <TextLine> | <NonDirective> | <UnknownDirective>
 */
using GroupPart = std::variant<IfSection, ControlLine, TextBlock, NonDirective, UnknownDirective>;

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
