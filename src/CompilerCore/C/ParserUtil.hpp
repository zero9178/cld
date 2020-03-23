#pragma once

#include <utility>

#include "ErrorMessages.hpp"
#include "Parser.hpp"

namespace cld::Parser
{
bool isAssignment(Lexer::TokenType type);

template <class T = void>
bool expect(Lexer::TokenType expected, std::vector<Lexer::Token>::const_iterator& curr,
            std::vector<Lexer::Token>::const_iterator end, Context& context, std::vector<Message> additional = {},
            T* value = nullptr)
{
    (void)value;
    if (curr == end || curr->getTokenType() != expected)
    {
        if (curr == end)
        {
            context.log({Message::error(cld::ErrorMessages::Parser::EXPECTED_N.args(Lexer::tokenName(expected)), curr,
                                        Modifier{end - 1, end, Modifier::InsertAtEnd, Lexer::tokenValue(expected)})});
        }
        else
        {
            context.log({Message::error(cld::ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                            Lexer::tokenName(expected), '\'' + curr->getRepresentation() + '\''),
                                        curr, Modifier{curr, curr + 1, Modifier::PointAtBeginning})});
        }
        context.log(std::move(additional));
        return false;
    }
    else
    {
        if constexpr (!std::is_void_v<T>)
        {
            if (value)
            {
                *value = std::get<T>(curr->getValue());
            }
        }
        curr++;
        return true;
    }
}

constexpr Context::TokenBitSet firstPostfixSet = Context::fromTokenTypes(
    cld::Lexer::TokenType::Arrow, cld::Lexer::TokenType::Dot, cld::Lexer::TokenType::OpenSquareBracket,
    cld::Lexer::TokenType::OpenParentheses, cld::Lexer::TokenType::Increment, cld::Lexer::TokenType::Decrement);

constexpr Context::TokenBitSet assignmentSet = Context::fromTokenTypes(
    Lexer::TokenType::Assignment, Lexer::TokenType::PlusAssign, Lexer::TokenType::MinusAssign,
    Lexer::TokenType::DivideAssign, Lexer::TokenType::MultiplyAssign, Lexer::TokenType::ModuloAssign,
    Lexer::TokenType::ShiftLeftAssign, Lexer::TokenType::ShiftRightAssign, Lexer::TokenType::BitAndAssign,
    Lexer::TokenType::BitOrAssign, Lexer::TokenType::BitXorAssign);

constexpr Context::TokenBitSet firstSpecifierQualifierSet = Context::fromTokenTypes(
    Lexer::TokenType::VoidKeyword, Lexer::TokenType::CharKeyword, Lexer::TokenType::ShortKeyword,
    Lexer::TokenType::IntKeyword, Lexer::TokenType::LongKeyword, Lexer::TokenType::FloatKeyword,
    Lexer::TokenType::DoubleKeyword, Lexer::TokenType::SignedKeyword, Lexer::TokenType::UnsignedKeyword,
    Lexer::TokenType::EnumKeyword, Lexer::TokenType::StructKeyword, Lexer::TokenType::UnionKeyword,
    Lexer::TokenType::ConstKeyword, Lexer::TokenType::RestrictKeyword, Lexer::TokenType::VolatileKeyword,
    Lexer::TokenType::InlineKeyword, Lexer::TokenType::Identifier);

constexpr Context::TokenBitSet firstDeclarationSpecifierSet = Context::fromTokenTypes(
    Lexer::TokenType::TypedefKeyword, Lexer::TokenType::ExternKeyword, Lexer::TokenType::StaticKeyword,
    Lexer::TokenType::AutoKeyword, Lexer::TokenType::RegisterKeyword, Lexer::TokenType::VoidKeyword,
    Lexer::TokenType::CharKeyword, Lexer::TokenType::ShortKeyword, Lexer::TokenType::IntKeyword,
    Lexer::TokenType::LongKeyword, Lexer::TokenType::FloatKeyword, Lexer::TokenType::DoubleKeyword,
    Lexer::TokenType::SignedKeyword, Lexer::TokenType::UnsignedKeyword, Lexer::TokenType::EnumKeyword,
    Lexer::TokenType::StructKeyword, Lexer::TokenType::UnionKeyword, Lexer::TokenType::ConstKeyword,
    Lexer::TokenType::RestrictKeyword, Lexer::TokenType::VolatileKeyword, Lexer::TokenType::InlineKeyword,
    Lexer::TokenType::Identifier);

constexpr Context::TokenBitSet firstPointerSet = Context::fromTokenTypes(Lexer::TokenType::Asterisk);

constexpr Context::TokenBitSet firstParameterListSet = firstDeclarationSpecifierSet;

constexpr Context::TokenBitSet firstDirectAbstractDeclaratorSet =
    Context::fromTokenTypes(Lexer::TokenType::OpenParentheses, Lexer::TokenType::OpenSquareBracket);

constexpr Context::TokenBitSet firstAbstractDeclaratorSet = firstPointerSet | firstDirectAbstractDeclaratorSet;

constexpr Context::TokenBitSet firstParameterTypeListSet = firstParameterListSet;

constexpr Context::TokenBitSet firstDirectDeclaratorSet =
    Context::fromTokenTypes(Lexer::TokenType::Identifier, Lexer::TokenType::OpenParentheses);

constexpr Context::TokenBitSet firstDeclaratorSet = firstPointerSet | firstDirectDeclaratorSet;

constexpr Context::TokenBitSet firstDeclarationSet = firstDeclarationSpecifierSet;

constexpr Context::TokenBitSet firstExpressionSet = Context::fromTokenTypes(
    Lexer::TokenType::OpenParentheses, Lexer::TokenType::Identifier, Lexer::TokenType::Literal,
    Lexer::TokenType::StringLiteral, Lexer::TokenType::Increment, Lexer::TokenType::Decrement, Lexer::TokenType::Minus,
    Lexer::TokenType::Plus, Lexer::TokenType::Ampersand, Lexer::TokenType::BitWiseNegation,
    Lexer::TokenType::LogicalNegation, Lexer::TokenType::SizeofKeyword);

constexpr Context::TokenBitSet firstInitializerSet =
    firstExpressionSet | Context::fromTokenTypes(Lexer::TokenType::OpenBrace);

constexpr Context::TokenBitSet firstInitializerListSet =
    firstInitializerSet | Context::fromTokenTypes(Lexer::TokenType::OpenSquareBracket, Lexer::TokenType::Dot);

constexpr Context::TokenBitSet firstStatementSet =
    Context::fromTokenTypes(Lexer::TokenType::IfKeyword, Lexer::TokenType::ForKeyword, Lexer::TokenType::OpenBrace,
                            Lexer::TokenType::SwitchKeyword, Lexer::TokenType::ContinueKeyword,
                            Lexer::TokenType::BreakKeyword, Lexer::TokenType::CaseKeyword,
                            Lexer::TokenType::DefaultKeyword, Lexer::TokenType::Identifier, Lexer::TokenType::DoKeyword,
                            Lexer::TokenType::WhileKeyword, Lexer::TokenType::ReturnKeyword,
                            Lexer::TokenType::GotoKeyword, Lexer::TokenType::SemiColon)
    | firstExpressionSet;

constexpr Context::TokenBitSet firstCompoundItem = firstDeclarationSet | firstStatementSet;

constexpr Context::TokenBitSet firstFunctionDefinitionSet = firstDeclarationSpecifierSet;

constexpr Context::TokenBitSet firstExternalDeclarationSet = firstDeclarationSet | firstFunctionDefinitionSet;

bool firstIsInExternalDeclaration(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInFunctionDefinition(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInDeclaration(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInDeclarationSpecifier(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInSpecifierQualifier(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInDeclarator(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInDirectDeclarator(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInParameterTypeList(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInAbstractDeclarator(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInDirectAbstractDeclarator(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInParameterList(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInPointer(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInCompoundItem(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInInitializer(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInInitializerList(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInStatement(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInAssignmentExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInConditionalExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInLogicalOrExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInLogicalAndExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInBitOrExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInBitXorExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInBitAndExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInEqualityExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInRelationalExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInShiftExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInAdditiveExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInTerm(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInTypeName(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInCastExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInUnaryExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInPostFixExpression(const Lexer::Token& token, const cld::Parser::Context& context);

bool firstIsInPrimaryExpression(const Lexer::Token& token, const cld::Parser::Context& context);
} // namespace cld::Parser
