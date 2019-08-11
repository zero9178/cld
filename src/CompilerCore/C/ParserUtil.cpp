#include "ParserUtil.hpp"

#include <algorithm>

bool OpenCL::Parser::isAssignment(Lexer::TokenType type)
{
    return type == Lexer::TokenType::Assignment || type == Lexer::TokenType::PlusAssign
           || type == Lexer::TokenType::MinusAssign || type == Lexer::TokenType::DivideAssign
           || type == Lexer::TokenType::MultiplyAssign || type == Lexer::TokenType::ModuloAssign
           || type == Lexer::TokenType::ShiftLeftAssign || type == Lexer::TokenType::ShiftRightAssign
           || type == Lexer::TokenType::BitAndAssign || type == Lexer::TokenType::BitOrAssign
           || type == Lexer::TokenType::BitXorAssign;
}

bool OpenCL::Parser::firstIsInExternalDeclaration(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInDeclaration(token, context) || firstIsInFunctionDefinition(token, context);
}

bool OpenCL::Parser::firstIsInFunctionDefinition(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInDeclarationSpecifier(token, context);
}

bool OpenCL::Parser::firstIsInDeclaration(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInDeclarationSpecifier(token, context);
}

bool OpenCL::Parser::firstIsInDeclarationSpecifier(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    switch (token.getTokenType())
    {
        case Lexer::TokenType::TypedefKeyword:
        case Lexer::TokenType::ExternKeyword:
        case Lexer::TokenType::StaticKeyword:
        case Lexer::TokenType::AutoKeyword:
        case Lexer::TokenType::RegisterKeyword:
        case Lexer::TokenType::VoidKeyword:
        case Lexer::TokenType::CharKeyword:
        case Lexer::TokenType::ShortKeyword:
        case Lexer::TokenType::IntKeyword:
        case Lexer::TokenType::LongKeyword:
        case Lexer::TokenType::FloatKeyword:
        case Lexer::TokenType::DoubleKeyword:
        case Lexer::TokenType::SignedKeyword:
        case Lexer::TokenType::UnsignedKeyword:
        case Lexer::TokenType::EnumKeyword:
        case Lexer::TokenType::StructKeyword:
        case Lexer::TokenType::UnionKeyword:
        case Lexer::TokenType::ConstKeyword:
        case Lexer::TokenType::RestrictKeyword:
        case Lexer::TokenType::VolatileKeyword:
        case Lexer::TokenType::InlineKeyword: return true;
        case Lexer::TokenType::Identifier: return context.isTypedefInScope(std::get<std::string>(token.getValue()));
        default: return false;
    }
}

bool OpenCL::Parser::firstIsInSpecifierQualifier(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    switch (token.getTokenType())
    {
        case Lexer::TokenType::VoidKeyword:
        case Lexer::TokenType::CharKeyword:
        case Lexer::TokenType::ShortKeyword:
        case Lexer::TokenType::IntKeyword:
        case Lexer::TokenType::LongKeyword:
        case Lexer::TokenType::FloatKeyword:
        case Lexer::TokenType::DoubleKeyword:
        case Lexer::TokenType::SignedKeyword:
        case Lexer::TokenType::UnsignedKeyword:
        case Lexer::TokenType::EnumKeyword:
        case Lexer::TokenType::StructKeyword:
        case Lexer::TokenType::UnionKeyword:
        case Lexer::TokenType::ConstKeyword:
        case Lexer::TokenType::RestrictKeyword:
        case Lexer::TokenType::VolatileKeyword: return true;
        case Lexer::TokenType::Identifier: return context.isTypedefInScope(std::get<std::string>(token.getValue()));
        default: return false;
    }
}

bool OpenCL::Parser::firstIsInDeclarator(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInPointer(token, context) || firstIsInDirectDeclarator(token, context);
}

bool OpenCL::Parser::firstIsInDirectDeclarator(const Lexer::Token& token, const OpenCL::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::Identifier
           || token.getTokenType() == Lexer::TokenType::OpenParenthese;
}

[[maybe_unused]] bool OpenCL::Parser::firstIsInParameterTypeList(const Lexer::Token& token,
                                                                 const OpenCL::Parser::Context& context)
{
    return firstIsInParameterList(token, context);
}

bool OpenCL::Parser::firstIsInAbstractDeclarator(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInPointer(token, context) || firstIsInDirectAbstractDeclarator(token, context);
}

bool OpenCL::Parser::firstIsInDirectAbstractDeclarator(const Lexer::Token& token, const OpenCL::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::OpenParenthese
           || token.getTokenType() == Lexer::TokenType::OpenSquareBracket;
}

bool OpenCL::Parser::firstIsInParameterList(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInDeclarationSpecifier(token, context);
}

bool OpenCL::Parser::firstIsInPointer(const Lexer::Token& token, const OpenCL::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::Asterisk;
}

[[maybe_unused]] bool OpenCL::Parser::firstIsInStructOrUnionSpecifier(const Lexer::Token& token,
                                                                      const OpenCL::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::StructKeyword
           || token.getTokenType() == Lexer::TokenType::UnionKeyword;
}

[[maybe_unused]] bool OpenCL::Parser::firstIsInEnumSpecifier(const Lexer::Token& token, const OpenCL::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::EnumKeyword;
}

[[maybe_unused]] bool OpenCL::Parser::firstIsInEnumDeclaration(const Lexer::Token& token,
                                                               const OpenCL::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::EnumKeyword;
}

[[maybe_unused]] bool OpenCL::Parser::firstIsInCompoundStatement(const Lexer::Token& token,
                                                                 const OpenCL::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::OpenBrace;
}

[[maybe_unused]] bool OpenCL::Parser::firstIsInCompoundItem(const Lexer::Token& token,
                                                            const OpenCL::Parser::Context& context)
{
    return firstIsInDeclaration(token, context) || firstIsInStatement(token, context);
}

bool OpenCL::Parser::firstIsInInitializer(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInAssignmentExpression(token, context) || token.getTokenType() == Lexer::TokenType::OpenBrace;
}

[[maybe_unused]] bool OpenCL::Parser::firstIsInInitializerList(const Lexer::Token& token,
                                                               const OpenCL::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::OpenSquareBracket || token.getTokenType() == Lexer::TokenType::Dot
           || firstIsInInitializer(token, context);
}

bool OpenCL::Parser::firstIsInStatement(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::IfKeyword || token.getTokenType() == Lexer::TokenType::ForKeyword
           || token.getTokenType() == Lexer::TokenType::SwitchKeyword
           || token.getTokenType() == Lexer::TokenType::ContinueKeyword
           || token.getTokenType() == Lexer::TokenType::BreakKeyword
           || token.getTokenType() == Lexer::TokenType::CaseKeyword
           || token.getTokenType() == Lexer::TokenType::DefaultKeyword
           || token.getTokenType() == Lexer::TokenType::Identifier
           || token.getTokenType() == Lexer::TokenType::DoKeyword
           || token.getTokenType() == Lexer::TokenType::WhileKeyword
           || token.getTokenType() == Lexer::TokenType::ReturnKeyword
           || token.getTokenType() == Lexer::TokenType::GotoKeyword
           || token.getTokenType() == Lexer::TokenType::SemiColon || firstIsInExpression(token, context);
}

bool OpenCL::Parser::firstIsInExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInAssignmentExpression(token, context);
}

bool OpenCL::Parser::firstIsInAssignmentExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInUnaryExpression(token, context) || firstIsInConditionalExpression(token, context);
}

bool OpenCL::Parser::firstIsInConditionalExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInLogicalOrExpression(token, context);
}

bool OpenCL::Parser::firstIsInLogicalOrExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInLogicalAndExpression(token, context);
}

bool OpenCL::Parser::firstIsInLogicalAndExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInBitOrExpression(token, context);
}

bool OpenCL::Parser::firstIsInBitOrExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInBitXorExpression(token, context);
}

bool OpenCL::Parser::firstIsInBitXorExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInBitAndExpression(token, context);
}

bool OpenCL::Parser::firstIsInBitAndExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInEqualityExpression(token, context);
}

bool OpenCL::Parser::firstIsInEqualityExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInRelationalExpression(token, context);
}

bool OpenCL::Parser::firstIsInRelationalExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInShiftExpression(token, context);
}

bool OpenCL::Parser::firstIsInShiftExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInAdditiveExpression(token, context);
}

bool OpenCL::Parser::firstIsInAdditiveExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInTerm(token, context);
}

bool OpenCL::Parser::firstIsInTerm(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInCastExpression(token, context);
}

[[maybe_unused]] bool OpenCL::Parser::firstIsInTypeName(const Lexer::Token& token,
                                                        const OpenCL::Parser::Context& context)
{
    return firstIsInSpecifierQualifier(token, context);
}

bool OpenCL::Parser::firstIsInCastExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::OpenParenthese || firstIsInUnaryExpression(token, context);
}

bool OpenCL::Parser::firstIsInUnaryExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return firstIsInPostFixExpression(token, context) || token.getTokenType() == Lexer::TokenType::Increment
           || token.getTokenType() == Lexer::TokenType::Decrement || token.getTokenType() == Lexer::TokenType::Ampersand
           || token.getTokenType() == Lexer::TokenType::Plus || token.getTokenType() == Lexer::TokenType::Minus
           || token.getTokenType() == Lexer::TokenType::BitWiseNegation
           || token.getTokenType() == Lexer::TokenType::LogicalNegation
           || token.getTokenType() == Lexer::TokenType::SizeofKeyword;
}

bool OpenCL::Parser::firstIsInPostFixExpression(const Lexer::Token& token, const OpenCL::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::OpenParenthese || firstIsInPrimaryExpression(token, context);
}

bool OpenCL::Parser::firstIsInPrimaryExpression(const Lexer::Token& token, const OpenCL::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::OpenParenthese
           || token.getTokenType() == Lexer::TokenType::Identifier || token.getTokenType() == Lexer::TokenType::Literal
           || token.getTokenType() == Lexer::TokenType::StringLiteral;
}
void OpenCL::Parser::skipUntil(Tokens::const_iterator& begin, Tokens::const_iterator end, InRecoverySet recoverySet)
{
    begin = std::find_if(begin, end, recoverySet);
}
