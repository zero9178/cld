#include "ParserUtil.hpp"

bool cld::Parser::expect(Lexer::TokenType expected, Lexer::TokenIterator& curr, Lexer::TokenIterator end,
                         Context& context, std::vector<Message> additional)
{
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
        curr++;
        return true;
    }
}

bool cld::Parser::isAssignment(Lexer::TokenType type)
{
    return type == Lexer::TokenType::Assignment || type == Lexer::TokenType::PlusAssign
           || type == Lexer::TokenType::MinusAssign || type == Lexer::TokenType::DivideAssign
           || type == Lexer::TokenType::MultiplyAssign || type == Lexer::TokenType::ModuloAssign
           || type == Lexer::TokenType::ShiftLeftAssign || type == Lexer::TokenType::ShiftRightAssign
           || type == Lexer::TokenType::BitAndAssign || type == Lexer::TokenType::BitOrAssign
           || type == Lexer::TokenType::BitXorAssign;
}

bool cld::Parser::firstIsInExternalDeclaration(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInDeclaration(token, context) || firstIsInFunctionDefinition(token, context);
}

bool cld::Parser::firstIsInFunctionDefinition(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInDeclarationSpecifier(token, context);
}

bool cld::Parser::firstIsInDeclaration(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInDeclarationSpecifier(token, context);
}

bool cld::Parser::firstIsInDeclarationSpecifier(const Lexer::Token& token, const cld::Parser::Context& context)
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

bool cld::Parser::firstIsInSpecifierQualifier(const Lexer::Token& token, const cld::Parser::Context& context)
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

bool cld::Parser::firstIsInDeclarator(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInPointer(token, context) || firstIsInDirectDeclarator(token, context);
}

bool cld::Parser::firstIsInDirectDeclarator(const Lexer::Token& token, const cld::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::Identifier
           || token.getTokenType() == Lexer::TokenType::OpenParentheses;
}

[[maybe_unused]] bool cld::Parser::firstIsInParameterTypeList(const Lexer::Token& token,
                                                              const cld::Parser::Context& context)
{
    return firstIsInParameterList(token, context);
}

bool cld::Parser::firstIsInAbstractDeclarator(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInPointer(token, context) || firstIsInDirectAbstractDeclarator(token, context);
}

bool cld::Parser::firstIsInDirectAbstractDeclarator(const Lexer::Token& token, const cld::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::OpenParentheses
           || token.getTokenType() == Lexer::TokenType::OpenSquareBracket;
}

bool cld::Parser::firstIsInParameterList(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInDeclarationSpecifier(token, context);
}

bool cld::Parser::firstIsInPointer(const Lexer::Token& token, const cld::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::Asterisk;
}

[[maybe_unused]] bool cld::Parser::firstIsInCompoundItem(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInDeclaration(token, context) || firstIsInStatement(token, context);
}

bool cld::Parser::firstIsInInitializer(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInAssignmentExpression(token, context) || token.getTokenType() == Lexer::TokenType::OpenBrace;
}

[[maybe_unused]] bool cld::Parser::firstIsInInitializerList(const Lexer::Token& token,
                                                            const cld::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::OpenSquareBracket || token.getTokenType() == Lexer::TokenType::Dot
           || firstIsInInitializer(token, context);
}

bool cld::Parser::firstIsInStatement(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::IfKeyword || token.getTokenType() == Lexer::TokenType::ForKeyword
           || token.getTokenType() == Lexer::TokenType::OpenBrace
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

bool cld::Parser::firstIsInExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInAssignmentExpression(token, context);
}

bool cld::Parser::firstIsInAssignmentExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInConditionalExpression(token, context);
}

bool cld::Parser::firstIsInConditionalExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInLogicalOrExpression(token, context);
}

bool cld::Parser::firstIsInLogicalOrExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInLogicalAndExpression(token, context);
}

bool cld::Parser::firstIsInLogicalAndExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInBitOrExpression(token, context);
}

bool cld::Parser::firstIsInBitOrExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInBitXorExpression(token, context);
}

bool cld::Parser::firstIsInBitXorExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInBitAndExpression(token, context);
}

bool cld::Parser::firstIsInBitAndExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInEqualityExpression(token, context);
}

bool cld::Parser::firstIsInEqualityExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInRelationalExpression(token, context);
}

bool cld::Parser::firstIsInRelationalExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInShiftExpression(token, context);
}

bool cld::Parser::firstIsInShiftExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInAdditiveExpression(token, context);
}

bool cld::Parser::firstIsInAdditiveExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInTerm(token, context);
}

bool cld::Parser::firstIsInTerm(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInCastExpression(token, context);
}

[[maybe_unused]] bool cld::Parser::firstIsInTypeName(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInSpecifierQualifier(token, context);
}

bool cld::Parser::firstIsInCastExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::OpenParentheses || firstIsInUnaryExpression(token, context);
}

bool cld::Parser::firstIsInUnaryExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return firstIsInPostFixExpression(token, context) || token.getTokenType() == Lexer::TokenType::Increment
           || token.getTokenType() == Lexer::TokenType::Decrement || token.getTokenType() == Lexer::TokenType::Ampersand
           || token.getTokenType() == Lexer::TokenType::Plus || token.getTokenType() == Lexer::TokenType::Minus
           || token.getTokenType() == Lexer::TokenType::BitWiseNegation
           || token.getTokenType() == Lexer::TokenType::LogicalNegation
           || token.getTokenType() == Lexer::TokenType::SizeofKeyword;
}

bool cld::Parser::firstIsInPostFixExpression(const Lexer::Token& token, const cld::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::OpenParentheses || firstIsInPrimaryExpression(token, context);
}

bool cld::Parser::firstIsInPrimaryExpression(const Lexer::Token& token, const cld::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::OpenParentheses
           || token.getTokenType() == Lexer::TokenType::Identifier || token.getTokenType() == Lexer::TokenType::Literal
           || token.getTokenType() == Lexer::TokenType::StringLiteral;
}
