#include "ParserUtil.hpp"

#include "ErrorMessages.hpp"

bool cld::Parser::expectIdentifier(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context,
                                   std::string_view& value, llvm::function_ref<Message()> additional)
{
    if (begin == end || begin->getTokenType() != Lexer::TokenType::Identifier)
    {
        if (begin == end)
        {
            context.log(cld::Errors::Parser::EXPECTED_N.args(*(end - 1), context.getSourceInterface(),
                                                             Lexer::TokenType::Identifier, *(end - 1)));
        }
        else
        {
            context.log(cld::Errors::Parser::EXPECTED_N_INSTEAD_OF_N.args(*begin, context.getSourceInterface(),
                                                                          Lexer::TokenType::Identifier, *begin));
        }
        if (additional)
        {
            context.log(additional());
        }
        return false;
    }
    value = begin->getText();
    begin++;
    return true;
}

bool cld::Parser::expect(Lexer::TokenType expected, Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                         Context& context, llvm::function_ref<Message()> additional)
{
    if (begin == end || begin->getTokenType() != expected)
    {
        if (begin == end)
        {
            context.log(cld::Errors::Parser::EXPECTED_N.args(diag::after(*(end - 1)), context.getSourceInterface(),
                                                             expected, *(end - 1)));
        }
        else
        {
            context.log(cld::Errors::Parser::EXPECTED_N_INSTEAD_OF_N.args(*begin, context.getSourceInterface(),
                                                                          expected, *begin));
        }
        if (additional)
        {
            context.log(additional());
        }
        return false;
    }
    begin++;
    return true;
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

bool cld::Parser::firstIsInExternalDeclaration(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInDeclaration(token, context) || firstIsInFunctionDefinition(token, context);
}

bool cld::Parser::firstIsInFunctionDefinition(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInDeclarationSpecifier(token, context);
}

bool cld::Parser::firstIsInDeclaration(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInDeclarationSpecifier(token, context);
}

bool cld::Parser::firstIsInDeclarationSpecifier(const Lexer::CToken& token, const cld::Parser::Context& context)
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
        case Lexer::TokenType::UnderlineBool:
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
        case Lexer::TokenType::Identifier: return context.isTypedefInScope(token.getText());
        default: return false;
    }
}

bool cld::Parser::firstIsInSpecifierQualifier(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    switch (token.getTokenType())
    {
        case Lexer::TokenType::VoidKeyword:
        case Lexer::TokenType::CharKeyword:
        case Lexer::TokenType::ShortKeyword:
        case Lexer::TokenType::IntKeyword:
        case Lexer::TokenType::LongKeyword:
        case Lexer::TokenType::FloatKeyword:
        case Lexer::TokenType::UnderlineBool:
        case Lexer::TokenType::DoubleKeyword:
        case Lexer::TokenType::SignedKeyword:
        case Lexer::TokenType::UnsignedKeyword:
        case Lexer::TokenType::EnumKeyword:
        case Lexer::TokenType::StructKeyword:
        case Lexer::TokenType::UnionKeyword:
        case Lexer::TokenType::ConstKeyword:
        case Lexer::TokenType::RestrictKeyword:
        case Lexer::TokenType::VolatileKeyword: return true;
        case Lexer::TokenType::Identifier: return context.isTypedefInScope(token.getText());
        default: return false;
    }
}

bool cld::Parser::firstIsInDeclarator(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInPointer(token, context) || firstIsInDirectDeclarator(token, context);
}

bool cld::Parser::firstIsInDirectDeclarator(const Lexer::CToken& token, const cld::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::Identifier
           || token.getTokenType() == Lexer::TokenType::OpenParentheses;
}

[[maybe_unused]] bool cld::Parser::firstIsInParameterTypeList(const Lexer::CToken& token,
                                                              const cld::Parser::Context& context)
{
    return firstIsInParameterList(token, context);
}

bool cld::Parser::firstIsInAbstractDeclarator(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInPointer(token, context) || firstIsInDirectAbstractDeclarator(token, context);
}

bool cld::Parser::firstIsInDirectAbstractDeclarator(const Lexer::CToken& token, const cld::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::OpenParentheses
           || token.getTokenType() == Lexer::TokenType::OpenSquareBracket;
}

bool cld::Parser::firstIsInParameterList(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInDeclarationSpecifier(token, context);
}

bool cld::Parser::firstIsInPointer(const Lexer::CToken& token, const cld::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::Asterisk;
}

[[maybe_unused]] bool cld::Parser::firstIsInCompoundItem(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInDeclaration(token, context) || firstIsInStatement(token, context);
}

bool cld::Parser::firstIsInInitializer(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInAssignmentExpression(token, context) || token.getTokenType() == Lexer::TokenType::OpenBrace;
}

[[maybe_unused]] bool cld::Parser::firstIsInInitializerList(const Lexer::CToken& token,
                                                            const cld::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::OpenSquareBracket || token.getTokenType() == Lexer::TokenType::Dot
           || firstIsInInitializer(token, context);
}

bool cld::Parser::firstIsInStatement(const Lexer::CToken& token, const cld::Parser::Context& context)
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

bool cld::Parser::firstIsInExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInAssignmentExpression(token, context);
}

bool cld::Parser::firstIsInAssignmentExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInConditionalExpression(token, context);
}

bool cld::Parser::firstIsInConditionalExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInLogicalOrExpression(token, context);
}

bool cld::Parser::firstIsInLogicalOrExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInLogicalAndExpression(token, context);
}

bool cld::Parser::firstIsInLogicalAndExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInBitOrExpression(token, context);
}

bool cld::Parser::firstIsInBitOrExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInBitXorExpression(token, context);
}

bool cld::Parser::firstIsInBitXorExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInBitAndExpression(token, context);
}

bool cld::Parser::firstIsInBitAndExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInEqualityExpression(token, context);
}

bool cld::Parser::firstIsInEqualityExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInRelationalExpression(token, context);
}

bool cld::Parser::firstIsInRelationalExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInShiftExpression(token, context);
}

bool cld::Parser::firstIsInShiftExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInAdditiveExpression(token, context);
}

bool cld::Parser::firstIsInAdditiveExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInTerm(token, context);
}

bool cld::Parser::firstIsInTerm(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInCastExpression(token, context);
}

[[maybe_unused]] bool cld::Parser::firstIsInTypeName(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInSpecifierQualifier(token, context);
}

bool cld::Parser::firstIsInCastExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::OpenParentheses || firstIsInUnaryExpression(token, context);
}

bool cld::Parser::firstIsInUnaryExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return firstIsInPostFixExpression(token, context) || token.getTokenType() == Lexer::TokenType::Increment
           || token.getTokenType() == Lexer::TokenType::Decrement || token.getTokenType() == Lexer::TokenType::Ampersand
           || token.getTokenType() == Lexer::TokenType::Asterisk || token.getTokenType() == Lexer::TokenType::Plus
           || token.getTokenType() == Lexer::TokenType::Minus
           || token.getTokenType() == Lexer::TokenType::BitWiseNegation
           || token.getTokenType() == Lexer::TokenType::LogicalNegation
           || token.getTokenType() == Lexer::TokenType::SizeofKeyword;
}

bool cld::Parser::firstIsInPostFixExpression(const Lexer::CToken& token, const cld::Parser::Context& context)
{
    return token.getTokenType() == Lexer::TokenType::OpenParentheses || firstIsInPrimaryExpression(token, context);
}

bool cld::Parser::firstIsInPrimaryExpression(const Lexer::CToken& token, const cld::Parser::Context&)
{
    return token.getTokenType() == Lexer::TokenType::OpenParentheses
           || token.getTokenType() == Lexer::TokenType::Identifier || token.getTokenType() == Lexer::TokenType::Literal
           || token.getTokenType() == Lexer::TokenType::StringLiteral;
}
