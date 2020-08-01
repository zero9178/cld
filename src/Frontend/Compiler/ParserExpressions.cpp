#include "Parser.hpp"

#include <llvm/Support/ConvertUTF.h>

#include <Frontend/Compiler/SourceObject.hpp>

#include <algorithm>

#include "ParserUtil.hpp"

using namespace cld::Syntax;

cld::Syntax::Expression cld::Parser::parseExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                     Context& context)
{
    const auto* start = begin;
    std::vector<AssignmentExpression> expressions;
    auto assignment = parseAssignmentExpression(
        begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
    if (assignment)
    {
        expressions.push_back(std::move(*assignment));
    }

    if (begin != end)
    {
        while (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
            assignment = parseAssignmentExpression(
                begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
            if (assignment)
            {
                expressions.push_back(std::move(*assignment));
            }
        }
    }
    return Expression(start, begin, std::move(expressions));
}

std::optional<cld::Syntax::AssignmentExpression>
    cld::Parser::parseAssignmentExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    auto result = parseConditionalExpression(begin, end, context.withRecoveryTokens(assignmentSet));

    std::vector<std::pair<AssignmentExpression::AssignOperator, ConditionalExpression>> list;
    while (begin != end && isAssignment(begin->getTokenType()))
    {
        auto token = begin->getTokenType();
        begin++;
        list.emplace_back(
            [token]() -> AssignmentExpression::AssignOperator {
                switch (token)
                {
                    case Lexer::TokenType::Assignment: return AssignmentExpression::AssignOperator::NoOperator;
                    case Lexer::TokenType::PlusAssign: return AssignmentExpression::AssignOperator::PlusAssign;
                    case Lexer::TokenType::MinusAssign: return AssignmentExpression::AssignOperator::MinusAssign;
                    case Lexer::TokenType::DivideAssign: return AssignmentExpression::AssignOperator::DivideAssign;
                    case Lexer::TokenType::MultiplyAssign: return AssignmentExpression::AssignOperator::MultiplyAssign;
                    case Lexer::TokenType::ModuloAssign: return AssignmentExpression::AssignOperator::ModuloAssign;
                    case Lexer::TokenType::ShiftLeftAssign:
                        return AssignmentExpression::AssignOperator::LeftShiftAssign;
                    case Lexer::TokenType::ShiftRightAssign:
                        return AssignmentExpression::AssignOperator::RightShiftAssign;
                    case Lexer::TokenType::BitAndAssign: return AssignmentExpression::AssignOperator::BitAndAssign;
                    case Lexer::TokenType::BitOrAssign: return AssignmentExpression::AssignOperator::BitOrAssign;
                    case Lexer::TokenType::BitXorAssign: return AssignmentExpression::AssignOperator::BitXorAssign;
                    default: CLD_UNREACHABLE;
                }
            }(),
            parseConditionalExpression(begin, end, context.withRecoveryTokens(assignmentSet)));
    }

    return AssignmentExpression(start, begin, std::move(result), std::move(list));
}

namespace
{
using StateVariant =
    std::variant<std::monostate, std::optional<Term>, std::optional<AdditiveExpression>, std::optional<ShiftExpression>,
                 std::optional<RelationalExpression>, std::optional<EqualityExpression>, BitAndExpression,
                 BitXorExpression, BitOrExpression, LogicalAndExpression, LogicalOrExpression>;

enum class EndState : std::uint8_t
{
    Term,
    Additive,
    Shift,
    Relational,
    Equality,
    BitAnd,
    BitXor,
    BitOr,
    LogicalAnd,
    LogicalOr
};

StateVariant parseBinaryOperators(EndState endState, cld::Lexer::CTokenIterator& begin, cld::Lexer::CTokenIterator end,
                                  cld::Parser::Context& context)
{
    StateVariant state;
    const auto* start = begin;
    auto firstSet = [endState, &state]() -> cld::Parser::Context::TokenBitSet {
        cld::Parser::Context::TokenBitSet result;
        switch (endState)
        {
            case EndState::LogicalOr:
                result |= cld::Parser::Context::fromTokenTypes(cld::Lexer::TokenType::LogicOr);
                if (std::holds_alternative<LogicalOrExpression>(state))
                {
                    return result;
                }
                [[fallthrough]];
            case EndState::LogicalAnd:
                result |= cld::Parser::Context::fromTokenTypes(cld::Lexer::TokenType::LogicAnd);
                if (std::holds_alternative<LogicalAndExpression>(state))
                {
                    return result;
                }
                [[fallthrough]];
            case EndState::BitOr:
                result |= cld::Parser::Context::fromTokenTypes(cld::Lexer::TokenType::BitOr);
                if (std::holds_alternative<BitOrExpression>(state))
                {
                    return result;
                }
                [[fallthrough]];
            case EndState::BitXor:
                result |= cld::Parser::Context::fromTokenTypes(cld::Lexer::TokenType::BitXor);
                if (std::holds_alternative<BitXorExpression>(state))
                {
                    return result;
                }
                [[fallthrough]];
            case EndState::BitAnd:
                result |= cld::Parser::Context::fromTokenTypes(cld::Lexer::TokenType::Ampersand);
                if (std::holds_alternative<BitAndExpression>(state))
                {
                    return result;
                }
                [[fallthrough]];
            case EndState::Equality:
                result |=
                    cld::Parser::Context::fromTokenTypes(cld::Lexer::TokenType::Equal, cld::Lexer::TokenType::NotEqual);
                if (std::holds_alternative<std::optional<EqualityExpression>>(state))
                {
                    return result;
                }
                [[fallthrough]];
            case EndState::Relational:
                result |= cld::Parser::Context::fromTokenTypes(
                    cld::Lexer::TokenType::LessThan, cld::Lexer::TokenType::LessThanOrEqual,
                    cld::Lexer::TokenType::GreaterThan, cld::Lexer::TokenType::GreaterThanOrEqual);
                if (std::holds_alternative<std::optional<RelationalExpression>>(state))
                {
                    return result;
                }
                [[fallthrough]];
            case EndState::Shift:
                result |= cld::Parser::Context::fromTokenTypes(cld::Lexer::TokenType::ShiftLeft,
                                                               cld::Lexer::TokenType::ShiftRight);
                if (std::holds_alternative<std::optional<ShiftExpression>>(state))
                {
                    return result;
                }
                [[fallthrough]];
            case EndState::Additive:
                result |=
                    cld::Parser::Context::fromTokenTypes(cld::Lexer::TokenType::Plus, cld::Lexer::TokenType::Minus);
                if (std::holds_alternative<std::optional<AdditiveExpression>>(state))
                {
                    return result;
                }
                [[fallthrough]];
            case EndState::Term:
                result |= cld::Parser::Context::fromTokenTypes(
                    cld::Lexer::TokenType::Asterisk, cld::Lexer::TokenType::Division, cld::Lexer::TokenType::Percent);
                [[fallthrough]];
            default: return result;
        }
    };
    while ([&state, endState]() -> bool {
        switch (endState)
        {
            case EndState::Term: return !std::holds_alternative<std::optional<Term>>(state);
            case EndState::Additive: return !std::holds_alternative<std::optional<AdditiveExpression>>(state);
            case EndState::Shift: return !std::holds_alternative<std::optional<ShiftExpression>>(state);
            case EndState::Relational: return !std::holds_alternative<std::optional<RelationalExpression>>(state);
            case EndState::Equality: return !std::holds_alternative<std::optional<EqualityExpression>>(state);
            case EndState::BitAnd: return !std::holds_alternative<BitAndExpression>(state);
            case EndState::BitXor: return !std::holds_alternative<BitXorExpression>(state);
            case EndState::BitOr: return !std::holds_alternative<BitOrExpression>(state);
            case EndState::LogicalAnd: return !std::holds_alternative<LogicalAndExpression>(state);
            case EndState::LogicalOr: return !std::holds_alternative<LogicalOrExpression>(state);
        }
        return false;
    }())
    {
        // We are using an index instead of visit to save on stack space and increase speed
        switch (state.index())
        {
            case cld::getIndex<std::monostate>(state):
            {
                auto result = parseCastExpression(begin, end, context.withRecoveryTokens(firstSet()));

                std::vector<std::pair<Term::BinaryDotOperator, CastExpression>> list;
                while (begin != end
                       && (begin->getTokenType() == cld::Lexer::TokenType::Asterisk
                           || begin->getTokenType() == cld::Lexer::TokenType::Division
                           || begin->getTokenType() == cld::Lexer::TokenType::Percent))
                {
                    auto token = begin->getTokenType();
                    begin++;
                    auto newCast = parseCastExpression(begin, end, context.withRecoveryTokens(firstSet()));
                    if (newCast)
                    {
                        list.emplace_back(
                            [token] {
                                switch (token)
                                {
                                    case cld::Lexer::TokenType::Asterisk:
                                        return Term::BinaryDotOperator::BinaryMultiply;
                                    case cld::Lexer::TokenType::Division: return Term::BinaryDotOperator::BinaryDivide;
                                    case cld::Lexer::TokenType::Percent:
                                        return Term::BinaryDotOperator::BinaryRemainder;
                                    default: CLD_UNREACHABLE;
                                }
                            }(),
                            std::move(*newCast));
                    }
                }

                if (!result)
                {
                    state = std::optional<Term>{};
                }
                else
                {
                    state = Term(start, begin, std::move(*result), std::move(list));
                }
                break;
            }
            case cld::getIndex<std::optional<Term>>(state):
            {
                auto& result = cld::get<std::optional<Term>>(state);

                std::vector<std::pair<AdditiveExpression::BinaryDashOperator, Term>> list;
                while (begin != end
                       && (begin->getTokenType() == cld::Lexer::TokenType::Plus
                           || begin->getTokenType() == cld::Lexer::TokenType::Minus))
                {
                    auto token = begin->getTokenType();
                    begin++;
                    auto newTerm = parseTerm(begin, end, context.withRecoveryTokens(firstSet()));
                    if (newTerm)
                    {
                        list.emplace_back(token == cld::Lexer::TokenType::Plus ?
                                              AdditiveExpression::BinaryDashOperator::BinaryPlus :
                                              AdditiveExpression::BinaryDashOperator::BinaryMinus,
                                          std::move(*newTerm));
                    }
                }

                if (!result)
                {
                    state = std::optional<AdditiveExpression>{};
                }
                else
                {
                    state = AdditiveExpression(start, begin, std::move(*result), std::move(list));
                }
                break;
            }
            case cld::getIndex<std::optional<AdditiveExpression>>(state):
            {
                auto& result = cld::get<std::optional<AdditiveExpression>>(state);

                std::vector<std::pair<ShiftExpression::ShiftOperator, AdditiveExpression>> list;
                while (begin != end
                       && (begin->getTokenType() == cld::Lexer::TokenType::ShiftRight
                           || begin->getTokenType() == cld::Lexer::TokenType::ShiftLeft))
                {
                    auto token = begin->getTokenType();
                    begin++;
                    auto newAdd = parseAdditiveExpression(begin, end, context.withRecoveryTokens(firstSet()));
                    if (newAdd)
                    {
                        list.emplace_back(token == cld::Lexer::TokenType::ShiftRight ?
                                              ShiftExpression::ShiftOperator::Right :
                                              ShiftExpression::ShiftOperator::Left,
                                          std::move(*newAdd));
                    }
                }

                if (!result)
                {
                    state = std::optional<ShiftExpression>{};
                }
                else
                {
                    state = ShiftExpression(start, begin, std::move(*result), std::move(list));
                }
                break;
            }
            case cld::getIndex<std::optional<ShiftExpression>>(state):
            {
                auto& result = cld::get<std::optional<ShiftExpression>>(state);

                std::vector<std::pair<RelationalExpression::RelationalOperator, ShiftExpression>> list;
                while (begin != end
                       && (begin->getTokenType() == cld::Lexer::TokenType::LessThan
                           || begin->getTokenType() == cld::Lexer::TokenType::LessThanOrEqual
                           || begin->getTokenType() == cld::Lexer::TokenType::GreaterThan
                           || begin->getTokenType() == cld::Lexer::TokenType::GreaterThanOrEqual))
                {
                    auto token = begin->getTokenType();
                    begin++;
                    auto newShift = parseShiftExpression(begin, end, context.withRecoveryTokens(firstSet()));
                    if (newShift)
                    {
                        list.emplace_back(
                            [token]() -> RelationalExpression::RelationalOperator {
                                switch (token)
                                {
                                    case cld::Lexer::TokenType::LessThan:
                                        return RelationalExpression::RelationalOperator::LessThan;
                                    case cld::Lexer::TokenType::LessThanOrEqual:
                                        return RelationalExpression::RelationalOperator::LessThanOrEqual;
                                    case cld::Lexer::TokenType::GreaterThan:
                                        return RelationalExpression::RelationalOperator::GreaterThan;
                                    case cld::Lexer::TokenType::GreaterThanOrEqual:
                                        return RelationalExpression::RelationalOperator::GreaterThanOrEqual;
                                    default: CLD_UNREACHABLE;
                                }
                            }(),
                            std::move(*newShift));
                    }
                }

                if (!result)
                {
                    state = std::optional<RelationalExpression>{};
                }
                else
                {
                    state = RelationalExpression(start, begin, std::move(*result), std::move(list));
                }
                break;
            }
            case cld::getIndex<std::optional<RelationalExpression>>(state):
            {
                auto& result = cld::get<std::optional<RelationalExpression>>(state);

                std::vector<std::pair<EqualityExpression::EqualityOperator, RelationalExpression>> list;
                while (begin != end
                       && (begin->getTokenType() == cld::Lexer::TokenType::Equal
                           || begin->getTokenType() == cld::Lexer::TokenType::NotEqual))
                {
                    auto token = begin->getTokenType();
                    begin++;
                    auto newRelational = parseRelationalExpression(begin, end, context.withRecoveryTokens(firstSet()));
                    if (newRelational)
                    {
                        list.emplace_back(token == cld::Lexer::TokenType::Equal ?
                                              EqualityExpression::EqualityOperator::Equal :
                                              EqualityExpression::EqualityOperator::NotEqual,
                                          std::move(*newRelational));
                    }
                }

                if (!result)
                {
                    state = std::optional<EqualityExpression>{};
                }
                else
                {
                    state = EqualityExpression(start, begin, std::move(*result), std::move(list));
                }
                break;
            }
            case cld::getIndex<std::optional<EqualityExpression>>(state):
            {
                auto& result = cld::get<std::optional<EqualityExpression>>(state);

                std::vector<EqualityExpression> list;
                if (result)
                {
                    list.push_back(std::move(*result));
                }
                while (begin != end && begin->getTokenType() == cld::Lexer::TokenType::Ampersand)
                {
                    begin++;
                    auto newEqual = parseEqualityExpression(begin, end, context.withRecoveryTokens(firstSet()));
                    if (newEqual)
                    {
                        list.push_back(std::move(*newEqual));
                    }
                }

                state = BitAndExpression(start, begin, std::move(list));
                break;
            }
            case cld::getIndex<BitAndExpression>(state):
            {
                std::vector<BitAndExpression> list;
                list.push_back(std::move(cld::get<BitAndExpression>(state)));
                while (begin != end && begin->getTokenType() == cld::Lexer::TokenType::BitXor)
                {
                    begin++;
                    list.push_back(parseBitAndExpression(begin, end, context.withRecoveryTokens(firstSet())));
                }

                state = BitXorExpression(start, begin, std::move(list));
                break;
            }
            case cld::getIndex<BitXorExpression>(state):
            {
                std::vector<BitXorExpression> list;
                list.push_back(std::move(cld::get<BitXorExpression>(state)));
                while (begin != end && begin->getTokenType() == cld::Lexer::TokenType::BitOr)
                {
                    begin++;
                    list.push_back(parseBitXorExpression(begin, end, context.withRecoveryTokens(firstSet())));
                }
                state = BitOrExpression(start, begin, std::move(list));
                break;
            }
            case cld::getIndex<BitOrExpression>(state):
            {
                std::vector<BitOrExpression> list;
                list.push_back(std::move(cld::get<BitOrExpression>(state)));
                while (begin != end && begin->getTokenType() == cld::Lexer::TokenType::LogicAnd)
                {
                    begin++;
                    list.push_back(parseBitOrExpression(begin, end, context.withRecoveryTokens(firstSet())));
                }

                state = LogicalAndExpression(start, begin, std::move(list));
                break;
            }
            case cld::getIndex<LogicalAndExpression>(state):
            {
                std::vector<LogicalAndExpression> list;
                list.push_back(std::move(cld::get<LogicalAndExpression>(state)));
                while (begin != end && begin->getTokenType() == cld::Lexer::TokenType::LogicOr)
                {
                    begin++;
                    list.push_back(parseLogicalAndExpression(begin, end, context));
                }

                state = LogicalOrExpression(start, begin, std::move(list));
                break;
            }
            default: break;
        }
    }

    return state;
}

} // namespace

cld::Syntax::ConditionalExpression cld::Parser::parseConditionalExpression(Lexer::CTokenIterator& begin,
                                                                           Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    auto logicalOrExpression = cld::get<LogicalOrExpression>(
        parseBinaryOperators(EndState::LogicalOr, begin, end,
                             context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::QuestionMark))));
    if (begin != end && begin->getTokenType() == Lexer::TokenType::QuestionMark)
    {
        const auto* questionMarkpos = begin;
        begin++;
        auto optionalExpression = std::make_unique<Expression>(
            parseExpression(begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Colon))));
        if (!expect(Lexer::TokenType::Colon, begin, end, context,
                    Notes::TO_MATCH_N_HERE.args(*questionMarkpos, context.getSourceInterface(), *questionMarkpos)))
        {
            context.skipUntil(begin, end, firstExpressionSet);
        }
        return ConditionalExpression(
            start, begin, std::move(logicalOrExpression), std::move(optionalExpression),
            std::make_unique<ConditionalExpression>(parseConditionalExpression(begin, end, context)));
    }
    return ConditionalExpression(start, begin, std::move(logicalOrExpression));
}

cld::Syntax::LogicalOrExpression cld::Parser::parseLogicalOrExpression(Lexer::CTokenIterator& begin,
                                                                       Lexer::CTokenIterator end, Context& context)
{
    return cld::get<LogicalOrExpression>(parseBinaryOperators(EndState::LogicalOr, begin, end, context));
}

cld::Syntax::LogicalAndExpression cld::Parser::parseLogicalAndExpression(Lexer::CTokenIterator& begin,
                                                                         Lexer::CTokenIterator end, Context& context)
{
    return cld::get<LogicalAndExpression>(parseBinaryOperators(EndState::LogicalAnd, begin, end, context));
}

cld::Syntax::BitOrExpression cld::Parser::parseBitOrExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                               Context& context)
{
    return cld::get<BitOrExpression>(parseBinaryOperators(EndState::BitOr, begin, end, context));
}

cld::Syntax::BitXorExpression cld::Parser::parseBitXorExpression(Lexer::CTokenIterator& begin,
                                                                 Lexer::CTokenIterator end, Context& context)
{
    return cld::get<BitXorExpression>(parseBinaryOperators(EndState::BitXor, begin, end, context));
}

cld::Syntax::BitAndExpression cld::Parser::parseBitAndExpression(Lexer::CTokenIterator& begin,
                                                                 Lexer::CTokenIterator end, Context& context)
{
    return cld::get<BitAndExpression>(parseBinaryOperators(EndState::BitAnd, begin, end, context));
}

std::optional<cld::Syntax::EqualityExpression>
    cld::Parser::parseEqualityExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    return cld::get<std::optional<EqualityExpression>>(parseBinaryOperators(EndState::Equality, begin, end, context));
}

std::optional<cld::Syntax::RelationalExpression>
    cld::Parser::parseRelationalExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    return cld::get<std::optional<RelationalExpression>>(
        parseBinaryOperators(EndState::Relational, begin, end, context));
}

std::optional<cld::Syntax::ShiftExpression>
    cld::Parser::parseShiftExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    return cld::get<std::optional<ShiftExpression>>(parseBinaryOperators(EndState::Shift, begin, end, context));
}

std::optional<cld::Syntax::AdditiveExpression>
    cld::Parser::parseAdditiveExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    return cld::get<std::optional<AdditiveExpression>>(parseBinaryOperators(EndState::Additive, begin, end, context));
}

std::optional<cld::Syntax::Term> cld::Parser::parseTerm(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                        Context& context)
{
    return cld::get<std::optional<Term>>(parseBinaryOperators(EndState::Term, begin, end, context));
}

std::optional<cld::Syntax::TypeName> cld::Parser::parseTypeName(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                                Context& context)
{
    const auto* start = begin;
    auto specifierQualifiers =
        parseSpecifierQualifierList(begin, end, context.withRecoveryTokens(firstAbstractDeclaratorSet));
    if (specifierQualifiers.empty())
    {
        if (begin != end)
        {
            context.log(Errors::Parser::EXPECTED_TYPENAME_BEFORE_N.args(*begin, context.getSourceInterface(), *begin));
        }
        else
        {
            context.log(Errors::Parser::EXPECTED_TYPENAME.args(diag::after(*(begin - 1)), context.getSourceInterface(),
                                                               *(begin - 1)));
        }
    }

    if (begin < end && firstIsInAbstractDeclarator(*begin, context))
    {
        auto abstractDeclarator = std::make_unique<AbstractDeclarator>(parseAbstractDeclarator(begin, end, context));
        return TypeName(start, begin, std::move(specifierQualifiers), std::move(abstractDeclarator));
    }

    return TypeName(start, begin, std::move(specifierQualifiers), nullptr);
}

namespace
{
bool isPostFixOperator(const cld::Lexer::CToken& token)
{
    switch (token.getTokenType())
    {
        case cld::Lexer::TokenType::Arrow:
        case cld::Lexer::TokenType::Dot:
        case cld::Lexer::TokenType::OpenSquareBracket:
        case cld::Lexer::TokenType::OpenParentheses:
        case cld::Lexer::TokenType::Increment:
        case cld::Lexer::TokenType::Decrement: return true;
        default: break;
    }
    return false;
}

void parsePostFixExpressionSuffix(cld::Lexer::CTokenIterator start, cld::Lexer::CTokenIterator& begin,
                                  cld::Lexer::CTokenIterator end, std::unique_ptr<PostFixExpression>& current,
                                  cld::Parser::Context& context)
{
    while (begin != end && isPostFixOperator(*begin))
    {
        if (begin->getTokenType() == cld::Lexer::TokenType::OpenParentheses)
        {
            context.parenthesesEntered(begin);
            const auto* openPpos = begin;
            begin++;
            std::vector<std::unique_ptr<AssignmentExpression>> nonCommaExpressions;
            bool first = true;
            while (begin < end && begin->getTokenType() != cld::Lexer::TokenType::CloseParentheses)
            {
                if (first)
                {
                    first = false;
                }
                else if (begin->getTokenType() == cld::Lexer::TokenType::Comma)
                {
                    begin++;
                }
                else if (firstIsInAssignmentExpression(*begin, context))
                {
                    expect(cld::Lexer::TokenType::Comma, begin, end, context);
                }
                else
                {
                    break;
                }

                auto assignment = parseAssignmentExpression(
                    begin, end,
                    context.withRecoveryTokens(cld::Parser::Context::fromTokenTypes(
                        cld::Lexer::TokenType::CloseParentheses, cld::Lexer::TokenType::Comma)));
                if (assignment)
                {
                    nonCommaExpressions.push_back(std::make_unique<AssignmentExpression>(std::move(*assignment)));
                }
            }

            if (!expect(cld::Lexer::TokenType::CloseParentheses, begin, end, context,
                        cld::Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos)))
            {
                context.skipUntil(begin, end, cld::Parser::firstPostfixSet);
            }
            if (current)
            {
                current = std::make_unique<PostFixExpression>(
                    PostFixExpressionFunctionCall(start, begin, std::move(current), std::move(nonCommaExpressions)));
            }
            context.parenthesesLeft();
        }
        else if (begin->getTokenType() == cld::Lexer::TokenType::OpenSquareBracket)
        {
            context.squareBracketEntered(begin);
            const auto* openPpos = begin;
            begin++;
            auto expression = parseExpression(begin, end,
                                              context.withRecoveryTokens(cld::Parser::Context::fromTokenTypes(
                                                  cld::Lexer::TokenType::CloseSquareBracket)));

            if (!expect(cld::Lexer::TokenType::CloseSquareBracket, begin, end, context,
                        cld::Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos)))
            {
                context.skipUntil(begin, end, cld::Parser::firstPostfixSet);
            }
            if (current)
            {
                current = std::make_unique<PostFixExpression>(
                    PostFixExpressionSubscript(start, begin, std::move(current), std::move(expression)));
            }
            context.squareBracketLeft();
        }
        else if (begin->getTokenType() == cld::Lexer::TokenType::Increment)
        {
            begin++;
            if (current)
            {
                current =
                    std::make_unique<PostFixExpression>(PostFixExpressionIncrement(start, begin, std::move(current)));
            }
        }
        else if (begin->getTokenType() == cld::Lexer::TokenType::Decrement)
        {
            begin++;
            if (current)
            {
                current =
                    std::make_unique<PostFixExpression>(PostFixExpressionDecrement(start, begin, std::move(current)));
            }
        }
        else if (begin->getTokenType() == cld::Lexer::TokenType::Dot)
        {
            begin++;
            std::string name;
            if (!expect(cld::Lexer::TokenType::Identifier, begin, end, context, name))
            {
                context.skipUntil(begin, end, cld::Parser::firstPostfixSet);
            }
            if (current)
            {
                current =
                    std::make_unique<PostFixExpression>(PostFixExpressionDot(start, begin, std::move(current), name));
            }
        }
        else
        {
            begin++;
            std::string name;
            if (!expect(cld::Lexer::TokenType::Identifier, begin, end, context, name))
            {
                context.skipUntil(begin, end, cld::Parser::firstPostfixSet);
            }
            if (current)
            {
                current =
                    std::make_unique<PostFixExpression>(PostFixExpressionArrow(start, begin, std::move(current), name));
            }
        }
    }
}
} // namespace

std::optional<cld::Syntax::CastExpression> cld::Parser::parseCastExpression(Lexer::CTokenIterator& begin,
                                                                            Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenParentheses || begin + 1 == end
        || !firstIsInTypeName(*(begin + 1), context))
    {
        auto unary = parseUnaryExpression(begin, end, context);
        if (!unary)
        {
            return {};
        }
        return CastExpression(start, begin, std::move(*unary));
    }
    begin++;
    auto typeName = parseTypeName(
        begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
    if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                Notes::TO_MATCH_N_HERE.args(*start, context.getSourceInterface(), *start)))
    {
        context.skipUntil(begin, end, firstExpressionSet);
    }
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        auto cast = parseCastExpression(begin, end, context);
        if (!cast || !typeName)
        {
            return {};
        }
        return CastExpression(start, begin,
                              std::pair{std::move(*typeName), std::make_unique<CastExpression>(std::move(*cast))});
    }

    std::optional<Lexer::CTokenIterator> openBrace;
    if (!expect(Lexer::TokenType::OpenBrace, begin, end, context))
    {
        context.skipUntil(begin, end, firstInitializerListSet);
    }
    else
    {
        openBrace = begin - 1;
    }

    auto initializer = parseInitializerList(
        begin, end,
        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseBrace, Lexer::TokenType::Comma)));
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
    {
        begin++;
    }
    if (openBrace)
    {
        if (!expect(Lexer::TokenType::CloseBrace, begin, end, context,
                    Notes::TO_MATCH_N_HERE.args(**openBrace, context.getSourceInterface(), **openBrace)))
        {
            context.skipUntil(begin, end, firstPostfixSet);
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::CloseBrace, begin, end, context))
        {
            context.skipUntil(begin, end, firstPostfixSet);
        }
    }
    std::unique_ptr<PostFixExpression> current;
    if (initializer && typeName)
    {
        current = std::make_unique<PostFixExpression>(
            PostFixExpressionTypeInitializer(start, begin, std::move(*typeName), std::move(*initializer)));
    }
    parsePostFixExpressionSuffix(start, begin, end, current, context);
    if (!current)
    {
        return {};
    }
    return CastExpression(start, begin, UnaryExpressionPostFixExpression(start, begin, std::move(*current)));
}

std::optional<cld::Syntax::UnaryExpression>
    cld::Parser::parseUnaryExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::SizeofKeyword)
    {
        begin++;
        if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenParentheses
            && (begin + 1 == end || firstIsInTypeName(*(begin + 1), context)))
        {
            const auto* openPpos = begin;
            begin++;
            auto type = parseTypeName(
                begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
            if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                        Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos)))
            {
                context.skipUntil(begin, end);
            }
            if (!type)
            {
                return {};
            }
            return UnaryExpression(UnaryExpressionSizeOf(start, begin, std::make_unique<TypeName>(std::move(*type))));
        }
        else
        {
            auto unary = parseUnaryExpression(begin, end, context);
            if (!unary)
            {
                return {};
            }
            return UnaryExpression(
                UnaryExpressionSizeOf(start, begin, std::make_unique<UnaryExpression>(std::move(*unary))));
        }
    }
    else if (context.isInPreprocessor() && begin < end && begin->getTokenType() == Lexer::TokenType::Identifier
             && cld::get<std::string>(begin->getValue()) == "defined")
    {
        if (begin->isMacroInserted())
        {
            context.log(Warnings::PP::MACRO_EXPANSION_PRODUCING_DEFINED_IS_NOT_PORTABLE.args(
                *begin, context.getSourceInterface(), *begin));
        }
        begin++;
        std::optional<Lexer::CTokenIterator> openP;
        if (begin->getTokenType() == Lexer::TokenType::OpenParentheses)
        {
            openP = begin;
            begin++;
        }
        std::string identifier;
        if (!expect(Lexer::TokenType::Identifier, begin, end, context, identifier))
        {
            context.skipUntil(begin, end,
                              openP ? Context::fromTokenTypes(Lexer::TokenType::CloseParentheses) :
                                      Context::TokenBitSet{});
        }
        if (openP)
        {
            if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                        Notes::TO_MATCH_N_HERE.args(**openP, context.getSourceInterface(), **openP)))
            {
                context.skipUntil(begin, end);
            }
        }
        return UnaryExpression(UnaryExpressionDefined(start, begin, std::move(identifier)));
    }
    else if (begin < end
             && (begin->getTokenType() == Lexer::TokenType::Increment
                 || begin->getTokenType() == Lexer::TokenType::Decrement
                 || begin->getTokenType() == Lexer::TokenType::Ampersand
                 || begin->getTokenType() == Lexer::TokenType::Asterisk
                 || begin->getTokenType() == Lexer::TokenType::Plus || begin->getTokenType() == Lexer::TokenType::Minus
                 || begin->getTokenType() == Lexer::TokenType::LogicalNegation
                 || begin->getTokenType() == Lexer::TokenType::BitWiseNegation))
    {
        auto token = begin->getTokenType();
        begin++;
        auto op = [token] {
            switch (token)
            {
                case Lexer::TokenType::Increment: return UnaryExpressionUnaryOperator::UnaryOperator::Increment;
                case Lexer::TokenType::Decrement: return UnaryExpressionUnaryOperator::UnaryOperator::Decrement;
                case Lexer::TokenType::Ampersand: return UnaryExpressionUnaryOperator::UnaryOperator::Ampersand;
                case Lexer::TokenType::Asterisk: return UnaryExpressionUnaryOperator::UnaryOperator::Asterisk;
                case Lexer::TokenType::Plus: return UnaryExpressionUnaryOperator::UnaryOperator::Plus;
                case Lexer::TokenType::Minus: return UnaryExpressionUnaryOperator::UnaryOperator::Minus;
                case Lexer::TokenType::LogicalNegation: return UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot;
                case Lexer::TokenType::BitWiseNegation: return UnaryExpressionUnaryOperator::UnaryOperator::BitNot;
                default: CLD_UNREACHABLE;
            }
        }();
        auto cast = parseCastExpression(begin, end, context);
        if (!cast)
        {
            return {};
        }
        return UnaryExpression(
            UnaryExpressionUnaryOperator(start, begin, op, std::make_unique<CastExpression>(std::move(*cast))));
    }

    auto postFix = parsePostFixExpression(begin, end, context);
    if (!postFix)
    {
        return {};
    }
    return UnaryExpression(UnaryExpressionPostFixExpression(start, begin, std::move(*postFix)));
}

std::optional<cld::Syntax::PostFixExpression>
    cld::Parser::parsePostFixExpression(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    std::unique_ptr<PostFixExpression> current;

    const auto* start = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenParentheses || begin + 1 == end
        || !firstIsInTypeName(*(begin + 1), context))
    {
        auto token = context.withRecoveryTokens(firstPostfixSet);
        std::optional<cld::Syntax::PrimaryExpression> newPrimary;
        if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
        {
            const auto& value = cld::get<std::string>(begin->getValue());
            begin++;
            newPrimary = PrimaryExpression(PrimaryExpressionIdentifier(start, begin, value));
        }
        else if (begin < end && begin->getTokenType() == Lexer::TokenType::Literal)
        {
            auto value = cld::match(begin->getValue(), [](auto&& value) -> typename PrimaryExpressionConstant::variant {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_constructible_v<typename PrimaryExpressionConstant::variant, T>)
                {
                    return {std::forward<decltype(value)>(value)};
                }
                else
                {
                    CLD_UNREACHABLE;
                }
            });
            auto type = begin->getType();
            begin++;
            newPrimary = PrimaryExpression(PrimaryExpressionConstant(start, begin, std::move(value), type));
        }
        else if (begin < end && begin->getTokenType() == Lexer::TokenType::StringLiteral)
        {
            using stringVariant = std::variant<std::string, Lexer::NonCharString>;
            stringVariant literal = match(
                begin->getValue(), [](const std::string& str) -> stringVariant { return str; },
                [](const Lexer::NonCharString& str) -> stringVariant { return str; },
                [](auto &&) -> stringVariant { CLD_UNREACHABLE; });
            begin++;

            while (begin < end && begin->getTokenType() == Lexer::TokenType::StringLiteral)
            {
                literal = match(
                    begin->getValue(),
                    [&literal, &context](const std::string& str) -> stringVariant {
                        return match(
                            literal, [&str](const std::string& lhs) -> stringVariant { return lhs + str; },
                            [&str, &context](Lexer::NonCharString lhs) -> stringVariant {
                                switch (context.getSourceInterface().getLanguageOptions().sizeOfWChar)
                                {
                                    case 2:
                                    {
                                        auto* sourceStart = str.data();
                                        std::vector<llvm::UTF16> utf16(str.size());
                                        auto* targetStart = utf16.data();
                                        auto result = llvm::ConvertUTF8toUTF16(
                                            reinterpret_cast<const llvm::UTF8**>(&sourceStart),
                                            reinterpret_cast<const llvm::UTF8*>(sourceStart + str.size()), &targetStart,
                                            targetStart + utf16.size(), llvm::strictConversion);
                                        if (result != llvm::conversionOK)
                                        {
                                            CLD_UNREACHABLE;
                                        }
                                        std::transform(utf16.data(), targetStart, std::back_inserter(lhs.characters),
                                                       [](llvm::UTF16 ch) -> std::uint32_t { return ch; });
                                        return lhs;
                                    }
                                    case 4:
                                    {
                                        auto* sourceStart = str.data();
                                        std::vector<llvm::UTF32> utf32(str.size());
                                        auto* targetStart = utf32.data();
                                        auto result = llvm::ConvertUTF8toUTF32(
                                            reinterpret_cast<const llvm::UTF8**>(&sourceStart),
                                            reinterpret_cast<const llvm::UTF8*>(sourceStart + str.size()), &targetStart,
                                            targetStart + utf32.size(), llvm::strictConversion);
                                        if (result != llvm::conversionOK)
                                        {
                                            CLD_UNREACHABLE;
                                        }
                                        std::transform(utf32.data(), targetStart, std::back_inserter(lhs.characters),
                                                       [](llvm::UTF32 ch) -> std::uint32_t { return ch; });
                                        return lhs;
                                    }
                                    default: CLD_UNREACHABLE;
                                }
                            },
                            [](const auto&) -> stringVariant { CLD_UNREACHABLE; });
                    },
                    [&literal, &context](Lexer::NonCharString str) -> stringVariant {
                        return match(
                            literal,
                            [&str, &context](const std::string& lhs) -> stringVariant {
                                switch (context.getSourceInterface().getLanguageOptions().sizeOfWChar)
                                {
                                    case 2:
                                    {
                                        auto* sourceStart = lhs.data();
                                        std::vector<llvm::UTF16> utf16(lhs.size());
                                        auto* targetStart = utf16.data();
                                        auto result = llvm::ConvertUTF8toUTF16(
                                            reinterpret_cast<const llvm::UTF8**>(&sourceStart),
                                            reinterpret_cast<const llvm::UTF8*>(sourceStart + lhs.size()), &targetStart,
                                            targetStart + utf16.size(), llvm::strictConversion);
                                        if (result != llvm::conversionOK)
                                        {
                                            CLD_UNREACHABLE;
                                        }
                                        std::transform(utf16.data(), targetStart,
                                                       std::inserter(str.characters, str.characters.begin()),
                                                       [](llvm::UTF16 ch) -> std::uint32_t { return ch; });
                                        return lhs;
                                    }
                                    case 4:
                                    {
                                        auto* sourceStart = lhs.data();
                                        std::vector<llvm::UTF32> utf32(lhs.size());
                                        auto* targetStart = utf32.data();
                                        auto result = llvm::ConvertUTF8toUTF32(
                                            reinterpret_cast<const llvm::UTF8**>(&sourceStart),
                                            reinterpret_cast<const llvm::UTF8*>(sourceStart + lhs.size()), &targetStart,
                                            targetStart + utf32.size(), llvm::strictConversion);
                                        if (result != llvm::conversionOK)
                                        {
                                            CLD_UNREACHABLE;
                                        }
                                        std::transform(utf32.data(), targetStart, std::back_inserter(str.characters),
                                                       [](llvm::UTF32 ch) -> std::uint32_t { return ch; });
                                        return lhs;
                                    }
                                    default: CLD_UNREACHABLE;
                                }
                            },
                            [&str](Lexer::NonCharString lhs) -> stringVariant {
                                lhs.characters.insert(lhs.characters.end(), str.characters.begin(),
                                                      str.characters.end());
                                return lhs;
                            },
                            [](const auto&) -> stringVariant { CLD_UNREACHABLE; });
                    },
                    [](auto &&) -> stringVariant { CLD_UNREACHABLE; });
                begin++;
            }
            newPrimary = PrimaryExpression(PrimaryExpressionConstant(
                start, begin,
                match(
                    literal, [](const std::string& str) -> PrimaryExpressionConstant::variant { return str; },
                    [](const Lexer::NonCharString& str) -> PrimaryExpressionConstant::variant { return str; },
                    [](const auto&) -> PrimaryExpressionConstant::variant { CLD_UNREACHABLE; }),
                Lexer::CToken::Type::None));
        }
        else if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenParentheses)
        {
            context.parenthesesEntered(begin);
            const auto* openPpos = begin++;
            auto expression = parseExpression(
                begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
            if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                        Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos)))
            {
                context.skipUntil(begin, end);
            }
            newPrimary = PrimaryExpression(PrimaryExpressionParenthese(start, begin, std::move(expression)));
            context.parenthesesLeft();
        }
        else
        {
            if (begin == end)
            {
                context.log(Errors::Parser::EXPECTED_LITERAL_N_OR_N.args(
                    diag::after(*(begin - 1)), context.getSourceInterface(), Lexer::TokenType::Identifier,
                    Lexer::TokenType::OpenParentheses, *(begin - 1)));
            }
            else
            {
                context.log(Errors::Parser::EXPECTED_LITERAL_N_OR_N_INSTEAD_OF_N.args(
                    *begin, context.getSourceInterface(), Lexer::TokenType::Identifier,
                    Lexer::TokenType::OpenParentheses, *begin));
            }
            context.skipUntil(begin, end);
        }
        if (newPrimary)
        {
            current = std::make_unique<PostFixExpression>(
                PostFixExpressionPrimaryExpression(start, begin, std::move(*newPrimary)));
        }
    }
    else
    {
        begin++;
        auto type = parseTypeName(begin, end, context);
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                    Notes::TO_MATCH_N_HERE.args(*start, context.getSourceInterface(), *start)))
        {
            context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenBrace));
        }

        std::optional<Lexer::CTokenIterator> openBrace;
        if (!expect(Lexer::TokenType::OpenBrace, begin, end, context))
        {
            context.skipUntil(begin, end, firstInitializerListSet);
        }
        else
        {
            openBrace = begin - 1;
        }

        auto initializer = parseInitializerList(begin, end, context);
        if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        if (openBrace)
        {
            if (!expect(Lexer::TokenType::CloseBrace, begin, end, context,
                        Notes::TO_MATCH_N_HERE.args(**openBrace, context.getSourceInterface(), **openBrace)))
            {
                context.skipUntil(begin, end, firstPostfixSet);
            }
        }
        else
        {
            if (!expect(Lexer::TokenType::CloseBrace, begin, end, context))
            {
                context.skipUntil(begin, end, firstPostfixSet);
            }
        }
        if (initializer && type)
        {
            current = std::make_unique<PostFixExpression>(
                PostFixExpressionTypeInitializer(start, begin, std::move(*type), std::move(*initializer)));
        }
    }

    parsePostFixExpressionSuffix(start, begin, end, current, context);

    if (!current)
    {
        return {};
    }
    return std::move(*current);
}
