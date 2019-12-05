#include "Parser.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>

#include "ParserUtil.hpp"

using namespace OpenCL::Syntax;

OpenCL::Syntax::Expression OpenCL::Parser::parseExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                           std::vector<Lexer::Token>::const_iterator end,
                                                           Context& context)
{
    auto start = begin;
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

std::optional<OpenCL::Syntax::AssignmentExpression>
    OpenCL::Parser::parseAssignmentExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                              std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    auto start = begin;
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
                    default: OPENCL_UNREACHABLE;
                }
            }(),
            parseConditionalExpression(begin, end, context.withRecoveryTokens(assignmentSet)));
    }

    return AssignmentExpression(start, begin, std::move(result), std::move(list));
}

namespace
{
    using StateVariant = std::variant<std::monostate, std::optional<Term>, std::optional<AdditiveExpression>,
                                      std::optional<ShiftExpression>, std::optional<RelationalExpression>,
                                      std::optional<EqualityExpression>, BitAndExpression, BitXorExpression,
                                      BitOrExpression, LogicalAndExpression, LogicalOrExpression>;

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

    StateVariant parseBinaryOperators(EndState endState, std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                      std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                      OpenCL::Parser::Context& context)
    {
        StateVariant state;
        auto start = begin;
        auto firstSet = [endState, &state]() -> OpenCL::Parser::Context::TokenBitSet {
            OpenCL::Parser::Context::TokenBitSet result;
            switch (endState)
            {
                case EndState::LogicalOr:
                    result |= OpenCL::Parser::Context::fromTokenTypes(OpenCL::Lexer::TokenType::LogicOr);
                    if (std::holds_alternative<LogicalOrExpression>(state))
                    {
                        return result;
                    }
                    [[fallthrough]];
                case EndState::LogicalAnd:
                    result |= OpenCL::Parser::Context::fromTokenTypes(OpenCL::Lexer::TokenType::LogicAnd);
                    if (std::holds_alternative<LogicalAndExpression>(state))
                    {
                        return result;
                    }
                    [[fallthrough]];
                case EndState::BitOr:
                    result |= OpenCL::Parser::Context::fromTokenTypes(OpenCL::Lexer::TokenType::BitOr);
                    if (std::holds_alternative<BitOrExpression>(state))
                    {
                        return result;
                    }
                    [[fallthrough]];
                case EndState::BitXor:
                    result |= OpenCL::Parser::Context::fromTokenTypes(OpenCL::Lexer::TokenType::BitXor);
                    if (std::holds_alternative<BitXorExpression>(state))
                    {
                        return result;
                    }
                    [[fallthrough]];
                case EndState::BitAnd:
                    result |= OpenCL::Parser::Context::fromTokenTypes(OpenCL::Lexer::TokenType::Ampersand);
                    if (std::holds_alternative<BitAndExpression>(state))
                    {
                        return result;
                    }
                    [[fallthrough]];
                case EndState::Equality:
                    result |= OpenCL::Parser::Context::fromTokenTypes(OpenCL::Lexer::TokenType::Equal,
                                                                      OpenCL::Lexer::TokenType::NotEqual);
                    if (std::holds_alternative<std::optional<EqualityExpression>>(state))
                    {
                        return result;
                    }
                    [[fallthrough]];
                case EndState::Relational:
                    result |= OpenCL::Parser::Context::fromTokenTypes(
                        OpenCL::Lexer::TokenType::LessThan, OpenCL::Lexer::TokenType::LessThanOrEqual,
                        OpenCL::Lexer::TokenType::GreaterThan, OpenCL::Lexer::TokenType::GreaterThanOrEqual);
                    if (std::holds_alternative<std::optional<RelationalExpression>>(state))
                    {
                        return result;
                    }
                    [[fallthrough]];
                case EndState::Shift:
                    result |= OpenCL::Parser::Context::fromTokenTypes(OpenCL::Lexer::TokenType::ShiftLeft,
                                                                      OpenCL::Lexer::TokenType::ShiftRight);
                    if (std::holds_alternative<std::optional<ShiftExpression>>(state))
                    {
                        return result;
                    }
                    [[fallthrough]];
                case EndState::Additive:
                    result |= OpenCL::Parser::Context::fromTokenTypes(OpenCL::Lexer::TokenType::Plus,
                                                                      OpenCL::Lexer::TokenType::Minus);
                    if (std::holds_alternative<std::optional<AdditiveExpression>>(state))
                    {
                        return result;
                    }
                    [[fallthrough]];
                case EndState::Term:
                    result |= OpenCL::Parser::Context::fromTokenTypes(OpenCL::Lexer::TokenType::Asterisk,
                                                                      OpenCL::Lexer::TokenType::Division,
                                                                      OpenCL::Lexer::TokenType::Percent);
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
                case OpenCL::getIndex<std::monostate>(state):
                {
                    auto result = parseCastExpression(begin, end, context.withRecoveryTokens(firstSet()));

                    std::vector<std::pair<Term::BinaryDotOperator, CastExpression>> list;
                    while (begin != end
                           && (begin->getTokenType() == OpenCL::Lexer::TokenType::Asterisk
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::Division
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::Percent))
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
                                        case OpenCL::Lexer::TokenType::Asterisk:
                                            return Term::BinaryDotOperator::BinaryMultiply;
                                        case OpenCL::Lexer::TokenType::Division:
                                            return Term::BinaryDotOperator::BinaryDivide;
                                        case OpenCL::Lexer::TokenType::Percent:
                                            return Term::BinaryDotOperator::BinaryRemainder;
                                        default: OPENCL_UNREACHABLE;
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
                case OpenCL::getIndex<std::optional<Term>>(state):
                {
                    auto& result = std::get<std::optional<Term>>(state);

                    std::vector<std::pair<AdditiveExpression::BinaryDashOperator, Term>> list;
                    while (begin != end
                           && (begin->getTokenType() == OpenCL::Lexer::TokenType::Plus
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::Minus))
                    {
                        auto token = begin->getTokenType();
                        begin++;
                        auto newTerm = parseTerm(begin, end, context.withRecoveryTokens(firstSet()));
                        if (newTerm)
                        {
                            list.emplace_back(token == OpenCL::Lexer::TokenType::Plus ?
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
                case OpenCL::getIndex<std::optional<AdditiveExpression>>(state):
                {
                    auto& result = std::get<std::optional<AdditiveExpression>>(state);

                    std::vector<std::pair<ShiftExpression::ShiftOperator, AdditiveExpression>> list;
                    while (begin != end
                           && (begin->getTokenType() == OpenCL::Lexer::TokenType::ShiftRight
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::ShiftLeft))
                    {
                        auto token = begin->getTokenType();
                        begin++;
                        auto newAdd = parseAdditiveExpression(begin, end, context.withRecoveryTokens(firstSet()));
                        if (newAdd)
                        {
                            list.emplace_back(token == OpenCL::Lexer::TokenType::ShiftRight ?
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
                case OpenCL::getIndex<std::optional<ShiftExpression>>(state):
                {
                    auto& result = std::get<std::optional<ShiftExpression>>(state);

                    std::vector<std::pair<RelationalExpression::RelationalOperator, ShiftExpression>> list;
                    while (begin != end
                           && (begin->getTokenType() == OpenCL::Lexer::TokenType::LessThan
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::LessThanOrEqual
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::GreaterThan
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::GreaterThanOrEqual))
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
                                        case OpenCL::Lexer::TokenType::LessThan:
                                            return RelationalExpression::RelationalOperator::LessThan;
                                        case OpenCL::Lexer::TokenType::LessThanOrEqual:
                                            return RelationalExpression::RelationalOperator::LessThanOrEqual;
                                        case OpenCL::Lexer::TokenType::GreaterThan:
                                            return RelationalExpression::RelationalOperator::GreaterThan;
                                        case OpenCL::Lexer::TokenType::GreaterThanOrEqual:
                                            return RelationalExpression::RelationalOperator::GreaterThanOrEqual;
                                        default: OPENCL_UNREACHABLE;
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
                case OpenCL::getIndex<std::optional<RelationalExpression>>(state):
                {
                    auto& result = std::get<std::optional<RelationalExpression>>(state);

                    std::vector<std::pair<EqualityExpression::EqualityOperator, RelationalExpression>> list;
                    while (begin != end
                           && (begin->getTokenType() == OpenCL::Lexer::TokenType::Equal
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::NotEqual))
                    {
                        auto token = begin->getTokenType();
                        begin++;
                        auto newRelational =
                            parseRelationalExpression(begin, end, context.withRecoveryTokens(firstSet()));
                        if (newRelational)
                        {
                            list.emplace_back(token == OpenCL::Lexer::TokenType::Equal ?
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
                case OpenCL::getIndex<std::optional<EqualityExpression>>(state):
                {
                    auto& result = std::get<std::optional<EqualityExpression>>(state);

                    std::vector<EqualityExpression> list;
                    if (result)
                    {
                        list.push_back(std::move(*result));
                    }
                    while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Ampersand)
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
                case OpenCL::getIndex<BitAndExpression>(state):
                {
                    std::vector<BitAndExpression> list;
                    list.push_back(std::move(std::get<BitAndExpression>(state)));
                    while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::BitXor)
                    {
                        begin++;
                        list.push_back(parseBitAndExpression(begin, end, context.withRecoveryTokens(firstSet())));
                    }

                    state = BitXorExpression(start, begin, std::move(list));
                    break;
                }
                case OpenCL::getIndex<BitXorExpression>(state):
                {
                    std::vector<BitXorExpression> list;
                    list.push_back(std::move(std::get<BitXorExpression>(state)));
                    while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::BitOr)
                    {
                        begin++;
                        list.push_back(parseBitXorExpression(begin, end, context.withRecoveryTokens(firstSet())));
                    }
                    state = BitOrExpression(start, begin, std::move(list));
                    break;
                }
                case OpenCL::getIndex<BitOrExpression>(state):
                {
                    std::vector<BitOrExpression> list;
                    list.push_back(std::move(std::get<BitOrExpression>(state)));
                    while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::LogicAnd)
                    {
                        begin++;
                        list.push_back(parseBitOrExpression(begin, end, context.withRecoveryTokens(firstSet())));
                    }

                    state = LogicalAndExpression(start, begin, std::move(list));
                    break;
                }
                case OpenCL::getIndex<LogicalAndExpression>(state):
                {
                    std::vector<LogicalAndExpression> list;
                    list.push_back(std::move(std::get<LogicalAndExpression>(state)));
                    while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::LogicOr)
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

OpenCL::Syntax::ConditionalExpression
    OpenCL::Parser::parseConditionalExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                               std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    auto start = begin;
    auto logicalOrExpression = std::get<LogicalOrExpression>(
        parseBinaryOperators(EndState::LogicalOr, begin, end,
                             context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::QuestionMark))));
    if (begin != end && begin->getTokenType() == Lexer::TokenType::QuestionMark)
    {
        auto questionMarkpos = begin;
        begin++;
        auto optionalExpression = std::make_unique<Expression>(
            parseExpression(begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Colon))));
        if (!expect(Lexer::TokenType::Colon, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'?'"), questionMarkpos,
                                   Modifier(questionMarkpos, questionMarkpos + 1, Modifier::PointAtBeginning))}))
        {
            context.skipUntil(begin, end, firstExpressionSet);
        }
        return ConditionalExpression(
            start, begin, std::move(logicalOrExpression), std::move(optionalExpression),
            std::make_unique<ConditionalExpression>(parseConditionalExpression(begin, end, context)));
    }
    return ConditionalExpression(start, begin, std::move(logicalOrExpression));
}

OpenCL::Syntax::LogicalOrExpression
    OpenCL::Parser::parseLogicalOrExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                             std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    return std::get<LogicalOrExpression>(parseBinaryOperators(EndState::LogicalOr, begin, end, context));
}

OpenCL::Syntax::LogicalAndExpression
    OpenCL::Parser::parseLogicalAndExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                              std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    return std::get<LogicalAndExpression>(parseBinaryOperators(EndState::LogicalAnd, begin, end, context));
}

OpenCL::Syntax::BitOrExpression OpenCL::Parser::parseBitOrExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                     std::vector<Lexer::Token>::const_iterator end,
                                                                     Context& context)
{
    return std::get<BitOrExpression>(parseBinaryOperators(EndState::BitOr, begin, end, context));
}

OpenCL::Syntax::BitXorExpression OpenCL::Parser::parseBitXorExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                       std::vector<Lexer::Token>::const_iterator end,
                                                                       Context& context)
{
    return std::get<BitXorExpression>(parseBinaryOperators(EndState::BitXor, begin, end, context));
}

OpenCL::Syntax::BitAndExpression OpenCL::Parser::parseBitAndExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                                                       std::vector<Lexer::Token>::const_iterator end,
                                                                       Context& context)
{
    return std::get<BitAndExpression>(parseBinaryOperators(EndState::BitAnd, begin, end, context));
}

std::optional<OpenCL::Syntax::EqualityExpression>
    OpenCL::Parser::parseEqualityExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                            std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    return std::get<std::optional<EqualityExpression>>(parseBinaryOperators(EndState::Equality, begin, end, context));
}

std::optional<OpenCL::Syntax::RelationalExpression>
    OpenCL::Parser::parseRelationalExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                              std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    return std::get<std::optional<RelationalExpression>>(
        parseBinaryOperators(EndState::Relational, begin, end, context));
}

std::optional<OpenCL::Syntax::ShiftExpression>
    OpenCL::Parser::parseShiftExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                         std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    return std::get<std::optional<ShiftExpression>>(parseBinaryOperators(EndState::Shift, begin, end, context));
}

std::optional<OpenCL::Syntax::AdditiveExpression>
    OpenCL::Parser::parseAdditiveExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                            std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    return std::get<std::optional<AdditiveExpression>>(parseBinaryOperators(EndState::Additive, begin, end, context));
}

std::optional<OpenCL::Syntax::Term> OpenCL::Parser::parseTerm(std::vector<Lexer::Token>::const_iterator& begin,
                                                              std::vector<Lexer::Token>::const_iterator end,
                                                              Context& context)
{
    return std::get<std::optional<Term>>(parseBinaryOperators(EndState::Term, begin, end, context));
}

std::optional<OpenCL::Syntax::TypeName> OpenCL::Parser::parseTypeName(std::vector<Lexer::Token>::const_iterator& begin,
                                                                      std::vector<Lexer::Token>::const_iterator end,
                                                                      Context& context)
{
    auto start = begin;
    auto specifierQualifiers =
        parseSpecifierQualifierList(begin, end, context.withRecoveryTokens(firstAbstractDeclaratorSet));
    if (specifierQualifiers.empty())
    {
        if (begin < end)
        {
            context.log({Message::error(
                ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("typename", '\'' + begin->getRepresentation() + '\''),
                start, begin, Modifier(begin, begin + 1, Modifier::PointAtBeginning))});
        }
        else
        {
            context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("typename"), start,
                                        Modifier(begin - 1, begin, Modifier::InsertAtEnd))});
        }
    }

    if (begin < end && firstIsInAbstractDeclarator(*begin, context))
    {
        return TypeName(start, begin, std::move(specifierQualifiers),
                        std::make_unique<AbstractDeclarator>(parseAbstractDeclarator(begin, end, context)));
    }

    return TypeName(start, begin, std::move(specifierQualifiers), nullptr);
}

namespace
{
    bool isPostFixOperator(const OpenCL::Lexer::Token& token)
    {
        switch (token.getTokenType())
        {
            case OpenCL::Lexer::TokenType::Arrow:
            case OpenCL::Lexer::TokenType::Dot:
            case OpenCL::Lexer::TokenType::OpenSquareBracket:
            case OpenCL::Lexer::TokenType::OpenParentheses:
            case OpenCL::Lexer::TokenType::Increment:
            case OpenCL::Lexer::TokenType::Decrement: return true;
            default: break;
        }
        return false;
    }

    void parsePostFixExpressionSuffix(std::vector<OpenCL::Lexer::Token>::const_iterator start,
                                      std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                      std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                      std::unique_ptr<PostFixExpression>& current, OpenCL::Parser::Context& context)
    {
        while (begin != end && isPostFixOperator(*begin))
        {
            if (begin->getTokenType() == OpenCL::Lexer::TokenType::OpenParentheses)
            {
                context.parenthesesEntered(begin);
                auto openPpos = begin;
                begin++;
                std::vector<std::unique_ptr<AssignmentExpression>> nonCommaExpressions;
                bool first = true;
                while (begin < end && begin->getTokenType() != OpenCL::Lexer::TokenType::CloseParentheses)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else if (begin->getTokenType() == OpenCL::Lexer::TokenType::Comma)
                    {
                        begin++;
                    }
                    else if (firstIsInAssignmentExpression(*begin, context))
                    {
                        expect(OpenCL::Lexer::TokenType::Comma, start, begin, end, context);
                    }
                    else
                    {
                        break;
                    }

                    auto assignment = parseAssignmentExpression(
                        begin, end,
                        context.withRecoveryTokens(OpenCL::Parser::Context::fromTokenTypes(
                            OpenCL::Lexer::TokenType::CloseParentheses, OpenCL::Lexer::TokenType::Comma)));
                    if (assignment)
                    {
                        nonCommaExpressions.push_back(std::make_unique<AssignmentExpression>(std::move(*assignment)));
                    }
                }

                if (!expect(OpenCL::Lexer::TokenType::CloseParentheses, start, begin, end, context,
                            {OpenCL::Message::note(
                                OpenCL::Notes::TO_MATCH_N_HERE.args("'('"), start, openPpos,
                                OpenCL::Modifier(openPpos, openPpos + 1, OpenCL::Modifier::PointAtBeginning))}))
                {
                    context.skipUntil(begin, end, OpenCL::Parser::firstPostfixSet);
                }
                if (current)
                {
                    current = std::make_unique<PostFixExpression>(PostFixExpressionFunctionCall(
                        start, begin, std::move(current), std::move(nonCommaExpressions)));
                }
                context.parenthesesLeft();
            }
            else if (begin->getTokenType() == OpenCL::Lexer::TokenType::OpenSquareBracket)
            {
                context.squareBracketEntered(begin);
                auto openPpos = begin;
                begin++;
                auto expression = parseExpression(begin, end,
                                                  context.withRecoveryTokens(OpenCL::Parser::Context::fromTokenTypes(
                                                      OpenCL::Lexer::TokenType::CloseSquareBracket)));

                if (!expect(OpenCL::Lexer::TokenType::CloseSquareBracket, start, begin, end, context,
                            {OpenCL::Message::note(
                                OpenCL::Notes::TO_MATCH_N_HERE.args("'['"), start, openPpos,
                                OpenCL::Modifier(openPpos, openPpos + 1, OpenCL::Modifier::PointAtBeginning))}))
                {
                    context.skipUntil(begin, end, OpenCL::Parser::firstPostfixSet);
                }
                if (current)
                {
                    current = std::make_unique<PostFixExpression>(
                        PostFixExpressionSubscript(start, begin, std::move(current), std::move(expression)));
                }
                context.squareBracketLeft();
            }
            else if (begin->getTokenType() == OpenCL::Lexer::TokenType::Increment)
            {
                begin++;
                if (current)
                {
                    current = std::make_unique<PostFixExpression>(
                        PostFixExpressionIncrement(start, begin, std::move(current)));
                }
            }
            else if (begin->getTokenType() == OpenCL::Lexer::TokenType::Decrement)
            {
                begin++;
                if (current)
                {
                    current = std::make_unique<PostFixExpression>(
                        PostFixExpressionDecrement(start, begin, std::move(current)));
                }
            }
            else if (begin->getTokenType() == OpenCL::Lexer::TokenType::Dot)
            {
                begin++;
                std::string name;
                if (!expect(OpenCL::Lexer::TokenType::Identifier, start, begin, end, context, {}, &name))
                {
                    context.skipUntil(begin, end, OpenCL::Parser::firstPostfixSet);
                }
                if (current)
                {
                    current = std::make_unique<PostFixExpression>(
                        PostFixExpressionDot(start, begin, std::move(current), name));
                }
            }
            else
            {
                begin++;
                std::string name;
                if (!expect(OpenCL::Lexer::TokenType::Identifier, start, begin, end, context, {}, &name))
                {
                    context.skipUntil(begin, end, OpenCL::Parser::firstPostfixSet);
                }
                if (current)
                {
                    current = std::make_unique<PostFixExpression>(
                        PostFixExpressionArrow(start, begin, std::move(current), name));
                }
            }
        }
    }
} // namespace

std::optional<OpenCL::Syntax::CastExpression>
    OpenCL::Parser::parseCastExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                        std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    auto start = begin;
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
    if (!expect(Lexer::TokenType::CloseParentheses, start, begin, end, context,
                {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), start, start,
                               Modifier(start, start + 1, Modifier::PointAtBeginning))}))
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

    std::optional<std::vector<Lexer::Token>::const_iterator> openBrace;
    if (!expect(Lexer::TokenType::OpenBrace, start, begin, end, context))
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
        if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'{'"), start, *openBrace,
                                   Modifier(*openBrace, *openBrace + 1, Modifier::PointAtBeginning))}))
        {
            context.skipUntil(begin, end, firstPostfixSet);
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context))
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

std::optional<OpenCL::Syntax::UnaryExpression>
    OpenCL::Parser::parseUnaryExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                         std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    auto start = begin;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::SizeofKeyword)
    {
        begin++;
        if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenParentheses
            && (begin + 1 == end || firstIsInTypeName(*(begin + 1), context)))
        {
            auto openPpos = begin;
            begin++;
            auto type = parseTypeName(
                begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
            if (!expect(Lexer::TokenType::CloseParentheses, start, begin, end, context,
                        {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), start, openPpos,
                                       Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
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
    else if (context.isInPreprocessor() && begin < end && begin->getTokenType() == Lexer::TokenType::DefinedKeyword)
    {
        begin++;
        std::optional<std::vector<Lexer::Token>::const_iterator> openP;
        if (begin->getTokenType() == Lexer::TokenType::OpenParentheses)
        {
            openP = begin;
            begin++;
        }
        std::string identifier;
        if (!expect(Lexer::TokenType::Identifier, start, begin, end, context, {}, &identifier))
        {
            context.skipUntil(begin, end,
                              openP ? Context::fromTokenTypes(Lexer::TokenType::CloseParentheses) :
                                      Context::TokenBitSet{});
        }
        if (openP)
        {
            if (!expect(Lexer::TokenType::CloseParentheses, start, begin, end, context,
                        {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), start, *openP,
                                       Modifier(*openP, *openP + 1, Modifier::PointAtBeginning))}))
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
                default: OPENCL_UNREACHABLE;
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

std::optional<OpenCL::Syntax::PostFixExpression>
    OpenCL::Parser::parsePostFixExpression(std::vector<Lexer::Token>::const_iterator& begin,
                                           std::vector<Lexer::Token>::const_iterator end, Context& context)
{
    std::unique_ptr<PostFixExpression> current;

    auto start = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenParentheses || begin + 1 == end
        || !firstIsInTypeName(*(begin + 1), context))
    {
        auto token = context.withRecoveryTokens(firstPostfixSet);
        std::optional<OpenCL::Syntax::PrimaryExpression> newPrimary;
        if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
        {
            const auto& value = std::get<std::string>(begin->getValue());
            begin++;
            newPrimary = PrimaryExpression(PrimaryExpressionIdentifier(start, begin, value));
        }
        else if (begin < end && begin->getTokenType() == Lexer::TokenType::Literal)
        {
            auto value = std::visit(
                [](auto&& value) -> typename PrimaryExpressionConstant::variant {
                    using T = std::decay_t<decltype(value)>;
                    if constexpr (std::is_constructible_v<typename PrimaryExpressionConstant::variant, T>)
                    {
                        return {std::forward<decltype(value)>(value)};
                    }
                    else
                    {
                        throw std::runtime_error("ICE: Can't convert type of variant to constant expression");
                    }
                },
                begin->getValue());
            begin++;
            newPrimary = PrimaryExpression(PrimaryExpressionConstant(start, begin, std::move(value)));
        }
        else if (begin < end && begin->getTokenType() == Lexer::TokenType::StringLiteral)
        {
            //            using stringVariant = std::variant<std::string, std::wstring>;
            //            stringVariant literal = match(
            //                begin->getValue(), [](const std::string& str) -> stringVariant { return str; },
            //                [](const std::wstring& str) -> stringVariant { return str; },
            //                [](auto &&) -> stringVariant { OPENCL_UNREACHABLE; });
            //            begin++;
            //
            //            std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> converter;
            //            while (begin < end && begin->getTokenType() == Lexer::TokenType::StringLiteral)
            //            {
            //                literal = match(
            //                    begin->getValue(),
            //                    [&literal, &converter](const std::string& str) -> stringVariant {
            //                        return match(
            //                            literal, [&str](const std::string& lhs) -> stringVariant { return lhs + str;
            //                            },
            //                            [&str, &converter](const std::wstring& lhs) -> stringVariant {
            //                                return lhs + converter.from_bytes(str);
            //                            },
            //                            [](const auto&) -> stringVariant { OPENCL_UNREACHABLE; });
            //                    },
            //                    [&literal, &converter](const std::wstring& str) -> stringVariant {
            //                        return match(
            //                            literal,
            //                            [&str, &converter](const std::string& lhs) -> stringVariant {
            //                                return converter.from_bytes(lhs) + str;
            //                            },
            //                            [&str](const std::wstring& lhs) -> stringVariant { return lhs + str; },
            //                            [](const auto&) -> stringVariant { OPENCL_UNREACHABLE; });
            //                    },
            //                    [](auto &&) -> stringVariant { OPENCL_UNREACHABLE; });
            //                begin++;
            //            }
            //            newPrimary = PrimaryExpression(PrimaryExpressionConstant(
            //                start, begin,
            //                match(
            //                    literal, [](const std::string& str) -> PrimaryExpressionConstant::variant { return
            //                    str; },
            //                    [](const std::wstring& str) -> PrimaryExpressionConstant::variant { return str; },
            //                    [](const auto&) -> PrimaryExpressionConstant::variant { OPENCL_UNREACHABLE; })));
        }
        else if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenParentheses)
        {
            context.parenthesesEntered(begin);
            auto openPpos = begin++;
            auto expression = parseExpression(
                begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
            if (!expect(Lexer::TokenType::CloseParentheses, start, begin, end, context,
                        {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), start, openPpos,
                                       Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
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
                context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args(
                                                OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")),
                                            start, begin, Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd))});
            }
            else
            {
                context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                        OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"),
                                        '\'' + begin->getRepresentation() + '\''),
                                    start, begin, Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning))});
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
        if (!expect(Lexer::TokenType::CloseParentheses, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), start, start,
                                   Modifier(start, start + 1, Modifier::PointAtBeginning))}))
        {
            context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenBrace));
        }

        std::optional<std::vector<Lexer::Token>::const_iterator> openBrace;
        if (!expect(Lexer::TokenType::OpenBrace, start, begin, end, context))
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
            if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context,
                        {Message::note(Notes::TO_MATCH_N_HERE.args("'{'"), start, *openBrace,
                                       Modifier(*openBrace, *openBrace + 1, Modifier::PointAtBeginning))}))
            {
                context.skipUntil(begin, end, firstPostfixSet);
            }
        }
        else
        {
            if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context))
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
