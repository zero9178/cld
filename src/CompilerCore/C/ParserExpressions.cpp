#include "Parser.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>

#include "ParserUtil.hpp"

using namespace OpenCL::Syntax;

Expression OpenCL::Parser::parseExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, Context& context,
                                           InRecoverySet recoverySet)
{
    auto start = begin;
    std::vector<AssignmentExpression> expressions;
    auto assignment = parseAssignmentExpression(
        begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)),
        [](const Lexer::Token&) { return false; });
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
                begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)),
                [](const Lexer::Token&) { return false; });
            if (assignment)
            {
                expressions.push_back(std::move(*assignment));
            }
        }
    }
    return Expression(start, begin, std::move(expressions));
}

std::optional<AssignmentExpression> OpenCL::Parser::parseAssignmentExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              Context& context,
                                                                              InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseConditionalExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return isAssignment(token.getTokenType()) || recoverySet(token);
    });

    std::vector<std::pair<AssignmentExpression::AssignOperator, ConditionalExpression>> list;
    while (begin != end && isAssignment(begin->getTokenType()))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newConditional = parseConditionalExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return isAssignment(token.getTokenType()) || recoverySet(token);
        });
        if (newConditional)
        {
            list.emplace_back(
                [token]() -> AssignmentExpression::AssignOperator {
                    switch (token)
                    {
                        case Lexer::TokenType::Assignment: return AssignmentExpression::AssignOperator::NoOperator;
                        case Lexer::TokenType::PlusAssign: return AssignmentExpression::AssignOperator::PlusAssign;
                        case Lexer::TokenType::MinusAssign: return AssignmentExpression::AssignOperator::MinusAssign;
                        case Lexer::TokenType::DivideAssign: return AssignmentExpression::AssignOperator::DivideAssign;
                        case Lexer::TokenType::MultiplyAssign:
                            return AssignmentExpression::AssignOperator::MultiplyAssign;
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
                std::move(*newConditional));
        }
    }

    if (!result)
    {
        return {};
    }
    return AssignmentExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<ConditionalExpression> OpenCL::Parser::parseConditionalExpression(Tokens::const_iterator& begin,
                                                                                Tokens::const_iterator end,
                                                                                Context& context,
                                                                                InRecoverySet recoverySet)
{
    auto start = begin;
    auto logicalOrExperssion = parseLogicalOrExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::QuestionMark || recoverySet(token);
    });

    if (begin != end && begin->getTokenType() == Lexer::TokenType::QuestionMark)
    {
        auto questionMarkpos = begin;
        begin++;
        auto optionalExpression = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::Colon || recoverySet(token);
        });
        if (!expect(Lexer::TokenType::Colon, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'?'"), context.getLineStart(questionMarkpos),
                                   context.getLineEnd(questionMarkpos),
                                   Modifier(questionMarkpos, questionMarkpos + 1, Modifier::PointAtBeginning))}))
        {
            //            skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            //                return firstIsInConditionalExpression(token, context) || recoverySet(token);
            //            });
            context.skipUntil(begin, end, firstExpressionSet);
        }
        auto optionalConditional = parseConditionalExpression(begin, end, context, recoverySet);
        if (!logicalOrExperssion || !optionalConditional)
        {
            return {};
        }
        return ConditionalExpression(start, begin, std::move(*logicalOrExperssion),
                                     std::make_unique<Expression>(std::move(optionalExpression)),
                                     std::make_unique<ConditionalExpression>(std::move(*optionalConditional)));
    }
    if (!logicalOrExperssion)
    {
        return {};
    }
    return ConditionalExpression(start, begin, std::move(*logicalOrExperssion));
}

namespace
{
    template <class... Args>
    using VariantOfOptionals = std::variant<std::monostate, std::optional<Args>...>;

    using StateVariant = VariantOfOptionals<Term, AdditiveExpression, ShiftExpression, RelationalExpression,
                                            EqualityExpression, BitAndExpression, BitXorExpression, BitOrExpression,
                                            LogicalAndExpression, LogicalOrExpression>;

    enum class EndState
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

    template <typename T, typename... Ts>
    constexpr size_t getIndex(std::variant<Ts...> const&)
    {
        size_t r = 0;
        auto test = [&](bool b) {
            if (!b)
                ++r;
            return b;
        };
        (test(std::is_same_v<T, Ts>) || ...);
        return r;
    }

    StateVariant parseBinaryOperators(EndState endState, OpenCL::Parser::Tokens::const_iterator& begin,
                                      OpenCL::Parser::Tokens::const_iterator end, OpenCL::Parser::Context& context,
                                      OpenCL::Parser::InRecoverySet recoverySet)
    {
        StateVariant state;
        auto isFirstIn = [endState, &state](const OpenCL::Lexer::Token& token) -> bool {
            switch (endState)
            {
                case EndState::LogicalOr:
                    if (token.getTokenType() == OpenCL::Lexer::TokenType::LogicOr)
                    {
                        return true;
                    }
                    if (std::holds_alternative<std::optional<LogicalOrExpression>>(state))
                    {
                        return false;
                    }
                    [[fallthrough]];
                case EndState::LogicalAnd:
                    if (token.getTokenType() == OpenCL::Lexer::TokenType::LogicAnd)
                    {
                        return true;
                    }
                    if (std::holds_alternative<std::optional<LogicalOrExpression>>(state))
                    {
                        return false;
                    }
                    [[fallthrough]];
                case EndState::BitOr:
                    if (token.getTokenType() == OpenCL::Lexer::TokenType::BitOr)
                    {
                        return true;
                    }
                    if (std::holds_alternative<std::optional<LogicalOrExpression>>(state))
                    {
                        return false;
                    }
                    [[fallthrough]];
                case EndState::BitXor:
                    if (token.getTokenType() == OpenCL::Lexer::TokenType::BitXor)
                    {
                        return true;
                    }
                    if (std::holds_alternative<std::optional<LogicalOrExpression>>(state))
                    {
                        return false;
                    }
                    [[fallthrough]];
                case EndState::BitAnd:
                    if (token.getTokenType() == OpenCL::Lexer::TokenType::Ampersand)
                    {
                        return true;
                    }
                    if (std::holds_alternative<std::optional<LogicalOrExpression>>(state))
                    {
                        return false;
                    }
                    [[fallthrough]];
                case EndState::Equality:
                    if (token.getTokenType() == OpenCL::Lexer::TokenType::Equal
                        || token.getTokenType() == OpenCL::Lexer::TokenType::NotEqual)
                    {
                        return true;
                    }
                    if (std::holds_alternative<std::optional<LogicalOrExpression>>(state))
                    {
                        return false;
                    }
                    [[fallthrough]];
                case EndState::Relational:
                    if (token.getTokenType() == OpenCL::Lexer::TokenType::LessThan
                        || token.getTokenType() == OpenCL::Lexer::TokenType::LessThanOrEqual
                        || token.getTokenType() == OpenCL::Lexer::TokenType::GreaterThan
                        || token.getTokenType() == OpenCL::Lexer::TokenType::GreaterThanOrEqual)
                    {
                        return true;
                    }
                    if (std::holds_alternative<std::optional<LogicalOrExpression>>(state))
                    {
                        return false;
                    }
                    [[fallthrough]];
                case EndState::Shift:
                    if (token.getTokenType() == OpenCL::Lexer::TokenType::ShiftLeft
                        || token.getTokenType() == OpenCL::Lexer::TokenType::ShiftRight)
                    {
                        return true;
                    }
                    if (std::holds_alternative<std::optional<LogicalOrExpression>>(state))
                    {
                        return false;
                    }
                    [[fallthrough]];
                case EndState::Additive:
                    if (token.getTokenType() == OpenCL::Lexer::TokenType::Plus
                        || token.getTokenType() == OpenCL::Lexer::TokenType::Minus)
                    {
                        return true;
                    }
                    if (std::holds_alternative<std::optional<LogicalOrExpression>>(state))
                    {
                        return false;
                    }
                    [[fallthrough]];
                case EndState::Term:
                    if (token.getTokenType() == OpenCL::Lexer::TokenType::Asterisk
                        || token.getTokenType() == OpenCL::Lexer::TokenType::Division
                        || token.getTokenType() == OpenCL::Lexer::TokenType::Percent)
                    {
                        return true;
                    }
                    [[fallthrough]];
                default: return false;
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
                case EndState::BitAnd: return !std::holds_alternative<std::optional<BitAndExpression>>(state);
                case EndState::BitXor: return !std::holds_alternative<std::optional<BitXorExpression>>(state);
                case EndState::BitOr: return !std::holds_alternative<std::optional<BitOrExpression>>(state);
                case EndState::LogicalAnd: return !std::holds_alternative<std::optional<LogicalAndExpression>>(state);
                case EndState::LogicalOr: return !std::holds_alternative<std::optional<LogicalOrExpression>>(state);
            }
            return false;
        }())
        {
            // We are using an index instead of visit to save on stack space and increase speed
            switch (state.index())
            {
                case getIndex<std::monostate>(state):
                {
                    auto start = begin;
                    auto result = parseCastExpression(begin, end, context,
                                                      [recoverySet, &isFirstIn](const OpenCL::Lexer::Token& token) {
                                                          return isFirstIn(token) || recoverySet(token);
                                                      });

                    std::vector<std::pair<Term::BinaryDotOperator, CastExpression>> list;
                    while (begin != end
                           && (begin->getTokenType() == OpenCL::Lexer::TokenType::Asterisk
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::Division
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::Percent))
                    {
                        auto token = begin->getTokenType();
                        begin++;
                        auto newCast = parseCastExpression(
                            begin, end, context, [recoverySet, &isFirstIn](const OpenCL::Lexer::Token& token) {
                                return isFirstIn(token) || recoverySet(token);
                            });
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
                case getIndex<std::optional<Term>>(state):
                {
                    auto start = begin;
                    auto& result = std::get<std::optional<Term>>(state);

                    std::vector<std::pair<AdditiveExpression::BinaryDashOperator, Term>> list;
                    while (begin != end
                           && (begin->getTokenType() == OpenCL::Lexer::TokenType::Plus
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::Minus))
                    {
                        auto token = begin->getTokenType();
                        begin++;
                        auto newTerm = std::get<std::optional<Term>>(parseBinaryOperators(
                            EndState::Term, begin, end, context, [recoverySet](const OpenCL::Lexer::Token& token) {
                                return token.getTokenType() == OpenCL::Lexer::TokenType::Minus
                                       || token.getTokenType() == OpenCL::Lexer::TokenType::Plus || recoverySet(token);
                            }));
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
                case getIndex<std::optional<AdditiveExpression>>(state):
                {
                    auto start = begin;
                    auto& result = std::get<std::optional<AdditiveExpression>>(state);

                    std::vector<std::pair<ShiftExpression::ShiftOperator, AdditiveExpression>> list;
                    while (begin != end
                           && (begin->getTokenType() == OpenCL::Lexer::TokenType::ShiftRight
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::ShiftLeft))
                    {
                        auto token = begin->getTokenType();
                        begin++;
                        auto newAdd = std::get<std::optional<AdditiveExpression>>(parseBinaryOperators(
                            EndState::Additive, begin, end, context, [recoverySet](const OpenCL::Lexer::Token& token) {
                                return token.getTokenType() == OpenCL::Lexer::TokenType::ShiftRight
                                       || token.getTokenType() == OpenCL::Lexer::TokenType::ShiftLeft
                                       || recoverySet(token);
                            }));
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
                case getIndex<std::optional<ShiftExpression>>(state):
                {
                    auto start = begin;
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
                        auto newShift = std::get<std::optional<ShiftExpression>>(parseBinaryOperators(
                            EndState::Shift, begin, end, context, [recoverySet](const OpenCL::Lexer::Token& token) {
                                return token.getTokenType() == OpenCL::Lexer::TokenType::LessThan
                                       || token.getTokenType() == OpenCL::Lexer::TokenType::LessThanOrEqual
                                       || token.getTokenType() == OpenCL::Lexer::TokenType::GreaterThan
                                       || token.getTokenType() == OpenCL::Lexer::TokenType::GreaterThanOrEqual
                                       || recoverySet(token);
                            }));
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
                case getIndex<std::optional<RelationalExpression>>(state):
                {
                    auto start = begin;
                    auto& result = std::get<std::optional<RelationalExpression>>(state);

                    std::vector<std::pair<EqualityExpression::EqualityOperator, RelationalExpression>> list;
                    while (begin != end
                           && (begin->getTokenType() == OpenCL::Lexer::TokenType::Equal
                               || begin->getTokenType() == OpenCL::Lexer::TokenType::NotEqual))
                    {
                        auto token = begin->getTokenType();
                        begin++;
                        auto newRelational = std::get<std::optional<RelationalExpression>>(parseBinaryOperators(
                            EndState::Relational, begin, end, context,
                            [recoverySet](const OpenCL::Lexer::Token& token) {
                                return token.getTokenType() == OpenCL::Lexer::TokenType::Equal
                                       || token.getTokenType() == OpenCL::Lexer::TokenType::NotEqual
                                       || recoverySet(token);
                            }));
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
                case getIndex<std::optional<EqualityExpression>>(state):
                {
                    auto start = begin;
                    auto& result = std::get<std::optional<EqualityExpression>>(state);

                    std::vector<EqualityExpression> list;
                    while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::Ampersand)
                    {
                        begin++;
                        auto newEqual = std::get<std::optional<EqualityExpression>>(parseBinaryOperators(
                            EndState::Equality, begin, end, context, [recoverySet](const OpenCL::Lexer::Token& token) {
                                return token.getTokenType() == OpenCL::Lexer::TokenType::Ampersand
                                       || recoverySet(token);
                            }));
                        if (newEqual)
                        {
                            list.push_back(std::move(*newEqual));
                        }
                    }

                    if (!result)
                    {
                        state = std::optional<BitAndExpression>{};
                    }
                    else
                    {
                        state = BitAndExpression(start, begin, std::move(*result), std::move(list));
                    }
                    break;
                }
                case getIndex<std::optional<BitAndExpression>>(state):
                {
                    auto start = begin;
                    auto& result = std::get<std::optional<BitAndExpression>>(state);

                    std::vector<BitAndExpression> list;
                    while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::BitXor)
                    {
                        begin++;
                        auto newAnd = std::get<std::optional<BitAndExpression>>(parseBinaryOperators(
                            EndState::BitAnd, begin, end, context, [recoverySet](const OpenCL::Lexer::Token& token) {
                                return token.getTokenType() == OpenCL::Lexer::TokenType::BitXor || recoverySet(token);
                            }));
                        if (newAnd)
                        {
                            list.push_back(std::move(*newAnd));
                        }
                    }

                    if (!result)
                    {
                        state = std::optional<BitXorExpression>{};
                    }
                    else
                    {
                        state = BitXorExpression(start, begin, std::move(*result), std::move(list));
                    }
                    break;
                }
                case getIndex<std::optional<BitXorExpression>>(state):
                {
                    auto start = begin;
                    auto& result = std::get<std::optional<BitXorExpression>>(state);

                    std::vector<BitXorExpression> list;
                    while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::BitOr)
                    {
                        begin++;
                        auto newXor = std::get<std::optional<BitXorExpression>>(parseBinaryOperators(
                            EndState::BitXor, begin, end, context, [recoverySet](const OpenCL::Lexer::Token& token) {
                                return token.getTokenType() == OpenCL::Lexer::TokenType::BitOr || recoverySet(token);
                            }));
                        if (newXor)
                        {
                            list.push_back(std::move(*newXor));
                        }
                    }

                    if (!result)
                    {
                        state = std::optional<BitOrExpression>{};
                    }
                    else
                    {
                        state = BitOrExpression(start, begin, std::move(*result), std::move(list));
                    }
                    break;
                }
                case getIndex<std::optional<BitOrExpression>>(state):
                {
                    auto start = begin;
                    auto& result = std::get<std::optional<BitOrExpression>>(state);

                    std::vector<BitOrExpression> list;
                    while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::LogicAnd)
                    {
                        begin++;
                        auto newOr = std::get<std::optional<BitOrExpression>>(parseBinaryOperators(
                            EndState::BitOr, begin, end, context, [recoverySet](const OpenCL::Lexer::Token& token) {
                                return token.getTokenType() == OpenCL::Lexer::TokenType::LogicAnd || recoverySet(token);
                            }));
                        if (newOr)
                        {
                            list.push_back(std::move(*newOr));
                        }
                    }

                    if (!result)
                    {
                        state = std::optional<LogicalAndExpression>{};
                    }
                    else
                    {
                        state = LogicalAndExpression(start, begin, std::move(*result), std::move(list));
                    }
                    break;
                }
                case getIndex<std::optional<LogicalAndExpression>>(state):
                {
                    auto start = begin;
                    auto& result = std::get<std::optional<LogicalAndExpression>>(state);

                    std::vector<LogicalAndExpression> list;
                    while (begin != end && begin->getTokenType() == OpenCL::Lexer::TokenType::LogicOr)
                    {
                        begin++;
                        auto newAnd = std::get<std::optional<LogicalAndExpression>>(parseBinaryOperators(
                            EndState::LogicalAnd, begin, end, context,
                            [recoverySet](const OpenCL::Lexer::Token& token) {
                                return token.getTokenType() == OpenCL::Lexer::TokenType::LogicOr || recoverySet(token);
                            }));
                        if (newAnd)
                        {
                            list.push_back(std::move(*newAnd));
                        }
                    }

                    if (!result)
                    {
                        state = std::optional<LogicalOrExpression>{};
                    }
                    else
                    {
                        state = LogicalOrExpression(start, begin, std::move(*result), std::move(list));
                    }
                    break;
                }
                default: break;
            }
        }

        return state;
    }

} // namespace

std::optional<LogicalOrExpression> OpenCL::Parser::parseLogicalOrExpression(Tokens::const_iterator& begin,
                                                                            Tokens::const_iterator end,
                                                                            Context& context, InRecoverySet recoverySet)
{
    return std::get<std::optional<LogicalOrExpression>>(
        parseBinaryOperators(EndState::LogicalOr, begin, end, context, recoverySet));
}

std::optional<LogicalAndExpression> OpenCL::Parser::parseLogicalAndExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              Context& context,
                                                                              InRecoverySet recoverySet)
{
    return std::get<std::optional<LogicalAndExpression>>(
        parseBinaryOperators(EndState::LogicalAnd, begin, end, context, recoverySet));
}

std::optional<BitOrExpression> OpenCL::Parser::parseBitOrExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end, Context& context,
                                                                    InRecoverySet recoverySet)
{
    return std::get<std::optional<BitOrExpression>>(
        parseBinaryOperators(EndState::BitOr, begin, end, context, recoverySet));
}

std::optional<BitXorExpression> OpenCL::Parser::parseBitXorExpression(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end, Context& context,
                                                                      InRecoverySet recoverySet)
{
    return std::get<std::optional<BitXorExpression>>(
        parseBinaryOperators(EndState::BitXor, begin, end, context, recoverySet));
}

std::optional<BitAndExpression> OpenCL::Parser::parseBitAndExpression(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end, Context& context,
                                                                      InRecoverySet recoverySet)
{
    return std::get<std::optional<BitAndExpression>>(
        parseBinaryOperators(EndState::BitAnd, begin, end, context, recoverySet));
}

std::optional<EqualityExpression> OpenCL::Parser::parseEqualityExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end, Context& context,
                                                                          InRecoverySet recoverySet)
{
    return std::get<std::optional<EqualityExpression>>(
        parseBinaryOperators(EndState::Equality, begin, end, context, recoverySet));
}

std::optional<RelationalExpression> OpenCL::Parser::parseRelationalExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              Context& context,
                                                                              InRecoverySet recoverySet)
{
    return std::get<std::optional<RelationalExpression>>(
        parseBinaryOperators(EndState::Relational, begin, end, context, recoverySet));
}

std::optional<ShiftExpression> OpenCL::Parser::parseShiftExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end, Context& context,
                                                                    InRecoverySet recoverySet)
{
    return std::get<std::optional<ShiftExpression>>(
        parseBinaryOperators(EndState::Shift, begin, end, context, recoverySet));
}

std::optional<AdditiveExpression> OpenCL::Parser::parseAdditiveExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end, Context& context,
                                                                          InRecoverySet recoverySet)
{
    return std::get<std::optional<AdditiveExpression>>(
        parseBinaryOperators(EndState::Additive, begin, end, context, recoverySet));
}

std::optional<Term> OpenCL::Parser::parseTerm(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                              Context& context, InRecoverySet recoverySet)
{
    return std::get<std::optional<Term>>(parseBinaryOperators(EndState::Term, begin, end, context, recoverySet));
}

std::optional<TypeName> OpenCL::Parser::parseTypeName(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                      OpenCL::Parser::Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    auto specifierQualifiers =
        parseSpecifierQualifierList(begin, end, context, [recoverySet, &context](const Lexer::Token& token) {
            return firstIsInAbstractDeclarator(token, context) || recoverySet(token);
        });
    if (specifierQualifiers.empty())
    {
        if (begin < end)
        {
            context.log({Message::error(
                ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("typename", '\'' + begin->emitBack() + '\''),
                context.getLineStart(start), context.getLineEnd(begin),
                Modifier(begin, begin + 1, Modifier::PointAtBeginning))});
        }
        else
        {
            context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("typename"), context.getLineStart(start),
                                        begin, Modifier(begin - 1, begin, Modifier::InsertAtEnd))});
        }
    }

    if (begin < end && firstIsInAbstractDeclarator(*begin, context))
    {
        auto abstractDec = parseAbstractDeclarator(begin, end, context, recoverySet);
        return TypeName(start, begin, std::move(specifierQualifiers),
                        std::make_unique<AbstractDeclarator>(std::move(abstractDec)));
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
            case OpenCL::Lexer::TokenType::OpenBracket:
            case OpenCL::Lexer::TokenType::Increment:
            case OpenCL::Lexer::TokenType::Decrement: return true;
            default: break;
        }
        return false;
    }

    void parsePostFixExpressionSuffix(std::vector<OpenCL::Lexer::Token>::const_iterator start,
                                      OpenCL::Parser::Tokens::const_iterator& begin,
                                      OpenCL::Parser::Tokens::const_iterator end,
                                      std::unique_ptr<PostFixExpression>& current, OpenCL::Parser::Context& context,
                                      OpenCL::Parser::InRecoverySet recoverySet)
    {
        while (begin != end && isPostFixOperator(*begin))
        {
            if (begin->getTokenType() == OpenCL::Lexer::TokenType::OpenBracket)
            {
                auto openPpos = begin;
                begin++;
                std::vector<std::unique_ptr<AssignmentExpression>> nonCommaExpressions;
                bool first = true;
                while (begin < end && begin->getTokenType() != OpenCL::Lexer::TokenType::CloseBracket)
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
                        begin, end, context, [recoverySet](const OpenCL::Lexer::Token& token) {
                            return token.getTokenType() == OpenCL::Lexer::TokenType::CloseBracket
                                   || token.getTokenType() == OpenCL::Lexer::TokenType::Comma || recoverySet(token);
                        });
                    if (assignment)
                    {
                        nonCommaExpressions.push_back(std::make_unique<AssignmentExpression>(std::move(*assignment)));
                    }
                }

                if (!expect(OpenCL::Lexer::TokenType::CloseBracket, start, begin, end, context,
                            {OpenCL::Message::note(
                                OpenCL::Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(start),
                                context.getLineEnd(openPpos),
                                OpenCL::Modifier(openPpos, openPpos + 1, OpenCL::Modifier::PointAtBeginning))}))
                {
                    //                    OpenCL::Parser::skipUntil(begin, end, [recoverySet](const
                    //                    OpenCL::Lexer::Token& token) {
                    //                        return isPostFixOperator(token) || recoverySet(token);
                    //                    });
                    context.skipUntil(begin, end,
                                      OpenCL::Parser::Context::fromTokenTypes(
                                          OpenCL::Lexer::TokenType::Arrow, OpenCL::Lexer::TokenType::Dot,
                                          OpenCL::Lexer::TokenType::OpenSquareBracket,
                                          OpenCL::Lexer::TokenType::OpenBracket, OpenCL::Lexer::TokenType::Increment,
                                          OpenCL::Lexer::TokenType::Decrement));
                }
                if (current)
                {
                    current = std::make_unique<PostFixExpression>(PostFixExpressionFunctionCall(
                        start, begin, std::move(current), std::move(nonCommaExpressions)));
                }
            }
            else if (begin->getTokenType() == OpenCL::Lexer::TokenType::OpenSquareBracket)
            {
                auto openPpos = begin;
                begin++;
                auto expression =
                    parseExpression(begin, end, context, [recoverySet](const OpenCL::Lexer::Token& token) {
                        return token.getTokenType() == OpenCL::Lexer::TokenType::CloseSquareBracket
                               || recoverySet(token);
                    });

                if (!expect(OpenCL::Lexer::TokenType::CloseSquareBracket, start, begin, end, context,
                            {OpenCL::Message::note(
                                OpenCL::Notes::TO_MATCH_N_HERE.args("'['"), context.getLineStart(start),
                                context.getLineEnd(openPpos),
                                OpenCL::Modifier(openPpos, openPpos + 1, OpenCL::Modifier::PointAtBeginning))}))
                {
                    //                    OpenCL::Parser::skipUntil(begin, end, [recoverySet](const
                    //                    OpenCL::Lexer::Token& token) {
                    //                        return isPostFixOperator(token) || recoverySet(token);
                    //                    });
                    context.skipUntil(begin, end,
                                      OpenCL::Parser::Context::fromTokenTypes(
                                          OpenCL::Lexer::TokenType::Arrow, OpenCL::Lexer::TokenType::Dot,
                                          OpenCL::Lexer::TokenType::OpenSquareBracket,
                                          OpenCL::Lexer::TokenType::OpenBracket, OpenCL::Lexer::TokenType::Increment,
                                          OpenCL::Lexer::TokenType::Decrement));
                }
                if (current)
                {
                    current = std::make_unique<PostFixExpression>(
                        PostFixExpressionSubscript(start, begin, std::move(current), std::move(expression)));
                }
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
                    //                    OpenCL::Parser::skipUntil(begin, end, [recoverySet](const
                    //                    OpenCL::Lexer::Token& token) {
                    //                        return isPostFixOperator(token) || recoverySet(token);
                    //                    });
                    context.skipUntil(begin, end,
                                      OpenCL::Parser::Context::fromTokenTypes(
                                          OpenCL::Lexer::TokenType::Arrow, OpenCL::Lexer::TokenType::Dot,
                                          OpenCL::Lexer::TokenType::OpenSquareBracket,
                                          OpenCL::Lexer::TokenType::OpenBracket, OpenCL::Lexer::TokenType::Increment,
                                          OpenCL::Lexer::TokenType::Decrement));
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
                    //                    OpenCL::Parser::skipUntil(begin, end, [recoverySet](const
                    //                    OpenCL::Lexer::Token& token) {
                    //                        return isPostFixOperator(token) || recoverySet(token);
                    //                    });
                    context.skipUntil(begin, end,
                                      OpenCL::Parser::Context::fromTokenTypes(
                                          OpenCL::Lexer::TokenType::Arrow, OpenCL::Lexer::TokenType::Dot,
                                          OpenCL::Lexer::TokenType::OpenSquareBracket,
                                          OpenCL::Lexer::TokenType::OpenBracket, OpenCL::Lexer::TokenType::Increment,
                                          OpenCL::Lexer::TokenType::Decrement));
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

std::optional<CastExpression> OpenCL::Parser::parseCastExpression(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end, Context& context,
                                                                  InRecoverySet recoverySet)
{
    auto start = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBracket || begin + 1 == end
        || !firstIsInTypeName(*(begin + 1), context))
    {
        auto unary = parseUnaryExpression(begin, end, context, recoverySet);
        if (!unary)
        {
            return {};
        }
        return CastExpression(start, begin, std::move(*unary));
    }
    begin++;
    auto typeName = parseTypeName(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::CloseBracket || recoverySet(token);
    });
    if (!expect(Lexer::TokenType::CloseBracket, start, begin, end, context,
                {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(start),
                               context.getLineEnd(start), Modifier(start, start + 1, Modifier::PointAtBeginning))}))
    {
        //        skipUntil(begin, end, [&context, recoverySet](const Lexer::Token& token) {
        //            return firstIsInCastExpression(token, context) || recoverySet(token);
        //        });
        context.skipUntil(begin, end, firstExpressionSet);
    }
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        auto cast = parseCastExpression(begin, end, context, recoverySet);
        if (!cast || !typeName)
        {
            return {};
        }
        return CastExpression(start, begin,
                              std::pair{std::move(*typeName), std::make_unique<CastExpression>(std::move(*cast))});
    }

    std::optional<Tokens::const_iterator> openBrace;
    if (!expect(Lexer::TokenType::OpenBrace, start, begin, end, context))
    {
        //        skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
        //            return firstIsInInitializerList(token, context) || recoverySet(token);
        //        });
        context.skipUntil(begin, end, firstInitializerListSet);
    }
    else
    {
        openBrace = begin - 1;
    }

    auto initializer = parseInitializerList(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::CloseBrace || token.getTokenType() == Lexer::TokenType::Comma
               || recoverySet(token);
    });
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
    {
        begin++;
    }
    if (openBrace)
    {
        if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'{'"), context.getLineStart(start),
                                   context.getLineEnd(*openBrace),
                                   Modifier(*openBrace, *openBrace + 1, Modifier::PointAtBeginning))}))
        {
            //            skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            //                return isPostFixOperator(token) || recoverySet(token);
            //            });
            context.skipUntil(begin, end,
                              OpenCL::Parser::Context::fromTokenTypes(
                                  OpenCL::Lexer::TokenType::Arrow, OpenCL::Lexer::TokenType::Dot,
                                  OpenCL::Lexer::TokenType::OpenSquareBracket, OpenCL::Lexer::TokenType::OpenBracket,
                                  OpenCL::Lexer::TokenType::Increment, OpenCL::Lexer::TokenType::Decrement));
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context))
        {
            //            skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            //                return isPostFixOperator(token) || recoverySet(token);
            //            });
            context.skipUntil(begin, end,
                              OpenCL::Parser::Context::fromTokenTypes(
                                  OpenCL::Lexer::TokenType::Arrow, OpenCL::Lexer::TokenType::Dot,
                                  OpenCL::Lexer::TokenType::OpenSquareBracket, OpenCL::Lexer::TokenType::OpenBracket,
                                  OpenCL::Lexer::TokenType::Increment, OpenCL::Lexer::TokenType::Decrement));
        }
    }
    std::unique_ptr<PostFixExpression> current;
    if (initializer && typeName)
    {
        current = std::make_unique<PostFixExpression>(
            PostFixExpressionTypeInitializer(start, begin, std::move(*typeName), std::move(*initializer)));
    }
    parsePostFixExpressionSuffix(start, begin, end, current, context, recoverySet);
    if (!current)
    {
        return {};
    }
    return CastExpression(start, begin, UnaryExpressionPostFixExpression(start, begin, std::move(*current)));
}

std::optional<UnaryExpression> OpenCL::Parser::parseUnaryExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end, Context& context,
                                                                    InRecoverySet recoverySet)
{
    auto start = begin;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::SizeofKeyword)
    {
        begin++;
        if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenBracket
            && (begin + 1 == end || firstIsInTypeName(*(begin + 1), context)))
        {
            auto openPpos = begin;
            begin++;
            auto type = parseTypeName(begin, end, context, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::CloseBracket || recoverySet(token);
            });
            if (!expect(Lexer::TokenType::CloseBracket, start, begin, end, context,
                        {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(start),
                                       context.getLineEnd(openPpos),
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
            auto unary = parseUnaryExpression(begin, end, context, recoverySet);
            if (!unary)
            {
                return {};
            }
            return UnaryExpression(
                UnaryExpressionSizeOf(start, begin, std::make_unique<UnaryExpression>(std::move(*unary))));
        }
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
        auto cast = parseCastExpression(begin, end, context, recoverySet);
        if (!cast)
        {
            return {};
        }
        return UnaryExpression(
            UnaryExpressionUnaryOperator(start, begin, op, std::make_unique<CastExpression>(std::move(*cast))));
    }

    auto postFix = parsePostFixExpression(begin, end, context, recoverySet);
    if (!postFix)
    {
        return {};
    }
    return UnaryExpression(UnaryExpressionPostFixExpression(start, begin, std::move(*postFix)));
}

std::optional<PostFixExpression> OpenCL::Parser::parsePostFixExpression(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end, Context& context,
                                                                        InRecoverySet recoverySet)
{
    std::unique_ptr<PostFixExpression> current;

    auto start = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBracket || begin + 1 == end
        || !firstIsInTypeName(*(begin + 1), context))
    {
        auto newPrimary = parsePrimaryExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return isPostFixOperator(token) || recoverySet(token);
        });
        if (newPrimary)
        {
            current = std::make_unique<PostFixExpression>(
                PostFixExpressionPrimaryExpression(start, begin, std::move(*newPrimary)));
        }
    }
    else
    {
        begin++;
        auto type = parseTypeName(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseBracket || recoverySet(token);
        });
        if (!expect(Lexer::TokenType::CloseBracket, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(start),
                                   context.getLineEnd(start), Modifier(start, start + 1, Modifier::PointAtBeginning))}))
        {
            //            skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            //                return token.getTokenType() == Lexer::TokenType::OpenBrace || recoverySet(token);
            //            });
            context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenBrace));
        }

        std::optional<Tokens::const_iterator> openBrace;
        if (!expect(Lexer::TokenType::OpenBrace, start, begin, end, context))
        {
            //            skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            //                return firstIsInInitializerList(token, context) || recoverySet(token);
            //            });
            context.skipUntil(begin, end, firstInitializerListSet);
        }
        else
        {
            openBrace = begin - 1;
        }

        auto initializer = parseInitializerList(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseBrace
                   || token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
        });
        if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        if (openBrace)
        {
            if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context,
                        {Message::note(Notes::TO_MATCH_N_HERE.args("'{'"), context.getLineStart(start),
                                       context.getLineEnd(*openBrace),
                                       Modifier(*openBrace, *openBrace + 1, Modifier::PointAtBeginning))}))
            {
                //                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                //                    return isPostFixOperator(token) || recoverySet(token);
                //                });
                context.skipUntil(begin, end,
                                  OpenCL::Parser::Context::fromTokenTypes(
                                      OpenCL::Lexer::TokenType::Arrow, OpenCL::Lexer::TokenType::Dot,
                                      OpenCL::Lexer::TokenType::OpenSquareBracket,
                                      OpenCL::Lexer::TokenType::OpenBracket, OpenCL::Lexer::TokenType::Increment,
                                      OpenCL::Lexer::TokenType::Decrement));
            }
        }
        else
        {
            if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context))
            {
                //                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                //                    return isPostFixOperator(token) || recoverySet(token);
                //                });
                context.skipUntil(begin, end,
                                  OpenCL::Parser::Context::fromTokenTypes(
                                      OpenCL::Lexer::TokenType::Arrow, OpenCL::Lexer::TokenType::Dot,
                                      OpenCL::Lexer::TokenType::OpenSquareBracket,
                                      OpenCL::Lexer::TokenType::OpenBracket, OpenCL::Lexer::TokenType::Increment,
                                      OpenCL::Lexer::TokenType::Decrement));
            }
        }
        if (initializer && type)
        {
            current = std::make_unique<PostFixExpression>(
                PostFixExpressionTypeInitializer(start, begin, std::move(*type), std::move(*initializer)));
        }
    }

    parsePostFixExpressionSuffix(start, begin, end, current, context, recoverySet);

    if (!current)
    {
        return {};
    }
    return std::move(*current);
}

std::optional<PrimaryExpression> OpenCL::Parser::parsePrimaryExpression(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end, Context& context,
                                                                        InRecoverySet recoverySet)
{
    auto start = begin;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
    {
        const auto& value = std::get<std::string>(begin->getValue());
        begin++;
        return PrimaryExpression(PrimaryExpressionIdentifier(start, begin, value));
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
        return PrimaryExpression(PrimaryExpressionConstant(start, begin, std::move(value)));
    }
    else if (begin < end && begin->getTokenType() == Lexer::TokenType::StringLiteral)
    {
        std::string result = std::get<std::string>(begin->getValue());
        begin++;
        while (begin < end && begin->getTokenType() == Lexer::TokenType::StringLiteral)
        {
            result += std::get<std::string>(begin->getValue());
            begin++;
        }
        return PrimaryExpression(PrimaryExpressionConstant(start, begin, result));
    }
    else if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenBracket)
    {
        auto openPpos = begin++;
        auto expression = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseBracket || recoverySet(token);
        });
        if (!expect(Lexer::TokenType::CloseBracket, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(start),
                                   context.getLineEnd(openPpos),
                                   Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
        {
            context.skipUntil(begin, end);
        }
        return PrimaryExpression(PrimaryExpressionParenthese(start, begin, std::move(expression)));
    }

    if (begin == end)
    {
        context.log({Message::error(
            ErrorMessages::Parser::EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")),
            context.getLineStart(start), begin, Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd))});
    }
    else
    {
        context.log({Message::error(
            ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"), '\'' + begin->emitBack() + '\''),
            context.getLineStart(start), context.getLineEnd(begin),
            Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning))});
    }
    context.skipUntil(begin, end);
    return {};
}
