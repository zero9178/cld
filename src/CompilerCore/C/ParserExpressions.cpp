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
    auto assignment = parseAssignmentExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
    });
    if (assignment)
    {
        expressions.push_back(std::move(*assignment));
    }

    if (begin != end)
    {
        while (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
            assignment = parseAssignmentExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
            });
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
    return context.doBacktracking([&]() -> std::optional<Syntax::AssignmentExpression> {
        bool reachedAssignment = false;
        auto assignmentBranch = context.createBranch(
            begin, [&reachedAssignment](Tokens::const_iterator, Tokens::const_iterator) { return reachedAssignment; });
        auto assignmentAssignment = parseAssignmentExpressionAssignment(assignmentBranch->getCurrent(), end, context,
                                                                        reachedAssignment, recoverySet);
        if (*assignmentBranch && assignmentAssignment)
        {
            return AssignmentExpression(start, begin, std::move(*assignmentAssignment));
        }

        auto condBranch = context.createBranch(begin);
        auto cond = parseConditionalExpression(condBranch->getCurrent(), end, context, recoverySet);
        if (!cond)
        {
            return {};
        }
        return AssignmentExpression(start, begin, std::move(*cond));
    });
}

std::optional<AssignmentExpressionAssignment>
    OpenCL::Parser::parseAssignmentExpressionAssignment(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                        Context& context, bool& reachedAssignment,
                                                        InRecoverySet recoverySet)
{
    auto start = begin;
    reachedAssignment = false;
    auto unary = parseUnaryExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return isAssignment(token.getTokenType()) || recoverySet(token);
    });
    AssignmentExpressionAssignment::AssignOperator assignOperator;
    if (begin == end || !isAssignment(begin->getTokenType()))
    {
        return {};
    }
    else
    {
        reachedAssignment = true;
        switch (begin->getTokenType())
        {
            case Lexer::TokenType::Assignment:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::NoOperator;
                break;
            case Lexer::TokenType::PlusAssign:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::PlusAssign;
                break;
            case Lexer::TokenType::MinusAssign:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::MinusAssign;
                break;
            case Lexer::TokenType::DivideAssign:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::DivideAssign;
                break;
            case Lexer::TokenType::MultiplyAssign:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::MultiplyAssign;
                break;
            case Lexer::TokenType::ModuloAssign:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::ModuloAssign;
                break;
            case Lexer::TokenType::ShiftLeftAssign:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::LeftShiftAssign;
                break;
            case Lexer::TokenType::ShiftRightAssign:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::RightShiftAssign;
                break;
            case Lexer::TokenType::BitAndAssign:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::BitAndAssign;
                break;
            case Lexer::TokenType::BitOrAssign:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::BitOrAssign;
                break;
            case Lexer::TokenType::BitXorAssign:
                assignOperator = AssignmentExpressionAssignment::AssignOperator::BitXorAssign;
                break;
            default: OPENCL_UNREACHABLE;
        }
        begin++;
    }

    auto assignment = parseAssignmentExpression(begin, end, context, recoverySet);
    if (!unary || !assignment)
    {
        return {};
    }
    return AssignmentExpressionAssignment(start, begin, std::move(*unary), assignOperator,
                                          std::make_unique<AssignmentExpression>(std::move(*assignment)));
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
            skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
                return firstIsInConditionalExpression(token, context) || recoverySet(token);
            });
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

std::optional<LogicalOrExpression> OpenCL::Parser::parseLogicalOrExpression(Tokens::const_iterator& begin,
                                                                            Tokens::const_iterator end,
                                                                            Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseLogicalAndExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::LogicOr || recoverySet(token);
    });

    std::vector<LogicalAndExpression> list;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::LogicOr)
    {
        begin++;
        auto newAnd = parseLogicalAndExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::LogicOr || recoverySet(token);
        });
        if (newAnd)
        {
            list.push_back(std::move(*newAnd));
        }
    }

    if (!result)
    {
        return {};
    }
    return LogicalOrExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<LogicalAndExpression> OpenCL::Parser::parseLogicalAndExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              Context& context,
                                                                              InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseBitOrExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::LogicAnd || recoverySet(token);
    });

    std::vector<BitOrExpression> list;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::LogicAnd)
    {
        begin++;
        auto newOr = parseBitOrExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::LogicAnd || recoverySet(token);
        });
        if (newOr)
        {
            list.push_back(std::move(*newOr));
        }
    }

    if (!result)
    {
        return {};
    }
    return LogicalAndExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<BitOrExpression> OpenCL::Parser::parseBitOrExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end, Context& context,
                                                                    InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseBitXorExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::BitOr || recoverySet(token);
    });

    std::vector<BitXorExpression> list;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::BitOr)
    {
        begin++;
        auto newXor = parseBitXorExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::BitOr || recoverySet(token);
        });
        if (newXor)
        {
            list.push_back(std::move(*newXor));
        }
    }

    if (!result)
    {
        return {};
    }
    return BitOrExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<BitXorExpression> OpenCL::Parser::parseBitXorExpression(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end, Context& context,
                                                                      InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseBitAndExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::BitXor || recoverySet(token);
    });

    std::vector<BitAndExpression> list;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::BitXor)
    {
        begin++;
        auto newAnd = parseBitAndExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::BitXor || recoverySet(token);
        });
        if (newAnd)
        {
            list.push_back(std::move(*newAnd));
        }
    }

    if (!result)
    {
        return {};
    }
    return BitXorExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<BitAndExpression> OpenCL::Parser::parseBitAndExpression(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end, Context& context,
                                                                      InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseEqualityExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::Ampersand || recoverySet(token);
    });

    std::vector<EqualityExpression> list;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::Ampersand)
    {
        begin++;
        auto newEqual = parseEqualityExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::Ampersand || recoverySet(token);
        });
        if (newEqual)
        {
            list.push_back(std::move(*newEqual));
        }
    }

    if (!result)
    {
        return {};
    }
    return BitAndExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<EqualityExpression> OpenCL::Parser::parseEqualityExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end, Context& context,
                                                                          InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseRelationalExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::Equal || token.getTokenType() == Lexer::TokenType::NotEqual
               || recoverySet(token);
    });

    std::vector<std::pair<EqualityExpression::EqualityOperator, RelationalExpression>> list;
    while (begin != end
           && (begin->getTokenType() == Lexer::TokenType::Equal || begin->getTokenType() == Lexer::TokenType::NotEqual))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newRelational = parseRelationalExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::Equal || token.getTokenType() == Lexer::TokenType::NotEqual
                   || recoverySet(token);
        });
        if (newRelational)
        {
            list.emplace_back(token == Lexer::TokenType::Equal ? EqualityExpression::EqualityOperator::Equal :
                                                                 EqualityExpression::EqualityOperator::NotEqual,
                              std::move(*newRelational));
        }
    }

    if (!result)
    {
        return {};
    }
    return EqualityExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<RelationalExpression> OpenCL::Parser::parseRelationalExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              Context& context,
                                                                              InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseShiftExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::LessThan
               || token.getTokenType() == Lexer::TokenType::LessThanOrEqual
               || token.getTokenType() == Lexer::TokenType::GreaterThan
               || token.getTokenType() == Lexer::TokenType::GreaterThanOrEqual || recoverySet(token);
    });

    std::vector<std::pair<RelationalExpression::RelationalOperator, ShiftExpression>> list;
    while (begin != end
           && (begin->getTokenType() == Lexer::TokenType::LessThan
               || begin->getTokenType() == Lexer::TokenType::LessThanOrEqual
               || begin->getTokenType() == Lexer::TokenType::GreaterThan
               || begin->getTokenType() == Lexer::TokenType::GreaterThanOrEqual))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newShift = parseShiftExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::LessThan
                   || token.getTokenType() == Lexer::TokenType::LessThanOrEqual
                   || token.getTokenType() == Lexer::TokenType::GreaterThan
                   || token.getTokenType() == Lexer::TokenType::GreaterThanOrEqual || recoverySet(token);
        });
        if (newShift)
        {
            list.emplace_back(
                [token]() -> RelationalExpression::RelationalOperator {
                    switch (token)
                    {
                        case Lexer::TokenType::LessThan: return RelationalExpression::RelationalOperator::LessThan;
                        case Lexer::TokenType::LessThanOrEqual:
                            return RelationalExpression::RelationalOperator::LessThanOrEqual;
                        case Lexer::TokenType::GreaterThan:
                            return RelationalExpression::RelationalOperator::GreaterThan;
                        case Lexer::TokenType::GreaterThanOrEqual:
                            return RelationalExpression::RelationalOperator::GreaterThanOrEqual;
                        default: OPENCL_UNREACHABLE;
                    }
                }(),
                std::move(*newShift));
        }
    }

    if (!result)
    {
        return {};
    }
    return RelationalExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<ShiftExpression> OpenCL::Parser::parseShiftExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end, Context& context,
                                                                    InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseAdditiveExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::ShiftRight
               || token.getTokenType() == Lexer::TokenType::ShiftLeft || recoverySet(token);
    });

    std::vector<std::pair<ShiftExpression::ShiftOperator, AdditiveExpression>> list;
    while (begin != end
           && (begin->getTokenType() == Lexer::TokenType::ShiftRight
               || begin->getTokenType() == Lexer::TokenType::ShiftLeft))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newAdd = parseAdditiveExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::ShiftRight
                   || token.getTokenType() == Lexer::TokenType::ShiftLeft || recoverySet(token);
        });
        if (newAdd)
        {
            list.emplace_back(token == Lexer::TokenType::ShiftRight ? ShiftExpression::ShiftOperator::Right :
                                                                      ShiftExpression::ShiftOperator::Left,
                              std::move(*newAdd));
        }
    }

    if (!result)
    {
        return {};
    }
    return ShiftExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<AdditiveExpression> OpenCL::Parser::parseAdditiveExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end, Context& context,
                                                                          InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseTerm(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::Minus || token.getTokenType() == Lexer::TokenType::Plus
               || recoverySet(token);
    });

    std::vector<std::pair<AdditiveExpression::BinaryDashOperator, Term>> list;
    while (begin != end
           && (begin->getTokenType() == Lexer::TokenType::Plus || begin->getTokenType() == Lexer::TokenType::Minus))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newTerm = parseTerm(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::Minus || token.getTokenType() == Lexer::TokenType::Plus
                   || recoverySet(token);
        });
        if (newTerm)
        {
            list.emplace_back(token == Lexer::TokenType::Plus ? AdditiveExpression::BinaryDashOperator::BinaryPlus :
                                                                AdditiveExpression::BinaryDashOperator::BinaryMinus,
                              std::move(*newTerm));
        }
    }

    if (!result)
    {
        return {};
    }
    return AdditiveExpression(start, begin, std::move(*result), std::move(list));
}

std::optional<Term> OpenCL::Parser::parseTerm(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                              Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    auto result = parseCastExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::Asterisk || token.getTokenType() == Lexer::TokenType::Division
               || token.getTokenType() == Lexer::TokenType::Percent || recoverySet(token);
    });

    std::vector<std::pair<Term::BinaryDotOperator, CastExpression>> list;
    while (begin != end
           && (begin->getTokenType() == Lexer::TokenType::Asterisk
               || begin->getTokenType() == Lexer::TokenType::Division
               || begin->getTokenType() == Lexer::TokenType::Percent))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newCast = parseCastExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::Asterisk
                   || token.getTokenType() == Lexer::TokenType::Division
                   || token.getTokenType() == Lexer::TokenType::Percent || recoverySet(token);
        });
        if (newCast)
        {
            list.emplace_back(
                [token] {
                    switch (token)
                    {
                        case Lexer::TokenType::Asterisk: return Term::BinaryDotOperator::BinaryMultiply;
                        case Lexer::TokenType::Division: return Term::BinaryDotOperator::BinaryDivide;
                        case Lexer::TokenType::Percent: return Term::BinaryDotOperator::BinaryRemainder;
                        default: OPENCL_UNREACHABLE;
                    }
                }(),
                std::move(*newCast));
        }
    }

    if (!result)
    {
        return {};
    }
    return Term(start, begin, std::move(*result), std::move(list));
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
        if (!abstractDec)
        {
            return {};
        }
        return TypeName(start, begin, std::move(specifierQualifiers),
                        std::make_unique<AbstractDeclarator>(std::move(*abstractDec)));
    }

    return TypeName(start, begin, std::move(specifierQualifiers), nullptr);
}

std::optional<CastExpression> OpenCL::Parser::parseCastExpression(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end, Context& context,
                                                                  InRecoverySet recoverySet)
{
    return context.doBacktracking([&]() -> std::optional<CastExpression> {
        auto start = begin;

        bool isNotTypeInitializer = false;
        auto castBranch =
            context.createBranch(begin, [&isNotTypeInitializer](Tokens::const_iterator, Tokens::const_iterator) {
                return isNotTypeInitializer;
            });
        if (castBranch->getCurrent() < end
            && castBranch->getCurrent()->getTokenType() == Lexer::TokenType::OpenParenthese)
        {
            auto prevErrorCount = context.getCurrentErrorCount();
            castBranch->getCurrent()++;
            auto typeName =
                parseTypeName(castBranch->getCurrent(), end, context, [recoverySet](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
                });
            if (castBranch->getCurrent() < end)
            {
                if (!expect(Lexer::TokenType::CloseParenthese, start, castBranch->getCurrent(), end, context))
                {
                    skipUntil(begin, end, [&context, recoverySet](const Lexer::Token& token) {
                        return firstIsInCastExpression(token, context) || recoverySet(token);
                    });
                }
                else if (typeName
                         && (castBranch->getCurrent() == end
                             || castBranch->getCurrent()->getTokenType() != Lexer::TokenType::OpenBrace)
                         && context.getCurrentErrorCount() == prevErrorCount)
                {
                    isNotTypeInitializer = true;
                }
                auto cast = parseCastExpression(castBranch->getCurrent(), end, context, recoverySet);
                if (cast && typeName && *castBranch)
                {
                    return CastExpression(
                        start, castBranch->getCurrent(),
                        std::pair{std::move(*typeName), std::make_unique<CastExpression>(std::move(*cast))});
                }
                else if (isNotTypeInitializer)
                {
                    return {};
                }
            }
        }

        auto unaryBranch = context.createBranch(begin);
        auto unary = parseUnaryExpression(unaryBranch->getCurrent(), end, context, recoverySet);
        if (!unary)
        {
            return {};
        }

        return CastExpression(start, unaryBranch->getCurrent(), std::move(*unary));
    });
}

std::optional<UnaryExpression> OpenCL::Parser::parseUnaryExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end, Context& context,
                                                                    InRecoverySet recoverySet)
{
    auto start = begin;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::SizeofKeyword)
    {
        begin++;
        if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenParenthese
            && (begin + 1 == end || firstIsInTypeName(*(begin + 1), context)))
        {
            auto openPpos = begin;
            begin++;
            auto type = parseTypeName(begin, end, context, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
            });
            if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context,
                        {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(start),
                                       context.getLineEnd(openPpos),
                                       Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
            {
                skipUntil(begin, end, recoverySet);
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

namespace
{
    bool isPostFixOperator(const OpenCL::Lexer::Token& token)
    {
        switch (token.getTokenType())
        {
            case OpenCL::Lexer::TokenType::Arrow:
            case OpenCL::Lexer::TokenType::Dot:
            case OpenCL::Lexer::TokenType::OpenSquareBracket:
            case OpenCL::Lexer::TokenType::OpenParenthese:
            case OpenCL::Lexer::TokenType::Increment:
            case OpenCL::Lexer::TokenType::Decrement: return true;
            default: break;
        }
        return false;
    }
} // namespace

std::optional<PostFixExpression> OpenCL::Parser::parsePostFixExpression(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end, Context& context,
                                                                        InRecoverySet recoverySet)
{
    std::unique_ptr<PostFixExpression> current;

    auto start = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenParenthese || begin + 1 == end
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
            return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
        });
        if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(start),
                                   context.getLineEnd(start), Modifier(start, start + 1, Modifier::PointAtBeginning))}))
        {
            skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::OpenBrace || recoverySet(token);
            });
        }

        std::optional<Tokens::const_iterator> openBrace;
        if (!expect(Lexer::TokenType::OpenBrace, start, begin, end, context))
        {
            skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
                return firstIsInInitializerList(token, context) || recoverySet(token);
            });
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
                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                    return isPostFixOperator(token) || recoverySet(token);
                });
            }
        }
        else
        {
            if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context))
            {
                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                    return isPostFixOperator(token) || recoverySet(token);
                });
            }
        }
        if (initializer && type)
        {
            current = std::make_unique<PostFixExpression>(
                PostFixExpressionTypeInitializer(start, begin, std::move(*type), std::move(*initializer)));
        }
    }

    while (begin != end && isPostFixOperator(*begin))
    {
        if (begin->getTokenType() == Lexer::TokenType::OpenParenthese)
        {
            auto openPpos = begin;
            begin++;
            std::vector<std::unique_ptr<AssignmentExpression>> nonCommaExpressions;
            bool first = true;
            while (begin < end && begin->getTokenType() != Lexer::TokenType::CloseParenthese)
            {
                if (first)
                {
                    first = false;
                }
                else if (begin->getTokenType() == Lexer::TokenType::Comma)
                {
                    begin++;
                }
                else if (firstIsInAssignmentExpression(*begin, context))
                {
                    expect(Lexer::TokenType::Comma, start, begin, end, context);
                }
                else
                {
                    break;
                }

                auto assignment =
                    parseAssignmentExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::CloseParenthese
                               || token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
                    });
                if (assignment)
                {
                    nonCommaExpressions.push_back(std::make_unique<AssignmentExpression>(std::move(*assignment)));
                }
            }

            if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context,
                        {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(start),
                                       context.getLineEnd(openPpos),
                                       Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
            {
                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                    return isPostFixOperator(token) || recoverySet(token);
                });
            }
            if (current)
            {
                current = std::make_unique<PostFixExpression>(
                    PostFixExpressionFunctionCall(start, begin, std::move(current), std::move(nonCommaExpressions)));
            }
        }
        else if (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket)
        {
            auto openPpos = begin;
            begin++;
            auto expression = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::CloseSquareBracket || recoverySet(token);
            });

            if (!expect(Lexer::TokenType::CloseSquareBracket, start, begin, end, context,
                        {Message::note(Notes::TO_MATCH_N_HERE.args("'['"), context.getLineStart(start),
                                       context.getLineEnd(openPpos),
                                       Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
            {
                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                    return isPostFixOperator(token) || recoverySet(token);
                });
            }
            if (current)
            {
                current = std::make_unique<PostFixExpression>(
                    PostFixExpressionSubscript(start, begin, std::move(current), std::move(expression)));
            }
        }
        else if (begin->getTokenType() == Lexer::TokenType::Increment)
        {
            begin++;
            if (current)
            {
                current =
                    std::make_unique<PostFixExpression>(PostFixExpressionIncrement(start, begin, std::move(current)));
            }
        }
        else if (begin->getTokenType() == Lexer::TokenType::Decrement)
        {
            begin++;
            if (current)
            {
                current =
                    std::make_unique<PostFixExpression>(PostFixExpressionDecrement(start, begin, std::move(current)));
            }
        }
        else if (begin->getTokenType() == Lexer::TokenType::Dot)
        {
            begin++;
            std::string name;
            if (!expect(Lexer::TokenType::Identifier, start, begin, end, context, {}, &name))
            {
                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                    return isPostFixOperator(token) || recoverySet(token);
                });
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
            if (!expect(Lexer::TokenType::Identifier, start, begin, end, context, {}, &name))
            {
                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                    return isPostFixOperator(token) || recoverySet(token);
                });
            }
            if (current)
            {
                current =
                    std::make_unique<PostFixExpression>(PostFixExpressionArrow(start, begin, std::move(current), name));
            }
        }
    }

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
    else if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenParenthese)
    {
        auto openPpos = begin++;
        auto expression = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
        });
        if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(start),
                                   context.getLineEnd(openPpos),
                                   Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
        {
            skipUntil(begin, end, recoverySet);
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
    skipUntil(begin, end, recoverySet);
    return {};
}
