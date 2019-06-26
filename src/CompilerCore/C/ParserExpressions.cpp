#include "Parser.hpp"
#include "ParserUtil.hpp"

#include <stack>
#include <algorithm>

using namespace OpenCL::Syntax;

std::optional<Expression> OpenCL::Parser::parseExpression(Tokens::const_iterator& begin,
                                                          Tokens::const_iterator end, ParsingContext& context)
{
    auto start = begin;
    std::vector<AssignmentExpression> expressions;
    auto assignment = parseAssignmentExpression(begin, end, context);
    if (!assignment && (begin >= end || begin->getTokenType() != Lexer::TokenType::Comma))
    {
        return {};
    }
    else if (assignment)
    {
        expressions.push_back(std::move(*assignment));
    }

    if (begin != end)
    {
        while (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
            assignment = parseAssignmentExpression(begin, end, context);
            if (!assignment && (begin >= end || begin->getTokenType() != Lexer::TokenType::Comma))
            {
                return {};
            }
            else if (assignment)
            {
                expressions.push_back(std::move(*assignment));
            }
        }
    }
    return Expression(start, begin, std::move(expressions));
}

std::optional<AssignmentExpression>
OpenCL::Parser::parseAssignmentExpression(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                          ParsingContext& context)
{
    auto start = begin;
    return context.doBacktracking([&]() -> std::optional<Syntax::AssignmentExpression>
                                  {
                                      bool reachedAssignment = false;
                                      auto assignmentBranch = context.createBranch(begin,
                                                                                   [&reachedAssignment](Tokens::const_iterator,
                                                                                                        Tokens::const_iterator)
                                                                                   {
                                                                                       return reachedAssignment;
                                                                                   });
                                      auto assignmentAssignment = parseAssignmentExpressionAssignment(assignmentBranch
                                                                                                          ->getCurrent(),
                                                                                                      end,
                                                                                                      context,
                                                                                                      &reachedAssignment);
                                      if (*assignmentBranch && assignmentAssignment)
                                      {
                                          return AssignmentExpression(start, begin, std::move(*assignmentAssignment));
                                      }

                                      auto condBranch = context.createBranch(begin);
                                      auto cond = parseConditionalExpression(condBranch->getCurrent(), end, context);
                                      if (!cond)
                                      {
                                          return {};
                                      }
                                      return AssignmentExpression(start, begin, std::move(*cond));
                                  });
}

std::optional<AssignmentExpressionAssignment>
OpenCL::Parser::parseAssignmentExpressionAssignment(Tokens::const_iterator& begin,
                                                    Tokens::const_iterator end,
                                                    ParsingContext& context,
                                                    bool* reachedAssignment)
{
    auto start = begin;
    if (reachedAssignment)
    {
        *reachedAssignment = false;
    }
    auto unary = parseUnaryExpression(begin, end, context);
    if (!unary)
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token)
        {
            return isAssignment(token.getTokenType());
        });
        if (begin == end)
        {
            return {};
        }
    }
    AssignmentExpressionAssignment::AssignOperator
        assignOperator = AssignmentExpressionAssignment::AssignOperator::NoOperator;
    if (begin >= end || !isAssignment(begin->getTokenType()))
    {
        return {};
    }
    else
    {
        if (reachedAssignment)
        {
            *reachedAssignment = true;
        }
        switch (begin->getTokenType())
        {
        case Lexer::TokenType::Assignment:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::NoOperator;
            break;
        case Lexer::TokenType::PlusAssign:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::PlusAssign;
            break;
        case Lexer::TokenType::MinusAssign:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::MinusAssign;
            break;
        case Lexer::TokenType::DivideAssign:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::DivideAssign;
            break;
        case Lexer::TokenType::MultiplyAssign:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::MultiplyAssign;
            break;
        case Lexer::TokenType::ModuloAssign:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::ModuloAssign;
            break;
        case Lexer::TokenType::ShiftLeftAssign:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::LeftShiftAssign;
            break;
        case Lexer::TokenType::ShiftRightAssign:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::RightShiftAssign;
            break;
        case Lexer::TokenType::BitAndAssign:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::BitAndAssign;
            break;
        case Lexer::TokenType::BitOrAssign:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::BitOrAssign;
            break;
        case Lexer::TokenType::BitXorAssign:
            assignOperator
                = AssignmentExpressionAssignment::AssignOperator::BitXorAssign;
            break;
        default:break;
        }
        begin++;
    }

    auto assignment = parseAssignmentExpression(begin,
                                                end,
                                                context);
    if (!assignment || !unary)
    {
        return {};
    }
    return AssignmentExpressionAssignment(
        start, begin, std::move(*unary), assignOperator,
        std::make_unique<AssignmentExpression>(std::move(*assignment)));
}

std::optional<ConditionalExpression> OpenCL::Parser::parseConditionalExpression(Tokens::const_iterator& begin,
                                                                                Tokens::const_iterator end,
                                                                                ParsingContext& context)
{
    auto start = begin;
    auto logicalOrExperssion = parseLogicalOrExpression(begin, end, context);
    if (!logicalOrExperssion && (begin >= end || begin->getTokenType() != Lexer::TokenType::QuestionMark))
    {
        return {};
    }
    if (begin != end && begin->getTokenType() == Lexer::TokenType::QuestionMark)
    {
        begin++;
        auto optionalExpression = parseExpression(begin, end, context);
        if (!optionalExpression)
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token)
            {
                return token.getTokenType() == Lexer::TokenType::Colon;
            });
            if (begin == end)
            {
                return {};
            }
        }
        if (!expect(Lexer::TokenType::Colon, begin, end, context))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInConditionalExpression(token, context);
            });
            if (begin == end)
            {
                return {};
            }
        }
        auto optionalConditional = parseConditionalExpression(begin, end, context);
        if (!optionalConditional || !optionalExpression || !logicalOrExperssion)
        {
            return optionalConditional;
        }
        return ConditionalExpression(start, begin, std::move(*logicalOrExperssion),
                                     std::make_unique<Expression>(std::move(*optionalExpression)),
                                     std::make_unique<ConditionalExpression>(std::move(*optionalConditional)));
    }
    return ConditionalExpression(start, begin, std::move(*logicalOrExperssion));
}

std::optional<LogicalOrExpression> OpenCL::Parser::parseLogicalOrExpression(Tokens::const_iterator& begin,
                                                                            Tokens::const_iterator end,
                                                                            ParsingContext& context)
{
    auto start = begin;
    auto logicalAnd = parseLogicalAndExpression(begin, end, context);
    if (!logicalAnd && (begin >= end || begin->getTokenType() == Lexer::TokenType::LogicOr))
    {
        return {};
    }

    std::vector<LogicalAndExpression> optionalLogicalAnds;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::LogicOr)
    {
        begin++;
        auto newAnd = parseLogicalAndExpression(begin, end, context);
        if (!newAnd && (begin >= end || begin->getTokenType() == Lexer::TokenType::LogicOr))
        {
            return {};
        }
        else if (newAnd)
        {
            optionalLogicalAnds.push_back(std::move(*newAnd));
        }
    }

    if (!logicalAnd)
    {
        return {};
    }
    return LogicalOrExpression(start, begin, std::move(*logicalAnd), std::move(optionalLogicalAnds));
}

std::optional<LogicalAndExpression> OpenCL::Parser::parseLogicalAndExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    auto start = begin;
    auto result = parseBitOrExpression(begin, end, context);
    if (!result && (begin >= end || begin->getTokenType() == Lexer::TokenType::LogicAnd))
    {
        return {};
    }

    std::vector<BitOrExpression> list;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::LogicAnd)
    {
        begin++;
        auto newOr = parseBitOrExpression(begin, end, context);
        if (!newOr && (begin >= end || begin->getTokenType() == Lexer::TokenType::LogicAnd))
        {
            return {};
        }
        else if (newOr)
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
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    auto start = begin;
    auto result = parseBitXorExpression(begin, end, context);
    if (!result && (begin >= end || begin->getTokenType() != Lexer::TokenType::BitOr))
    {
        return {};
    }

    std::vector<BitXorExpression> list;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::BitOr)
    {
        begin++;
        auto newXor = parseBitXorExpression(begin, end, context);
        if (!newXor && (begin >= end || begin->getTokenType() != Lexer::TokenType::BitOr))
        {
            return {};
        }
        else if (newXor)
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
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    auto start = begin;
    auto result = parseBitAndExpression(begin, end, context);
    if (!result && (begin >= end || begin->getTokenType() != Lexer::TokenType::BitXor))
    {
        return {};
    }

    std::vector<BitAndExpression> list;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::BitXor)
    {
        begin++;
        auto newAnd = parseBitAndExpression(begin, end, context);
        if (!newAnd && (begin >= end || begin->getTokenType() != Lexer::TokenType::BitXor))
        {
            return {};
        }
        else if (newAnd)
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
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    auto start = begin;
    auto result = parseEqualityExpression(begin, end, context);
    if (!result && (begin >= end || begin->getTokenType() != Lexer::TokenType::Ampersand))
    {
        return {};
    }

    std::vector<EqualityExpression> list;
    while (begin != end && begin->getTokenType() == Lexer::TokenType::Ampersand)
    {
        begin++;
        auto newEqual = parseEqualityExpression(begin, end, context);
        if (!newEqual && (begin >= end || begin->getTokenType() != Lexer::TokenType::Ampersand))
        {
            return {};
        }
        else if (newEqual)
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
                                                                          Tokens::const_iterator end,
                                                                          ParsingContext& context)
{
    auto start = begin;
    auto result = parseRelationalExpression(begin, end, context);
    if (!result
        && (begin >= end
            || (begin->getTokenType() != Lexer::TokenType::Equal
                && begin->getTokenType() != Lexer::TokenType::NotEqual)))
    {
        return {};
    }

    std::vector<std::pair<EqualityExpression::EqualityOperator, RelationalExpression>> relationalExpressions;
    while (begin != end
        && (begin->getTokenType() == Lexer::TokenType::Equal || begin->getTokenType() == Lexer::TokenType::NotEqual))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newRelational = parseRelationalExpression(begin, end, context);
        if (!newRelational && (begin >= end
            || (begin->getTokenType() != Lexer::TokenType::Equal
                && begin->getTokenType() != Lexer::TokenType::NotEqual)))
        {
            return {};
        }
        else if (newRelational)
        {
            relationalExpressions
                .emplace_back(token == Lexer::TokenType::Equal ? EqualityExpression::EqualityOperator::Equal :
                              EqualityExpression::EqualityOperator::NotEqual,
                              std::move(*newRelational));
        }
    }

    if (!result)
    {
        return {};
    }
    return EqualityExpression(start, begin, std::move(*result), std::move(relationalExpressions));
}

std::optional<RelationalExpression> OpenCL::Parser::parseRelationalExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    auto start = begin;
    auto result = parseShiftExpression(begin, end, context);
    if (!result && (begin >= end
        || (begin->getTokenType() != Lexer::TokenType::LessThan
            && begin->getTokenType() != Lexer::TokenType::LessThanOrEqual
            && begin->getTokenType() != Lexer::TokenType::GreaterThan
            && begin->getTokenType() != Lexer::TokenType::GreaterThanOrEqual)))
    {
        return {};
    }

    std::vector<std::pair<RelationalExpression::RelationalOperator, ShiftExpression>> list;
    while (begin != end
        && (begin->getTokenType() == Lexer::TokenType::LessThan
            || begin->getTokenType() == Lexer::TokenType::LessThanOrEqual
            || begin->getTokenType() == Lexer::TokenType::GreaterThan
            || begin->getTokenType() == Lexer::TokenType::GreaterThanOrEqual))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newShift = parseShiftExpression(begin, end, context);
        if (!newShift && (begin >= end
            || (begin->getTokenType() != Lexer::TokenType::LessThan
                && begin->getTokenType() != Lexer::TokenType::LessThanOrEqual
                && begin->getTokenType() != Lexer::TokenType::GreaterThan
                && begin->getTokenType() != Lexer::TokenType::GreaterThanOrEqual)))
        {
            return {};
        }
        else if (newShift)
        {
            list.emplace_back(
                [token]() -> RelationalExpression::RelationalOperator
                {
                    switch (token)
                    {
                    case Lexer::TokenType::LessThan: return RelationalExpression::RelationalOperator::LessThan;
                    case Lexer::TokenType::LessThanOrEqual: return RelationalExpression::RelationalOperator::LessThanOrEqual;
                    case Lexer::TokenType::GreaterThan: return RelationalExpression::RelationalOperator::GreaterThan;
                    case Lexer::TokenType::GreaterThanOrEqual:return RelationalExpression::RelationalOperator::GreaterThanOrEqual;
                    default: return RelationalExpression::RelationalOperator::LessThan;
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
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    auto start = begin;
    auto result = parseAdditiveExpression(begin, end, context);
    if (!result && (begin >= end
        || (begin->getTokenType() != Lexer::TokenType::ShiftRight
            || begin->getTokenType() != Lexer::TokenType::ShiftLeft)))
    {
        return {};
    }

    std::vector<std::pair<ShiftExpression::ShiftOperator, AdditiveExpression>> list;
    while (begin != end
        && (begin->getTokenType() == Lexer::TokenType::ShiftRight
            || begin->getTokenType() == Lexer::TokenType::ShiftLeft))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newAdd = parseAdditiveExpression(begin, end, context);
        if (!newAdd && (begin >= end
            || (begin->getTokenType() != Lexer::TokenType::ShiftRight
                || begin->getTokenType() != Lexer::TokenType::ShiftLeft)))
        {
            return {};
        }
        else if (newAdd)
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
                                                                          Tokens::const_iterator end,
                                                                          ParsingContext& context)
{
    auto start = begin;
    auto result = parseTerm(begin, end, context);
    if (!result
        && (begin >= end
            || (begin->getTokenType() != Lexer::TokenType::Plus && begin->getTokenType() != Lexer::TokenType::Minus)))
    {
        return {};
    }

    std::vector<std::pair<AdditiveExpression::BinaryDashOperator, Term>> list;
    while (begin != end
        && (begin->getTokenType() == Lexer::TokenType::Plus || begin->getTokenType() == Lexer::TokenType::Minus))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newTerm = parseTerm(begin, end, context);
        if (!newTerm && (begin >= end
            || (begin->getTokenType() != Lexer::TokenType::Plus && begin->getTokenType() != Lexer::TokenType::Minus)))
        {
            return {};
        }
        else if (newTerm)
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
                                              ParsingContext& context)
{
    auto start = begin;
    auto result = parseCastExpression(begin, end, context);
    if (!result && (begin >= end
        || (begin->getTokenType() != Lexer::TokenType::Asterisk && begin->getTokenType() != Lexer::TokenType::Division
            && begin->getTokenType() != Lexer::TokenType::Percent)))
    {
        return {};
    }

    std::vector<std::pair<Term::BinaryDotOperator, CastExpression>> list;
    while (begin != end
        && (begin->getTokenType() == Lexer::TokenType::Asterisk || begin->getTokenType() == Lexer::TokenType::Division
            || begin->getTokenType() == Lexer::TokenType::Percent))
    {
        auto token = begin->getTokenType();
        begin++;
        auto newCast = parseCastExpression(begin, end, context);
        if (!newCast && (begin >= end
            || (begin->getTokenType() != Lexer::TokenType::Asterisk
                && begin->getTokenType() != Lexer::TokenType::Division
                && begin->getTokenType() != Lexer::TokenType::Percent)))
        {
            return {};
        }
        else if (newCast)
        {
            list.emplace_back(
                [token]
                {
                    switch (token)
                    {
                    case Lexer::TokenType::Asterisk: return Term::BinaryDotOperator::BinaryMultiply;
                    case Lexer::TokenType::Division: return Term::BinaryDotOperator::BinaryDivide;
                    case Lexer::TokenType::Percent: return Term::BinaryDotOperator::BinaryRemainder;
                    default:return Term::BinaryDotOperator::BinaryMultiply;
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

std::optional<TypeName> OpenCL::Parser::parseTypeName(Tokens::const_iterator& begin,
                                                      Tokens::const_iterator end,
                                                      OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    std::vector<SpecifierQualifier> specifierQualifiers;
    while (begin < end && firstIsInSpecifierQualifier(*begin, context))
    {
        auto result = parseSpecifierQualifier(begin, end, context);
        if (result)
        {
            specifierQualifiers.push_back(std::move(*result));
        }
    }
    if (specifierQualifiers.empty())
    {
        if (begin < end)
        {
            context.logError(ErrorMessages::EXPECTED_N_BEFORE_N.args("typename", '\'' + begin->emitBack() + '\''),
                             findSemicolonOrEOL(begin, end),
                             Modifier(begin, begin + 1, Modifier::PointAtBeginning));
        }
        else
        {
            context.logError(ErrorMessages::EXPECTED_N.args("typename"),
                             findSemicolonOrEOL(begin, end),
                             Modifier(begin, begin + 1, Modifier::InsertAtEnd));
        }
    }

    if (firstIsInAbstractDeclarator(*begin, context))
    {
        auto abstractDec = parseAbstractDeclarator(begin, end, context);
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
                                                                  Tokens::const_iterator end,
                                                                  ParsingContext& context)
{
    return context.doBacktracking([&]() -> std::optional<CastExpression>
                                  {
                                      auto start = begin;

                                      bool isNotTypeInitializer = false;
                                      auto
                                          castBranch = context.createBranch(begin,
                                                                            [&isNotTypeInitializer](Tokens::const_iterator,
                                                                                                    Tokens::const_iterator)
                                                                            {
                                                                                return isNotTypeInitializer;
                                                                            });
                                      if (castBranch->getCurrent() < end
                                          && castBranch->getCurrent()->getTokenType()
                                              == Lexer::TokenType::OpenParenthese)
                                      {
                                          castBranch->getCurrent()++;
                                          auto typeName = parseTypeName(castBranch->getCurrent(), end, context);
                                          if (!typeName)
                                          {
                                              castBranch->getCurrent() = std::find_if(castBranch->getCurrent(),
                                                                                      end,
                                                                                      [](const Lexer::Token& token)
                                                                                      {
                                                                                          return token.getTokenType()
                                                                                              == Lexer::TokenType::CloseParenthese;
                                                                                      });
                                          }
                                          if (castBranch->getCurrent() < end)
                                          {
                                              if (!expect(Lexer::TokenType::CloseParenthese,
                                                          castBranch->getCurrent(),
                                                          findSemicolonOrEOL(castBranch->getCurrent(), end),
                                                          context))
                                              {
                                                  castBranch->getCurrent() = std::find_if(castBranch->getCurrent(),
                                                                                          end,
                                                                                          [&context](const Lexer::Token& token)
                                                                                          {
                                                                                              return firstIsInCastExpression(
                                                                                                  token,
                                                                                                  context);
                                                                                          });
                                              }
                                              else if (typeName && (castBranch->getCurrent() >= end
                                                  || castBranch->getCurrent()->getTokenType()
                                                      != Lexer::TokenType::OpenBrace))
                                              {
                                                  isNotTypeInitializer = true;
                                              }
                                              auto cast = parseCastExpression(castBranch->getCurrent(), end, context);
                                              if (cast && typeName)
                                              {
                                                  return CastExpression(
                                                      start, castBranch->getCurrent(),
                                                      std::pair{std::move(*typeName),
                                                                std::make_unique<CastExpression>(std::move(*cast))});
                                              }
                                          }
                                      }

                                      auto unaryBranch = context.createBranch(begin);
                                      auto unary = parseUnaryExpression(unaryBranch->getCurrent(), end, context);
                                      if (!unary)
                                      {
                                          return {};
                                      }

                                      return CastExpression(start, begin, std::move(*unary));
                                  });
}

std::optional<UnaryExpression> OpenCL::Parser::parseUnaryExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    auto start = begin;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::SizeofKeyword)
    {
        begin++;
        if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenParenthese && begin + 1 < end
            && firstIsInTypeName(*(begin + 1), context))
        {
            auto openPpos = begin;
            begin++;
            auto type = parseTypeName(begin, end, context);
            if (!type)
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token)
                {
                    return token.getTokenType() == Lexer::TokenType::CloseParenthese;
                });
                if (begin == end)
                {
                    return {};
                }
            }
            expect(Lexer::TokenType::CloseParenthese,
                   begin,
                   end,
                   context,
                   {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                     Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}});
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
            return UnaryExpression(UnaryExpressionSizeOf(start,
                                                         begin,
                                                         std::make_unique<UnaryExpression>(std::move(*unary))));
        }
    }
    else if (begin < end
        && (begin->getTokenType() == Lexer::TokenType::Increment || begin->getTokenType() == Lexer::TokenType::Decrement
            || begin->getTokenType() == Lexer::TokenType::Ampersand
            || begin->getTokenType() == Lexer::TokenType::Asterisk
            || begin->getTokenType() == Lexer::TokenType::Plus || begin->getTokenType() == Lexer::TokenType::Minus
            || begin->getTokenType() == Lexer::TokenType::LogicalNegation
            || begin->getTokenType() == Lexer::TokenType::BitWiseNegation))
    {
        auto token = begin->getTokenType();
        begin++;
        auto op = [token]
        {
            switch (token)
            {
            case Lexer::TokenType::Increment: return UnaryExpressionUnaryOperator::UnaryOperator::Increment;
            case Lexer::TokenType::Decrement: return UnaryExpressionUnaryOperator::UnaryOperator::Decrement;
            case Lexer::TokenType::Ampersand: return UnaryExpressionUnaryOperator::UnaryOperator::Ampersand;
            case Lexer::TokenType::Asterisk: return UnaryExpressionUnaryOperator::UnaryOperator::Asterisk;
            case Lexer::TokenType::Plus: return UnaryExpressionUnaryOperator::UnaryOperator::Plus;
            case Lexer::TokenType::Minus: return UnaryExpressionUnaryOperator::UnaryOperator::Minus;
            case Lexer::TokenType::LogicalNegation: return UnaryExpressionUnaryOperator::UnaryOperator::BitNot;
            case Lexer::TokenType::BitWiseNegation: return UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot;
            default: return UnaryExpressionUnaryOperator::UnaryOperator::Plus;
            }
        }();
        auto unary = parseUnaryExpression(begin, end, context);
        if (!unary)
        {
            return {};
        }
        return UnaryExpression(
            UnaryExpressionUnaryOperator(start, begin, op, std::make_unique<UnaryExpression>(std::move(*unary))));
    }

    auto postFix = parsePostFixExpression(begin, end, context);
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
        case OpenCL::Lexer::TokenType::Identifier:
        case OpenCL::Lexer::TokenType::OpenParenthese:
        case OpenCL::Lexer::TokenType::Literal:
        case OpenCL::Lexer::TokenType::Increment:
        case OpenCL::Lexer::TokenType::Decrement: return true;
        default: break;
        }
        return false;
    }
}

std::optional<PostFixExpression> OpenCL::Parser::parsePostFixExpression(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end,
                                                                        ParsingContext& context)
{
    std::stack<std::unique_ptr<PostFixExpression>> stack;
    while (begin != end && isPostFixOperator(*begin)
        && ((begin->getTokenType() != Lexer::TokenType::Identifier
            && begin->getTokenType() != Lexer::TokenType::Literal)
            || stack.empty()))
    {
        if (begin->getTokenType() == Lexer::TokenType::Identifier || begin->getTokenType() == Lexer::TokenType::Literal)
        {
            auto start = begin;
            auto newPrimary = parsePrimaryExpression(begin, end, context);
            if (!newPrimary)
            {
                return {};
            }
            stack.push(std::make_unique<PostFixExpression>(PostFixExpressionPrimaryExpression(start,
                                                                                              begin,
                                                                                              std::move(*newPrimary))));
        }
        else if (begin->getTokenType() == Lexer::TokenType::OpenParenthese && stack.empty())
        {
            auto start = begin;
            if (begin + 1 < end && firstIsInDeclarationSpecifier(*(begin + 1), context))
            {
                auto type = parseTypeName(begin, end, context);
                if (!type)
                {
                    begin = std::find_if(begin, end, [](const Lexer::Token& token)
                    {
                        return token.getTokenType() == Lexer::TokenType::CloseParenthese;
                    });
                    if (begin == end)
                    {
                        return {};
                    }
                }
                if (!expect(Lexer::TokenType::CloseParenthese,
                            begin,
                            end,
                            context,
                            {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                              Modifier(start, start + 1, Modifier::PointAtBeginning)}}))
                {
                    begin = std::find_if(begin, end, [](const Lexer::Token& token)
                    {
                        return token.getTokenType() == Lexer::TokenType::OpenBrace;
                    });
                    if (begin == end)
                    {
                        return {};
                    }
                }

                auto openBrace = begin;
                if (!expect(Lexer::TokenType::OpenBrace,
                            begin,
                            end,
                            context))
                {
                    openBrace = {};
                    begin = std::find_if(begin, end, [](const Lexer::Token& token)
                    {
                        return token.getTokenType() == Lexer::TokenType::OpenBrace;
                    });
                    if (begin == end)
                    {
                        return {};
                    }
                }

                auto initializer = parseInitializerList(begin, end, context);
                if (!initializer)
                {
                    begin = std::find_if(begin, end, [](const Lexer::Token& token)
                    {
                        return token.getTokenType() == Lexer::TokenType::CloseBrace
                            || token.getTokenType() == Lexer::TokenType::Comma;
                    });
                    if (begin == end)
                    {
                        return {};
                    }
                }
                if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
                {
                    begin++;
                }
                if (openBrace != Tokens::const_iterator{})
                {
                    if (!expect(Lexer::TokenType::CloseBrace,
                                begin,
                                end,
                                context,
                                {{Notes::TO_MATCH_N_HERE.args("'{'"), start, findSemicolonOrEOL(begin, end),
                                  Modifier(start, start + 1, Modifier::PointAtBeginning)}}))
                    {
                        return {};
                    }
                }
                else
                {
                    if (!expect(Lexer::TokenType::CloseBrace,
                                begin,
                                end,
                                context))
                    {
                        return {};
                    }
                }
                stack.push(std::make_unique<PostFixExpression>(
                    PostFixExpressionTypeInitializer(start, begin, std::move(*type), std::move(*initializer))));
            }
            else
            {
                auto newPrimary = parsePrimaryExpression(begin, end, context);
                if (!newPrimary)
                {
                    return {};
                }
                stack.push(std::make_unique<PostFixExpression>(PostFixExpressionPrimaryExpression(start,
                                                                                                  begin,
                                                                                                  std::move(*newPrimary))));
            }
        }
        else if (begin->getTokenType() == Lexer::TokenType::OpenParenthese)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            auto openPpos = begin;
            begin++;
            std::vector<std::unique_ptr<AssignmentExpression>> nonCommaExpressions;
            while (begin < end && begin->getTokenType() != Lexer::TokenType::CloseParenthese)
            {
                auto assignment = parseAssignmentExpression(begin, end, context);
                if (!assignment)
                {
                    begin = std::find_if(begin, end, [](const Lexer::Token& token)
                    {
                        return token.getTokenType() == Lexer::TokenType::CloseParenthese
                            || token.getTokenType() == Lexer::TokenType::Comma;
                    });
                    if (begin != end)
                    {
                        return {};
                    }
                }
                nonCommaExpressions.push_back(std::make_unique<AssignmentExpression>(std::move(*assignment)));
                if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
                {
                    begin++;
                }
                else if (begin >= end || begin->getTokenType() != Lexer::TokenType::CloseParenthese)
                {
                    if (begin == end)
                    {
                        context.logError(OpenCL::Parser::ErrorMessages::EXPECTED_N
                                             .args("')'"),
                                         begin,
                                         Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd));
                        return {};
                    }
                    else
                    {
                        context.logError(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                             .args("','",
                                                   '\'' + begin->emitBack() + '\''),
                                         end, Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning));
                        begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
                        {
                            return firstIsInAssignmentExpression(token, context);
                        });
                        if (begin == end)
                        {
                            return {};
                        }
                    }
                }
            }
            if (!expect(Lexer::TokenType::CloseParenthese,
                        begin,
                        end,
                        context,
                        {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                          Modifier(start, start + 1, Modifier::PointAtBeginning)}}))
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token)
                {
                    return token.getTokenType() == Lexer::TokenType::OpenBrace;
                });
                if (begin == end)
                {
                    return {};
                }
            }
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(
                PostFixExpressionFunctionCall(start, begin, std::move(postExpression),
                                              std::move(nonCommaExpressions))));
        }
        else if (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket)
        {
            auto openPpos = begin;
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            begin++;
            auto expression = parseExpression(begin, end, context);
            if (!expression)
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token)
                {
                    return token.getTokenType() == Lexer::TokenType::CloseSquareBracket;
                });
                if (begin != end)
                {

                }
            }

            if (!expect(Lexer::TokenType::CloseSquareBracket,
                        begin,
                        end,
                        context,
                        {{Notes::TO_MATCH_N_HERE.args("'['"), start, findSemicolonOrEOL(begin, end),
                          Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
            {
                return {};
            }
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(
                PostFixExpressionSubscript(start, begin, std::move(postExpression), std::move(*expression))));
        }
        else if (begin->getTokenType() == Lexer::TokenType::Increment)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            begin++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(PostFixExpressionIncrement(start,
                                                                                      begin,
                                                                                      std::move(postExpression))));
        }
        else if (begin->getTokenType() == Lexer::TokenType::Decrement)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(PostFixExpressionDecrement(start,
                                                                                      begin,
                                                                                      std::move(postExpression))));
        }
        else if (begin->getTokenType() == Lexer::TokenType::Dot)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            begin++;
            std::string name;
            if (!expect(Lexer::TokenType::Identifier, begin, end, context, {}, &name))
            {
                return {};
            }
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(PostFixExpressionDot(start,
                                                                                begin,
                                                                                std::move(postExpression),
                                                                                name)));
        }
        else if (begin->getTokenType() == Lexer::TokenType::Arrow)
        {
            auto start = nodeFromNodeDerivedVariant(*stack.top()).begin();
            begin++;
            std::string name;
            if (!expect(Lexer::TokenType::Identifier, begin, end, context, {}, &name))
            {
                return {};
            }
            auto postExpression = std::move(stack.top());
            stack.pop();
            stack.push(std::make_unique<PostFixExpression>(PostFixExpressionArrow(start,
                                                                                  begin,
                                                                                  std::move(postExpression),
                                                                                  name)));
        }
    }

    if (stack.empty())
    {
        if (begin == end)
        {
            context.logError(OpenCL::Parser::ErrorMessages::EXPECTED_N
                                 .args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('")),
                             begin,
                             Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd));
        }
        else
        {
            context.logError(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                 .args(OpenCL::Format::List(", ", " or ", "literal", "identifier", "'('"),
                                       '\'' + begin->emitBack() + '\''),
                             end, Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning));
        }
    }
    auto ret = std::move(*stack.top());
    stack.pop();

    return ret;
}

std::optional<PrimaryExpression> OpenCL::Parser::parsePrimaryExpression(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end,
                                                                        ParsingContext& context)
{
    auto start = begin;
    auto currToken = *begin;
    begin++;
    if (currToken.getTokenType() == Lexer::TokenType::Identifier)
    {
        return PrimaryExpression(PrimaryExpressionIdentifier(start,
                                                             begin,
                                                             std::get<std::string>(currToken.getValue())));
    }
    else if (currToken.getTokenType() == Lexer::TokenType::Literal)
    {
        return PrimaryExpression(
            PrimaryExpressionConstant(
                start, begin,
                std::visit([](auto&& value) -> typename PrimaryExpressionConstant::variant
                           {
                               using T = std::decay_t<decltype(value)>;
                               if constexpr (std::is_constructible_v<typename PrimaryExpressionConstant::variant, T>)
                               {
                                   return {std::forward<decltype(value)>(value)};
                               }
                               else
                               {
                                   throw std::runtime_error("Can't convert type of variant to constant expression");
                               }
                           },
                           currToken.getValue())));
    }
    else if (currToken.getTokenType() == Lexer::TokenType::StringLiteral)
    {
        std::string result = std::get<std::string>(currToken.getValue());
        while (begin < end && begin->getTokenType() == Lexer::TokenType::StringLiteral)
        {
            result += std::get<std::string>(begin->getValue());
        }
        return PrimaryExpression(PrimaryExpressionConstant(start, begin, result));
    }
    else if (currToken.getTokenType() == Lexer::TokenType::OpenParenthese)
    {
        auto expression = parseExpression(begin, end, context);
        if (!expression)
        {
            return {};
        }
        if (begin->getTokenType() != Lexer::TokenType::CloseParenthese)
        {
            context.logError({"Expected Close Parentheses after expression in primary expression"},
                             std::vector<OpenCL::Lexer::Token>::const_iterator(),
                             std::optional<Modifier>(),
                             std::vector<Message::Note>());
        }
        begin++;

        return PrimaryExpression(PrimaryExpressionParenthese(start, begin, std::move(*expression)));
    }
    else
    {
        context.logError({"Invalid token for primary expression"},
                         std::vector<OpenCL::Lexer::Token>::const_iterator(),
                         std::optional<Modifier>(),
                         std::vector<Message::Note>());
        return {};
    }
}
