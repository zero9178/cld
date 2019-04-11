#include "Parser.hpp"

#include "ConstantEvaluator.hpp"

#include <stack>
#include <algorithm>

OpenCL::Syntax::TranslationUnit OpenCL::Parser::buildTree(std::vector<OpenCL::Lexer::Token>&& tokens)
{
    ParsingContext context;
    return parseTranslationUnit(tokens, context);
}

using namespace OpenCL::Lexer;
using namespace OpenCL::Syntax;

namespace
{
    bool isAssignment(TokenType type)
    {
        return type == TokenType::Assignment
            || type == TokenType::PlusAssign
            || type == TokenType::MinusAssign
            || type == TokenType::DivideAssign
            || type == TokenType::MultiplyAssign
            || type == TokenType::ModuloAssign
            || type == TokenType::ShiftLeftAssign
            || type == TokenType::ShiftRightAssign
            || type == TokenType::BitAndAssign
            || type == TokenType::BitOrAssign
            || type == TokenType::BitXorAssign;
    }
}

OpenCL::Syntax::TranslationUnit OpenCL::Parser::parseTranslationUnit(Tokens& tokens, ParsingContext& context)
{
    std::vector<ExternalDeclaration> global;
    while (!tokens.empty())
    {
        global.push_back(parseExternalDeclaration(tokens, context));
    }
    return TranslationUnit(std::move(global));
}

OpenCL::Syntax::ExternalDeclaration OpenCL::Parser::parseExternalDeclaration(Tokens& tokens, ParsingContext& context)
{
    std::size_t i = 0;
    auto result = std::find_if(tokens.rbegin(), tokens.rend(), [&i](const Token& token)
    {
        switch (token.getTokenType())
        {
        case TokenType::SemiColon:return true;
        case TokenType::OpenBrace:
        {
            if (!i)
            {
                return true;
            }
            else
            {
                i++;
            }
            break;
        }
        case TokenType::OpenParenthese:
        case TokenType::OpenSquareBracket:
        {
            i++;
            break;
        }
        case TokenType::CloseBrace:
        case TokenType::CloseParenthese:
        case TokenType::CloseSquareBracket:
        {
            i--;
            break;
        }
        case TokenType::TypedefKeyword:
        case TokenType::AutoKeyword:
        case TokenType::RegisterKeyword:
            return true;
        default:return false;
        }
        return false;
    });
    if (result == tokens.rend())
    {
        throw std::runtime_error("Unexpected end of tokens");
    }

    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    if (result->getTokenType() == TokenType::OpenBrace)
    {
        return ExternalDeclaration(line, column, parseFunctionDefinition(tokens, context));
    }
    else
    {
        return ExternalDeclaration(line, column, parseDeclaration(tokens, context));
    }
}

namespace
{
    bool backIsDeclarationSpecifier(OpenCL::Parser::Tokens& tokens,OpenCL::Parser::ParsingContext& context)
    {
        switch(tokens.back().getTokenType())
        {
        case TokenType::TypedefKeyword:
        case TokenType::ExternKeyword:
        case TokenType::StaticKeyword:
        case TokenType::AutoKeyword:
        case TokenType::RegisterKeyword:
        case TokenType::VoidKeyword:
        case TokenType::CharKeyword:
        case TokenType::ShortKeyword:
        case TokenType::IntKeyword:
        case TokenType::LongKeyword:
        case TokenType::FloatKeyword:
        case TokenType::DoubleKeyword:
        case TokenType::SignedKeyword:
        case TokenType::UnsignedKeyword:
        case TokenType::EnumKeyword:
        case TokenType::StructKeyword:
        case TokenType::UnionKeyword:
        case TokenType::ConstKeyword:
        case TokenType::RestrictKeyword:
        case TokenType::VolatileKeyword:
        case TokenType::InlineKeyword:
            return true;
        case TokenType::Identifier:
            return !context.isInScope(std::get<std::string>(tokens.back().getValue()))
                && context.typedefs.count(std::get<std::string>(tokens.back().getValue()));
        default:return false;
        }
    }
}

OpenCL::Syntax::Declaration OpenCL::Parser::parseDeclaration(Tokens& tokens,ParsingContext& context)
{
    std::vector<DeclarationSpecifier> declarationSpecifiers;
    while(backIsDeclarationSpecifier(tokens,context))
    {
        declarationSpecifiers.push_back(parseDeclarationSpecifier(tokens,context));
    }
}

OpenCL::Syntax::DeclarationSpecifier OpenCL::Parser::parseDeclarationSpecifier(OpenCL::Parser::Tokens& tokens,
                                                                               OpenCL::Parser::ParsingContext& context)
{
    auto currToken = tokens.back();
    tokens.pop_back();
    auto line = currToken.getLine();
    auto column = currToken.getColumn();
    switch(currToken.getTokenType())
    {
    case TokenType::TypedefKeyword:return StorageClassSpecifier::Typedef;
    case TokenType::ExternKeyword:return StorageClassSpecifier::Extern;
    case TokenType::StaticKeyword:return StorageClassSpecifier::Static;
    case TokenType::AutoKeyword:return StorageClassSpecifier::Auto;
    case TokenType::RegisterKeyword:return StorageClassSpecifier::Register;
    case TokenType::ConstKeyword:return TypeQualifier::Const;
    case TokenType::RestrictKeyword: return TypeQualifier::Restrict;
    case TokenType::VolatileKeyword:return TypeQualifier::Volatile;
    case TokenType::InlineKeyword:return FunctionSpecifier{};
    case TokenType::VoidKeyword:return TypeSpecifier(line,column,TypeSpecifier::PrimitiveTypeSpecifier::Void);
    case TokenType::CharKeyword:return TypeSpecifier(line,column,TypeSpecifier::PrimitiveTypeSpecifier::Char);
    case TokenType::ShortKeyword:return TypeSpecifier(line,column,TypeSpecifier::PrimitiveTypeSpecifier::Short);
    case TokenType::IntKeyword:return TypeSpecifier(line,column,TypeSpecifier::PrimitiveTypeSpecifier::Int);
    case TokenType::LongKeyword:return TypeSpecifier(line,column,TypeSpecifier::PrimitiveTypeSpecifier::Long);
    case TokenType::FloatKeyword:return TypeSpecifier(line,column,TypeSpecifier::PrimitiveTypeSpecifier::Float);
    case TokenType::DoubleKeyword:return TypeSpecifier(line,column,TypeSpecifier::PrimitiveTypeSpecifier::Double);
    case TokenType::SignedKeyword:return TypeSpecifier(line,column,TypeSpecifier::PrimitiveTypeSpecifier::Signed);
    case TokenType::UnsignedKeyword:return TypeSpecifier(line,column,TypeSpecifier::PrimitiveTypeSpecifier::Unsigned);
    case TokenType::UnionKeyword:
    case TokenType::StructKeyword:return TypeSpecifier(line,column,std::make_unique<StructOrUnionSpecifier>(parseStructOrUnionSpecifier(tokens,context)));
    case TokenType::EnumKeyword:return TypeSpecifier(line,column,std::make_unique<EnumSpecifier>(parseEnumSpecifier(tokens,context)));
    case TokenType::Identifier:
    {
        auto name = std::get<std::string>(currToken.getValue());
        if(!context.isInScope(name) && context.typedefs.count(name))
        {
            return TypeSpecifier(line,column,name);
        }
        break;
    }
    default:break;
    }
    throw std::runtime_error("Invalid token for declaration specifier");
}

OpenCL::Syntax::StructOrUnionSpecifier OpenCL::Parser::parseStructOrUnionSpecifier(OpenCL::Parser::Tokens& tokens,
                                                                                   OpenCL::Parser::ParsingContext& context)
{

}

OpenCL::Syntax::EnumSpecifier OpenCL::Parser::parseEnumSpecifier(OpenCL::Parser::Tokens& tokens,
                                                                 OpenCL::Parser::ParsingContext& context)
{

}

OpenCL::Syntax::FunctionDefinition OpenCL::Parser::parseFunctionDefinition(Tokens& tokens, ParsingContext& context)
{

}

CompoundItem OpenCL::Parser::parseCompoundItem(Tokens& tokens, ParsingContext& context)
{

}

Initializer OpenCL::Parser::parseInitializer(Tokens& tokens,
                                             ParsingContext& context)
{
    if (tokens.back().getTokenType() != TokenType::OpenBrace)
    {
        return Initializer(tokens.back().getLine(),
                           tokens.back().getColumn(), parseAssignmentExpression(tokens, context));
    }
    else
    {
        tokens.pop_back();
        auto initializerList = parseInitializerList(tokens, context);
        if (tokens.back().getTokenType() != TokenType::CloseBrace && tokens.back().getTokenType() != TokenType::Comma)
        {
            throw std::runtime_error("Expected } after initializer list");
        }
        if (tokens.back().getTokenType() == TokenType::Comma)
        {
            tokens.pop_back();
        }
        if (tokens.back().getTokenType() != TokenType::CloseBrace)
        {
            throw std::runtime_error("Expected } after initializer list");
        }
        tokens.pop_back();
        return {tokens.back().getLine(), tokens.back().getColumn(), std::move(initializerList)};
    }
}

OpenCL::Syntax::InitializerList OpenCL::Parser::parseInitializerList(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    typename OpenCL::Syntax::InitializerList::vector vector;
    while (!tokens.empty() && tokens.back().getTokenType() != TokenType::CloseBrace)
    {
        std::vector<std::variant<std::size_t, std::string>> variants;
        while (tokens.back().getTokenType() == TokenType::OpenSquareBracket
            || tokens.back().getTokenType() == TokenType::Dot)
        {
            if (tokens.back().getTokenType() == TokenType::OpenSquareBracket)
            {
                tokens.pop_back();
                auto constant = parseAssignmentExpression(tokens, context);
                if (tokens.back().getTokenType() != TokenType::CloseSquareBracket)
                {
                    throw std::runtime_error("Expected ] to close designator in initializer list");
                }
                tokens.pop_back();
                Codegen::ConstantEvaluator evaluator(context.structOrUnions);
                variants.emplace_back(std::visit([](auto&& value) -> std::size_t
                                                 {
                                                     using T = std::decay_t<decltype(value)>;
                                                     if constexpr(std::is_convertible_v<T, std::size_t>)
                                                     {
                                                         return value;
                                                     }
                                                     else
                                                     {
                                                         throw std::runtime_error("Invalid type of constanst expression");
                                                     }
                                                 }, *evaluator.visit(constant)));
            }
            else if (tokens.back().getTokenType() == TokenType::Dot)
            {
                tokens.pop_back();
                variants.emplace_back(std::get<std::string>(tokens.back().getValue()));
                tokens.pop_back();
            }
        }
        if (!variants.empty())
        {
            if (tokens.back().getTokenType() == TokenType::Assignment)
            {
                tokens.pop_back();
            }
            else
            {
                throw std::runtime_error("Expected = after designators");
            }
        }
        vector.push_back({parseInitializer(tokens, context), variants});
        if (tokens.back().getTokenType() == TokenType::Comma)
        {
            tokens.pop_back();
        }
        else if (tokens.back().getTokenType() != TokenType::CloseBrace)
        {
            throw std::runtime_error("Expected , between initializers in initializer list");
        }
    }
    return {line, column, std::move(vector)};
}

Statement OpenCL::Parser::parseStatement(Tokens& tokens, ParsingContext& context)
{
    auto result = [&tokens, &context]() -> Statement
    {
        auto curentToken = tokens.back();
        auto line = curentToken.getLine();
        auto column = curentToken.getColumn();
        switch (curentToken.getTokenType())
        {
        case TokenType::ReturnKeyword:
        {
            tokens.pop_back();
            return {line, column, ReturnStatement(curentToken.getLine(),
                                                  curentToken.getColumn(),
                                                  parseExpression(tokens, context))};
        }
        case TokenType::IfKeyword:
        {
            tokens.pop_back();
            curentToken = tokens.back();
            tokens.pop_back();
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                throw std::runtime_error("Expected ( after if");
            }
            auto expression = parseExpression(tokens, context);
            curentToken = tokens.back();
            tokens.pop_back();
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                throw std::runtime_error("Expected ) at the end of if statement");
            }
            auto statement = parseStatement(tokens, context);
            curentToken = tokens.back();
            if (!tokens.empty() && curentToken.getTokenType() == TokenType::ElseKeyword)
            {
                tokens.pop_back();
                return {line, column, IfStatement(line, column, std::move(expression),
                                                  std::make_unique<Statement>(std::move(statement)),
                                                  std::make_unique<Statement>(parseStatement(tokens, context)))};
            }
            else
            {
                return {line, column, IfStatement(line,
                                                  column,
                                                  std::move(expression),
                                                  std::make_unique<Statement>(std::move(statement)))};
            }
        }
        case TokenType::OpenBrace:
        {
            tokens.pop_back();
            context.pushScope();
            std::vector<CompoundItem> blockItems;
            while (!tokens.empty() && tokens.back().getTokenType() != TokenType::CloseBrace)
            {
                blockItems.push_back(parseCompoundItem(tokens, context));
            }
            if (tokens.empty() || tokens.back().getTokenType() != TokenType::CloseBrace)
            {
                throw std::runtime_error("Expected } to close Block");
            }
            tokens.pop_back();
            context.popScope();
            return {line, column, CompoundStatement(line, column, std::move(blockItems))};
        }
        case TokenType::ForKeyword:
        {
            tokens.pop_back();
            curentToken = tokens.back();
            tokens.pop_back();
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                throw std::runtime_error("Expected ( after for");
            }
            auto blockitem = parseCompoundItem(tokens, context);

            auto control = [&]() -> std::unique_ptr<Expression>
            {
                if (std::holds_alternative<Declaration>(blockitem.getVariant())
                    || tokens.back().getTokenType() != TokenType::SemiColon)
                {
                    auto expression = parseExpression(tokens, context);
                    if (tokens.back().getTokenType() != TokenType::SemiColon)
                    {
                        throw std::runtime_error("Expected ; after control part of for loop header");
                    }
                    tokens.pop_back();
                    return std::make_unique<Expression>(std::move(expression));
                }
                else
                {
                    tokens.pop_back();
                    return nullptr;
                }
            }();

            auto post = [&]() -> std::unique_ptr<Expression>
            {
                if (tokens.back().getTokenType() != TokenType::CloseParenthese)
                {
                    auto expression = parseExpression(tokens, context);
                    if (tokens.back().getTokenType() != TokenType::CloseParenthese)
                    {
                        throw std::runtime_error("Expected ) after control part of for loop header");
                    }
                    tokens.pop_back();
                    return std::make_unique<Expression>(std::move(expression));
                }
                else
                {
                    tokens.pop_back();
                    return nullptr;
                }
            }();

            auto statement = parseStatement(tokens, context);

            if (auto declaration = std::get_if<Declaration>(&blockitem.getVariant());declaration)
            {
                return Statement(line, column, ForDeclarationStatement(line,
                                                                       column,
                                                                       std::make_unique<Statement>(std::move(statement)),
                                                                       std::move(*declaration),
                                                                       std::move(control),
                                                                       std::move(post)));
            }
            else if (auto
                    expressionStatement = std::get_if<ExpressionStatement>(&std::get<Statement>(blockitem.getVariant())
                    .getVariant());expressionStatement)
            {
                return Statement(line,
                                 column,
                                 ForStatement(line, column, std::make_unique<Statement>(std::move(statement)),
                                              expressionStatement->moveOptionalExpression(),
                                              std::move(control),
                                              std::move(post)));
            }
            else
            {
                throw std::runtime_error("Invalid expression or declaration for initial part of for loop header");
            }
        }
        case TokenType::WhileKeyword:
        {
            tokens.pop_back();
            curentToken = tokens.back();
            tokens.pop_back();
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                throw std::runtime_error("Expected ( after while");
            }
            auto expression = parseExpression(tokens, context);
            curentToken = tokens.back();
            tokens.pop_back();
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                throw std::runtime_error("Expected ) after expression in while");
            }
            return Statement(line,
                             column,
                             HeadWhileStatement(line,
                                                column,
                                                std::move(expression),
                                                std::make_unique<Statement>(parseStatement(tokens, context))));
        }
        case TokenType::DoKeyword:
        {
            tokens.pop_back();
            auto statement = parseStatement(tokens, context);
            curentToken = tokens.back();
            tokens.pop_back();
            if (curentToken.getTokenType() != TokenType::WhileKeyword)
            {
                throw std::runtime_error("Expected while after do");
            }
            curentToken = tokens.back();
            tokens.pop_back();
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                throw std::runtime_error("Expected ( after while");
            }
            auto expression = parseExpression(tokens, context);
            curentToken = tokens.back();
            tokens.pop_back();
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                throw std::runtime_error("Expected ) after expression in while");
            }
            return Statement(line, column, FootWhileStatement(line,
                                                              column,
                                                              std::make_unique<Statement>(std::move(statement)),
                                                              std::move(expression)));
        }
        case TokenType::BreakKeyword:
        {
            tokens.pop_back();
            return Statement(line, column, BreakStatement(line, column));
        }
        case TokenType::ContinueKeyword:
        {
            tokens.pop_back();
            return Statement(line, column, ContinueStatement(line, column));
        }
        case TokenType::SwitchKeyword:
        {
            tokens.pop_back();
            curentToken = tokens.back();
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                throw std::runtime_error("Expected ( after switch keyword");
            }
            tokens.pop_back();
            auto expression = parseExpression(tokens, context);
            curentToken = tokens.back();
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                throw std::runtime_error("Expected ) after expression in switch ");
            }
            tokens.pop_back();
            return Statement(line, column, SwitchStatement(line,
                                                           column,
                                                           std::move(expression),
                                                           std::make_unique<Statement>(parseStatement(tokens,
                                                                                                      context))));
        }
        case TokenType::DefaultKeyword:
        {
            tokens.pop_back();
            curentToken = tokens.back();
            if (curentToken.getTokenType() != TokenType::Colon)
            {
                throw std::runtime_error("Expected : after default");
            }
            tokens.pop_back();
            return Statement(line,
                             column,
                             DefaultStatement(line,
                                              column,
                                              std::make_unique<Statement>(parseStatement(tokens, context))));
        }
        case TokenType::CaseKeyword:
        {
            tokens.pop_back();
            auto expression = parseAssignmentExpression(tokens, context);
            curentToken = tokens.back();
            if (curentToken.getTokenType() != TokenType::Colon)
            {
                throw std::runtime_error("Expected : after constant expression of case");
            }
            tokens.pop_back();
            Codegen::ConstantEvaluator evaluator(context.structOrUnions);
            return Statement(line, column, CaseStatement(line, column,
                                                         std::move(*evaluator.visit(expression)),
                                                         tokens.back().getTokenType() != TokenType::CaseKeyword
                                                         ? std::make_unique<Statement>(parseStatement(tokens, context))
                                                         : nullptr));
        }
        default:
        {
            if (!tokens.empty() && tokens.back().getTokenType() != TokenType::SemiColon)
            {
                return Statement(line, column, ExpressionStatement(line,
                                                                   column,
                                                                   std::make_unique<Expression>(parseExpression(tokens,
                                                                                                                context))));
            }
            else
            {
                return Statement(line, column, ExpressionStatement(line, column));
            }
        }
        }
    }();

    if ((std::holds_alternative<ExpressionStatement>(result.getVariant())
        || std::holds_alternative<ReturnStatement>(result.getVariant())
        || std::holds_alternative<FootWhileStatement>(result.getVariant())
        || std::holds_alternative<BreakStatement>(result.getVariant())
        || std::holds_alternative<ContinueStatement>(result.getVariant()))
        && (tokens.empty() || tokens.back().getTokenType() != TokenType::SemiColon))
    {
        throw std::runtime_error("Statement not terminated with ;");
    }
    else if (std::holds_alternative<ExpressionStatement>(result.getVariant())
        || std::holds_alternative<ReturnStatement>(result.getVariant())
        || std::holds_alternative<FootWhileStatement>(result.getVariant())
        || std::holds_alternative<BreakStatement>(result.getVariant())
        || std::holds_alternative<ContinueStatement>(result.getVariant()))
    {
        tokens.pop_back();
    }
    return result;
}

OpenCL::Syntax::Expression OpenCL::Parser::parseExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    std::vector<AssignmentExpression> expressions;
    expressions.push_back(parseAssignmentExpression(tokens, context));

    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::Comma)
        {
            tokens.pop_back();
            expressions.push_back(parseAssignmentExpression(tokens, context));
        }
    }
    return Expression(line, column, std::move(expressions));
}

AssignmentExpression OpenCL::Parser::parseAssignmentExpression(Tokens& tokens,
                                                               ParsingContext& context)
{
    std::size_t parentheseCount = 0;
    std::size_t squareCount = 0;
    std::size_t braceCount = 0;
    auto result = std::find_if(tokens.rbegin(), tokens.rend(), [&](const Token& token)
    {
        if (token.getTokenType() == TokenType::SemiColon || isAssignment(token.getTokenType())
            || (parentheseCount == 0 && token.getTokenType() == TokenType::Comma))
        {
            return true;
        }
        else if (token.getTokenType() == TokenType::OpenParenthese)
        {
            parentheseCount++;
        }
        else if (token.getTokenType() == TokenType::CloseParenthese)
        {
            if (!parentheseCount)
            {
                return true;
            }
            else
            {
                parentheseCount--;
            }
        }
        else if (token.getTokenType() == TokenType::OpenSquareBracket)
        {
            squareCount++;
        }
        else if (token.getTokenType() == TokenType::CloseSquareBracket)
        {
            if (!squareCount)
            {
                return true;
            }
            else
            {
                squareCount--;
            }
        }
        else if (token.getTokenType() == TokenType::OpenBrace)
        {
            braceCount++;
        }
        else if (token.getTokenType() == TokenType::CloseBrace)
        {
            if (!braceCount)
            {
                return true;
            }
            else
            {
                braceCount--;
            }
        }
        return false;
    });
    bool assignment = result != tokens.rend() && isAssignment(result->getTokenType());
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    if (assignment)
    {
        auto unary = parseUnaryExpression(tokens, context);
        auto currentToken = tokens.back();
        tokens.pop_back();
        return AssignmentExpression(line, column, AssignmentExpressionAssignment(line,
                                                                                 column,
                                                                                 std::move(unary),
                                                                                 [assignment = currentToken
                                                                                     .getTokenType()]
                                                                                 {
                                                                                     switch (assignment)
                                                                                     {
                                                                                     case TokenType::Assignment:return AssignmentExpressionAssignment::AssignOperator::NoOperator;
                                                                                     case TokenType::PlusAssign:return AssignmentExpressionAssignment::AssignOperator::PlusAssign;
                                                                                     case TokenType::MinusAssign:return AssignmentExpressionAssignment::AssignOperator::MinusAssign;
                                                                                     case TokenType::DivideAssign:return AssignmentExpressionAssignment::AssignOperator::DivideAssign;
                                                                                     case TokenType::MultiplyAssign:return AssignmentExpressionAssignment::AssignOperator::MultiplyAssign;
                                                                                     case TokenType::ModuloAssign:return AssignmentExpressionAssignment::AssignOperator::ModuloAssign;
                                                                                     case TokenType::ShiftLeftAssign:return AssignmentExpressionAssignment::AssignOperator::LeftShiftAssign;
                                                                                     case TokenType::ShiftRightAssign:return AssignmentExpressionAssignment::AssignOperator::RightShiftAssign;
                                                                                     case TokenType::BitAndAssign:return AssignmentExpressionAssignment::AssignOperator::BitAndAssign;
                                                                                     case TokenType::BitOrAssign:return AssignmentExpressionAssignment::AssignOperator::BitOrAssign;
                                                                                     case TokenType::BitXorAssign:return AssignmentExpressionAssignment::AssignOperator::BitXorAssign;
                                                                                     default:
                                                                                         throw std::runtime_error(
                                                                                             "Invalid token for assignment");
                                                                                     }
                                                                                 }(),
                                                                                 std::make_unique<AssignmentExpression>(
                                                                                     parseAssignmentExpression(tokens,
                                                                                                               context))));
    }
    else
    {
        return AssignmentExpression(line, column, ConditionalExpression(parseConditionalExpression(tokens, context)));
    }
}

OpenCL::Syntax::ConditionalExpression OpenCL::Parser::parseConditionalExpression(Tokens& tokens,
                                                                                 ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto logicalOrExperssion = parseLogicalOrExpression(tokens, context);
    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        if (currToken.getTokenType() == TokenType::QuestionMark)
        {
            tokens.pop_back();
            auto optionalExpression = parseExpression(tokens, context);
            currToken = tokens.back();
            if (currToken.getTokenType() != TokenType::Colon)
            {
                throw std::runtime_error("Expected : to match ?");
            }
            tokens.pop_back();
            auto optionalConditional = parseConditionalExpression(tokens, context);
            return ConditionalExpression(line, column, std::move(logicalOrExperssion),
                                         std::make_unique<Expression>(std::move(optionalExpression)),
                                         std::make_unique<ConditionalExpression>(std::move(optionalConditional)));
        }
    }
    return ConditionalExpression(line, column, std::move(logicalOrExperssion));
}

OpenCL::Syntax::LogicalOrExpression OpenCL::Parser::parseLogicalOrExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto logicalAnd = parseLogicalAndExpression(tokens, context);

    std::vector<LogicalAndExpression> optionalLogicalAnds;
    if (!tokens.empty())
    {
        auto curentToken = tokens.back();
        while (curentToken.getTokenType() == TokenType::LogicOr)
        {
            tokens.pop_back();
            optionalLogicalAnds.push_back(parseLogicalAndExpression(tokens, context));
            if (tokens.empty())
            {
                break;
            }
            curentToken = tokens.back();
        }
    }

    return LogicalOrExpression(line, column, std::move(logicalAnd), std::move(optionalLogicalAnds));
}

OpenCL::Syntax::LogicalAndExpression OpenCL::Parser::parseLogicalAndExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto result = parseBitOrExpression(tokens, context);

    std::vector<BitOrExpression> list;
    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::LogicAnd)
        {
            tokens.pop_back();
            list.push_back(parseBitOrExpression(tokens, context));
            if (tokens.empty())
            {
                break;
            }
            currToken = tokens.back();
        }
    }

    return LogicalAndExpression(line, column, std::move(result), std::move(list));
}

OpenCL::Syntax::BitOrExpression OpenCL::Parser::parseBitOrExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto result = parseBitXorExpression(tokens, context);

    std::vector<BitXorExpression> list;
    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::BitOr)
        {
            tokens.pop_back();
            list.push_back(parseBitXorExpression(tokens, context));
            if (tokens.empty())
            {
                break;
            }
            currToken = tokens.back();
        }
    }

    return BitOrExpression(line, column, std::move(result), std::move(list));
}

OpenCL::Syntax::BitXorExpression OpenCL::Parser::parseBitXorExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto result = parseBitAndExpression(tokens, context);

    std::vector<BitAndExpression> list;
    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::BitXor)
        {
            tokens.pop_back();
            list.push_back(parseBitAndExpression(tokens, context));
            if (tokens.empty())
            {
                break;
            }
            currToken = tokens.back();
        }
    }

    return BitXorExpression(line, column, std::move(result), std::move(list));
}

OpenCL::Syntax::BitAndExpression OpenCL::Parser::parseBitAndExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto result = parseEqualityExpression(tokens, context);

    std::vector<EqualityExpression> list;
    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::Ampersand)
        {
            tokens.pop_back();
            list.push_back(parseEqualityExpression(tokens, context));
            if (tokens.empty())
            {
                break;
            }
            currToken = tokens.back();
        }
    }

    return BitAndExpression(line, column, std::move(result), std::move(list));
}

OpenCL::Syntax::EqualityExpression OpenCL::Parser::parseEqualityExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto result = parseRelationalExpression(tokens, context);

    std::vector<std::pair<EqualityExpression::EqualityOperator, RelationalExpression>> relationalExpressions;
    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::Equal || currToken.getTokenType() == TokenType::NotEqual)
        {
            tokens.pop_back();
            relationalExpressions
                .emplace_back(
                    currToken.getTokenType() == TokenType::Equal ? EqualityExpression::EqualityOperator::Equal
                                                                 : EqualityExpression::EqualityOperator::NotEqual,
                    parseRelationalExpression(tokens, context));
            if (tokens.empty())
            {
                break;
            }
            currToken = tokens.back();
        }
    }

    return EqualityExpression(line, column, std::move(result), std::move(relationalExpressions));
}

OpenCL::Syntax::RelationalExpression OpenCL::Parser::parseRelationalExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto result = parseShiftExpression(tokens, context);

    std::vector<std::pair<RelationalExpression::RelationalOperator, ShiftExpression>> list;
    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::LessThan
            || currToken.getTokenType() == TokenType::LessThanOrEqual
            || currToken.getTokenType() == TokenType::GreaterThan
            || currToken.getTokenType() == TokenType::GreaterThanOrEqual)
        {
            tokens.pop_back();
            list.emplace_back([currToken]() -> RelationalExpression::RelationalOperator
                              {
                                  switch (currToken.getTokenType())
                                  {
                                  case TokenType::LessThan:return RelationalExpression::RelationalOperator::LessThan;
                                  case TokenType::LessThanOrEqual:return RelationalExpression::RelationalOperator::LessThanOrEqual;
                                  case TokenType::GreaterThan:return RelationalExpression::RelationalOperator::GreaterThan;
                                  case TokenType::GreaterThanOrEqual:return RelationalExpression::RelationalOperator::GreaterThanOrEqual;
                                  default:throw std::runtime_error("Invalid token for relational LogicalOrExpression");
                                  }
                              }(), parseShiftExpression(tokens, context));
            if (tokens.empty())
            {
                break;
            }
            currToken = tokens.back();
        }
    }

    return RelationalExpression(line, column, std::move(result), std::move(list));
}

OpenCL::Syntax::ShiftExpression OpenCL::Parser::parseShiftExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto result = parseAdditiveExpression(tokens, context);

    std::vector<std::pair<ShiftExpression::ShiftOperator, AdditiveExpression>> list;
    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::ShiftRight
            || currToken.getTokenType() == TokenType::ShiftLeft)
        {
            tokens.pop_back();
            list.emplace_back(
                currToken.getTokenType() == TokenType::ShiftRight ? ShiftExpression::ShiftOperator::Right
                                                                  : ShiftExpression::ShiftOperator::Left,
                parseAdditiveExpression(tokens, context));
            if (tokens.empty())
            {
                break;
            }
            currToken = tokens.back();
        }
    }

    return ShiftExpression(line, column, std::move(result), std::move(list));
}

OpenCL::Syntax::AdditiveExpression OpenCL::Parser::parseAdditiveExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto result = parseTerm(tokens, context);

    std::vector<std::pair<AdditiveExpression::BinaryDashOperator, Term>> list;
    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::Addition
            || currToken.getTokenType() == TokenType::Negation)
        {
            tokens.pop_back();
            list.emplace_back(
                currToken.getTokenType() == TokenType::Addition ? AdditiveExpression::BinaryDashOperator::BinaryPlus
                                                                : AdditiveExpression::BinaryDashOperator::BinaryMinus,
                parseTerm(tokens, context));
            if (tokens.empty())
            {
                break;
            }
            currToken = tokens.back();
        }
    }

    return AdditiveExpression(line, column, std::move(result), std::move(list));
}

OpenCL::Syntax::Term OpenCL::Parser::parseTerm(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto result = parseCastExpression(tokens, context);

    std::vector<std::pair<Term::BinaryDotOperator, CastExpression>> list;
    if (!tokens.empty())
    {
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::Asterisk
            || currToken.getTokenType() == TokenType::Division
            || currToken.getTokenType() == TokenType::Modulo)
        {
            tokens.pop_back();
            list.emplace_back([currToken]
                              {
                                  switch (currToken.getTokenType())
                                  {
                                  case TokenType::Asterisk:return Term::BinaryDotOperator::BinaryMultiply;
                                  case TokenType::Division:return Term::BinaryDotOperator::BinaryDivide;
                                  case TokenType::Modulo:return Term::BinaryDotOperator::BinaryRemainder;
                                  default:throw std::runtime_error("Invalid token");
                                  }
                              }(), parseCastExpression(tokens, context));
            if (tokens.empty())
            {
                break;
            }
            currToken = tokens.back();
        }
    }

    return Term(line, column, std::move(result), std::move(list));
}

OpenCL::Syntax::CastExpression OpenCL::Parser::parseCastExpression(Tokens& tokens, ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto currToken = tokens.rbegin();
    if (currToken->getTokenType() == TokenType::OpenParenthese)
    {
        currToken++;
        auto closing = std::find_if(currToken, tokens.rend(), [](const Token& tokens)
        {
            return tokens.getTokenType() == TokenType::CloseParenthese;
        });
        if (closing == tokens.rend())
        {
            throw std::runtime_error("Unexpected end of tokens");
        }
        closing++;
        //        if (isType(*currToken, context) && closing->getTokenType() != TokenType::OpenBrace)
        //        {
        //            auto result = std::find_if(currToken + 1, tokens.rend(), [](const Token& token)
        //            {
        //                return token.getTokenType() == TokenType::CloseParenthese
        //                    || token.getTokenType() == TokenType::OpenBrace;
        //            });
        //            if (result == tokens.rend())
        //            {
        //                throw std::runtime_error("Unexpected end of tokens");
        //            }
        //            if (result->getTokenType() == TokenType::CloseParenthese)
        //            {
        //                tokens.pop_back();
        //                auto type = parseType(tokens, context);
        //                if (tokens.back().getTokenType() != TokenType::CloseParenthese)
        //                {
        //                    throw std::runtime_error("Expected Close Parenthese after type cast");
        //                }
        //                tokens.pop_back();
        //                return CastExpression(line,
        //                                      column,
        //                                      std::pair<std::unique_ptr<IType>, std::unique_ptr<CastExpression>>{
        //                                          std::move(type),
        //                                          std::make_unique<CastExpression>(parseCastExpression(tokens, context))});
        //            }
        //        }
    }
    return CastExpression(line, column, parseUnaryExpression(tokens, context));
}

UnaryExpression OpenCL::Parser::parseUnaryExpression(Tokens& tokens,
                                                     ParsingContext& context)
{
    auto line = tokens.back().getLine();
    auto column = tokens.back().getColumn();
    auto currToken = tokens.back();
    if (currToken.getTokenType() == TokenType::SizeofKeyword)
    {
        tokens.pop_back();
        currToken = tokens.back();
        if (currToken.getTokenType() == TokenType::OpenParenthese)
        {
            //            tokens.pop_back();
            //            auto type = parseType(tokens, context);
            //            currToken = tokens.back();
            //            if (currToken.getTokenType() != TokenType::CloseParenthese)
            //            {
            //                throw std::runtime_error("Expected Close Parenthese after type in sizeof");
            //            }
            //            tokens.pop_back();
            //            return UnaryExpression(line, column, UnaryExpressionSizeOf(line, column, std::move(type)));
        }
        else
        {
            auto unary = std::make_unique<UnaryExpression>(parseUnaryExpression(tokens, context));
            return UnaryExpression(line, column, UnaryExpressionSizeOf(line, column, std::move(unary)));
        }
    }
    else if (currToken.getTokenType() == TokenType::Increment
        || currToken.getTokenType() == TokenType::Decrement
        || currToken.getTokenType() == TokenType::Ampersand
        || currToken.getTokenType() == TokenType::Asterisk
        || currToken.getTokenType() == TokenType::Addition
        || currToken.getTokenType() == TokenType::Negation
        || currToken.getTokenType() == TokenType::LogicalNegation
        || currToken.getTokenType() == TokenType::BitWiseNegation)
    {
        tokens.pop_back();
        auto op = [&currToken]
        {
            switch (currToken.getTokenType())
            {
            case TokenType::Increment:return UnaryExpressionUnaryOperator::UnaryOperator::Increment;
            case TokenType::Decrement:return UnaryExpressionUnaryOperator::UnaryOperator::Decrement;
            case TokenType::Ampersand:return UnaryExpressionUnaryOperator::UnaryOperator::Ampersand;
            case TokenType::Asterisk:return UnaryExpressionUnaryOperator::UnaryOperator::Asterisk;
            case TokenType::Addition:return UnaryExpressionUnaryOperator::UnaryOperator::Plus;
            case TokenType::Negation:return UnaryExpressionUnaryOperator::UnaryOperator::Minus;
            case TokenType::LogicalNegation:return UnaryExpressionUnaryOperator::UnaryOperator::BitNot;
            case TokenType::BitWiseNegation:return UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot;
            default:throw std::runtime_error("Invalid token");
            }
        }();
        return UnaryExpression(line, column, UnaryExpressionUnaryOperator(line,
                                                                          column,
                                                                          op,
                                                                          std::make_unique<UnaryExpression>(
                                                                              parseUnaryExpression(tokens, context))));
    }
    return UnaryExpression(line, column, UnaryExpressionPostFixExpression(line,
                                                                          column,
                                                                          parsePostFixExpression(tokens,
                                                                                                 context)));
}

namespace
{
    bool isPostFixExpression(OpenCL::Parser::Tokens& token)
    {
        switch (token.back().getTokenType())
        {
        case TokenType::Arrow:
        case TokenType::Dot:
        case TokenType::OpenSquareBracket:
        case TokenType::Identifier:
        case TokenType::OpenParenthese:
        case TokenType::Literal:
        case TokenType::Increment:
        case TokenType::Decrement:return true;
        default:break;
        }
        return false;
    }
}

PostFixExpression OpenCL::Parser::parsePostFixExpression(Tokens& tokens,
                                                         ParsingContext& context)
{
    std::stack<std::unique_ptr<PostFixExpression>> stack;
    while (!tokens.empty() && isPostFixExpression(tokens))
    {
        auto currToken = tokens.back();
        if (currToken.getTokenType() == TokenType::Identifier
            || currToken.getTokenType() == TokenType::Literal)
        {
            if (!stack.empty())
            {
                throw std::runtime_error("Can't combine post fix expressions");
            }
            auto line = currToken.getLine();
            auto column = currToken.getColumn();
            stack.push(std::make_unique<PostFixExpression>(line, column, PostFixExpressionPrimaryExpression(line,
                                                                                                            column,
                                                                                                            parsePrimaryExpression(
                                                                                                                tokens,
                                                                                                                context))));
        }
        else if (currToken.getTokenType() == TokenType::OpenParenthese && stack.empty())
        {
            if (tokens.size() == 1)
            {
                throw std::runtime_error("Unexpected end of token");
            }
            //            if (isType(tokens.at(tokens.size() - 2), context))
            //            {
            //                auto line = currToken.getLine();
            //                auto column = currToken.getColumn();
            //                if (!stack.empty())
            //                {
            //                    throw std::runtime_error("Can't combine post fix expressions");
            //                }
            //                tokens.pop_back();
            //                auto type = parseType(tokens, context);
            //                currToken = tokens.back();
            //                if (currToken.getTokenType() != TokenType::CloseParenthese)
            //                {
            //                    throw std::runtime_error("Expected ) after type in type initialization");
            //                }
            //                tokens.pop_back();
            //                currToken = tokens.back();
            //                if (currToken.getTokenType() != TokenType::OpenBrace)
            //                {
            //                    throw std::runtime_error("Expected { after type around parenthesis");
            //                }
            //                tokens.pop_back();
            //                auto initializer = parseInitializerList(tokens, context);
            //                if (tokens.back().getTokenType() == TokenType::Comma)
            //                {
            //                    tokens.pop_back();
            //                }
            //                if (tokens.back().getTokenType() != TokenType::CloseBrace)
            //                {
            //                    throw std::runtime_error("Expected { after type around parenthesis");
            //                }
            //                tokens.pop_back();
            //                stack.push(std::make_unique<PostFixExpression>(line, column, PostFixExpressionTypeInitializer(line,
            //                                                                                                              column,
            //                                                                                                              std::move(
            //                                                                                                                  type),
            //                                                                                                              std::move(
            //                                                                                                                  initializer))));
            //            }
            //            else
            {
                auto line = currToken.getLine();
                auto column = currToken.getColumn();
                stack.push(std::make_unique<PostFixExpression>(line, column, PostFixExpressionPrimaryExpression(line,
                                                                                                                column,
                                                                                                                parsePrimaryExpression(
                                                                                                                    tokens,
                                                                                                                    context))));
            }
        }
        else if (currToken.getTokenType() == TokenType::OpenParenthese)
        {
            tokens.pop_back();
            std::vector<std::unique_ptr<AssignmentExpression>> nonCommaExpressions;
            while (tokens.back().getTokenType() != TokenType::CloseParenthese)
            {
                nonCommaExpressions
                    .push_back(std::make_unique<AssignmentExpression>(parseAssignmentExpression(tokens, context)));
                if (tokens.back().getTokenType() == TokenType::CloseParenthese)
                {
                    break;
                }
                else if (tokens.back().getTokenType() != TokenType::Comma)
                {
                    throw std::runtime_error("Expected , after argument of function");
                }
                tokens.pop_back();
            }
            tokens.pop_back();
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = currToken.getLine();
            auto column = currToken.getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionFunctionCall(line,
                                                                                         column,
                                                                                         std::move(postExpression),
                                                                                         std::move(nonCommaExpressions))));
        }
        else if (currToken.getTokenType() == TokenType::OpenSquareBracket)
        {
            tokens.pop_back();
            std::size_t i = 0;
            auto result = std::find_if(tokens.rbegin(), tokens.rend(), [&i](const Token& token)
            {
                if (token.getTokenType() == TokenType::CloseSquareBracket)
                {
                    if (i == 0)
                    {
                        return true;
                    }
                    i--;
                }
                else if (token.getTokenType() == TokenType::OpenSquareBracket)
                {
                    i++;
                }
                return false;
            });
            if (result == tokens.rend())
            {
                throw std::runtime_error("Unexpected end of tokens");
            }
            Tokens newTokens(tokens.rbegin(), result);
            auto expression = parseExpression(newTokens, context);
            tokens.erase(result.base(), tokens.end());
            if (tokens.back().getTokenType() != TokenType::CloseSquareBracket)
            {
                throw std::runtime_error("Expected ] after [");
            }
            tokens.pop_back();
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = currToken.getLine();
            auto column = currToken.getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionSubscript(line,
                                                                                      column,
                                                                                      std::move(postExpression),
                                                                                      std::move(expression))));
        }
        else if (currToken.getTokenType() == TokenType::Increment)
        {
            tokens.pop_back();
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = currToken.getLine();
            auto column = currToken.getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionIncrement(line,
                                                                                      column,
                                                                                      std::move(postExpression))));
        }
        else if (currToken.getTokenType() == TokenType::Decrement)
        {
            tokens.pop_back();
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = currToken.getLine();
            auto column = currToken.getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionDecrement(line,
                                                                                      column,
                                                                                      std::move(postExpression))));
        }
        else if (currToken.getTokenType() == TokenType::Dot)
        {
            tokens.pop_back();
            currToken = tokens.back();
            if (currToken.getTokenType() != TokenType::Identifier)
            {
                throw std::runtime_error("Expected Identifier after .");
            }
            tokens.pop_back();
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = currToken.getLine();
            auto column = currToken.getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionDot(line, column, std::move(postExpression),
                                                                                std::get<std::string>(currToken
                                                                                                          .getValue()))));
        }
        else if (currToken.getTokenType() == TokenType::Arrow)
        {
            tokens.pop_back();
            currToken = tokens.back();
            if (currToken.getTokenType() != TokenType::Identifier)
            {
                throw std::runtime_error("Expected Identifier after .");
            }
            tokens.pop_back();
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = currToken.getLine();
            auto column = currToken.getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionArrow(line,
                                                                                  column,
                                                                                  std::move(postExpression),
                                                                                  std::get<std::string>(currToken
                                                                                                            .getValue()))));
        }
    }
    if (stack.size() != 1)
    {
        throw std::runtime_error("Invalid amount of post fix expressions");
    }
    auto ret = std::move(*stack.top());
    stack.pop();
    return ret;
}

PrimaryExpression OpenCL::Parser::parsePrimaryExpression(Tokens& tokens,
                                                         ParsingContext& context)
{
    auto currToken = tokens.back();
    tokens.pop_back();
    auto line = currToken.getLine();
    auto column = currToken.getColumn();
    if (currToken.getTokenType() == TokenType::Identifier)
    {
        const auto& name = std::get<std::string>(currToken.getValue());
        if (context.isInScope(name))
        {
            return PrimaryExpression(line, column, PrimaryExpressionIdentifier(line,
                                                                               column,
                                                                               name));
        }
        else if (context.functions.count(name))
        {
            return PrimaryExpression(line, column, PrimaryExpressionIdentifier(line, column, name));
        }
        else
        {
            auto* result = context.getEnumConstant(name);
            if (!result)
            {
                throw std::runtime_error("Unknown reference to variable or enum constant " + name);
            }
            return PrimaryExpression(line, column, PrimaryExpressionConstant(line, column, *result));
        }
    }
    else if (currToken.getTokenType() == TokenType::Literal)
    {
        return PrimaryExpression(line, column, PrimaryExpressionConstant(line,
                                                                         column,
                                                                         std::visit([](auto&& value) -> typename PrimaryExpressionConstant::variant
                                                                                    {
                                                                                        using T = std::decay_t<decltype(value)>;
                                                                                        if constexpr(std::is_constructible_v<
                                                                                            typename PrimaryExpressionConstant::variant,
                                                                                            T>)
                                                                                        {
                                                                                            return {
                                                                                                std::forward<decltype(value)>(
                                                                                                    value)};
                                                                                        }
                                                                                        else
                                                                                        {
                                                                                            throw std::runtime_error(
                                                                                                "Can't convert type of variant to constant expression");
                                                                                        }
                                                                                    }, currToken.getValue())));
    }
    else if (currToken.getTokenType() == TokenType::OpenParenthese)
    {
        auto expression = parseExpression(tokens, context);
        if (tokens.back().getTokenType() != TokenType::CloseParenthese)
        {
            throw std::runtime_error("Expected Close Parenthese after expression in primary expression");
        }
        tokens.pop_back();
        return PrimaryExpression(line, column, PrimaryExpressionParenthese(line, column, std::move(expression)));
    }
    else
    {
        throw std::runtime_error("Invalid token for primary expression");
    }
}
