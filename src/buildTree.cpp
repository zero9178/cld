#include "Parser.hpp"

using Tokens = std::vector<OpenCL::Lexer::Token>;

namespace
{
    OpenCL::Parser::Program parseProgram(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::Global> parseGlobal(Tokens& tokens);

    OpenCL::Parser::GlobalDeclaration parseGlobalDeclaration(Tokens& tokens);

    OpenCL::Parser::Function parseFunction(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::BlockItem> parseBlockItem(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::Statement> parseStatement(Tokens& tokens);

    OpenCL::Parser::Expression parseExpression(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::NonCommaExpression> parseNonCommaExpression(Tokens& tokens);

    OpenCL::Parser::ConditionalExpression parseConditionalExpression(Tokens& tokens);

    OpenCL::Parser::LogicalOrExpression parseLogicalOrExpression(Tokens& tokens);

    OpenCL::Parser::LogicalAndExpression parseLogicalAndExpression(Tokens& tokens);

    OpenCL::Parser::BitOrExpression parseBitOrExpression(Tokens& tokens);

    OpenCL::Parser::BitXorExpression parseBitXorExpression(Tokens& tokens);

    OpenCL::Parser::BitAndExpression parseBitAndExpression(Tokens& tokens);

    OpenCL::Parser::EqualityExpression parseEqualityExpression(Tokens& tokens);

    OpenCL::Parser::RelationalExpression parseRelationalExpression(Tokens& tokens);

    OpenCL::Parser::ShiftExpression parseShiftExpression(Tokens& tokens);

    OpenCL::Parser::AdditiveExpression parseAdditiveExpression(Tokens& tokens);

    OpenCL::Parser::Term parseTerm(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::Factor> parseFactor(Tokens& tokens);

    OpenCL::Parser::UnaryFactor parseUnary(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::Type> parseType(Tokens& tokens);
}

OpenCL::Parser::Program OpenCL::Parser::buildTree(std::vector<OpenCL::Lexer::Token>&& tokens)
{
    return parseProgram(tokens);
}

namespace
{
    using namespace OpenCL::Lexer;
    using namespace OpenCL::Parser;

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

    bool isType(TokenType type)
    {
        return type == TokenType::VoidKeyword
            || type == TokenType::CharKeyword
            || type == TokenType::ShortKeyword
            || type == TokenType::IntKeyword
            || type == TokenType::LongKeyword
            || type == TokenType::FloatKeyword
            || type == TokenType::DoubleKeyword
            || type == TokenType::SignedKeyword
            || type == TokenType::UnsignedKeyword
            || type == TokenType::Asterisk;
    }

    OpenCL::Parser::Program parseProgram(Tokens& tokens)
    {
        std::vector<std::unique_ptr<Global>> global;
        while (!tokens.empty())
        {
            global.push_back(parseGlobal(tokens));
        }
        return Program(std::move(global));
    }

    std::unique_ptr<OpenCL::Parser::Global> parseGlobal(Tokens& tokens)
    {
        std::uint64_t pos = 1;
        auto currToken = tokens.back();
        if (!isType(currToken.getTokenType()))
        {
            throw std::runtime_error("Expected type for global declaration");
        }
        while (isType(currToken.getTokenType()))
        {
            pos++;
            currToken = tokens.at(tokens.size() - pos);
        }
        if (currToken.getTokenType() != TokenType::Identifier)
        {
            throw std::runtime_error("Expected Identifier after type");
        }
        currToken = tokens.at(tokens.size() - pos - 1);
        if (currToken.getTokenType() == TokenType::OpenParenthese)
        {
            return std::make_unique<Function>(parseFunction(tokens));
        }
        else if (currToken.getTokenType() == TokenType::SemiColon
            || currToken.getTokenType() == TokenType::Assignment)
        {
            return std::make_unique<GlobalDeclaration>(parseGlobalDeclaration(tokens));
        }
        else
        {
            throw std::runtime_error("Invalid token after global identifier");
        }
    }

    OpenCL::Parser::GlobalDeclaration parseGlobalDeclaration(Tokens& tokens)
    {
        auto currToken = tokens.back();
        if (!isType(currToken.getTokenType()))
        {
            throw std::runtime_error("Unsupported return type");
        }
        auto type = parseType(tokens);
        currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() != TokenType::Identifier)
        {
            throw std::runtime_error("Invalid identifier for Global declaration");
        }
        auto name = std::get<std::string>(currToken.getValue());
        currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() == TokenType::Assignment)
        {
            currToken = tokens.back();
            tokens.pop_back();
            if (currToken.getTokenType() != TokenType::Literal)
            {
                throw std::runtime_error("Can only use Integer literal to initialize global declaration");
            }
            auto variant = std::visit([](auto&& value) -> typename ConstantFactor::variant
                                      {
                                          if constexpr(std::is_constructible_v<typename ConstantFactor::variant, decltype(value)>)
                                          {
                                              return {value};
                                          }
                                          else
                                          {
                                              throw std::runtime_error("Invalid value in variant of literal");
                                          }
                                      }, currToken.getValue());
            currToken = tokens.back();
            if (currToken.getTokenType() != TokenType::SemiColon)
            {
                throw std::runtime_error("Expected ; after initialization of global declaration");
            }
            tokens.pop_back();
            return GlobalDeclaration(std::move(type), name, std::make_unique<ConstantFactor>(std::move(variant)));
        }
        else
        {
            tokens.pop_back();
            return GlobalDeclaration(std::move(type), name);
        }
    }

    OpenCL::Parser::Function parseFunction(Tokens& tokens)
    {
        auto currToken = tokens.back();
        if (!isType(currToken.getTokenType()))
        {
            throw std::runtime_error("Unsupported return type");
        }
        auto retType = parseType(tokens);
        currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() != TokenType::Identifier)
        {
            throw std::runtime_error("Invalid identifier for function");
        }
        auto name = std::get<std::string>(currToken.getValue());
        currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() != TokenType::OpenParenthese)
        {
            throw std::runtime_error("Expected Opening Parantheses after function identifier");
        }
        std::vector<std::pair<std::unique_ptr<Type>, std::string>> arguments;
        currToken = tokens.back();
        if (currToken.getTokenType() != TokenType::CloseParenthese)
        {
            while (true)
            {
                currToken = tokens.back();
                if (!isType(currToken.getTokenType()))
                {
                    throw std::runtime_error("Unsupported argument type");
                }
                auto type = parseType(tokens);
                currToken = tokens.back();
                if (currToken.getTokenType() == TokenType::Identifier)
                {
                    arguments.emplace_back(std::move(type), std::get<std::string>(currToken.getValue()));
                    tokens.pop_back();
                }
                else if (currToken.getTokenType() == TokenType::CloseParenthese
                    || currToken.getTokenType() == TokenType::Comma)
                {
                    arguments.emplace_back(std::move(type), "");
                }
                else
                {
                    throw std::runtime_error("Expected identifier,Close Parentheses or Comma after parameter type");
                }
                currToken = tokens.back();
                if (currToken.getTokenType() == TokenType::CloseParenthese)
                {
                    break;
                }
                else if (currToken.getTokenType() != TokenType::Comma)
                {
                    throw std::runtime_error("Expected Comma between arguments");
                }
                else
                {
                    tokens.pop_back();
                }
            }
        }
        currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() != TokenType::CloseParenthese)
        {
            throw std::runtime_error("Expected Close Parantheses after Argument List");
        }

        currToken = tokens.back();
        if(currToken.getTokenType() == TokenType::OpenBrace)
        {
            auto statement = parseStatement(tokens);
            auto pointer = dynamic_cast<BlockStatement*>(statement.get());
            if (!pointer)
            {
                throw std::runtime_error("Expected Block statement after function");
            }

            return Function(std::move(retType), std::move(name), std::move(arguments),std::make_unique<BlockStatement>(std::move(*pointer)));
        }
        else if(currToken.getTokenType() == TokenType::SemiColon)
        {
            tokens.pop_back();
            return Function(std::move(retType),std::move(name),std::move(arguments));
        }
        else
        {
            throw std::runtime_error("Expected closing Brace or Semicolon after function declaration");
        }
    }

    std::unique_ptr<Type> parseType(Tokens& tokens)
    {
        Tokens typeTokens;
        {
            auto currToken = tokens.back();
            do
            {
                tokens.pop_back();
                typeTokens.push_back(currToken);
                if(tokens.empty())
                {
                    break;
                }
                currToken = tokens.back();
            }
            while(isType(currToken.getTokenType()));
        }

        if(typeTokens.back().getTokenType() == TokenType::Asterisk)
        {
            typeTokens.pop_back();
            std::reverse(typeTokens.begin(),typeTokens.end());
            return std::make_unique<PointerType>(parseType(typeTokens));
        }
        else
        {
            std::vector<PrimitiveType::Types> types;
            for(auto& currToken : typeTokens)
            {
                switch(currToken.getTokenType())
                {
                case TokenType::VoidKeyword:return std::make_unique<PrimitiveType>(std::move(types));
                case TokenType::CharKeyword:
                    types.push_back(PrimitiveType::Types::Char);
                    break;
                case TokenType::ShortKeyword:
                    types.push_back(PrimitiveType::Types::Short);
                    break;
                case TokenType::IntKeyword:
                    types.push_back(PrimitiveType::Types::Int);
                    break;
                case TokenType::LongKeyword:
                    types.push_back(PrimitiveType::Types::Long);
                    break;
                case TokenType::FloatKeyword:
                    types.push_back(PrimitiveType::Types::Float);
                    break;
                case TokenType::DoubleKeyword:
                    types.push_back(PrimitiveType::Types::Double);
                    break;
                case TokenType::SignedKeyword:
                    types.push_back(PrimitiveType::Types::Signed);
                    break;
                case TokenType::UnsignedKeyword:
                    types.push_back(PrimitiveType::Types::Unsigned);
                    break;
                default:throw std::runtime_error("Invalid token");
                }
            }
            return std::make_unique<PrimitiveType>(std::move(types));
        }
    }

    std::unique_ptr<OpenCL::Parser::BlockItem> parseBlockItem(Tokens& tokens)
    {
        auto currToken = tokens.back();
        if (isType(currToken.getTokenType()) && currToken.getTokenType() != TokenType::Asterisk)
        {
            auto type = parseType(tokens);
            currToken = tokens.back();
            if (currToken.getTokenType() != TokenType::Identifier)
            {
                throw std::runtime_error("Expected Identifier after variable declaration");
            }
            tokens.pop_back();
            auto name = std::get<std::string>(currToken.getValue());
            auto result = [&tokens, name = std::move(name), type = std::move(type)]()mutable
            {
                auto currToken = tokens.back().getTokenType();
                if (currToken == TokenType::Assignment)
                {
                    tokens.pop_back();
                    return Declaration(std::move(type),
                                       name,
                                       std::make_unique<Expression>(parseExpression(tokens)));
                }
                else
                {
                    return Declaration(std::move(type), name);
                }
            }();
            if (tokens.empty() || tokens.back().getTokenType() != TokenType::SemiColon)
            {
                throw std::runtime_error("Declaration not terminated with ;");
            }
            else
            {
                tokens.pop_back();
            }
            return std::make_unique<Declaration>(std::move(result));
        }
        else
        {
            return parseStatement(tokens);
        }
    }

    std::unique_ptr<OpenCL::Parser::Statement> parseStatement(Tokens& tokens)
    {
        auto result = [&tokens]() -> std::unique_ptr<Statement>
        {
            auto curentToken = tokens.back();
            switch (curentToken.getTokenType())
            {
            case TokenType::ReturnKeyword:
            {
                tokens.pop_back();
                return std::make_unique<ReturnStatement>(parseExpression(tokens));
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
                auto expression = parseExpression(tokens);
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::CloseParenthese)
                {
                    throw std::runtime_error("Expected ) at the end of if statement");
                }
                auto statement = parseStatement(tokens);
                curentToken = tokens.back();
                if (!tokens.empty() && curentToken.getTokenType() == TokenType::ElseKeyword)
                {
                    tokens.pop_back();
                    return std::make_unique<IfStatement>(std::move(expression),
                                                         std::move(statement),
                                                         parseStatement(tokens));
                }
                else
                {
                    return std::make_unique<IfStatement>(std::move(expression), std::move(statement));
                }
            }
            case TokenType::OpenBrace:
            {
                tokens.pop_back();
                std::vector<std::unique_ptr<BlockItem>> blockItems;
                while (!tokens.empty() && tokens.back().getTokenType() != TokenType::CloseBrace)
                {
                    blockItems.push_back(parseBlockItem(tokens));
                }
                if (tokens.empty() || tokens.back().getTokenType() != TokenType::CloseBrace)
                {
                    throw std::runtime_error("Expected } to close Block");
                }
                tokens.pop_back();
                return std::make_unique<BlockStatement>(std::move(blockItems));
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
                auto blockitem = parseBlockItem(tokens);

                auto control = [&]() -> std::unique_ptr<Expression>
                {
                    if (dynamic_cast<Declaration*>(blockitem.get())
                        || tokens.back().getTokenType() != TokenType::SemiColon)
                    {
                        auto expression = parseExpression(tokens);
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
                        auto expression = parseExpression(tokens);
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

                auto statement = parseStatement(tokens);

                if (auto declaration = dynamic_cast<Declaration*>(blockitem.get());declaration)
                {
                    return std::make_unique<ForDeclarationStatement>(std::move(statement), std::move(*declaration),
                                                                     std::move(control),
                                                                     std::move(post));
                }
                else if (auto
                        expressionStatement = dynamic_cast<ExpressionStatement*>(blockitem.get());expressionStatement)
                {
                    return std::make_unique<ForStatement>(std::move(statement),
                                                          expressionStatement->moveOptionalExpression(),
                                                          std::move(control),
                                                          std::move(post));
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
                auto expression = parseExpression(tokens);
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::CloseParenthese)
                {
                    throw std::runtime_error("Expected ) after expression in while");
                }
                auto statement = parseStatement(tokens);
                return std::make_unique<HeadWhileStatement>(std::move(expression), std::move(statement));
            }
            case TokenType::DoKeyword:
            {
                tokens.pop_back();
                auto statement = parseStatement(tokens);
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
                auto expression = parseExpression(tokens);
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::CloseParenthese)
                {
                    throw std::runtime_error("Expected ) after expression in while");
                }
                return std::make_unique<FootWhileStatement>(std::move(statement), std::move(expression));
            }
            case TokenType::BreakKeyword:
            {
                tokens.pop_back();
                return std::make_unique<BreakStatement>();
            }
            case TokenType::ContinueKeyword:
            {
                tokens.pop_back();
                return std::make_unique<ContinueStatement>();
            }
            default:
            {
                if (!tokens.empty() && tokens.back().getTokenType() != TokenType::SemiColon)
                {
                    return std::make_unique<ExpressionStatement>(std::make_unique<Expression>(parseExpression(tokens)));
                }
                else
                {
                    return std::make_unique<ExpressionStatement>();
                }
            }
            }
        }();

        if ((dynamic_cast<ExpressionStatement*>(result.get())
            || dynamic_cast<ReturnStatement*>(result.get())
            || dynamic_cast<FootWhileStatement*>(result.get())
            || dynamic_cast<BreakStatement*>(result.get())
            || dynamic_cast<ContinueStatement*>(result.get()))
            && (tokens.empty() || tokens.back().getTokenType() != TokenType::SemiColon))
        {
            throw std::runtime_error("Statement not terminated with ;");
        }
        else if (dynamic_cast<ExpressionStatement*>(result.get())
            || dynamic_cast<ReturnStatement*>(result.get())
            || dynamic_cast<FootWhileStatement*>(result.get())
            || dynamic_cast<BreakStatement*>(result.get())
            || dynamic_cast<ContinueStatement*>(result.get()))
        {
            tokens.pop_back();
        }
        return result;
    }

    OpenCL::Parser::Expression parseExpression(Tokens& tokens)
    {
        auto expression = parseNonCommaExpression(tokens);

        std::unique_ptr<NonCommaExpression> optional;
        auto currToken = tokens.back();
        if (currToken.getTokenType() == TokenType::Comma)
        {
            tokens.pop_back();
            optional = parseNonCommaExpression(tokens);
        }
        return Expression(std::move(expression), std::move(optional));
    }

    OpenCL::Parser::AssignmentExpression parseAssignment(Tokens& tokens);

    std::unique_ptr<OpenCL::Parser::NonCommaExpression> parseNonCommaExpression(Tokens& tokens)
    {
        auto result = std::find_if(tokens.rbegin(),tokens.rend(),[](const Token& token)
        {
            return token.getTokenType() == TokenType::SemiColon || isAssignment(token.getTokenType());
        });
        if(result == tokens.rend())
        {
            throw std::runtime_error("Unexpected end of tokens");
        }
        bool assignment = result->getTokenType() != TokenType::SemiColon;
        if (assignment)
        {
            return std::make_unique<AssignmentExpression>(parseAssignment(tokens));
        }
        else
        {
            return std::make_unique<ConditionalExpression>(parseConditionalExpression(tokens));
        }
    }

    OpenCL::Parser::AssignmentExpression parseAssignment(Tokens& tokens)
    {
        auto unary = parseUnary(tokens);
        auto currentToken = tokens.back();
        tokens.pop_back();
        auto assignment = parseNonCommaExpression(tokens);
        return AssignmentExpression(std::move(unary),
                                                          [assignment = currentToken.getTokenType()]
                                                          {
                                                              switch (assignment)
                                                              {
                                                              case TokenType::Assignment:return AssignmentExpression::AssignOperator::NoOperator;
                                                              case TokenType::PlusAssign:return AssignmentExpression::AssignOperator::PlusAssign;
                                                              case TokenType::MinusAssign:return AssignmentExpression::AssignOperator::MinusAssign;
                                                              case TokenType::DivideAssign:return AssignmentExpression::AssignOperator::DivideAssign;
                                                              case TokenType::MultiplyAssign:return AssignmentExpression::AssignOperator::MultiplyAssign;
                                                              case TokenType::ModuloAssign:return AssignmentExpression::AssignOperator::ModuloAssign;
                                                              case TokenType::ShiftLeftAssign:return AssignmentExpression::AssignOperator::LeftShiftAssign;
                                                              case TokenType::ShiftRightAssign:return AssignmentExpression::AssignOperator::RightShiftAssign;
                                                              case TokenType::BitAndAssign:return AssignmentExpression::AssignOperator::BitAndAssign;
                                                              case TokenType::BitOrAssign:return AssignmentExpression::AssignOperator::BitOrAssign;
                                                              case TokenType::BitXorAssign:return AssignmentExpression::AssignOperator::BitXorAssign;
                                                              default:
                                                                  throw std::runtime_error(
                                                                      "Invalid token for assignment");
                                                              }
                                                          }(),std::move(assignment));
    }

    OpenCL::Parser::ConditionalExpression parseConditionalExpression(Tokens& tokens)
    {
        auto logicalOrExperssion = parseLogicalOrExpression(tokens);
        auto currToken = tokens.back();
        if (currToken.getTokenType() == TokenType::QuestionMark)
        {
            tokens.pop_back();
            auto optionalExpression = parseExpression(tokens);
            currToken = tokens.back();
            if (currToken.getTokenType() != TokenType::Colon)
            {
                throw std::runtime_error("Expected : to match ?");
            }
            tokens.pop_back();
            auto optionalConditional = parseConditionalExpression(tokens);
            return ConditionalExpression(std::move(logicalOrExperssion),
                                         std::make_unique<Expression>(std::move(optionalExpression)),
                                         std::make_unique<ConditionalExpression>(std::move(optionalConditional)));
        }
        return ConditionalExpression(std::move(logicalOrExperssion));
    }

    OpenCL::Parser::LogicalOrExpression parseLogicalOrExpression(Tokens& tokens)
    {
        auto logicalAnd = parseLogicalAndExpression(tokens);

        std::vector<LogicalAndExpression> optionalLogicalAnds;
        auto curentToken = tokens.back();
        while (curentToken.getTokenType() == TokenType::LogicOr)
        {
            tokens.pop_back();
            optionalLogicalAnds.push_back(parseLogicalAndExpression(tokens));
            curentToken = tokens.back();
        }

        return LogicalOrExpression(std::move(logicalAnd), std::move(optionalLogicalAnds));
    }

    OpenCL::Parser::LogicalAndExpression parseLogicalAndExpression(Tokens& tokens)
    {
        auto result = parseBitOrExpression(tokens);

        std::vector<BitOrExpression> list;
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::LogicAnd)
        {
            tokens.pop_back();
            list.push_back(parseBitOrExpression(tokens));
            currToken = tokens.back();
        }

        return LogicalAndExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::BitOrExpression parseBitOrExpression(Tokens& tokens)
    {
        auto result = parseBitXorExpression(tokens);

        std::vector<BitXorExpression> list;
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::BitOr)
        {
            tokens.pop_back();
            list.push_back(parseBitXorExpression(tokens));
            currToken = tokens.back();
        }

        return BitOrExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::BitXorExpression parseBitXorExpression(Tokens& tokens)
    {
        auto result = parseBitAndExpression(tokens);

        std::vector<BitAndExpression> list;
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::BitXor)
        {
            tokens.pop_back();
            list.push_back(parseBitAndExpression(tokens));
            currToken = tokens.back();
        }

        return BitXorExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::BitAndExpression parseBitAndExpression(Tokens& tokens)
    {
        auto result = parseEqualityExpression(tokens);

        std::vector<EqualityExpression> list;
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::Ampersand)
        {
            tokens.pop_back();
            list.push_back(parseEqualityExpression(tokens));
            currToken = tokens.back();
        }

        return BitAndExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::EqualityExpression parseEqualityExpression(Tokens& tokens)
    {
        auto result = parseRelationalExpression(tokens);

        std::vector<std::pair<EqualityExpression::EqualityOperator, RelationalExpression>> relationalExpressions;
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::Equal || currToken.getTokenType() == TokenType::NotEqual)
        {
            tokens.pop_back();
            relationalExpressions
                .emplace_back(currToken.getTokenType() == TokenType::Equal ? EqualityExpression::EqualityOperator::Equal
                                                                           : EqualityExpression::EqualityOperator::NotEqual,
                              parseRelationalExpression(tokens));
            currToken = tokens.back();
        }

        return EqualityExpression(std::move(result), std::move(relationalExpressions));
    }

    OpenCL::Parser::RelationalExpression parseRelationalExpression(Tokens& tokens)
    {
        auto result = parseShiftExpression(tokens);

        std::vector<std::pair<RelationalExpression::RelationalOperator, ShiftExpression>> list;
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
                              }(), parseShiftExpression(tokens));
            currToken = tokens.back();
        }

        return RelationalExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::ShiftExpression parseShiftExpression(Tokens& tokens)
    {
        auto result = parseAdditiveExpression(tokens);

        std::vector<std::pair<ShiftExpression::ShiftOperator, AdditiveExpression>> list;
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::ShiftRight || currToken.getTokenType() == TokenType::ShiftLeft)
        {
            tokens.pop_back();
            list.emplace_back(currToken.getTokenType() == TokenType::ShiftRight ? ShiftExpression::ShiftOperator::Right
                                                                                : ShiftExpression::ShiftOperator::Left,
                              parseAdditiveExpression(tokens));
            currToken = tokens.back();
        }

        return ShiftExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::AdditiveExpression parseAdditiveExpression(Tokens& tokens)
    {
        auto result = parseTerm(tokens);

        std::vector<std::pair<AdditiveExpression::BinaryDashOperator, Term>> list;
        auto currToken = tokens.back();
        while (currToken.getTokenType() == TokenType::Addition
            || currToken.getTokenType() == TokenType::Negation)
        {
            tokens.pop_back();
            list.emplace_back(
                currToken.getTokenType() == TokenType::Addition ? AdditiveExpression::BinaryDashOperator::BinaryPlus
                                                                : AdditiveExpression::BinaryDashOperator::BinaryMinus,
                parseTerm(tokens));
            currToken = tokens.back();
        }

        return AdditiveExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::Term parseTerm(Tokens& tokens)
    {
        auto result = parseFactor(tokens);

        std::vector<std::pair<Term::BinaryDotOperator, std::unique_ptr<Factor>>> list;
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
                              }(), parseFactor(tokens));
            currToken = tokens.back();
        }

        return Term(std::move(result), std::move(list));
    }

    std::unique_ptr<OpenCL::Parser::Factor> parseFactor(Tokens& tokens)
    {
        auto currToken = tokens.back();
        tokens.pop_back();
        auto result = [&]() -> std::unique_ptr<Factor>
        {
            switch (currToken.getTokenType())
            {
            case TokenType::OpenParenthese:
            {
                currToken = tokens.back();
                if(isType(currToken.getTokenType()))
                {
                    auto type = parseType(tokens);
                    currToken = tokens.back();
                    tokens.pop_back();
                    if(currToken.getTokenType() != TokenType::CloseParenthese)
                    {
                        throw std::runtime_error("Expected Closing Parenthese after type cast");
                    }
                    return std::make_unique<CastFactor>(std::move(type),parseExpression(tokens));
                }
                else
                {
                    return std::make_unique<ParentheseFactor>(parseExpression(tokens));
                }
            }
            case TokenType::Ampersand:
            case TokenType::Asterisk:
            case TokenType::Negation:
            case TokenType::BitWiseNegation:
            case TokenType::LogicalNegation:
                return std::make_unique<UnaryFactor>(parseUnary(tokens));
            case TokenType::Increment:
            {
                auto nextToken = tokens.back();
                if (nextToken.getTokenType() != TokenType::Identifier)
                {
                    throw std::runtime_error("Increment can only be applied to a variable");
                }
                tokens.pop_back();
                return std::make_unique<PreIncrement>(std::get<std::string>(nextToken.getValue()));
            }
            case TokenType::Decrement:
            {
                auto nextToken = tokens.back();
                if (nextToken.getTokenType() != TokenType::Identifier)
                {
                    throw std::runtime_error("Increment can only be applied to a variable");
                }
                tokens.pop_back();
                return std::make_unique<PreDecrement>(std::get<std::string>(nextToken.getValue()));
            }
            case TokenType::Identifier:
            {
                auto nextToken = tokens.back();
                switch (nextToken.getTokenType())
                {
                case TokenType::Increment:
                {
                    tokens.pop_back();
                    return std::make_unique<PostIncrement>(std::get<std::string>(currToken.getValue()));
                }
                case TokenType::Decrement:
                {
                    tokens.pop_back();
                    return std::make_unique<PostDecrement>(std::get<std::string>(currToken.getValue()));
                }
                case TokenType::OpenParenthese:
                {
                    tokens.pop_back();
                    auto name = std::get<std::string>(currToken.getValue());
                    std::vector<std::unique_ptr<NonCommaExpression>> result;
                    if (tokens.back().getTokenType() != TokenType::CloseParenthese)
                    {
                        while (true)
                        {
                            result.push_back(parseNonCommaExpression(tokens));
                            currToken = tokens.back();
                            if (currToken.getTokenType() == TokenType::CloseParenthese)
                            {
                                break;
                            }
                            else if (currToken.getTokenType() != TokenType::Comma)
                            {
                                throw std::runtime_error("Expected Comma between function arguments");
                            }
                            else
                            {
                                tokens.pop_back();
                            }
                        }
                    }
                    currToken = tokens.back();
                    tokens.pop_back();
                    if (currToken.getTokenType() != TokenType::CloseParenthese)
                    {
                        throw std::runtime_error("Expected ) at the end of function call");
                    }
                    return std::make_unique<FunctionCall>(name, std::move(result));
                }
                default:
                {
                    return std::make_unique<VariableFactor>(std::get<std::string>(currToken.getValue()));
                }
                }
            }
            case TokenType::Literal:
            {
                return std::make_unique<ConstantFactor>(std::visit([](auto&& value) -> typename ConstantFactor::variant
                                                                   {
                    if constexpr(std::is_constructible_v<typename ConstantFactor::variant, decltype(value)>)
                    {
                        return {value};
                    }
                    else
                    {
                        throw std::runtime_error("Invalid value in variant of literal");
                    }
                                                                   }, currToken.getValue()));
            }
            default:throw std::runtime_error("Invalid token");
            }
        }();

        if (dynamic_cast<ParentheseFactor*>(result.get()))
        {
            if (tokens.empty() || tokens.back().getTokenType() != TokenType::CloseParenthese)
            {
                throw std::runtime_error("Expected )");
            }
            tokens.pop_back();
        }

        return result;
    }

    OpenCL::Parser::UnaryFactor parseUnary(Tokens& tokens)
    {
        switch(tokens.back().getTokenType())
        {
        case TokenType::Ampersand:
            return UnaryFactor(UnaryFactor::UnaryOperator::UnaryAddressOf,parseFactor(tokens));
        case TokenType::Asterisk:
            return UnaryFactor(UnaryFactor::UnaryOperator::UnaryDereference,parseFactor(tokens));
        case TokenType::Negation:
            return UnaryFactor(UnaryFactor::UnaryOperator::UnaryNegation,parseFactor(tokens));
        case TokenType::BitWiseNegation:
            return UnaryFactor(UnaryFactor::UnaryOperator::UnaryBitWiseNegation,parseFactor(tokens));
        case TokenType::LogicalNegation:
            return UnaryFactor(UnaryFactor::UnaryOperator::UnaryLogicalNegation,parseFactor(tokens));
        default:break;
        }
    }
}
