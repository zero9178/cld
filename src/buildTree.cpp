#include "Parser.hpp"

#include <stack>
#include <set>

using Tokens = std::vector<OpenCL::Lexer::Token>;

namespace
{
    class ParsingContext final
    {
        std::vector<std::set<std::string>> m_currentScope;

    public:

        ParsingContext()
        {
            m_currentScope.emplace_back();
        }

        ~ParsingContext() = default;

        ParsingContext(const ParsingContext&) = delete;
        ParsingContext(ParsingContext&&) = delete;
        ParsingContext&operator=(const ParsingContext&) = delete;
        ParsingContext&operator=(ParsingContext&&) = delete;

        void addToScope(std::string name)
        {
            if(!m_currentScope.back().insert(std::move(name)).second)
            {
                throw std::runtime_error(name + " already exists in this scope");
            }
        }

        bool isInScope(const std::string& name)
        {
            for(auto& iter : m_currentScope)
            {
                if(iter.count(name))
                {
                    return true;
                }
            }
            return false;
        }

        void pushScope()
        {
            m_currentScope.emplace_back();
        }

        void popScope()
        {
            m_currentScope.pop_back();
        }
    };

    OpenCL::Parser::Program parseProgram(Tokens& tokens, ParsingContext& context);

    std::unique_ptr<OpenCL::Parser::Global> parseGlobal(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::StructOrUnionDeclaration parseStructOrUnion(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::TypedefDeclaration parseTypedefDeclaration(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::GlobalDeclaration parseGlobalDeclaration(std::unique_ptr<OpenCL::Parser::Type>&& type,
                                                                 Tokens& tokens,
                                                                 ParsingContext& context);

    OpenCL::Parser::Function parseFunction(std::unique_ptr<OpenCL::Parser::Type>&& rettype,
                                               Tokens& tokens,
                                               ParsingContext& context);

    std::unique_ptr<OpenCL::Parser::BlockItem> parseBlockItem(Tokens& tokens, ParsingContext& context);

    std::unique_ptr<OpenCL::Parser::Statement> parseStatement(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::Expression parseExpression(Tokens& tokens, ParsingContext& context);

    std::unique_ptr<OpenCL::Parser::NonCommaExpression> parseNonCommaExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::ConditionalExpression parseConditionalExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::LogicalOrExpression parseLogicalOrExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::LogicalAndExpression parseLogicalAndExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::BitOrExpression parseBitOrExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::BitXorExpression parseBitXorExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::BitAndExpression parseBitAndExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::EqualityExpression parseEqualityExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::RelationalExpression parseRelationalExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::ShiftExpression parseShiftExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::AdditiveExpression parseAdditiveExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::Term parseTerm(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::CastExpression parseCastExpression(Tokens& tokens, ParsingContext& context);

    std::unique_ptr<OpenCL::Parser::UnaryExpression> parseUnaryExpression(Tokens& tokens, ParsingContext& context);

    std::unique_ptr<OpenCL::Parser::PostFixExpression> parsePostFixExpression(Tokens& tokens, ParsingContext& context);

    std::unique_ptr<OpenCL::Parser::PrimaryExpression> parsePrimaryExpression(Tokens& tokens, ParsingContext& context);

    std::unique_ptr<OpenCL::Parser::Type> parseType(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::PrimitiveType parsePrimitiveType(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::StructType parseStructType(Tokens& tokens, ParsingContext& context);

    OpenCL::Parser::UnionType parseUnionType(Tokens& tokens, ParsingContext& context);
}

OpenCL::Parser::Program OpenCL::Parser::buildTree(std::vector<OpenCL::Lexer::Token>&& tokens)
{
    ParsingContext context;
    return parseProgram(tokens, context);
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

    bool isType(Token type,ParsingContext& context)
    {
        return type.getTokenType() == TokenType::VoidKeyword
            || type.getTokenType() == TokenType::CharKeyword
            || type.getTokenType() == TokenType::ShortKeyword
            || type.getTokenType() == TokenType::IntKeyword
            || type.getTokenType() == TokenType::LongKeyword
            || type.getTokenType() == TokenType::FloatKeyword
            || type.getTokenType() == TokenType::DoubleKeyword
            || type.getTokenType() == TokenType::SignedKeyword
            || type.getTokenType() == TokenType::UnsignedKeyword
            || type.getTokenType() == TokenType::StructKeyword
            || type.getTokenType() == TokenType::ConstKeyword
            || type.getTokenType() == TokenType::UnionKeyword
            || (type.getTokenType() == TokenType::Identifier && !context.isInScope(std::get<std::string>(type.getValue())));
    }

    OpenCL::Parser::Program parseProgram(Tokens& tokens, ParsingContext& context)
    {
        std::vector<std::unique_ptr<Global>> global;
        while (!tokens.empty())
        {
            global.push_back(parseGlobal(tokens, context));
        }
        return Program(std::move(global));
    }

    std::unique_ptr<OpenCL::Parser::Global> parseGlobal(Tokens& tokens, ParsingContext& context)
    {
        if ((tokens.back().getTokenType() == TokenType::StructKeyword
        || tokens.back().getTokenType() == TokenType::UnionKeyword) && tokens.size() > 2
            && tokens.at(tokens.size() - 3).getTokenType() == TokenType::OpenBrace)
        {
            auto structOrUnion = parseStructOrUnion(tokens, context);
            if(tokens.back().getTokenType() != TokenType::SemiColon)
            {
                throw std::runtime_error("Expected ; after struct or union declaration");
            }
            return std::make_unique<StructOrUnionDeclaration>(std::move(structOrUnion));
        }
        else if(tokens.back().getTokenType() == TokenType::TypedefKeyword)
        {
            return std::make_unique<TypedefDeclaration>(parseTypedefDeclaration(tokens,context));
        }
        auto currToken = tokens.back();
        if (!isType(currToken,context))
        {
            throw std::runtime_error("Expected type for global declaration");
        }
        auto type = parseType(tokens,context);
        currToken = tokens.back();
        if (currToken.getTokenType() != TokenType::Identifier)
        {
            throw std::runtime_error("Expected Identifier after type");
        }
        if (tokens.size() == 1)
        {
            throw std::runtime_error("Unexpected end of tokens");
        }
        currToken = tokens.at(tokens.size() - 2);
        if (currToken.getTokenType() == TokenType::OpenParenthese)
        {
            return std::make_unique<Function>(parseFunction(std::move(type), tokens, context));
        }
        else if (currToken.getTokenType() == TokenType::SemiColon
            || currToken.getTokenType() == TokenType::Assignment)
        {
            return std::make_unique<GlobalDeclaration>(parseGlobalDeclaration(std::move(type), tokens, context));
        }
        else
        {
            throw std::runtime_error("Invalid token after global identifier");
        }
    }

    OpenCL::Parser::StructOrUnionDeclaration parseStructOrUnion(Tokens& tokens, ParsingContext& context)
    {
        auto currToken = tokens.back();
        if (currToken.getTokenType() != TokenType::StructKeyword
         && currToken.getTokenType() != TokenType::UnionKeyword)
        {
            throw std::runtime_error("Expected struct or union keyword");
        }
        bool isUnion = currToken.getTokenType() == TokenType::UnionKeyword;
        tokens.pop_back();
        currToken = tokens.back();
        if (currToken.getTokenType() != TokenType::Identifier)
        {
            throw std::runtime_error("Expected Identifier following Sturct");
        }
        std::string name = std::get<std::string>(currToken.getValue());
        tokens.pop_back();
        currToken = tokens.back();
        if (currToken.getTokenType() != TokenType::OpenBrace)
        {
            throw std::runtime_error("Expected { after struct definition");
        }
        tokens.pop_back();
        std::vector<std::pair<std::shared_ptr<Type>, std::string>> fields;
        while (!tokens.empty() && tokens.back().getTokenType() != TokenType::CloseBrace)
        {
            auto type = parseType(tokens, context);
            currToken = tokens.back();
            if (currToken.getTokenType() != TokenType::Identifier)
            {
                throw std::runtime_error("Expected identifier after type in field definition");
            }
            while(true)
            {
                std::string fieldName = std::get<std::string>(currToken.getValue());
                tokens.pop_back();
                std::vector<std::size_t> sizes;
                while (tokens.back().getTokenType() == TokenType::OpenSquareBracket)
                {
                    tokens.pop_back();
                    auto primaryConstant = parsePrimaryExpression(tokens, context);
                    auto* result = dynamic_cast<const PrimaryExpressionConstant*>(primaryConstant.get());
                    if (!result)
                    {
                        throw std::runtime_error("Expected primary constant expression for array size");
                    }
                    sizes.push_back(std::visit([](auto&& value) -> std::size_t
                                               {
                                                   using T = std::decay_t<decltype(value)>;
                                                   if constexpr(std::is_integral_v<T>)
                                                   {
                                                       return value;
                                                   }
                                                   else
                                                   {
                                                       throw std::runtime_error(
                                                           "Only integral type supported for array size");
                                                   }
                                               }, result->getValue()));
                    if (tokens.back().getTokenType() != TokenType::CloseSquareBracket)
                    {
                        throw std::runtime_error("Expected ] after array declaration");
                    }
                    tokens.pop_back();
                }
                auto clone = type->clone();
                std::for_each(sizes.rbegin(), sizes.rend(), [&clone](std::size_t value)
                {
                    clone = std::make_unique<ArrayType>(std::move(clone), value);
                });
                fields.emplace_back(std::move(clone), fieldName);
                if(tokens.back().getTokenType() != TokenType::Comma)
                {
                    break;
                }
                tokens.pop_back();
                currToken = tokens.back();
            }
            if (tokens.back().getTokenType() != TokenType::SemiColon)
            {
                throw std::runtime_error("Expected ; at the end of field definition");
            }
            tokens.pop_back();
        }
        tokens.pop_back();
        return StructOrUnionDeclaration(isUnion, std::move(name), std::move(fields));
    }

    OpenCL::Parser::GlobalDeclaration parseGlobalDeclaration(std::unique_ptr<OpenCL::Parser::Type>&& type,
                                                                 Tokens& tokens,
                                                                 ParsingContext& context)
    {
        auto currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() != TokenType::Identifier)
        {
            throw std::runtime_error("Invalid identifier for Global declaration");
        }
        auto name = std::get<std::string>(currToken.getValue());
        context.addToScope(name);
        currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() == TokenType::Assignment)
        {
            currToken = tokens.back();
            tokens.pop_back();
            if (currToken.getTokenType() != TokenType::Literal)
            {
                throw std::runtime_error("Can only use Constant expression to initialize global declaration");
            }
            auto variant = std::visit([](auto&& value) -> typename PrimaryExpressionConstant::variant
                                      {
                                          if constexpr(std::is_constructible_v<typename PrimaryExpressionConstant::variant,
                                                                               decltype(value)>)
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
            return GlobalDeclaration(std::move(type),
                                     name,
                                     std::make_unique<PrimaryExpressionConstant>(std::move(variant)));
        }
        else
        {
            return GlobalDeclaration(std::move(type), name);
        }
    }

    OpenCL::Parser::TypedefDeclaration parseTypedefDeclaration(Tokens& tokens, ParsingContext& context)
    {
        if(tokens.back().getTokenType() != TokenType::TypedefKeyword)
        {
            throw std::runtime_error("Expected typedef keyword at beginning of typedef declaration");
        }
        tokens.pop_back();
        std::unique_ptr<StructOrUnionDeclaration> optionalDeclaration;
        std::unique_ptr<Type> type;
        if(tokens.back().getTokenType() == TokenType::StructKeyword
        || tokens.back().getTokenType() == TokenType::UnionKeyword)
        {
            optionalDeclaration = std::make_unique<StructOrUnionDeclaration>(parseStructOrUnion(tokens,context));
            if(optionalDeclaration->isUnion())
            {
                type = std::make_unique<UnionType>(optionalDeclaration->getName(),false);
            }
            else
            {
                type = std::make_unique<StructType>(optionalDeclaration->getName(),false);
            }
        }
        else
        {
            type = parseType(tokens, context);
        }

        std::vector<std::pair<std::shared_ptr<Type>,std::string>> typedefs;
        while(!tokens.empty())
        {
            bool isPointer = false;
            if(tokens.back().getTokenType() == TokenType::Asterisk)
            {
                isPointer = true;
                tokens.pop_back();
            }
            bool isConst = false;
            if(tokens.back().getTokenType() == TokenType::ConstKeyword)
            {
                isConst = true;
                tokens.pop_back();
            }
            if(tokens.back().getTokenType() != TokenType::Identifier)
            {
                throw std::runtime_error("Expected identifier following type in typedef declaration");
            }
            std::string name = std::get<std::string>(tokens.back().getValue());
            tokens.pop_back();
            std::vector<std::size_t> sizes;
            while (tokens.back().getTokenType() == TokenType::OpenSquareBracket)
            {
                tokens.pop_back();
                auto primaryConstant = parsePrimaryExpression(tokens, context);
                auto* result = dynamic_cast<const PrimaryExpressionConstant*>(primaryConstant.get());
                if (!result)
                {
                    throw std::runtime_error("Expected primary constant expression for array size");
                }
                sizes.push_back(std::visit([](auto&& value) -> std::size_t
                                           {
                                               using T = std::decay_t<decltype(value)>;
                                               if constexpr(std::is_integral_v<T>)
                                               {
                                                   return value;
                                               }
                                               else
                                               {
                                                   throw std::runtime_error(
                                                       "Only integral type supported for array size");
                                               }
                                           }, result->getValue()));
                if (tokens.back().getTokenType() != TokenType::CloseSquareBracket)
                {
                    throw std::runtime_error("Expected ] after array declaration");
                }
                tokens.pop_back();
            }
            auto clone = type->clone();
            if(isConst && !isPointer)
            {
                if(dynamic_cast<PrimitiveType*>(clone.get()))
                {
                    auto* prim = static_cast<PrimitiveType*>(clone.get());
                    clone = std::make_unique<PrimitiveType>(prim->getBitCount(),true,prim->isFloatingPoint(),prim->isSigned());
                }
                else if(dynamic_cast<PointerType*>(clone.get()))
                {
                    auto* pointer = static_cast<PointerType*>(clone.get());
                    clone = std::make_unique<PointerType>(pointer->getType().clone(),true);
                }
                else
                {
                    throw std::runtime_error("Not implemented yet");
                }
            }
            if (isPointer)
            {
                clone = std::make_unique<PointerType>(std::move(clone),isConst);
            }
            std::for_each(sizes.rbegin(), sizes.rend(), [&clone](std::size_t value)
            {
                clone = std::make_unique<ArrayType>(std::move(clone), value);
            });
            typedefs.emplace_back(std::move(clone),std::move(name));
            if(tokens.back().getTokenType() == TokenType::Comma)
            {
                tokens.pop_back();
            }
            else
            {
                break;
            }
        }
        if(tokens.back().getTokenType() != TokenType::SemiColon)
        {
            throw std::runtime_error("Expected ; at the end of typedef");
        }
        tokens.pop_back();
        return TypedefDeclaration(std::move(typedefs),std::move(optionalDeclaration));
    }

    OpenCL::Parser::Function parseFunction(std::unique_ptr<OpenCL::Parser::Type>&& rettype,
                                               Tokens& tokens,
                                               ParsingContext& context)
    {
        auto currToken = tokens.back();
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
        std::vector<std::pair<std::shared_ptr<Type>, std::string>> arguments;
        currToken = tokens.back();
        if (currToken.getTokenType() != TokenType::CloseParenthese)
        {
            while (true)
            {
                currToken = tokens.back();
                if (!isType(currToken,context))
                {
                    throw std::runtime_error("Unsupported argument type");
                }
                auto type = parseType(tokens, context);
                currToken = tokens.back();
                std::string argname;
                if (currToken.getTokenType() == TokenType::Identifier)
                {
                    argname = std::get<std::string>(currToken.getValue());
                    tokens.pop_back();
                }
                std::vector<std::size_t> sizes;
                while (tokens.back().getTokenType() == TokenType::OpenSquareBracket)
                {
                    tokens.pop_back();
                    auto primaryConstant = parsePrimaryExpression(tokens, context);
                    auto* result = dynamic_cast<const PrimaryExpressionConstant*>(primaryConstant.get());
                    if (!result)
                    {
                        throw std::runtime_error("Expected primary constant expression for array size");
                    }
                    sizes.push_back(std::visit([](auto&& value) -> std::size_t
                                               {
                                                   using T = std::decay_t<decltype(value)>;
                                                   if constexpr(std::is_integral_v<T>)
                                                   {
                                                       return value;
                                                   }
                                                   else
                                                   {
                                                       throw std::runtime_error(
                                                           "Only integral type supported for array size");
                                                   }
                                               }, result->getValue()));
                    if (tokens.back().getTokenType() != TokenType::CloseSquareBracket)
                    {
                        throw std::runtime_error("Expected ] after array declaration");
                    }
                    tokens.pop_back();
                }
                std::for_each(sizes.rbegin(), sizes.rend(), [&type](std::size_t value)
                {
                    type = std::make_unique<ArrayType>(std::move(type), value);
                });
                arguments.emplace_back(std::shared_ptr<Type>(type.release()), argname);
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
        context.pushScope();
        if (currToken.getTokenType() == TokenType::OpenBrace)
        {
            for (auto&[type, name] : arguments)
            {
                if (name.empty())
                {
                    throw std::runtime_error("Omitted parameter name");
                }
                context.addToScope(name);
            }
            auto statement = parseStatement(tokens, context);
            context.popScope();
            auto pointer = dynamic_cast<BlockStatement*>(statement.get());
            if (!pointer)
            {
                throw std::runtime_error("Expected Block statement after function");
            }

            return Function(std::shared_ptr<Type>(rettype.release()),
                            std::move(name),
                            std::move(arguments),
                            std::make_unique<BlockStatement>(std::move(*pointer)));
        }
        else if (currToken.getTokenType() == TokenType::SemiColon)
        {
            tokens.pop_back();
            return Function(std::shared_ptr<Type>(rettype.release()), std::move(name), std::move(arguments));
        }
        else
        {
            throw std::runtime_error("Expected closing Brace or Semicolon after function declaration");
        }
    }

    std::unique_ptr<OpenCL::Parser::Type> parseType(Tokens& tokens, ParsingContext& context)
    {
        std::stack<std::unique_ptr<Type>> types;
        for (auto iter = tokens.rbegin(); iter != tokens.rend();)
        {
            switch (iter->getTokenType())
            {
            case TokenType::VoidKeyword:
            case TokenType::CharKeyword:
            case TokenType::ShortKeyword:
            case TokenType::IntKeyword:
            case TokenType::LongKeyword:
            case TokenType::FloatKeyword:
            case TokenType::DoubleKeyword:
            case TokenType::SignedKeyword:
            case TokenType::UnsignedKeyword:
            {
                types.push(std::make_unique<PrimitiveType>(parsePrimitiveType(tokens, context)));
                iter = tokens.rbegin();
                break;
            }
            case TokenType::ConstKeyword:
            {
                iter++;
                break;
            }
            case TokenType::StructKeyword:
            {
                types.push(std::make_unique<StructType>(parseStructType(tokens, context)));
                iter = tokens.rbegin();
                break;
            }
            case TokenType::UnionKeyword:
            {
                types.push(std::make_unique<UnionType>(parseUnionType(tokens, context)));
                iter = tokens.rbegin();
                break;
            }
            case TokenType::Asterisk:
            {
                tokens.pop_back();
                auto prevType = std::move(types.top());
                types.pop();
                bool isConst = false;
                if(tokens.back().getTokenType() == TokenType::ConstKeyword)
                {
                    isConst = true;
                    tokens.pop_back();
                }
                types.push(std::make_unique<PointerType>(std::move(prevType),isConst));
                iter = tokens.rbegin();
                break;
            }
            default:
                if(types.size() != 1)
                {
                    throw std::runtime_error("Invalid type token count");
                }
                return std::move(types.top());
            }
        }
        if(types.size() != 1)
        {
            throw std::runtime_error("Invalid type token count");
        }
        return std::move(types.top());
    }

    OpenCL::Parser::PrimitiveType parsePrimitiveType(Tokens& tokens, ParsingContext& context)
    {
        (void)context;
        std::vector<PrimitiveType::Types> types;
        auto currToken = tokens.back();
        while (isType(currToken,context))
        {
            tokens.pop_back();
            switch (currToken.getTokenType())
            {
            case TokenType::VoidKeyword:return PrimitiveType(std::move(types));
            case TokenType::CharKeyword:types.push_back(PrimitiveType::Types::Char);
                break;
            case TokenType::ShortKeyword:types.push_back(PrimitiveType::Types::Short);
                break;
            case TokenType::IntKeyword:types.push_back(PrimitiveType::Types::Int);
                break;
            case TokenType::LongKeyword:types.push_back(PrimitiveType::Types::Long);
                break;
            case TokenType::FloatKeyword:types.push_back(PrimitiveType::Types::Float);
                break;
            case TokenType::DoubleKeyword:types.push_back(PrimitiveType::Types::Double);
                break;
            case TokenType::SignedKeyword:types.push_back(PrimitiveType::Types::Signed);
                break;
            case TokenType::UnsignedKeyword:types.push_back(PrimitiveType::Types::Unsigned);
                break;
            case TokenType::ConstKeyword:types.push_back(PrimitiveType::Types::Const);
                break;
            default:throw std::runtime_error("Invalid token");
            }
            currToken = tokens.back();
        }
        return PrimitiveType(std::move(types));
    }

    OpenCL::Parser::StructType parseStructType(Tokens& tokens, ParsingContext& context)
    {
        (void)context;
        bool isConst = false;
        if(tokens.back().getTokenType() == TokenType::ConstKeyword)
        {
            isConst = true;
            tokens.pop_back();
        }
        if(tokens.back().getTokenType() != TokenType::StructKeyword)
        {
            throw std::runtime_error("Expected struct keyword in struct type instantiation");
        }
        tokens.pop_back();
        if(tokens.back().getTokenType() != TokenType::Identifier)
        {
            throw std::runtime_error("Expected identifier after struct keyword");
        }
        std::string name = std::get<std::string>(tokens.back().getValue());
        tokens.pop_back();
        if(tokens.back().getTokenType() == TokenType::ConstKeyword)
        {
            isConst = true;
            tokens.pop_back();
        }
        return StructType(std::move(name), isConst);
    }

    OpenCL::Parser::UnionType parseUnionType(Tokens& tokens, ParsingContext& context)
    {
        (void)context;
        bool isConst = false;
        if(tokens.back().getTokenType() == TokenType::ConstKeyword)
        {
            isConst = true;
            tokens.pop_back();
        }
        if(tokens.back().getTokenType() != TokenType::UnionKeyword)
        {
            throw std::runtime_error("Expected struct keyword in struct type instantiation");
        }
        tokens.pop_back();
        if(tokens.back().getTokenType() != TokenType::Identifier)
        {
            throw std::runtime_error("Expected identifier after struct keyword");
        }
        std::string name = std::get<std::string>(tokens.back().getValue());
        tokens.pop_back();
        if(tokens.back().getTokenType() == TokenType::ConstKeyword)
        {
            isConst = true;
            tokens.pop_back();
        }
        return UnionType(std::move(name), isConst);
    }

    std::unique_ptr<OpenCL::Parser::BlockItem> parseBlockItem(Tokens& tokens, ParsingContext& context)
    {
        auto currToken = tokens.back();
        if (isType(currToken,context) && currToken.getTokenType() != TokenType::Asterisk)
        {
            auto type = parseType(tokens, context);
            std::vector<std::tuple<std::shared_ptr<Type>, std::string, std::unique_ptr<Expression>>> declarations;
            while (true)
            {
                currToken = tokens.back();
                if (currToken.getTokenType() != TokenType::Identifier)
                {
                    throw std::runtime_error("Expected Identifier after variable declaration");
                }
                tokens.pop_back();
                std::vector<std::size_t> sizes;
                while (tokens.back().getTokenType() == TokenType::OpenSquareBracket)
                {
                    tokens.pop_back();
                    auto primaryConstant = parsePrimaryExpression(tokens, context);
                    auto* result = dynamic_cast<const PrimaryExpressionConstant*>(primaryConstant.get());
                    if (!result)
                    {
                        throw std::runtime_error("Expected primary constant expression for array size");
                    }
                    sizes.push_back(std::visit([](auto&& value) -> std::size_t
                                               {
                                                   using T = std::decay_t<decltype(value)>;
                                                   if constexpr(std::is_integral_v<T>)
                                                   {
                                                       return value;
                                                   }
                                                   else
                                                   {
                                                       throw std::runtime_error(
                                                           "Only integral type supported for array size");
                                                   }
                                               }, result->getValue()));
                    if (tokens.back().getTokenType() != TokenType::CloseSquareBracket)
                    {
                        throw std::runtime_error("Expected ] after array declaration");
                    }
                    tokens.pop_back();
                }
                auto thisType = type->clone();
                std::for_each(sizes.rbegin(), sizes.rend(), [&thisType](std::size_t value)
                {
                    thisType = std::make_unique<ArrayType>(std::move(thisType), value);
                });
                auto name = std::get<std::string>(currToken.getValue());
                context.addToScope(name);
                currToken = tokens.back();
                if (currToken.getTokenType() == TokenType::Assignment)
                {
                    tokens.pop_back();
                    declarations
                        .emplace_back(std::shared_ptr<Type>(thisType.release()), name, std::make_unique<Expression>(
                            parseExpression(
                                tokens,
                                context)));
                }
                else
                {
                    declarations.emplace_back(std::move(thisType), name, nullptr);
                }
                if (tokens.empty() || (tokens.back().getTokenType() != TokenType::SemiColon
                    && tokens.back().getTokenType() != TokenType::Comma))
                {
                    throw std::runtime_error("Declaration not terminated with ;");
                }
                else if (tokens.back().getTokenType() == TokenType::SemiColon)
                {
                    tokens.pop_back();
                    break;
                }
                else
                {
                    tokens.pop_back();
                }
            }
            return std::make_unique<Declarations>(std::move(declarations));
        }
        else
        {
            return parseStatement(tokens, context);
        }
    }

    std::unique_ptr<OpenCL::Parser::Statement> parseStatement(Tokens& tokens, ParsingContext& context)
    {
        auto result = [&tokens,&context]() -> std::unique_ptr<Statement>
        {
            auto curentToken = tokens.back();
            switch (curentToken.getTokenType())
            {
            case TokenType::ReturnKeyword:
            {
                tokens.pop_back();
                return std::make_unique<ReturnStatement>(parseExpression(tokens, context));
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
                    return std::make_unique<IfStatement>(std::move(expression),
                                                         std::move(statement),
                                                         parseStatement(tokens, context));
                }
                else
                {
                    return std::make_unique<IfStatement>(std::move(expression), std::move(statement));
                }
            }
            case TokenType::OpenBrace:
            {
                tokens.pop_back();
                context.pushScope();
                std::vector<std::unique_ptr<BlockItem>> blockItems;
                while (!tokens.empty() && tokens.back().getTokenType() != TokenType::CloseBrace)
                {
                    blockItems.push_back(parseBlockItem(tokens, context));
                }
                if (tokens.empty() || tokens.back().getTokenType() != TokenType::CloseBrace)
                {
                    throw std::runtime_error("Expected } to close Block");
                }
                tokens.pop_back();
                context.popScope();
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
                auto blockitem = parseBlockItem(tokens, context);

                auto control = [&]() -> std::unique_ptr<Expression>
                {
                    if (dynamic_cast<Declarations*>(blockitem.get())
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

                if (auto declaration = dynamic_cast<Declarations*>(blockitem.get());declaration)
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
                auto expression = parseExpression(tokens, context);
                curentToken = tokens.back();
                tokens.pop_back();
                if (curentToken.getTokenType() != TokenType::CloseParenthese)
                {
                    throw std::runtime_error("Expected ) after expression in while");
                }
                auto statement = parseStatement(tokens, context);
                return std::make_unique<HeadWhileStatement>(std::move(expression), std::move(statement));
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
                return std::make_unique<SwitchStatement>(std::move(expression), parseStatement(tokens, context));
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
                return std::make_unique<DefaultStatement>(parseStatement(tokens, context));
            }
            case TokenType::CaseKeyword:
            {
                tokens.pop_back();
                auto expression = parseExpression(tokens, context);
                curentToken = tokens.back();
                if (curentToken.getTokenType() != TokenType::Colon)
                {
                    throw std::runtime_error("Expected : after constant expression of case");
                }
                tokens.pop_back();
                return std::make_unique<CaseStatement>(std::move(expression),
                                                       tokens.back().getTokenType() != TokenType::CaseKeyword
                                                       ? parseStatement(tokens, context) : nullptr);
            }
            default:
            {
                if (!tokens.empty() && tokens.back().getTokenType() != TokenType::SemiColon)
                {
                    return std::make_unique<ExpressionStatement>(std::make_unique<Expression>(parseExpression(tokens,
                                                                                                              context)));
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

    OpenCL::Parser::Expression parseExpression(Tokens& tokens, ParsingContext& context)
    {
        auto expression = parseNonCommaExpression(tokens, context);

        std::unique_ptr<NonCommaExpression> optional;
        if (!tokens.empty())
        {
            auto currToken = tokens.back();
            if (currToken.getTokenType() == TokenType::Comma)
            {
                tokens.pop_back();
                optional = parseNonCommaExpression(tokens, context);
            }
        }
        return Expression(std::move(expression), std::move(optional));
    }

    std::unique_ptr<OpenCL::Parser::NonCommaExpression> parseNonCommaExpression(Tokens& tokens, ParsingContext& context)
    {
        std::size_t braceCount = 0;
        std::size_t squareCount = 0;
        auto result = std::find_if(tokens.rbegin(), tokens.rend(), [&](const Token& token)
        {
            if (token.getTokenType() == TokenType::SemiColon || isAssignment(token.getTokenType()))
            {
                return true;
            }
            else if (token.getTokenType() == TokenType::OpenParenthese)
            {
                braceCount++;
            }
            else if (token.getTokenType() == TokenType::CloseParenthese)
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
            return false;
        });
        bool assignment = result != tokens.rend() && isAssignment(result->getTokenType());
        if (assignment)
        {
            auto unary = parseUnaryExpression(tokens, context);
            auto currentToken = tokens.back();
            tokens.pop_back();
            auto nonCommaExpression = parseNonCommaExpression(tokens, context);
            return std::make_unique<AssignmentExpression>(std::move(unary),
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
                                                          }(), std::move(nonCommaExpression));
        }
        else
        {
            return std::make_unique<ConditionalExpression>(parseConditionalExpression(tokens, context));
        }
    }

    OpenCL::Parser::ConditionalExpression parseConditionalExpression(Tokens& tokens, ParsingContext& context)
    {
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
                return ConditionalExpression(std::move(logicalOrExperssion),
                                             std::make_unique<Expression>(std::move(optionalExpression)),
                                             std::make_unique<ConditionalExpression>(std::move(optionalConditional)));
            }
        }
        return ConditionalExpression(std::move(logicalOrExperssion));
    }

    OpenCL::Parser::LogicalOrExpression parseLogicalOrExpression(Tokens& tokens, ParsingContext& context)
    {
        auto logicalAnd = parseLogicalAndExpression(tokens, context);

        std::vector<LogicalAndExpression> optionalLogicalAnds;
        if (!tokens.empty())
        {
            auto curentToken = tokens.back();
            while (curentToken.getTokenType() == TokenType::LogicOr)
            {
                tokens.pop_back();
                optionalLogicalAnds.push_back(parseLogicalAndExpression(tokens, context));
                curentToken = tokens.back();
            }
        }

        return LogicalOrExpression(std::move(logicalAnd), std::move(optionalLogicalAnds));
    }

    OpenCL::Parser::LogicalAndExpression parseLogicalAndExpression(Tokens& tokens, ParsingContext& context)
    {
        auto result = parseBitOrExpression(tokens, context);

        std::vector<BitOrExpression> list;
        if (!tokens.empty())
        {
            auto currToken = tokens.back();
            while (currToken.getTokenType() == TokenType::LogicAnd)
            {
                tokens.pop_back();
                list.push_back(parseBitOrExpression(tokens, context));
                currToken = tokens.back();
            }
        }

        return LogicalAndExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::BitOrExpression parseBitOrExpression(Tokens& tokens, ParsingContext& context)
    {
        auto result = parseBitXorExpression(tokens, context);

        std::vector<BitXorExpression> list;
        if (!tokens.empty())
        {
            auto currToken = tokens.back();
            while (currToken.getTokenType() == TokenType::BitOr)
            {
                tokens.pop_back();
                list.push_back(parseBitXorExpression(tokens, context));
                currToken = tokens.back();
            }
        }

        return BitOrExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::BitXorExpression parseBitXorExpression(Tokens& tokens, ParsingContext& context)
    {
        auto result = parseBitAndExpression(tokens, context);

        std::vector<BitAndExpression> list;
        if (!tokens.empty())
        {
            auto currToken = tokens.back();
            while (currToken.getTokenType() == TokenType::BitXor)
            {
                tokens.pop_back();
                list.push_back(parseBitAndExpression(tokens, context));
                currToken = tokens.back();
            }
        }

        return BitXorExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::BitAndExpression parseBitAndExpression(Tokens& tokens, ParsingContext& context)
    {
        auto result = parseEqualityExpression(tokens, context);

        std::vector<EqualityExpression> list;
        if (!tokens.empty())
        {
            auto currToken = tokens.back();
            while (currToken.getTokenType() == TokenType::Ampersand)
            {
                tokens.pop_back();
                list.push_back(parseEqualityExpression(tokens, context));
                currToken = tokens.back();
            }
        }

        return BitAndExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::EqualityExpression parseEqualityExpression(Tokens& tokens, ParsingContext& context)
    {
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
                currToken = tokens.back();
            }
        }

        return EqualityExpression(std::move(result), std::move(relationalExpressions));
    }

    OpenCL::Parser::RelationalExpression parseRelationalExpression(Tokens& tokens, ParsingContext& context)
    {
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
                                      default:
                                          throw std::runtime_error("Invalid token for relational LogicalOrExpression");
                                      }
                                  }(), parseShiftExpression(tokens, context));
                currToken = tokens.back();
            }
        }

        return RelationalExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::ShiftExpression parseShiftExpression(Tokens& tokens, ParsingContext& context)
    {
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
                currToken = tokens.back();
            }
        }

        return ShiftExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::AdditiveExpression parseAdditiveExpression(Tokens& tokens, ParsingContext& context)
    {
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
                currToken = tokens.back();
            }
        }

        return AdditiveExpression(std::move(result), std::move(list));
    }

    OpenCL::Parser::Term parseTerm(Tokens& tokens, ParsingContext& context)
    {
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
                currToken = tokens.back();
            }
        }

        return Term(std::move(result), std::move(list));
    }

    OpenCL::Parser::CastExpression parseCastExpression(Tokens& tokens, ParsingContext& context)
    {
        auto currToken = tokens.rbegin();
        if (currToken->getTokenType() == TokenType::OpenParenthese)
        {
            currToken++;
            auto closing = std::find_if(currToken,tokens.rend(),[](const Token& tokens)
            {
                return tokens.getTokenType() == TokenType::CloseParenthese;
            });
            if(closing == tokens.rend())
            {
                throw std::runtime_error("Unexpected end of tokens");
            }
            closing++;
            if (isType(*currToken,context) && closing->getTokenType() != TokenType::OpenBrace)
            {
                auto result = std::find_if(currToken + 1, tokens.rend(), [](const Token& token)
                {
                    return token.getTokenType() == TokenType::CloseParenthese
                        || token.getTokenType() == TokenType::OpenBrace;
                });
                if (result == tokens.rend())
                {
                    throw std::runtime_error("Unexpected end of tokens");
                }
                if (result->getTokenType() == TokenType::CloseParenthese)
                {
                    tokens.pop_back();
                    auto type = parseType(tokens, context);
                    if (tokens.back().getTokenType() != TokenType::CloseParenthese)
                    {
                        throw std::runtime_error("Expected Close Parenthese after type cast");
                    }
                    tokens.pop_back();
                    return CastExpression(std::pair<std::unique_ptr<Type>, std::unique_ptr<CastExpression>>{
                        std::move(type), std::make_unique<CastExpression>(parseCastExpression(tokens, context))});
                }
            }
        }
        return CastExpression(parseUnaryExpression(tokens, context));
    }

    std::unique_ptr<OpenCL::Parser::UnaryExpression> parseUnaryExpression(Tokens& tokens, ParsingContext& context)
    {
        auto currToken = tokens.back();
        if (currToken.getTokenType() == TokenType::SizeofKeyword)
        {
            tokens.pop_back();
            currToken = tokens.back();
            if (currToken.getTokenType() == TokenType::OpenParenthese)
            {
                tokens.pop_back();
                auto type = parseType(tokens, context);
                currToken = tokens.back();
                if (currToken.getTokenType() != TokenType::CloseParenthese)
                {
                    throw std::runtime_error("Expected Close Parenthese after type in sizeof");
                }
                tokens.pop_back();
                return std::make_unique<UnaryExpressionSizeOf>(std::move(type));
            }
            else
            {
                auto unary = parseUnaryExpression(tokens, context);
                return std::make_unique<UnaryExpressionSizeOf>(std::move(unary));
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
            return std::make_unique<UnaryExpressionUnaryOperator>(op, parseUnaryExpression(tokens, context));
        }
        return std::make_unique<UnaryExpressionPostFixExpression>(parsePostFixExpression(tokens, context));
    }

    bool isPostFixExpression(Tokens& token)
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

    std::unique_ptr<OpenCL::Parser::PostFixExpression> parsePostFixExpression(Tokens& tokens, ParsingContext& context)
    {
        std::stack<std::unique_ptr<PostFixExpression>> stack;
        while (!tokens.empty() && isPostFixExpression(tokens))
        {
            auto currToken = tokens.back();
            if (currToken.getTokenType() == TokenType::Identifier
                || currToken.getTokenType() == TokenType::Literal)
            {
                if(!stack.empty())
                {
                    throw std::runtime_error("Can't combine post fix expressions");
                }
                stack.push(std::make_unique<PostFixExpressionPrimaryExpression>(parsePrimaryExpression(tokens,
                                                                                                       context)));
            }
            else if(currToken.getTokenType() == TokenType::OpenParenthese && stack.empty())
            {
                if(tokens.size() == 1)
                {
                    throw std::runtime_error("Unexpected end of token");
                }
                if(isType(tokens.at(tokens.size()-2),context))
                {
                    if(!stack.empty())
                    {
                        throw std::runtime_error("Can't combine post fix expressions");
                    }
                    tokens.pop_back();
                    auto type = parseType(tokens, context);
                    currToken = tokens.back();
                    if(currToken.getTokenType() != TokenType::CloseParenthese)
                    {
                        throw std::runtime_error("Expected ) after type in type initialization");
                    }
                    tokens.pop_back();
                    currToken = tokens.back();
                    if(currToken.getTokenType() != TokenType::OpenBrace)
                    {
                        throw std::runtime_error("Expected { after type around parenthesis");
                    }
                    tokens.pop_back();
                    std::vector<std::unique_ptr<NonCommaExpression>> nonCommaExpressions;
                    while(true)
                    {
                        nonCommaExpressions.push_back(parseNonCommaExpression(tokens, context));
                        if(tokens.back().getTokenType() == TokenType::Comma)
                        {
                            tokens.pop_back();
                        }
                        else
                        {
                            break;
                        }
                    }
                    if(tokens.back().getTokenType() != TokenType::CloseBrace)
                    {
                        throw std::runtime_error("Expected { after type around parenthesis");
                    }
                    tokens.pop_back();
                    stack.push(std::make_unique<PostFixExpressionTypeInitializer>(std::move(type),std::move(nonCommaExpressions)));
                }
                else
                {
                    stack.push(std::make_unique<PostFixExpressionPrimaryExpression>(parsePrimaryExpression(tokens,
                                                                                                           context)));
                }
            }
            else if (currToken.getTokenType() == TokenType::OpenParenthese)
            {
                tokens.pop_back();
                std::vector<std::unique_ptr<NonCommaExpression>> nonCommaExpressions;
                while (tokens.back().getTokenType() != TokenType::CloseParenthese)
                {
                    nonCommaExpressions.push_back(parseNonCommaExpression(tokens, context));
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
                stack.push(std::make_unique<PostFixExpressionFunctionCall>(std::move(postExpression),
                                                                           std::move(nonCommaExpressions)));
            }
            else if (currToken.getTokenType() == TokenType::OpenSquareBracket)
            {
                tokens.pop_back();
                std::size_t i = 0;
                auto result = std::find_if(tokens.rbegin(), tokens.rend(), [&i](const Token& token)
                {
                    if (token.getTokenType() == TokenType::CloseSquareBracket && i == 0)
                    {
                        return true;
                    }
                    else if (token.getTokenType() == TokenType::CloseSquareBracket)
                    {
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
                stack.push(std::make_unique<PostFixExpressionSubscript>(std::move(postExpression),
                                                                        std::move(expression)));
            }
            else if (currToken.getTokenType() == TokenType::Increment)
            {
                tokens.pop_back();
                auto postExpression = std::move(stack.top());
                stack.pop();
                stack.push(std::make_unique<PostFixExpressionIncrement>(std::move(postExpression)));
            }
            else if (currToken.getTokenType() == TokenType::Decrement)
            {
                tokens.pop_back();
                auto postExpression = std::move(stack.top());
                stack.pop();
                stack.push(std::make_unique<PostFixExpressionDecrement>(std::move(postExpression)));
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
                stack.push(std::make_unique<PostFixExpressionDot>(std::move(postExpression),
                                                                  std::get<std::string>(currToken.getValue())));
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
                stack.push(std::make_unique<PostFixExpressionArrow>(std::move(postExpression),
                                                                    std::get<std::string>(currToken.getValue())));
            }
        }
        if (stack.size() != 1)
        {
            throw std::runtime_error("Invalid amount of post fix expressions");
        }
        auto ret = std::move(stack.top());
        stack.pop();
        return ret;
    }

    std::unique_ptr<OpenCL::Parser::PrimaryExpression> parsePrimaryExpression(Tokens& tokens, ParsingContext& context)
    {
        auto currToken = tokens.back();
        tokens.pop_back();
        if (currToken.getTokenType() == TokenType::Identifier)
        {
            return std::make_unique<PrimaryExpressionIdentifier>(std::get<std::string>(currToken.getValue()));
        }
        else if (currToken.getTokenType() == TokenType::Literal)
        {
            return std::make_unique<PrimaryExpressionConstant>(std::visit([](auto&& value) -> typename PrimaryExpressionConstant::variant
                                                                          {
                                                                              using T = std::decay_t<decltype(value)>;
                                                                              if constexpr(std::is_constructible_v<
                                                                                  typename PrimaryExpressionConstant::variant,
                                                                                  T>)
                                                                              {
                                                                                  return {std::forward<decltype(value)>(
                                                                                      value)};
                                                                              }
                                                                              else
                                                                              {
                                                                                  throw std::runtime_error(
                                                                                      "Can't convert type of variant to constant expression");
                                                                              }
                                                                          }, currToken.getValue()));
        }
        else if (currToken.getTokenType() == TokenType::OpenParenthese)
        {
            auto expression = parseExpression(tokens, context);
            if (tokens.back().getTokenType() != TokenType::CloseParenthese)
            {
                throw std::runtime_error("Expected Close Parenthese after expression in primary expression");
            }
            tokens.pop_back();
            return std::make_unique<PrimaryExpressionParenthese>(std::move(expression));
        }
        else
        {
            throw std::runtime_error("Invalid token for primary expression");
        }
    }
}
