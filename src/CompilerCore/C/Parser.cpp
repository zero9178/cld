#include "Parser.hpp"

#include "ConstantEvaluator.hpp"

#include <stack>
#include <algorithm>

using namespace OpenCL;
using namespace OpenCL::Lexer;
using namespace OpenCL::Syntax;

Expected<TranslationUnit, FailureReason> OpenCL::Parser::buildTree(const std::vector<Token>& tokens)
{
    ParsingContext context;
    auto begin = tokens.cbegin();
    return parseTranslationUnit(begin, tokens.cend(), context);
}

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

OpenCL::Expected<OpenCL::Syntax::TranslationUnit,
                 OpenCL::FailureReason> OpenCL::Parser::parseTranslationUnit(Tokens::const_iterator& begin,
                                                                             Tokens::const_iterator end,
                                                                             ParsingContext& context)
{
    std::vector<ExternalDeclaration> global;
    while (begin != end)
    {
        auto result = parseExternalDeclaration(begin, end, context);
        if (result)
        {
            global.push_back(std::move(*result));
        }
        else
        {
            return result;
        }
    }
    return TranslationUnit(std::move(global));
}

OpenCL::Expected<OpenCL::Syntax::ExternalDeclaration,
                 OpenCL::FailureReason> OpenCL::Parser::parseExternalDeclaration(Tokens::const_iterator& begin,
                                                                                 Tokens::const_iterator end,
                                                                                 ParsingContext& context)
{
    auto line = begin->getLine();
    auto column = begin->getColumn();
    auto function = parseFunctionDefinition(begin, end, context);
    if (function)
    {
        return ExternalDeclaration(line, column, std::move(*function));
    }
    else if (auto declaration = parseDeclaration(begin, end, context))
    {
        return ExternalDeclaration(line, column, std::move(*declaration));
    }
    else
    {
        return declaration;
    }
}

namespace
{
    bool isDeclarationSpecifier(const Token& token, OpenCL::Parser::ParsingContext& context)
    {
        switch (token.getTokenType())
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
        case TokenType::InlineKeyword:return true;
        case TokenType::Identifier:
            return !context.isInScope(std::get<std::string>(token.getValue()))
                && context.typedefs.count(std::get<std::string>(token.getValue()));
        default:return false;
        }
    }

    template <typename G>
    struct Y
    {
        template <typename... X>
        decltype(auto) operator()(X&& ... x) const&
        {
            return g(*this, std::forward<X>(x)...);
        }

        G g;
    };

    template <typename G>
    Y(G) -> Y<G>;
}

Expected<Declaration, FailureReason> OpenCL::Parser::parseDeclaration(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    auto curr = begin;
    std::vector<DeclarationSpecifier> declarationSpecifiers;
    while (auto result = parseDeclarationSpecifier(curr, end, context))
    {
        declarationSpecifiers.push_back(std::move(*result));
    }
    if (declarationSpecifiers.empty())
    {
        return FailureReason("Expected declaration specifiers at beginning of declaration");
    }
    if (curr->getTokenType() == TokenType::SemiColon)
    {
        curr++;
        begin = curr;
        for(auto& iter : declarationSpecifiers)
        {
            if (auto* typeSpec = std::get_if<TypeSpecifier>(&iter))
            {
                if (auto* enumSpec = std::get_if<std::unique_ptr<EnumSpecifier>>(&typeSpec->getVariant()); enumSpec && std::holds_alternative<EnumDeclaration>((*enumSpec)->getVariant()))
                {
                    auto& enumDecl = std::get<EnumDeclaration>((*enumSpec)->getVariant());
                    for(auto& [name,value] : enumDecl.getValues())
                    {
                        context.addEnumConstant(name,value);
                    }
                }
            }
        }
        return Declaration(line, column, std::move(declarationSpecifiers), {});
    }
    std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>> initDeclarators;
    bool first = true;
    do
    {
        if (first)
        {
            first = false;
        }
        else if (curr->getTokenType() == TokenType::Comma)
        {
            curr++;
        }
        else
        {
            break;
        }
        auto declarator = parseDeclarator(curr, end, context);
        if (!declarator)
        {
            break;
        }
        if (curr->getTokenType() != TokenType::Assignment)
        {
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
        }
        else
        {
            curr++;
            auto initializer = parseInitializer(curr, end, context);
            if (!initializer)
            {
                return initializer;
            }
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::make_unique<Initializer>(std::move(*initializer)));
        }
    }
    while (true);
    if (curr->getTokenType() != TokenType::SemiColon)
    {
        return FailureReason("Expected ; at the end of declaration");
    }
    curr++;
    if (auto* storage = std::get_if<StorageClassSpecifier>(&declarationSpecifiers.front());storage
        && *storage == StorageClassSpecifier::Typedef)
    {
        for (auto&[declator, init] : initDeclarators)
        {
            (void)init;
            auto visitor = [](auto self, auto&& value) -> std::string
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr(std::is_same_v<std::string, T>)
                {
                    (void)self;
                    return value;
                }
                else if constexpr(std::is_same_v<T, std::unique_ptr<Declarator>>)
                {
                    return std::visit([&](auto&& value) -> std::string
                                      {
                                          return self(value);
                                      }, value->getDirectDeclarator().getVariant());
                }
                else
                {
                    return std::visit([&](auto&& value) -> std::string
                                      {
                                          return self(value);
                                      }, value.getDirectDeclarator().getVariant());
                }
            };
            context.typedefs.insert(std::visit(Y{visitor}, declator->getDirectDeclarator().getVariant()));
        }
    }
    else
    {
        for (auto&[declator, init] : initDeclarators)
        {
            (void)init;
            bool isFunction = false;
            auto visitor = [&isFunction](auto self, auto&& value) -> std::string
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr(std::is_same_v<std::string, T>)
                {
                    (void)self;
                    return value;
                }
                else if constexpr(std::is_same_v<T, std::unique_ptr<Declarator>>)
                {
                    return std::visit([&](auto&& value) -> std::string
                                      {
                                          return self(value);
                                      }, value->getDirectDeclarator().getVariant());
                }
                else
                {
                    if constexpr(std::is_same_v<T, DirectDeclaratorParentheseIdentifiers>
                        || std::is_same_v<T, DirectDeclaratorParentheseParameters>)
                    {
                        isFunction = true;
                    }
                    return std::visit([&](auto&& value) -> std::string
                                      {
                                          return self(value);
                                      }, value.getDirectDeclarator().getVariant());
                }
            };
            auto result = std::visit(Y{visitor}, declator->getDirectDeclarator().getVariant());
            if (!isFunction)
            {
                context.addToScope(result);
            }
            else
            {
                context.functions.insert(result);
            }
        }
    }
    for(auto& iter : declarationSpecifiers)
    {
        if (auto* typeSpec = std::get_if<TypeSpecifier>(&iter))
        {
            if (auto* enumSpec = std::get_if<std::unique_ptr<EnumSpecifier>>(&typeSpec->getVariant()); enumSpec && std::holds_alternative<EnumDeclaration>((*enumSpec)->getVariant()))
            {
                auto& enumDecl = std::get<EnumDeclaration>((*enumSpec)->getVariant());
                for(auto& [name,value] : enumDecl.getValues())
                {
                    context.addEnumConstant(name,value);
                }
            }
        }
    }
    begin = curr;
    return Declaration(line, column, std::move(declarationSpecifiers), std::move(initDeclarators));
}

OpenCL::Expected<OpenCL::Syntax::DeclarationSpecifier, OpenCL::FailureReason> OpenCL::Parser::parseDeclarationSpecifier(
    Tokens::const_iterator& begin,
    Tokens::const_iterator end,
    OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto currToken = *curr;
    switch (curr->getTokenType())
    {
    case TokenType::TypedefKeyword:
    case TokenType::ExternKeyword:
    case TokenType::StaticKeyword:
    case TokenType::AutoKeyword:
    case TokenType::RegisterKeyword:
    case TokenType::ConstKeyword:
    case TokenType::RestrictKeyword:
    case TokenType::VolatileKeyword:
    case TokenType::InlineKeyword:
    case TokenType::VoidKeyword:
    case TokenType::CharKeyword:
    case TokenType::ShortKeyword:
    case TokenType::IntKeyword:
    case TokenType::LongKeyword:
    case TokenType::FloatKeyword:
    case TokenType::DoubleKeyword:
    case TokenType::SignedKeyword:
    case TokenType::UnsignedKeyword:begin++;
        [[fallthrough]];
    case TokenType::Identifier:curr++;
    default:break;
    }
    switch (currToken.getTokenType())
    {
    case TokenType::TypedefKeyword:return DeclarationSpecifier{StorageClassSpecifier::Typedef};
    case TokenType::ExternKeyword:return DeclarationSpecifier{StorageClassSpecifier::Extern};
    case TokenType::StaticKeyword:return DeclarationSpecifier{StorageClassSpecifier::Static};
    case TokenType::AutoKeyword:return DeclarationSpecifier{StorageClassSpecifier::Auto};
    case TokenType::RegisterKeyword:return DeclarationSpecifier{StorageClassSpecifier::Register};
    case TokenType::ConstKeyword:return DeclarationSpecifier{TypeQualifier::Const};
    case TokenType::RestrictKeyword: return DeclarationSpecifier{TypeQualifier::Restrict};
    case TokenType::VolatileKeyword:return DeclarationSpecifier{TypeQualifier::Volatile};
    case TokenType::InlineKeyword:return DeclarationSpecifier{FunctionSpecifier{}};
    case TokenType::VoidKeyword:
        return Syntax::DeclarationSpecifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Void)};
    case TokenType::CharKeyword:
        return Syntax::DeclarationSpecifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Char)};
    case TokenType::ShortKeyword:
        return Syntax::DeclarationSpecifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Short)};
    case TokenType::IntKeyword:
        return Syntax::DeclarationSpecifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Int)};
    case TokenType::LongKeyword:
        return Syntax::DeclarationSpecifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Long)};
    case TokenType::FloatKeyword:
        return Syntax::DeclarationSpecifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Float)};
    case TokenType::DoubleKeyword:
        return Syntax::DeclarationSpecifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Double)};
    case TokenType::SignedKeyword:
        return Syntax::DeclarationSpecifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Signed)};
    case TokenType::UnsignedKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Unsigned)};
    case TokenType::UnionKeyword:
    case TokenType::StructKeyword:
    {
        auto expected = parseStructOrUnionSpecifier(curr,end,context);
        if (expected)
        {
            auto result = std::make_unique<StructOrUnionSpecifier>(std::move(*expected));
            context.structOrUnions.emplace(result->getIdentifier(), result.get());
            begin = curr;
            return DeclarationSpecifier{TypeSpecifier(line, column, std::move(result))};
        }
        else
        {
            return expected;
        }
    }
    case TokenType::EnumKeyword:
    {
        auto expected = parseEnumSpecifier(curr, end, context);
        if (expected)
        {
            begin = curr;
            return DeclarationSpecifier{TypeSpecifier(line,
                                                      column,
                                                      std::make_unique<EnumSpecifier>(std::move(*expected)))};
        }
        else
        {
            return expected;
        }
    }
    case TokenType::Identifier:
    {
        auto name = std::get<std::string>(currToken.getValue());
        if (!context.isInScope(name) && context.typedefs.count(name))
        {
            begin = curr;
            return Syntax::DeclarationSpecifier{TypeSpecifier(line, column, name)};
        }
        else if (context.typedefs.count(name))
        {
            return FailureReason(
                "\"" + name + "\" is a typedef but cannot be used as such because another symbol overshadows it");
        }
        break;
    }
    default:break;
    }
    return FailureReason("Invalid token for declaration specifier");
}

OpenCL::Expected<OpenCL::Syntax::StructOrUnionSpecifier,
                 OpenCL::FailureReason> OpenCL::Parser::parseStructOrUnionSpecifier(Tokens::const_iterator& begin,
                                                                                    Tokens::const_iterator end,
                                                                                    OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    bool isUnion;
    if (curr->getTokenType() == TokenType::StructKeyword)
    {
        isUnion = false;
    }
    else if (curr->getTokenType() == TokenType::UnionKeyword)
    {
        isUnion = true;
    }
    else
    {
        return FailureReason("Expected struct or union keyword at beginning of struct or union specifier");
    }
    curr++;
    if (curr->getTokenType() != TokenType::Identifier)
    {
        return FailureReason(std::string("Expected identifier after ") + (isUnion ? "union" : "struct"));
    }
    const auto& name = std::get<std::string>(curr->getValue());
    curr++;
    if (curr->getTokenType() != TokenType::OpenBrace)
    {
        begin = curr;
        return StructOrUnionSpecifier(line, column, isUnion, name, {});
    }
    curr++;
    std::vector<StructOrUnionSpecifier::StructDeclaration> structDeclarations;
    do
    {
        std::vector<SpecifierQualifier> specifierQualifiers;
        while (auto result = parseSpecifierQualifier(curr, end, context))
        {
            specifierQualifiers.push_back(std::move(*result));
        }
        if (specifierQualifiers.empty())
        {
            return FailureReason("Expected Specifier Qualifiers at beginning of struct declarations");
        }
        std::vector<std::pair<std::unique_ptr<Declarator>, std::int64_t>> declarators;
        bool first = true;
        do
        {
            if (first)
            {
                first = false;
            }
            else if (curr->getTokenType() == TokenType::Comma)
            {
                curr++;
            }
            else
            {
                break;
            }
            if (curr->getTokenType() == TokenType::Colon)
            {
                curr++;
                auto constant = parseAssignmentExpression(curr, end, context);
                if (!constant)
                {
                    return constant;
                }
                Codegen::ConstantEvaluator evaluator(context.structOrUnions);
                auto value = *evaluator.visit(*constant);
                if(!value)
                {
                    return value;
                }
                declarators.emplace_back(nullptr, std::visit([](auto&& value) -> std::size_t
                                                             {
                                                                 using T = std::decay_t<decltype(value)>;
                                                                 if constexpr(std::is_convertible_v<T, std::size_t>)
                                                                 {
                                                                     return value;
                                                                 }
                                                                 else
                                                                 {
                                                                     throw std::runtime_error(
                                                                         "Invalid type of constant expression");
                                                                 }
                                                             },*value));
            }
            auto declarator = parseDeclarator(curr, end, context);
            if (!declarator)
            {
                break;
            }
            if (curr->getTokenType() == TokenType::Colon)
            {
                curr++;
                auto constant = parseAssignmentExpression(curr, end, context);
                if (!constant)
                {
                    return constant;
                }
                Codegen::ConstantEvaluator evaluator(context.structOrUnions);
                auto value = *evaluator.visit(*constant);
                if(!value)
                {
                    return value;
                }
                declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::visit([](auto&& value) -> std::size_t
                                                    {
                                                        using T = std::decay_t<decltype(value)>;
                                                        if constexpr(std::is_convertible_v<T, std::size_t>)
                                                        {
                                                            return value;
                                                        }
                                                        else
                                                        {
                                                            throw std::runtime_error(
                                                                "Invalid type of constant expression");
                                                        }
                                                    },*value));
            }
            else
            {
                declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), -1);
            }
        }
        while (true);
        if (curr->getTokenType() != TokenType::SemiColon)
        {
            return FailureReason(
                std::string("Expected ; at the end of ") + (isUnion ? "union" : "struct") + " field declaration");
        }
        curr++;
        structDeclarations.push_back({std::move(specifierQualifiers), std::move(declarators)});
    }
    while (curr->getTokenType() != TokenType::CloseBrace);
    curr++;
    begin = curr;
    return StructOrUnionSpecifier(line, column, isUnion, name, std::move(structDeclarations));
}

OpenCL::Expected<OpenCL::Syntax::SpecifierQualifier, OpenCL::FailureReason> OpenCL::Parser::parseSpecifierQualifier(
    Tokens::const_iterator& begin,
    Tokens::const_iterator end,
    OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto currToken = *curr;
    switch (currToken.getTokenType())
    {
    case TokenType::ConstKeyword:
    case TokenType::RestrictKeyword:
    case TokenType::VolatileKeyword:
    case TokenType::InlineKeyword:
    case TokenType::VoidKeyword:
    case TokenType::CharKeyword:
    case TokenType::ShortKeyword:
    case TokenType::IntKeyword:
    case TokenType::LongKeyword:
    case TokenType::FloatKeyword:
    case TokenType::DoubleKeyword:
    case TokenType::SignedKeyword:
    case TokenType::UnsignedKeyword:begin++;
        [[fallthrough]];
    case TokenType::Identifier:curr++;
    default:break;
    }
    auto line = currToken.getLine();
    auto column = currToken.getColumn();
    switch (currToken.getTokenType())
    {
    case TokenType::ConstKeyword:return SpecifierQualifier{TypeQualifier::Const};
    case TokenType::RestrictKeyword: return SpecifierQualifier{TypeQualifier::Restrict};
    case TokenType::VolatileKeyword:return SpecifierQualifier{TypeQualifier::Volatile};
    case TokenType::VoidKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Void)};
    case TokenType::CharKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Char)};
    case TokenType::ShortKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Short)};
    case TokenType::IntKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Int)};
    case TokenType::LongKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Long)};
    case TokenType::FloatKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Float)};
    case TokenType::DoubleKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Double)};
    case TokenType::SignedKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Signed)};
    case TokenType::UnsignedKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Unsigned)};
    case TokenType::UnionKeyword:
    case TokenType::StructKeyword:
    {
        auto expected = parseStructOrUnionSpecifier(curr,end,context);
        if (expected)
        {
            auto result = std::make_unique<StructOrUnionSpecifier>(std::move(*expected));
            context.structOrUnions.emplace(result->getIdentifier(), result.get());
            begin = curr;
            return SpecifierQualifier{TypeSpecifier(line, column, std::move(result))};
        }
        else
        {
            return expected;
        }
    }
    case TokenType::EnumKeyword:
    {
        auto expected = parseEnumSpecifier(begin, end, context);
        if (expected)
        {
            begin = curr;
            return SpecifierQualifier{TypeSpecifier(line,
                                                    column,
                                                    std::make_unique<EnumSpecifier>(std::move(*expected)))};
        }
        else
        {
            return expected;
        }
    }
    case TokenType::Identifier:
    {
        auto name = std::get<std::string>(currToken.getValue());
        if (!context.isInScope(name) && context.typedefs.count(name))
        {
            begin = curr;
            return Syntax::SpecifierQualifier{TypeSpecifier(line, column, name)};
        }
        else if (context.typedefs.count(name))
        {
            return FailureReason(
                "\"" + name + "\" is a typedef but cannot be used as such because another symbol overshadows it");
        }
        break;
    }
    default:break;
    }
    return FailureReason("Invalid token for declaration specifier");
}

Expected<OpenCL::Syntax::Declarator, FailureReason> OpenCL::Parser::parseDeclarator(Tokens::const_iterator& begin,
                                                                                    Tokens::const_iterator end,
                                                                                    OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    std::vector<Syntax::Pointer> pointers;
    while (auto result = parsePointer(curr, end, context))
    {
        pointers.push_back(std::move(*result));
    }
    auto directDeclarator = parseDirectDeclarator(curr, end, context);
    if (!directDeclarator)
    {
        return directDeclarator;
    }
    begin = curr;
    return Declarator(line, column, std::move(pointers), std::move(*directDeclarator));
}

namespace
{
    Expected<DirectDeclaratorNoStaticOrAsterisk,
             FailureReason> parseDirectDeclaratorNoStaticOrAsterisk(DirectDeclarator& declarator,
                                                                    OpenCL::Parser::Tokens::const_iterator& begin,
                                                                    OpenCL::Parser::Tokens::const_iterator end,
                                                                    OpenCL::Parser::ParsingContext& context)
    {
        if (begin == end)
        {
            return FailureReason("Unexpected end of tokens");
        }
        auto curr = begin;
        auto line = curr->getLine();
        auto column = curr->getColumn();
        if (curr->getTokenType() != TokenType::OpenSquareBracket)
        {
            return FailureReason("Expected [");
        }
        curr++;
        std::vector<TypeQualifier> typeQualifiers;
        while (curr != end && (curr->getTokenType() == TokenType::ConstKeyword
            || curr->getTokenType() == TokenType::RestrictKeyword
            || curr->getTokenType() == TokenType::VolatileKeyword))
        {
            switch (curr->getTokenType())
            {
            case TokenType::ConstKeyword:typeQualifiers.push_back(TypeQualifier::Const);
                break;
            case TokenType::RestrictKeyword:typeQualifiers.push_back(TypeQualifier::Restrict);
                break;
            case TokenType::VolatileKeyword:typeQualifiers.push_back(TypeQualifier::Volatile);
                break;
            default:break;
            }
            curr++;
        }
        auto assignment = OpenCL::Parser::parseAssignmentExpression(curr, end, context);
        if (curr->getTokenType() != TokenType::CloseSquareBracket)
        {
            return FailureReason("Expected ] to close [ in direct declarator");
        }
        curr++;
        begin = curr;
        return DirectDeclaratorNoStaticOrAsterisk(line,
                                                  column,
                                                  std::make_unique<DirectDeclarator>(std::move(declarator)),
                                                  std::move(typeQualifiers),
                                                  assignment
                                                  ? std::make_unique<AssignmentExpression>(std::move(*assignment))
                                                  : nullptr);
    }

    Expected<DirectDeclaratorStatic, FailureReason> parseDirectDeclaratorStatic(DirectDeclarator& declarator,
                                                                                OpenCL::Parser::Tokens::const_iterator& begin,
                                                                                OpenCL::Parser::Tokens::const_iterator end,
                                                                                OpenCL::Parser::ParsingContext& context)
    {
        if (begin == end)
        {
            return FailureReason("Unexpected end of tokens");
        }
        auto curr = begin;
        auto line = curr->getLine();
        auto column = curr->getColumn();
        if (curr->getTokenType() != TokenType::OpenSquareBracket)
        {
            return FailureReason("Expected [");
        }
        curr++;
        bool wasStatic = false;
        if (curr->getTokenType() == TokenType::StaticKeyword)
        {
            wasStatic = true;
            curr++;
        }
        std::vector<TypeQualifier> typeQualifiers;
        while (curr != end && (curr->getTokenType() == TokenType::ConstKeyword
            || curr->getTokenType() == TokenType::RestrictKeyword
            || curr->getTokenType() == TokenType::VolatileKeyword))
        {
            switch (curr->getTokenType())
            {
            case TokenType::ConstKeyword:typeQualifiers.push_back(TypeQualifier::Const);
                break;
            case TokenType::RestrictKeyword:typeQualifiers.push_back(TypeQualifier::Restrict);
                break;
            case TokenType::VolatileKeyword:typeQualifiers.push_back(TypeQualifier::Volatile);
                break;
            default:break;
            }
            curr++;
        }
        if (curr->getTokenType() == TokenType::StaticKeyword)
        {
            if (wasStatic)
            {
                return FailureReason("static appearing twice in direct declarator");
            }
            wasStatic = true;
            curr++;
        }
        auto assignmentExpression = OpenCL::Parser::parseAssignmentExpression(curr, end, context);
        if (!assignmentExpression)
        {
            return assignmentExpression;
        }
        if (curr->getTokenType() != TokenType::CloseSquareBracket)
        {
            return FailureReason("Expected ]");
        }
        curr++;
        begin = curr;
        return DirectDeclaratorStatic(line,
                                      column,
                                      std::make_unique<DirectDeclarator>(std::move(declarator)),
                                      std::move(typeQualifiers),
                                      std::move(*assignmentExpression));
    }

    Expected<DirectDeclaratorAsterisk, FailureReason> parseDirectDeclaratorAsterisk(DirectDeclarator& declarator,OpenCL::Parser::Tokens::const_iterator& begin,
        OpenCL::Parser::Tokens::const_iterator end,
        OpenCL::Parser::ParsingContext& )
    {
        if(begin == end)
        {
            return FailureReason("Unexpected end of tokens");
        }
        auto curr = begin;
        auto line = curr->getLine();
        auto column = curr->getColumn();
        if (curr->getTokenType() != TokenType::OpenSquareBracket)
        {
            return FailureReason("Expected [");
        }
        curr++;
        std::vector<TypeQualifier> typeQualifiers;
        while (curr != end && (curr->getTokenType() == TokenType::ConstKeyword
            || curr->getTokenType() == TokenType::RestrictKeyword
            || curr->getTokenType() == TokenType::VolatileKeyword))
        {
            switch (curr->getTokenType())
            {
            case TokenType::ConstKeyword:typeQualifiers.push_back(TypeQualifier::Const);
                break;
            case TokenType::RestrictKeyword:typeQualifiers.push_back(TypeQualifier::Restrict);
                break;
            case TokenType::VolatileKeyword:typeQualifiers.push_back(TypeQualifier::Volatile);
                break;
            default:break;
            }
            curr++;
        }
        if(curr->getTokenType() != TokenType::Asterisk)
        {
            return FailureReason("Expected *");
        }
        if (curr->getTokenType() != TokenType::CloseSquareBracket)
        {
            return FailureReason("Expected ]");
        }
        curr++;
        begin = curr;
        return DirectDeclaratorAsterisk(line,column,std::move(declarator),std::move(typeQualifiers));
    }
}

Expected<DirectDeclarator, FailureReason> OpenCL::Parser::parseDirectDeclarator(Tokens::const_iterator& begin,
                                                                                Tokens::const_iterator end,
                                                                                ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    std::unique_ptr<DirectDeclarator> directDeclarator;
    while (true)
    {
        switch (curr->getTokenType())
        {
        case TokenType::Identifier:
        {
            auto line = curr->getLine();
            auto column = curr->getColumn();
            auto currToken = *curr;
            curr++;
            directDeclarator = std::make_unique<DirectDeclarator>(line,
                                                                  column,
                                                                  std::get<std::string>(currToken.getValue()));
            break;
        }
        case TokenType::OpenParenthese:
        {
            auto line = curr->getLine();
            auto column = curr->getColumn();
            curr++;
            if (directDeclarator)
            {
                if (isDeclarationSpecifier(*curr, context))
                {
                    auto parameterTypeList = parseParameterTypeList(curr, end, context);
                    if (!parameterTypeList)
                    {
                        return parameterTypeList;
                    }
                    directDeclarator = std::make_unique<DirectDeclarator>(line,
                                                                          column,
                                                                          DirectDeclaratorParentheseParameters(line,
                                                                                                               column,
                                                                                                               std::move(
                                                                                                                   *directDeclarator),
                                                                                                               std::move(
                                                                                                                   *parameterTypeList)));
                }
                else
                {
                    std::vector<std::string> identifiers;
                    while (curr->getTokenType() == TokenType::Identifier)
                    {
                        identifiers.push_back(std::get<std::string>(curr->getValue()));
                        curr++;
                        if (curr->getTokenType() == TokenType::Comma)
                        {
                            curr++;
                        }
                        else if (curr->getTokenType() != TokenType::CloseParenthese)
                        {
                            return FailureReason("Expected , to separate identifiers");
                        }
                    }
                    directDeclarator = std::make_unique<DirectDeclarator>(line,
                                                                          column,
                                                                          DirectDeclaratorParentheseIdentifiers(line,
                                                                                                                column,
                                                                                                                std::move(
                                                                                                                    *directDeclarator),
                                                                                                                std::move(
                                                                                                                    identifiers)));
                }
            }
            else
            {
                auto declarator = parseDeclarator(curr, end, context);
                if (!declarator)
                {
                    return declarator;
                }
                directDeclarator = std::make_unique<DirectDeclarator>(line,
                                                                      column,
                                                                      std::make_unique<Declarator>(std::move(*declarator)));
            }
            if (curr->getTokenType() != TokenType::CloseParenthese)
            {
                return FailureReason("Expected ) ");
            }
            curr++;
            break;
        }
        case TokenType::OpenSquareBracket:
        {
            if (!directDeclarator)
            {
                return FailureReason("Expected Direct Declarator before [");
            }
            auto line = directDeclarator->getLine();
            auto column = directDeclarator->getColumn();
            if (auto
                noAsteriskOrStatic = parseDirectDeclaratorNoStaticOrAsterisk(*directDeclarator, curr, end, context))
            {
                directDeclarator = std::make_unique<DirectDeclarator>(line, column, std::move(*noAsteriskOrStatic));
            }
            else if(auto staticDecl = parseDirectDeclaratorStatic(*directDeclarator,curr,end,context))
            {
                directDeclarator = std::make_unique<DirectDeclarator>(line,column,std::move(*staticDecl));
            }
            else if(auto asterisk = parseDirectDeclaratorAsterisk(*directDeclarator,curr,end,context))
            {
                directDeclarator = std::make_unique<DirectDeclarator>(line,column,std::move(*asterisk));
            }
            else
            {
                return noAsteriskOrStatic;
            }
            break;
        }
        default:
        {
            if (!directDeclarator)
            {
                return FailureReason("Invalid token for direct declarator");
            }
            begin = curr;
            return std::move(*directDeclarator);
        }
        }
    }
}

Expected<ParameterTypeList, FailureReason> OpenCL::Parser::parseParameterTypeList(Tokens::const_iterator& begin,
                                                                                  Tokens::const_iterator end,
                                                                                  OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto parameterList = parseParameterList(curr, end, context);
    if (!parameterList)
    {
        return parameterList;
    }
    bool hasEllipse = false;
    if (curr->getTokenType() == TokenType::Comma)
    {
        curr++;
        if (curr->getTokenType() != TokenType::Ellipse)
        {
            return FailureReason("Expected ... after , as last parameter in paramter type list");
        }
        curr++;
        hasEllipse = true;
    }
    begin = curr;
    return ParameterTypeList(line, column, std::move(*parameterList), hasEllipse);
}

Expected<ParameterList, FailureReason> OpenCL::Parser::parseParameterList(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end,
                                                                          OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    std::vector<ParameterDeclaration> parameterDeclarations;
    bool first = true;
    while (true)
    {
        auto before = curr;
        if (first)
        {
            first = false;
        }
        else if (curr->getTokenType() == TokenType::Comma)
        {
            curr++;
        }
        else
        {
            break;
        }
        std::vector<DeclarationSpecifier> declarationSpecifiers;
        while (auto result = parseDeclarationSpecifier(curr, end, context))
        {
            declarationSpecifiers.push_back(std::move(*result));
        }
        if (declarationSpecifiers.empty())
        {
            curr = before;
            break;
        }
        auto result = std::find_if(curr, end, [](const Token& token)
        {
            switch (token.getTokenType())
            {
            case TokenType::Asterisk:
            case TokenType::ConstKeyword:
            case TokenType::VolatileKeyword:
            case TokenType::RestrictKeyword:return false;
            default:break;
            }
            return true;
        });

        if (result->getTokenType() == TokenType::OpenSquareBracket)
        {
            auto abstractDeclarator = parseAbstractDeclarator(curr, end, context);
            if (!abstractDeclarator)
            {
                curr = before;
                break;
            }
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                               std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator)));
        }
        else if (result->getTokenType() == TokenType::Identifier)
        {
            auto declarator = parseDeclarator(curr, end, context);
            if (!declarator)
            {
                curr = before;
                break;
            }
            parameterDeclarations
                .emplace_back(std::move(declarationSpecifiers), std::make_unique<Declarator>(std::move(*declarator)));
        }
        else if (result->getTokenType() == TokenType::OpenParenthese)
        {
            while (result->getTokenType() == TokenType::OpenParenthese)
            {
                //Ambigious
                result++;
                if (result->getTokenType() == TokenType::Identifier)
                {
                    auto declarator = parseDeclarator(curr, end, context);
                    if (!declarator)
                    {
                        declarationSpecifiers.pop_back();
                        curr = before;
                        break;
                    }
                    parameterDeclarations
                        .emplace_back(std::move(declarationSpecifiers),
                                      std::make_unique<Declarator>(std::move(*declarator)));
                    break;
                }
                else if (result->getTokenType() != TokenType::OpenParenthese)
                {
                    auto abstractDeclarator = parseAbstractDeclarator(curr, end, context);
                    if (!abstractDeclarator)
                    {
                        declarationSpecifiers.pop_back();
                        curr = before;
                        break;
                    }
                    parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                                       std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator)));
                    break;
                }
            }
        }
        else
        {
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers), std::unique_ptr<AbstractDeclarator>());
        }
    }
    if (parameterDeclarations.empty())
    {
        return FailureReason("Expected at least one parameter declaration");
    }
    begin = curr;
    return ParameterList(line, column, std::move(parameterDeclarations));
}

Expected<Pointer, FailureReason> OpenCL::Parser::parsePointer(Tokens::const_iterator& begin,
                                                              Tokens::const_iterator end,
                                                              ParsingContext&)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    if (curr->getTokenType() != TokenType::Asterisk)
    {
        return FailureReason("Expected * at the beginning of pointer");
    }
    auto line = curr->getLine();
    auto column = curr->getColumn();
    curr++;
    std::vector<TypeQualifier> typeQualifier;
    while (curr->getTokenType() == TokenType::ConstKeyword
        || curr->getTokenType() == TokenType::RestrictKeyword
        || curr->getTokenType() == TokenType::VolatileKeyword)
    {
        switch (curr->getTokenType())
        {
        case TokenType::ConstKeyword:typeQualifier.push_back(TypeQualifier::Const);
            break;
        case TokenType::RestrictKeyword:typeQualifier.push_back(TypeQualifier::Restrict);
            break;
        case TokenType::VolatileKeyword:typeQualifier.push_back(TypeQualifier::Volatile);
            break;
        default:break;
        }
        curr++;
    }
    begin = curr;
    return Pointer(line, column, std::move(typeQualifier));
}

Expected<AbstractDeclarator,
         FailureReason> OpenCL::Parser::parseAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end,
                                                                OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    std::vector<Syntax::Pointer> pointers;
    while (auto result = parsePointer(curr, end, context))
    {
        pointers.push_back(std::move(*result));
    }
    auto result = parseDirectAbstractDeclarator(curr, end, context);
    if (!result)
    {
        return result;
    }
    begin = curr;
    return AbstractDeclarator(line, column, std::move(pointers), std::move(*result));
}

Expected<DirectAbstractDeclarator,
         FailureReason> OpenCL::Parser::parseDirectAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end,
                                                                      OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    std::unique_ptr<DirectAbstractDeclarator> directAbstractDeclarator;
    while (true)
    {
        auto before = curr;
        switch (curr->getTokenType())
        {
        case TokenType::OpenParenthese:
        {
            auto line = directAbstractDeclarator ? directAbstractDeclarator->getLine() : curr->getLine();
            auto colunn = directAbstractDeclarator ? directAbstractDeclarator->getColumn() : curr->getColumn();
            curr++;
            if (isDeclarationSpecifier(*curr, context))
            {
                auto parameterTypeList = parseParameterTypeList(curr, end, context);
                if (!parameterTypeList)
                {
                    curr = before;
                    goto Exit;
                }
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(line,
                                                                                      colunn,
                                                                                      DirectAbstractDeclaratorParameterTypeList(
                                                                                          line,
                                                                                          colunn,
                                                                                          std::move(
                                                                                              directAbstractDeclarator),
                                                                                          std::make_unique<
                                                                                              ParameterTypeList>(std::move(
                                                                                              *parameterTypeList))));
            }
            else if (curr->getTokenType() == TokenType::OpenParenthese
                || curr->getTokenType() == TokenType::OpenSquareBracket
                || curr->getTokenType() == TokenType::Asterisk)
            {
                auto abstractDeclarator = parseAbstractDeclarator(curr, end, context);
                if (!abstractDeclarator)
                {
                    curr = before;
                    goto Exit;
                }
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(line,
                                                                                      colunn,
                                                                                      std::make_unique<
                                                                                          AbstractDeclarator>(std::move(
                                                                                          *abstractDeclarator)));
            }
            else
            {
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(line,
                                                                                      colunn,
                                                                                      DirectAbstractDeclaratorParameterTypeList(
                                                                                          line,
                                                                                          colunn,
                                                                                          std::move(
                                                                                              directAbstractDeclarator),
                                                                                          nullptr));
            }
            if (curr->getTokenType() != TokenType::CloseParenthese)
            {
                curr = before;
                goto Exit;
            }
            curr++;
            break;
        }
        case TokenType::OpenSquareBracket:
        {
            auto line = directAbstractDeclarator ? directAbstractDeclarator->getLine() : curr->getLine();
            auto colunn = directAbstractDeclarator ? directAbstractDeclarator->getColumn() : curr->getColumn();
            curr++;
            if (curr->getTokenType() == TokenType::Asterisk)
            {
                curr++;
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(line,
                                                                                      colunn,
                                                                                      std::move(directAbstractDeclarator));
            }
            else
            {
                if (curr->getTokenType() != TokenType::CloseSquareBracket)
                {
                    auto assignment = parseAssignmentExpression(curr, end, context);
                    if (!assignment)
                    {
                        curr = before;
                        goto Exit;
                    }
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(line,
                                                                                          colunn,
                                                                                          DirectAbstractDeclaratorAssignmentExpression(
                                                                                              line,
                                                                                              colunn,
                                                                                              std::move(
                                                                                                  directAbstractDeclarator),
                                                                                              std::make_unique<
                                                                                                  AssignmentExpression>(
                                                                                                  std::move(*assignment))));
                }
                else
                {
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(line,
                                                                                          colunn,
                                                                                          DirectAbstractDeclaratorAssignmentExpression(
                                                                                              line,
                                                                                              colunn,
                                                                                              std::move(
                                                                                                  directAbstractDeclarator),
                                                                                              nullptr));
                }
            }
            if (curr->getTokenType() != TokenType::CloseSquareBracket)
            {
                curr = before;
                goto Exit;
            }
            curr++;
            break;
        }
        default:
        {
Exit:
            if (!directAbstractDeclarator)
            {
                return FailureReason("Invalid tokens for direct abstract declarator");
            }
            begin = curr;
            return std::move(*directAbstractDeclarator);
        }
        }
    }
}

Expected<EnumSpecifier, FailureReason> OpenCL::Parser::parseEnumSpecifier(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end,
                                                                          OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = begin->getLine();
    auto colunn = begin->getColumn();
    if (curr->getTokenType() != TokenType::EnumKeyword)
    {
        return FailureReason("Expected enum keyword at begin of enum specifier");
    }
    curr++;
    if (curr->getTokenType() == TokenType::OpenBrace)
    {
        auto declaration = parseEnumDeclaration(begin, end, context);
        if (!declaration)
        {
            return declaration;
        }
        return EnumSpecifier(line, colunn, std::move(*declaration));
    }
    else if (curr->getTokenType() != TokenType::Identifier)
    {
        return FailureReason("Expected Identifier or { after enum");
    }
    curr++;
    if (curr->getTokenType() == TokenType::OpenBrace)
    {
        auto declaration = parseEnumDeclaration(begin, end, context);
        if (!declaration)
        {
            return declaration;
        }
        return EnumSpecifier(line, colunn, std::move(*declaration));
    }
    begin++;
    auto name = std::get<std::string>(begin->getValue());
    begin++;
    return EnumSpecifier(line, colunn, std::move(name));
}

Expected<EnumDeclaration, FailureReason> OpenCL::Parser::parseEnumDeclaration(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    curr++;
    std::string name;
    if (curr->getTokenType() == TokenType::Identifier)
    {
        name = std::get<std::string>(curr->getValue());
        curr++;
    }
    if (curr->getTokenType() != TokenType::OpenBrace)
    {
        return FailureReason("Expected { after enum declaration");
    }
    curr++;
    std::vector<std::pair<std::string, std::int32_t>> values;
    do
    {
        if (curr->getTokenType() != TokenType::Identifier)
        {
            return FailureReason("Expected Identifier in enum value list");
        }
        const auto& valueName = std::get<std::string>(curr->getValue());
        curr++;
        std::int32_t value = values.empty() ? 0 : values.back().second + 1;
        if (curr->getTokenType() == TokenType::Assignment)
        {
            curr++;
            auto constant = parseAssignmentExpression(curr, end, context);
            if (!constant)
            {
                return constant;
            }
            Codegen::ConstantEvaluator evaluator(context.structOrUnions);
            auto constValue = *evaluator.visit(*constant);
            if(!constValue)
            {
                return constValue;
            }
            value = std::visit([](auto&& value) -> std::int32_t
                               {
                                   using T = std::decay_t<decltype(value)>;
                                   if constexpr(std::is_same_v<T, void*>)
                                   {
                                       return (std::int32_t)(std::intptr_t)value;
                                   }
                                   else
                                   {
                                       return value;
                                   }
                               },*constValue);
        }
        if (curr->getTokenType() == TokenType::Comma)
        {
            curr++;
        }
        else if (curr->getTokenType() != TokenType::CloseBrace)
        {
            throw std::runtime_error("Expected , after non final value in enum list");
        }
        values.emplace_back(valueName, value);
    }
    while (curr->getTokenType() != TokenType::CloseBrace);
    curr++;
    begin = curr;
    return EnumDeclaration(line, column, std::move(name), values);
}

OpenCL::Expected<OpenCL::Syntax::FunctionDefinition, OpenCL::FailureReason> OpenCL::Parser::parseFunctionDefinition(
    Tokens::const_iterator& begin,
    Tokens::const_iterator end,
    ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    std::vector<DeclarationSpecifier> declarationSpecifiers;
    auto current = begin;
    while (auto result = parseDeclarationSpecifier(current, end, context))
    {
        declarationSpecifiers.push_back(std::move(*result));
    }
    if (declarationSpecifiers.empty())
    {
        return FailureReason("Expected declaration specifiers at beginning of function definition");
    }
    auto declarator = parseDeclarator(current, end, context);
    if (!declarator)
    {
        return declarator;
    }
    std::vector<Declaration> declarations;
    while (auto result = parseDeclaration(current, end, context))
    {
        declarations.push_back(std::move(*result));
    }

    context.pushScope();
    if (auto
        * paramters = std::get_if<DirectDeclaratorParentheseParameters>(&declarator->getDirectDeclarator()
                                                                                   .getVariant()))
    {
        for (auto&[specifier, paramDeclarator] : paramters->getParameterTypeList().getParameterList()
                                                          .getParameterList())
        {
            (void)specifier;
            if (std::holds_alternative<std::unique_ptr<AbstractDeclarator>>(paramDeclarator))
            {
                return FailureReason("Only full declaration allowed in function definition");
            }
            auto& decl = std::get<std::unique_ptr<Declarator>>(paramDeclarator);
            auto visitor = [](auto self, auto&& value) -> std::string
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr(std::is_same_v<std::string, T>)
                {
                    (void)self;
                    return value;
                }
                else if constexpr(std::is_same_v<T, std::unique_ptr<Declarator>>)
                {
                    return std::visit([&](auto&& value) -> std::string
                                      {
                                          return self(value);
                                      }, value->getDirectDeclarator().getVariant());
                }
                else if constexpr(!std::is_same_v<T, DirectDeclaratorParentheseIdentifiers>
                    && !std::is_same_v<T, DirectDeclaratorParentheseParameters>)
                {
                    return std::visit([&](auto&& value) -> std::string
                                      {
                                          return self(value);
                                      }, value.getDirectDeclarator().getVariant());
                }
                else
                {
                    (void)self;
                    return "";
                }
            };
            auto result = std::visit(Y{visitor}, decl->getDirectDeclarator().getVariant());
            if (!result.empty())
            {
                context.addToScope(result);
            }
        }
    }
    else if(auto* identifierList = std::get_if<DirectDeclaratorParentheseIdentifiers>(&declarator->getDirectDeclarator().getVariant()))
    {
        for(auto& iter : identifierList->getIdentifiers())
        {
            context.addToScope(iter);
        }
    }
    auto compoundStatement = parseCompoundStatement(current, end, context);
    context.popScope();
    if (!compoundStatement)
    {
        return compoundStatement;
    }

    begin = current;
    {
        auto visitor = [](auto self, auto&& value) -> std::string
        {
            using T = std::decay_t<decltype(value)>;
            if constexpr(std::is_same_v<std::string, T>)
            {
                (void)self;
                return value;
            }
            else if constexpr(std::is_same_v<T, std::unique_ptr<Declarator>>)
            {
                return std::visit([&](auto&& value) -> std::string
                                  {
                                      return self(value);
                                  }, value->getDirectDeclarator().getVariant());
            }
            else
            {
                return std::visit([&](auto&& value) -> std::string
                                  {
                                      return self(value);
                                  }, value.getDirectDeclarator().getVariant());
            }
        };
        context.functions.insert(std::visit(Y{visitor}, declarator->getDirectDeclarator().getVariant()));
    }
    return FunctionDefinition(line,
                              column,
                              std::move(declarationSpecifiers),
                              std::move(*declarator),
                              std::move(declarations),
                              std::move(*compoundStatement));
}

Expected<CompoundStatement,
         FailureReason> OpenCL::Parser::parseCompoundStatement(OpenCL::Parser::Tokens::const_iterator& begin,
                                                               Tokens::const_iterator end,
                                                               OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    if (curr->getTokenType() != TokenType::OpenBrace)
    {
        return FailureReason("Expected { at start of Compound Statement");
    }
    curr++;
    std::vector<CompoundItem> items;
    context.pushScope();
    while (auto result = parseCompoundItem(curr, end, context))
    {
        items.push_back(std::move(*result));
    }
    context.popScope();
    if (curr->getTokenType() != TokenType::CloseBrace)
    {
        return FailureReason("Expected } at end of Compound Statement");
    }
    curr++;
    begin = curr;
    return CompoundStatement(line, column, std::move(items));
}

Expected<CompoundItem, FailureReason> OpenCL::Parser::parseCompoundItem(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end,
                                                                        ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    if (auto declaration = parseDeclaration(curr, end, context))
    {
        begin = curr;
        return CompoundItem(line, column, std::move(*declaration));
    }
    else
    {
        auto statement = parseStatement(curr, end, context);
        if (!statement)
        {
            return statement;
        }
        begin = curr;
        return CompoundItem(line, column, std::move(*statement));
    }
}

Expected<Initializer, FailureReason> OpenCL::Parser::parseInitializer(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    if (curr->getTokenType() != TokenType::OpenBrace)
    {
        auto assignment = parseAssignmentExpression(begin, end, context);
        if (!assignment)
        {
            return assignment;
        }
        return Initializer(curr->getLine(),
                           curr->getColumn(), std::move(*assignment));
    }
    else
    {
        curr++;
        auto initializerList = parseInitializerList(curr, end, context);
        if (!initializerList)
        {
            return initializerList;
        }
        if (curr == end || (curr->getTokenType() != TokenType::CloseBrace && curr->getTokenType() != TokenType::Comma))
        {
            return FailureReason("Expected } after initializer list");
        }
        if (curr->getTokenType() == TokenType::Comma)
        {
            curr++;
        }
        if (curr == end || curr->getTokenType() != TokenType::CloseBrace)
        {
            return FailureReason("Expected } after initializer list");
        }
        curr++;
        begin = curr;
        return Initializer{curr->getLine(), curr->getColumn(), std::move(*initializerList)};
    }
}

Expected<InitializerList, FailureReason> OpenCL::Parser::parseInitializerList(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    typename OpenCL::Syntax::InitializerList::vector vector;
    bool first = true;
    while (true)
    {
        auto before = curr;
        if (first)
        {
            first = false;
        }
        else if (curr->getTokenType() == TokenType::Comma)
        {
            curr++;
        }
        else
        {
            break;
        }
        std::vector<std::variant<std::size_t, std::string>> variants;
        while (curr->getTokenType() == TokenType::OpenSquareBracket
            || curr->getTokenType() == TokenType::Dot)
        {
            if (curr->getTokenType() == TokenType::OpenSquareBracket)
            {
                curr++;
                auto constant = parseAssignmentExpression(curr, end, context);
                if (!constant)
                {
                    if (vector.empty())
                    {
                        return constant;
                    }
                    else
                    {
                        curr = before;
                        goto Exit;
                    }
                }
                if (curr->getTokenType() != TokenType::CloseSquareBracket)
                {
                    if (vector.empty())
                    {
                        return FailureReason("Expected ] to close designator in initializer list");
                    }
                    else
                    {
                        curr = before;
                        goto Exit;
                    }
                }
                curr++;
                Codegen::ConstantEvaluator evaluator(context.structOrUnions);
                auto constValue = *evaluator.visit(*constant);
                if(!constValue)
                {
                    return constValue;
                }
                variants.emplace_back(std::visit([](auto&& value) -> std::size_t
                                                 {
                                                     using T = std::decay_t<decltype(value)>;
                                                     if constexpr(std::is_convertible_v<T, std::size_t>)
                                                     {
                                                         return value;
                                                     }
                                                     else
                                                     {
                                                         throw std::runtime_error("Invalid type of constant expression");
                                                     }
                                                 },*constValue));
            }
            else if (curr->getTokenType() == TokenType::Dot)
            {
                curr++;
                if (curr->getTokenType() != TokenType::Identifier)
                {
                    if (vector.empty())
                    {
                        return FailureReason("Expected identifier following dot in designation of initializer list");
                    }
                    else
                    {
                        curr = before;
                        goto Exit;
                    }
                }
                variants.emplace_back(std::get<std::string>(curr->getValue()));
                curr++;
            }
        }
        if (!variants.empty())
        {
            if (curr->getTokenType() == TokenType::Assignment)
            {
                curr++;
            }
            else if (vector.empty())
            {
                return FailureReason("Expected = after designators");
            }
            else
            {
                curr = before;
                goto Exit;
            }
        }
        auto initializer = parseInitializer(curr, end, context);
        if (!initializer)
        {
            if (vector.empty())
            {
                return initializer;
            }
            else
            {
                curr = before;
                goto Exit;
            }
        }
        vector.push_back({std::move(*initializer), variants});
    }
Exit:
    begin = curr;
    return InitializerList{line, column, std::move(vector)};
}

Expected<Statement, FailureReason> OpenCL::Parser::parseStatement(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end, ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto result = [&context, &curr, end]() -> Expected<Statement, FailureReason>
    {
        auto curentToken = *curr;
        auto line = curentToken.getLine();
        auto column = curentToken.getColumn();
        switch (curentToken.getTokenType())
        {
        case TokenType::ReturnKeyword:
        {
            curr++;
            auto expression = parseExpression(curr, end, context);
            if (!expression)
            {
                return expression;
            }
            return Statement{line, column, ReturnStatement(curentToken.getLine(),
                                                           curentToken.getColumn(), std::move(*expression))};
        }
        case TokenType::IfKeyword:
        {
            curr++;
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                return FailureReason("Expected ( after if");
            }
            auto expression = parseExpression(curr, end, context);
            if (!expression)
            {
                return expression;
            }
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                return FailureReason("Expected ) at the end of if statement");
            }
            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return statement;
            }
            curentToken = *curr;
            if (curr != end && curentToken.getTokenType() == TokenType::ElseKeyword)
            {
                curr++;
                auto elseStatement = parseStatement(curr, end, context);
                if (!elseStatement)
                {
                    return statement;
                }
                return Statement{line, column, IfStatement(line, column, std::move(*expression),
                                                           std::make_unique<Statement>(std::move(*statement)),
                                                           std::make_unique<Statement>(std::move(*elseStatement)))};
            }
            else
            {
                return Statement{line, column, IfStatement(line,
                                                           column,
                                                           std::move(*expression),
                                                           std::make_unique<Statement>(std::move(*statement)))};
            }
        }
        case TokenType::OpenBrace:
        {
            auto compoundStatement = parseCompoundStatement(curr, end, context);
            if (!compoundStatement)
            {
                return compoundStatement;
            }
            return Statement{line, column, std::move(*compoundStatement)};
        }
        case TokenType::ForKeyword:
        {
            curr++;
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                return FailureReason("Expected ( after for");
            }
            auto blockitem = parseCompoundItem(curr, end, context);
            if (!blockitem)
            {
                return blockitem;
            }

            std::unique_ptr<Expression> control;
            {
                if (std::holds_alternative<Declaration>(blockitem->getVariant())
                    || curr->getTokenType() != TokenType::SemiColon)
                {
                    auto expression = parseExpression(curr, end, context);
                    if (!expression)
                    {
                        return expression;
                    }
                    if (curr == end || curr->getTokenType() != TokenType::SemiColon)
                    {
                        return FailureReason("Expected ; after control part of for loop header");
                    }
                    curr++;
                    control = std::make_unique<Expression>(std::move(*expression));
                }
                else
                {
                    curr++;
                }
            }

            std::unique_ptr<Expression> post;
            {
                if (curr->getTokenType() != TokenType::CloseParenthese)
                {
                    auto expression = parseExpression(curr, end, context);
                    if (!expression)
                    {
                        return expression;
                    }
                    if (curr == end || curr->getTokenType() != TokenType::CloseParenthese)
                    {
                        return FailureReason("Expected ) after control part of for loop header");
                    }
                    curr++;
                    post = std::make_unique<Expression>(std::move(*expression));
                }
                else
                {
                    curr++;
                }
            }

            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return statement;
            }
            if (auto declaration = std::get_if<Declaration>(&blockitem->getVariant()))
            {
                return Statement(line, column, ForDeclarationStatement(line,
                                                                       column,
                                                                       std::make_unique<Statement>(std::move(*statement)),
                                                                       std::move(*declaration),
                                                                       std::move(control),
                                                                       std::move(post)));
            }
            else if (auto
                expressionStatement = std::get_if<ExpressionStatement>(&std::get<Statement>(blockitem->getVariant())
                .getVariant()))
            {
                return Statement(line,
                                 column,
                                 ForStatement(line, column, std::make_unique<Statement>(std::move(*statement)),
                                              expressionStatement->moveOptionalExpression(),
                                              std::move(control),
                                              std::move(post)));
            }
            else
            {
                return FailureReason("Invalid expression or declaration for initial part of for loop header");
            }
        }
        case TokenType::WhileKeyword:
        {
            curr++;
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                return FailureReason("Expected ( after while");
            }
            auto expression = parseExpression(curr, end, context);
            if (!expression)
            {
                return expression;
            }
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                return FailureReason("Expected ) after expression in while");
            }
            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return statement;
            }
            return Statement(line,
                             column,
                             HeadWhileStatement(line,
                                                column,
                                                std::move(*expression),
                                                std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::DoKeyword:
        {
            curr++;
            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return statement;
            }
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::WhileKeyword)
            {
                return FailureReason("Expected while after do");
            }
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                return FailureReason("Expected ( after while");
            }
            auto expression = parseExpression(curr, end, context);
            if (!expression)
            {
                return expression;
            }
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                throw std::runtime_error("Expected ) after expression in while");
            }
            return Statement(line, column, FootWhileStatement(line,
                                                              column,
                                                              std::make_unique<Statement>(std::move(*statement)),
                                                              std::move(*expression)));
        }
        case TokenType::BreakKeyword:
        {
            curr++;
            return Statement(line, column, BreakStatement(line, column));
        }
        case TokenType::ContinueKeyword:
        {
            curr++;
            return Statement(line, column, ContinueStatement(line, column));
        }
        case TokenType::SwitchKeyword:
        {
            curr++;
            curentToken = *curr;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                throw std::runtime_error("Expected ( after switch keyword");
            }
            curr++;
            auto expression = parseExpression(curr, end, context);
            if (!expression)
            {
                return expression;
            }
            curentToken = *curr;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                throw std::runtime_error("Expected ) after expression in switch ");
            }
            curr++;
            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return statement;
            }
            return Statement(line, column, SwitchStatement(line,
                                                           column,
                                                           std::move(*expression),
                                                           std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::DefaultKeyword:
        {
            curr++;
            curentToken = *curr;
            if (curentToken.getTokenType() != TokenType::Colon)
            {
                return FailureReason("Expected : after default");
            }
            curr++;
            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return statement;
            }
            return Statement(line,
                             column,
                             DefaultStatement(line,
                                              column,
                                              std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::CaseKeyword:
        {
            curr++;
            auto expression = parseAssignmentExpression(curr, end, context);
            if (!expression)
            {
                return expression;
            }
            curentToken = *curr;
            if (curentToken.getTokenType() != TokenType::Colon)
            {
                return FailureReason("Expected : after constant expression of case");
            }
            curr++;
            Codegen::ConstantEvaluator evaluator(context.structOrUnions);
            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return statement;
            }
            auto constValue = *evaluator.visit(*expression);
            if(!constValue)
            {
                return constValue;
            }
            return Statement(line, column, CaseStatement(line,
                                                         column,
                                                         *constValue,
                                                         std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::GotoKeyword:
        {
            curr++;
            if (curr->getTokenType() != TokenType::Identifier)
            {
                return FailureReason("Expected identifier following goto keyword");
            }
            const auto& name = std::get<std::string>(curr->getValue());
            curr++;
            return Statement(line, column, GotoStatement(line, column, name));
        }
        case TokenType::Identifier:
        {
            if ((curr + 1)->getTokenType() == TokenType::Colon)
            {
                const auto& name = std::get<std::string>(curr->getValue());
                curr += 2;
                return Statement(line, column, LabelStatement(line, column, name));
            }
            [[fallthrough]];
        }
        default:
        {
            if (curr != end && curr->getTokenType() != TokenType::SemiColon)
            {
                auto expression = parseExpression(curr, end, context);
                if (!expression)
                {
                    return expression;
                }
                return Statement(line, column, ExpressionStatement(line,
                                                                   column,
                                                                   std::make_unique<Expression>(std::move(*expression))));
            }
            else
            {
                return Statement(line, column, ExpressionStatement(line, column));
            }
        }
        }
    }();
    if (!result)
    {
        return result;
    }

    if ((std::holds_alternative<ExpressionStatement>(result->getVariant())
        || std::holds_alternative<ReturnStatement>(result->getVariant())
        || std::holds_alternative<FootWhileStatement>(result->getVariant())
        || std::holds_alternative<BreakStatement>(result->getVariant())
        || std::holds_alternative<ContinueStatement>(result->getVariant())
        || std::holds_alternative<GotoStatement>(result->getVariant()))
        && (curr == end || curr->getTokenType() != TokenType::SemiColon))
    {
        return FailureReason("Statement not terminated with ;");
    }
    else if (std::holds_alternative<ExpressionStatement>(result->getVariant())
        || std::holds_alternative<ReturnStatement>(result->getVariant())
        || std::holds_alternative<FootWhileStatement>(result->getVariant())
        || std::holds_alternative<BreakStatement>(result->getVariant())
        || std::holds_alternative<ContinueStatement>(result->getVariant())
        || std::holds_alternative<GotoStatement>(result->getVariant()))
    {
        curr++;
    }
    begin = curr;
    return result;
}

Expected<Expression, FailureReason> OpenCL::Parser::parseExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    std::vector<AssignmentExpression> expressions;
    auto assignment = parseAssignmentExpression(curr, end, context);
    if (!assignment)
    {
        return assignment;
    }
    expressions.push_back(std::move(*assignment));

    if (curr != end)
    {
        while (curr->getTokenType() == TokenType::Comma)
        {
            auto before = curr;
            curr++;
            assignment = parseAssignmentExpression(curr, end, context);
            if (!assignment)
            {
                curr = before;
                break;
            }
            expressions.push_back(std::move(*assignment));
        }
    }
    begin = curr;
    return Expression(line, column, std::move(expressions));
}

Expected<Syntax::AssignmentExpression,
         FailureReason> OpenCL::Parser::parseAssignmentExpression(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end,
                                                                  ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto before = curr;
    auto unary = parseUnaryExpression(curr, end, context);
    if (unary)
    {
        if (isAssignment(curr->getTokenType()))
        {
            auto currentToken = *curr;
            curr++;
            auto assignment = parseAssignmentExpression(curr, end, context);
            if (!assignment)
            {
                return assignment;
            }
            begin = curr;
            return AssignmentExpression(line, column, AssignmentExpressionAssignment(line,
                                                                                     column,
                                                                                     std::move(*unary),
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
                                                                                     std::make_unique<
                                                                                         AssignmentExpression>(std::move(
                                                                                         *assignment))));
        }
        else
        {
            curr = before;
        }
    }
    auto cond = parseConditionalExpression(curr, end, context);
    if (!cond)
    {
        return cond;
    }
    begin = curr;
    return AssignmentExpression(line, column, std::move(*cond));
}

Expected<ConditionalExpression, FailureReason> OpenCL::Parser::parseConditionalExpression(Tokens::const_iterator& begin,
                                                                                          Tokens::const_iterator end,
                                                                                          ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto logicalOrExperssion = parseLogicalOrExpression(curr, end, context);
    if (!logicalOrExperssion)
    {
        return logicalOrExperssion;
    }
    if (curr != end)
    {
        if (curr->getTokenType() == TokenType::QuestionMark)
        {
            curr++;
            auto optionalExpression = parseExpression(curr, end, context);
            if (!optionalExpression)
            {
                return optionalExpression;
            }
            if (curr->getTokenType() != TokenType::Colon)
            {
                return FailureReason("Expected : to match ?");
            }
            curr++;
            auto optionalConditional = parseConditionalExpression(curr, end, context);
            if (!optionalConditional)
            {
                return optionalConditional;
            }
            begin = curr;
            return ConditionalExpression(line, column, std::move(*logicalOrExperssion),
                                         std::make_unique<Expression>(std::move(*optionalExpression)),
                                         std::make_unique<ConditionalExpression>(std::move(*optionalConditional)));
        }
    }
    begin = curr;
    return ConditionalExpression(line, column, std::move(*logicalOrExperssion));
}

Expected<LogicalOrExpression, FailureReason> OpenCL::Parser::parseLogicalOrExpression(Tokens::const_iterator& begin,
                                                                                      Tokens::const_iterator end,
                                                                                      ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto logicalAnd = parseLogicalAndExpression(curr, end, context);
    if (!logicalAnd)
    {
        return logicalAnd;
    }

    std::vector<LogicalAndExpression> optionalLogicalAnds;
    while (curr != end && curr->getTokenType() == TokenType::LogicOr)
    {
        auto before = curr;
        curr++;
        auto newAnd = parseLogicalAndExpression(curr, end, context);
        if (!newAnd)
        {
            curr = before;
            break;
        }
        optionalLogicalAnds.push_back(std::move(*newAnd));
    }

    begin = curr;
    return LogicalOrExpression(line, column, std::move(*logicalAnd), std::move(optionalLogicalAnds));
}

Expected<LogicalAndExpression, FailureReason> OpenCL::Parser::parseLogicalAndExpression(Tokens::const_iterator& begin,
                                                                                        Tokens::const_iterator end,
                                                                                        ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseBitOrExpression(curr, end, context);
    if (!result)
    {
        return result;
    }

    std::vector<BitOrExpression> list;
    while (curr != end && curr->getTokenType() == TokenType::LogicAnd)
    {
        auto before = curr;
        curr++;
        auto newOr = parseBitOrExpression(curr, end, context);
        if (!newOr)
        {
            curr = before;
            break;
        }
        list.push_back(std::move(*newOr));
    }

    begin = curr;
    return LogicalAndExpression(line, column, std::move(*result), std::move(list));
}

Expected<BitOrExpression, FailureReason> OpenCL::Parser::parseBitOrExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseBitXorExpression(curr, end, context);
    if (!result)
    {
        return result;
    }

    std::vector<BitXorExpression> list;
    while (curr != end && curr->getTokenType() == TokenType::BitOr)
    {
        auto before = curr;
        curr++;
        auto newXor = parseBitXorExpression(curr, end, context);
        if (!newXor)
        {
            curr = before;
            break;
        }
        list.push_back(std::move(*newXor));
    }

    begin = curr;
    return BitOrExpression(line, column, std::move(*result), std::move(list));
}

Expected<BitXorExpression, FailureReason> OpenCL::Parser::parseBitXorExpression(Tokens::const_iterator& begin,
                                                                                Tokens::const_iterator end,
                                                                                ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }

    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseBitAndExpression(curr, end, context);
    if (!result)
    {
        return result;
    }

    std::vector<BitAndExpression> list;
    while (curr != end && curr->getTokenType() == TokenType::BitXor)
    {
        auto before = curr;
        curr++;
        auto newAnd = parseBitAndExpression(curr, end, context);
        if (!newAnd)
        {
            curr = before;
            break;
        }
        list.push_back(std::move(*newAnd));
    }

    begin = curr;
    return BitXorExpression(line, column, std::move(*result), std::move(list));
}

Expected<BitAndExpression, FailureReason> OpenCL::Parser::parseBitAndExpression(Tokens::const_iterator& begin,
                                                                                Tokens::const_iterator end,
                                                                                ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseEqualityExpression(curr, end, context);
    if (!result)
    {
        return result;
    }

    std::vector<EqualityExpression> list;
    while (curr != end && curr->getTokenType() == TokenType::Ampersand)
    {
        auto before = curr;
        curr++;
        auto newEqual = parseEqualityExpression(curr, end, context);
        if (!newEqual)
        {
            curr = before;
            break;
        }
        list.push_back(std::move(*newEqual));
    }

    begin = curr;
    return BitAndExpression(line, column, std::move(*result), std::move(list));
}

Expected<EqualityExpression, FailureReason> OpenCL::Parser::parseEqualityExpression(Tokens::const_iterator& begin,
                                                                                    Tokens::const_iterator end,
                                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseRelationalExpression(curr, end, context);
    if (!result)
    {
        return result;
    }

    std::vector<std::pair<EqualityExpression::EqualityOperator, RelationalExpression>> relationalExpressions;
    while (curr != end && (curr->getTokenType() == TokenType::Equal || curr->getTokenType() == TokenType::NotEqual))
    {
        auto before = curr;
        auto token = curr->getTokenType();
        curr++;
        auto newRelational = parseRelationalExpression(curr, end, context);
        if (!newRelational)
        {
            curr = before;
            break;
        }
        relationalExpressions
            .emplace_back(
                token == TokenType::Equal ? EqualityExpression::EqualityOperator::Equal
                                          : EqualityExpression::EqualityOperator::NotEqual, std::move(*newRelational));
    }

    begin = curr;
    return EqualityExpression(line, column, std::move(*result), std::move(relationalExpressions));
}

Expected<RelationalExpression, FailureReason> OpenCL::Parser::parseRelationalExpression(Tokens::const_iterator& begin,
                                                                                        Tokens::const_iterator end,
                                                                                        ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseShiftExpression(curr, end, context);
    if (!result)
    {
        return result;
    }

    std::vector<std::pair<RelationalExpression::RelationalOperator, ShiftExpression>> list;
    while (curr != end && (curr->getTokenType() == TokenType::LessThan
        || curr->getTokenType() == TokenType::LessThanOrEqual
        || curr->getTokenType() == TokenType::GreaterThan
        || curr->getTokenType() == TokenType::GreaterThanOrEqual))
    {
        auto before = curr;
        auto token = curr->getTokenType();
        curr++;
        auto newShift = parseShiftExpression(curr, end, context);
        if (!newShift)
        {
            curr = before;
            break;
        }

        list.emplace_back([token]() -> RelationalExpression::RelationalOperator
                          {
                              switch (token)
                              {
                              case TokenType::LessThan:return RelationalExpression::RelationalOperator::LessThan;
                              case TokenType::LessThanOrEqual:return RelationalExpression::RelationalOperator::LessThanOrEqual;
                              case TokenType::GreaterThan:return RelationalExpression::RelationalOperator::GreaterThan;
                              case TokenType::GreaterThanOrEqual:return RelationalExpression::RelationalOperator::GreaterThanOrEqual;
                              default:throw std::runtime_error("Invalid token for relational LogicalOrExpression");
                              }
                          }(), std::move(*newShift));
    }

    begin = curr;
    return RelationalExpression(line, column, std::move(*result), std::move(list));
}

Expected<ShiftExpression, FailureReason> OpenCL::Parser::parseShiftExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseAdditiveExpression(curr, end, context);
    if (!result)
    {
        return result;
    }

    std::vector<std::pair<ShiftExpression::ShiftOperator, AdditiveExpression>> list;
    while (curr != end && (curr->getTokenType() == TokenType::ShiftRight
        || curr->getTokenType() == TokenType::ShiftLeft))
    {
        auto before = curr;
        auto token = curr->getTokenType();
        curr++;
        auto newAdd = parseAdditiveExpression(curr, end, context);
        if (!newAdd)
        {
            curr = before;
            break;
        }
        list.emplace_back(
            token == TokenType::ShiftRight ? ShiftExpression::ShiftOperator::Right
                                           : ShiftExpression::ShiftOperator::Left, std::move(*newAdd));
    }

    begin = curr;
    return ShiftExpression(line, column, std::move(*result), std::move(list));
}

Expected<AdditiveExpression, FailureReason> OpenCL::Parser::parseAdditiveExpression(Tokens::const_iterator& begin,
                                                                                    Tokens::const_iterator end,
                                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseTerm(curr, end, context);
    if (!result)
    {
        return result;
    }

    std::vector<std::pair<AdditiveExpression::BinaryDashOperator, Term>> list;
    while (curr != end && (curr->getTokenType() == TokenType::Addition
        || curr->getTokenType() == TokenType::Negation))
    {
        auto before = curr;
        auto token = curr->getTokenType();
        curr++;
        auto newTerm = parseTerm(curr, end, context);
        if (!newTerm)
        {
            curr = before;
            break;
        }
        list.emplace_back(
            token == TokenType::Addition ? AdditiveExpression::BinaryDashOperator::BinaryPlus
                                         : AdditiveExpression::BinaryDashOperator::BinaryMinus,
            std::move(*newTerm));
    }

    begin = curr;
    return AdditiveExpression(line, column, std::move(*result), std::move(list));
}

Expected<Term, FailureReason> OpenCL::Parser::parseTerm(Tokens::const_iterator& begin,
                                                        Tokens::const_iterator end,
                                                        ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseCastExpression(curr, end, context);
    if(!result)
    {
        return result;
    }

    std::vector<std::pair<Term::BinaryDotOperator, CastExpression>> list;
    while (curr != end && (curr->getTokenType() == TokenType::Asterisk
        || curr->getTokenType() == TokenType::Division
        || curr->getTokenType() == TokenType::Modulo))
    {
        auto before = curr;
        auto token = curr->getTokenType();
        curr++;
        auto newCast = parseCastExpression(curr, end, context);
        if (!newCast)
        {
            curr = before;
            break;
        }
        list.emplace_back([token]
                          {
                              switch (token)
                              {
                              case TokenType::Asterisk:return Term::BinaryDotOperator::BinaryMultiply;
                              case TokenType::Division:return Term::BinaryDotOperator::BinaryDivide;
                              case TokenType::Modulo:return Term::BinaryDotOperator::BinaryRemainder;
                              default:throw std::runtime_error("Invalid token");
                              }
                          }(), std::move(*newCast));
    }

    begin = curr;
    return Term(line, column, std::move(*result), std::move(list));
}

Expected<TypeName, FailureReason> OpenCL::Parser::parseTypeName(Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end,
                                                                OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    std::vector<SpecifierQualifier> specifierQualifiers;
    while (auto result = parseSpecifierQualifier(curr, end, context))
    {
        specifierQualifiers.push_back(std::move(*result));
    }
    if (specifierQualifiers.empty())
    {
        return FailureReason("Expected atleast one specifier qualifier at beginning of typename");
    }

    if (auto abstractDec = parseAbstractDeclarator(curr, end, context))
    {
        begin = curr;
        return TypeName(line,
                        column,
                        std::move(specifierQualifiers),
                        std::make_unique<AbstractDeclarator>(std::move(*abstractDec)));
    }
    begin = curr;
    return TypeName(line, column, std::move(specifierQualifiers), nullptr);
}

Expected<CastExpression, FailureReason> OpenCL::Parser::parseCastExpression(Tokens::const_iterator& begin,
                                                                            Tokens::const_iterator end,
                                                                            ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto before = curr;
    if (before->getTokenType() == TokenType::OpenParenthese)
    {
        before++;
        auto typeName = parseTypeName(before, end, context);
        if (typeName)
        {
            if (before->getTokenType() == TokenType::CloseParenthese)
            {
                before++;
                auto cast = parseCastExpression(before, end, context);
                if (cast)
                {
                    begin = before;
                    return CastExpression(line,
                                          column,
                                          std::pair{std::move(*typeName),
                                                    std::make_unique<CastExpression>(std::move(*cast))});
                }
            }
        }
    }
    auto unary = parseUnaryExpression(curr, end, context);
    if (!unary)
    {
        return unary;
    }
    begin = curr;
    return CastExpression(line, column, std::move(*unary));
}

Expected<UnaryExpression, FailureReason> OpenCL::Parser::parseUnaryExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    if (curr->getTokenType() == TokenType::SizeofKeyword)
    {
        curr++;
        if (curr->getTokenType() == TokenType::OpenParenthese)
        {
            curr++;
            auto type = parseTypeName(curr, end, context);
            if (!type)
            {
                return type;
            }
            if (curr->getTokenType() != TokenType::CloseParenthese)
            {
                return FailureReason("Expected Close Parenthese after type in sizeof");
            }
            curr++;
            begin = curr;
            return UnaryExpression(line,
                                   column,
                                   UnaryExpressionSizeOf(line, column, std::make_unique<TypeName>(std::move(*type))));
        }
        else
        {
            auto unary = parseUnaryExpression(curr, end, context);
            if (!unary)
            {
                return unary;
            }
            begin = curr;
            return UnaryExpression(line,
                                   column,
                                   UnaryExpressionSizeOf(line,
                                                         column,
                                                         std::make_unique<UnaryExpression>(std::move(*unary))));
        }
    }
    else if (curr->getTokenType() == TokenType::Increment
        || curr->getTokenType() == TokenType::Decrement
        || curr->getTokenType() == TokenType::Ampersand
        || curr->getTokenType() == TokenType::Asterisk
        || curr->getTokenType() == TokenType::Addition
        || curr->getTokenType() == TokenType::Negation
        || curr->getTokenType() == TokenType::LogicalNegation
        || curr->getTokenType() == TokenType::BitWiseNegation)
    {
        auto token = curr->getTokenType();
        curr++;
        auto op = [token]
        {
            switch (token)
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
        auto unary = parseUnaryExpression(curr, end, context);
        if (!unary)
        {
            return unary;
        }
        begin = curr;
        return UnaryExpression(line, column, UnaryExpressionUnaryOperator(line,
                                                                          column,
                                                                          op,
                                                                          std::make_unique<UnaryExpression>(std::move(*unary))));
    }

    auto postFix = parsePostFixExpression(curr, end, context);
    if (!postFix)
    {
        return postFix;
    }
    begin = curr;
    return UnaryExpression(line, column, UnaryExpressionPostFixExpression(line, column, std::move(*postFix)));
}

namespace
{
    bool isPostFixExpression(const Token& token)
    {
        switch (token.getTokenType())
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

Expected<PostFixExpression, FailureReason> OpenCL::Parser::parsePostFixExpression(Tokens::const_iterator& begin,
                                                                                  Tokens::const_iterator end,
                                                                                  ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    std::stack<std::unique_ptr<PostFixExpression>> stack;
    while (curr != end && isPostFixExpression(*curr))
    {
        auto before = curr;
        if (curr->getTokenType() == TokenType::Identifier
            || curr->getTokenType() == TokenType::Literal)
        {
            if (!stack.empty())
            {
                curr = before;
                break;
            }
            auto line = curr->getLine();
            auto column = curr->getColumn();
            auto newPrimary = parsePrimaryExpression(curr, end, context);
            if (!newPrimary)
            {
                if (!stack.empty())
                {
                    curr = before;
                    break;
                }
                else
                {
                    return newPrimary;
                }
            }
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionPrimaryExpression(line,
                                                                                              column,
                                                                                              std::move(*newPrimary))));
        }
        else if (curr->getTokenType() == TokenType::OpenParenthese && stack.empty())
        {
            curr++;
            if (auto type = parseTypeName(curr, end, context))
            {
                if (curr->getTokenType() != TokenType::CloseParenthese)
                {
                    return FailureReason("Expected ) after type name in type initializer");
                }
                curr++;
                if (curr->getTokenType() != TokenType::OpenBrace)
                {
                    return FailureReason("Expected { after type around parenthesis");
                }
                curr++;
                auto initializer = parseInitializerList(curr, end, context);
                if (!initializer)
                {
                    return initializer;
                }
                if (curr->getTokenType() == TokenType::Comma)
                {
                    curr++;
                }
                if (curr->getTokenType() != TokenType::CloseBrace)
                {
                    return FailureReason("Expected { after type around parenthesis");
                }
                curr++;
                auto line = type->getLine();
                auto column = type->getColumn();
                stack.push(std::make_unique<PostFixExpression>(line, column, PostFixExpressionTypeInitializer(line,
                                                                                                              column,
                                                                                                              std::move(
                                                                                                                  *type),
                                                                                                              std::move(
                                                                                                                  *initializer))));
            }
            else
            {
                curr = before;
                auto line = curr->getLine();
                auto column = curr->getColumn();
                auto primary = parsePrimaryExpression(curr, end, context);
                if (!primary)
                {
                    return primary;
                }
                stack.push(std::make_unique<PostFixExpression>(line,
                                                               column,
                                                               PostFixExpressionPrimaryExpression(line,
                                                                                                  column,
                                                                                                  std::move(*primary))));
            }
        }
        else if (curr->getTokenType() == TokenType::OpenParenthese)
        {
            curr++;
            std::vector<std::unique_ptr<AssignmentExpression>> nonCommaExpressions;
            while (curr->getTokenType() != TokenType::CloseParenthese)
            {
                auto assignment = parseAssignmentExpression(curr, end, context);
                if (!assignment)
                {
                    curr = before;
                    break;
                }
                nonCommaExpressions.push_back(std::make_unique<AssignmentExpression>(std::move(*assignment)));
                if (curr->getTokenType() == TokenType::CloseParenthese)
                {
                    break;
                }
                else if (curr->getTokenType() != TokenType::Comma)
                {
                    curr = before;
                    break;
                }
                curr++;
            }
            curr++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = curr->getLine();
            auto column = curr->getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionFunctionCall(line,
                                                                                         column,
                                                                                         std::move(postExpression),
                                                                                         std::move(nonCommaExpressions))));
        }
        else if (curr->getTokenType() == TokenType::OpenSquareBracket)
        {
            curr++;
            auto expression = parseExpression(curr, end, context);
            if (!expression)
            {
                curr = before;
                break;
            }
            if (curr->getTokenType() != TokenType::CloseSquareBracket)
            {
                curr = before;
                break;
            }
            curr++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = curr->getLine();
            auto column = curr->getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionSubscript(line,
                                                                                      column,
                                                                                      std::move(postExpression),
                                                                                      std::move(*expression))));
        }
        else if (curr->getTokenType() == TokenType::Increment)
        {
            curr++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = curr->getLine();
            auto column = curr->getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionIncrement(line,
                                                                                      column,
                                                                                      std::move(postExpression))));
        }
        else if (curr->getTokenType() == TokenType::Decrement)
        {
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = curr->getLine();
            auto column = curr->getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionDecrement(line,
                                                                                      column,
                                                                                      std::move(postExpression))));
        }
        else if (curr->getTokenType() == TokenType::Dot)
        {
            curr++;
            if (curr->getTokenType() != TokenType::Identifier)
            {
                curr = before;
                break;
            }
            const auto& name = std::get<std::string>(curr->getValue());
            curr++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = curr->getLine();
            auto column = curr->getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionDot(line,
                                                                                column,
                                                                                std::move(postExpression),
                                                                                name)));
        }
        else if (curr->getTokenType() == TokenType::Arrow)
        {
            curr++;
            if (curr->getTokenType() != TokenType::Identifier)
            {
                curr = before;
                break;
            }
            const auto& name = std::get<std::string>(curr->getValue());
            curr++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = curr->getLine();
            auto column = curr->getColumn();
            stack.push(std::make_unique<PostFixExpression>(line,
                                                           column,
                                                           PostFixExpressionArrow(line,
                                                                                  column,
                                                                                  std::move(postExpression), name)));
        }
    }

    if (stack.size() != 1)
    {
        return FailureReason("Invalid amount of post fix expressions");
    }
    auto ret = std::move(*stack.top());
    stack.pop();
    begin = curr;
    return ret;
}

Expected<PrimaryExpression, FailureReason> OpenCL::Parser::parsePrimaryExpression(Tokens::const_iterator& begin,
                                                                                  Tokens::const_iterator end,
                                                                                  ParsingContext& context)
{
    if (begin == end)
    {
        return FailureReason("Unexpected end of tokens");
    }
    auto curr = begin;
    auto currToken = *curr;
    curr++;
    auto line = currToken.getLine();
    auto column = currToken.getColumn();
    if (currToken.getTokenType() == TokenType::Identifier)
    {
        const auto& name = std::get<std::string>(currToken.getValue());
        if (context.isInScope(name) || context.functions.count(name))
        {
            begin = curr;
            return PrimaryExpression(line, column, PrimaryExpressionIdentifier(line,
                                                                               column,
                                                                               name));
        }
        else
        {
            auto* result = context.getEnumConstant(name);
            if (!result)
            {
                return FailureReason("Unknown reference to variable or enum constant " + name);
            }
            begin = curr;
            return PrimaryExpression(line, column, PrimaryExpressionConstant(line, column, *result));
        }
    }
    else if (currToken.getTokenType() == TokenType::Literal)
    {
        begin = curr;
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
        auto expression = parseExpression(curr, end, context);
        if (!expression)
        {
            return expression;
        }
        if (curr->getTokenType() != TokenType::CloseParenthese)
        {
            return FailureReason("Expected Close Parenthese after expression in primary expression");
        }
        curr++;
        begin = curr;
        return PrimaryExpression(line, column, PrimaryExpressionParenthese(line, column, std::move(*expression)));
    }
    else
    {
        return FailureReason("Invalid token for primary expression");
    }
}
