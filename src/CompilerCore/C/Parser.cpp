#include "Parser.hpp"

#include "ConstantEvaluator.hpp"

#include <algorithm>
#include <numeric>
#include <stack>

using namespace OpenCL;
using namespace OpenCL::Lexer;
using namespace OpenCL::Syntax;

std::pair<TranslationUnit, bool> OpenCL::Parser::buildTree(const std::vector<Token>& tokens)
{
    ParsingContext context;
    auto begin = tokens.cbegin();
    return {parseTranslationUnit(begin, tokens.cend(), context), context.getErrors().empty()};
}

namespace
{
    bool isAssignment(TokenType type)
    {
        return type == TokenType::Assignment || type == TokenType::PlusAssign || type == TokenType::MinusAssign
            || type == TokenType::DivideAssign || type == TokenType::MultiplyAssign
            || type == TokenType::ModuloAssign || type == TokenType::ShiftLeftAssign
            || type == TokenType::ShiftRightAssign || type == TokenType::BitAndAssign
            || type == TokenType::BitOrAssign || type == TokenType::BitXorAssign;
    }
} // namespace

OpenCL::Syntax::TranslationUnit
OpenCL::Parser::parseTranslationUnit(Tokens::const_iterator& begin, Tokens::const_iterator end,
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
    }
    return TranslationUnit(std::move(global));
}

std::optional<Syntax::ExternalDeclaration>
OpenCL::Parser::parseExternalDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                         ParsingContext& context)
{
    auto line = begin->getLine();
    auto column = begin->getColumn();
    auto curr = begin;
    //backtracking
    auto prevErrorCount = context.getErrors().size();
    auto function = parseFunctionDefinition(curr, end, context);
    auto pos = curr;
    if (function && context.getErrors().size() == prevErrorCount)
    {
        begin = curr;
        return ExternalDeclaration(line, column, std::move(*function));
    }
    auto copy = std::vector(context.getErrors().begin() + prevErrorCount, context.getErrors().end());
    context.getErrors().resize(prevErrorCount);
    if (auto declaration = parseDeclaration(curr = begin, end, context);declaration
        && context.getErrors().size() == prevErrorCount)
    {
        begin = curr;
        return ExternalDeclaration(line, column, std::move(*declaration));
    }
    else
    {
        bool declWentFurther = std::distance(begin, curr) > std::distance(begin, pos);
        begin = declWentFurther ? curr : pos;
        if (!declWentFurther)
        {
            context.getErrors().resize(prevErrorCount);
            context.getErrors().insert(context.getErrors().end(), copy.begin(), copy.end());
            return {};
        }
        return {};
    }
}

namespace
{
    bool isDeclarationSpecifier(const Token& token, const OpenCL::Parser::ParsingContext& context)
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
        case TokenType::InlineKeyword: return true;
        case TokenType::Identifier:
            return !context.isInScope(std::get<std::string>(token.getValue()))
                && context.isTypedef(std::get<std::string>(token.getValue()));
        default: return false;
        }
    }

    bool isSpecifierQualifier(const Token& token, const OpenCL::Parser::ParsingContext& context)
    {
        switch (token.getTokenType())
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
        case TokenType::EnumKeyword:
        case TokenType::StructKeyword:
        case TokenType::UnionKeyword:
        case TokenType::ConstKeyword:
        case TokenType::RestrictKeyword:
        case TokenType::VolatileKeyword:return true;
        case TokenType::Identifier:
            return !context.isInScope(std::get<std::string>(token.getValue()))
                && context.isTypedef(std::get<std::string>(token.getValue()));
        default: return false;
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
    Y(G)->Y<G>;
} // namespace

std::optional<Syntax::Declaration>
OpenCL::Parser::parseDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    std::vector<DeclarationSpecifier> declarationSpecifiers;
    while (begin < end && isDeclarationSpecifier(*begin, context))
    {
        auto result = parseDeclarationSpecifier(begin, end, context);
        if (!result)
        {
            return {};
        }
        declarationSpecifiers.push_back(std::move(*result));
    }
    if (declarationSpecifiers.empty())
    {
        context.logError("Expected declaration specifiers at beginning of declaration");
    }
    if (begin >= end || begin->getTokenType() == TokenType::SemiColon)
    {
        if (begin < end)
        {
            begin++;
        }
        else
        {
            context.logError("Missing ; at the end of declaration");
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
        else if (begin < end && begin->getTokenType() == TokenType::Comma)
        {
            begin++;
        }
        else
        {
            break;
        }
        auto declarator = parseDeclarator(begin, end, context);
        if (!declarator)
        {
            return {};
        }
        context.addToScope(Semantics::declaratorToName(*declarator));
        if (begin >= end || begin->getTokenType() != TokenType::Assignment)
        {
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
        }
        else
        {
            begin++;
            auto initializer = parseInitializer(begin, end, context);
            if (!initializer)
            {
                return {};
            }
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::make_unique<Initializer>(std::move(*initializer)));
        }
    }
    while (true);
    if (begin >= end || begin->getTokenType() != TokenType::SemiColon)
    {
        context.logError("Expected ; at the end of declaration");
    }
    else
    {
        begin++;
    }
    if (auto* storage = std::get_if<StorageClassSpecifier>(&declarationSpecifiers.front());
        storage && *storage == StorageClassSpecifier::Typedef)
    {
        for (auto&[declator, init] : initDeclarators)
        {
            (void)init;
            auto visitor = [](auto self, auto&& value) -> std::string
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<std::string, T>)
                {
                    (void)self;
                    return value;
                }
                else if constexpr (std::is_same_v<T, std::unique_ptr<Declarator>>)
                {
                    return std::visit([&](auto&& value) -> std::string
                                      { return self(value); },
                                      value->getDirectDeclarator().getVariant());
                }
                else
                {
                    return std::visit([&](auto&& value) -> std::string
                                      { return self(value); },
                                      value.getDirectDeclarator().getVariant());
                }
            };
            context.typedefs.back().insert(std::visit(Y{visitor}, declator->getDirectDeclarator().getVariant()));
        }
    }
    return Declaration(line, column, std::move(declarationSpecifiers), std::move(initDeclarators));
}

std::optional<Syntax::DeclarationSpecifier>
OpenCL::Parser::parseDeclarationSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                          OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    auto currToken = *begin;
    switch (begin->getTokenType())
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
    case TokenType::Identifier:
    case TokenType::UnsignedKeyword: begin++;
    default: break;
    }
    switch (currToken.getTokenType())
    {
    case TokenType::TypedefKeyword: return DeclarationSpecifier{StorageClassSpecifier::Typedef};
    case TokenType::ExternKeyword: return DeclarationSpecifier{StorageClassSpecifier::Extern};
    case TokenType::StaticKeyword: return DeclarationSpecifier{StorageClassSpecifier::Static};
    case TokenType::AutoKeyword: return DeclarationSpecifier{StorageClassSpecifier::Auto};
    case TokenType::RegisterKeyword: return DeclarationSpecifier{StorageClassSpecifier::Register};
    case TokenType::ConstKeyword: return DeclarationSpecifier{TypeQualifier::Const};
    case TokenType::RestrictKeyword: return DeclarationSpecifier{TypeQualifier::Restrict};
    case TokenType::VolatileKeyword: return DeclarationSpecifier{TypeQualifier::Volatile};
    case TokenType::InlineKeyword: return DeclarationSpecifier{FunctionSpecifier{}};
    case TokenType::VoidKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Void)};
    case TokenType::CharKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Char)};
    case TokenType::ShortKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Short)};
    case TokenType::IntKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Int)};
    case TokenType::LongKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Long)};
    case TokenType::FloatKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Float)};
    case TokenType::DoubleKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Double)};
    case TokenType::SignedKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Signed)};
    case TokenType::UnsignedKeyword:
        return Syntax::DeclarationSpecifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Unsigned)};
    case TokenType::UnionKeyword:
    case TokenType::StructKeyword:
    {
        auto expected = parseStructOrUnionSpecifier(begin, end, context);
        if (expected)
        {
            auto name = expected->getIdentifier();
            auto isDefinition = !expected->getStructDeclarations().empty();
            auto result =
                TypeSpecifier(line, column, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)));
            if (isDefinition)
            {
                auto type = Semantics::declaratorsToType({std::cref(result)});
                if (!type)
                {
                    return {};
                }
                context.structOrUnions.emplace(name, std::get<Semantics::RecordType>(type->get()));
            }
            return DeclarationSpecifier{std::move(result)};
        }
        else
        {
            return {};
        }
    }
    case TokenType::EnumKeyword:
    {
        auto expected = parseEnumSpecifier(begin, end, context);
        if (expected)
        {
            return DeclarationSpecifier{
                TypeSpecifier(line, column, std::make_unique<EnumSpecifier>(std::move(*expected)))};
        }
        else
        {
            return {};
        }
    }
    case TokenType::Identifier:
    {
        auto name = std::get<std::string>(currToken.getValue());
        if (!context.isInScope(name) && context.isTypedef(name))
        {
            return Syntax::DeclarationSpecifier{TypeSpecifier(line, column, name)};
        }
        else if (context.isTypedef(name))
        {
            context.logError(
                "\"" + name + "\" is a typedef but cannot be used as such because another symbol overshadows it");
            return {};
        }
        break;
    }
    default: break;
    }
    context.logError("Invalid token for declaration specifier");
    return {};
}

std::optional<OpenCL::Syntax::StructOrUnionSpecifier>
OpenCL::Parser::parseStructOrUnionSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                            OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    bool isUnion;
    if (begin->getTokenType() == TokenType::StructKeyword)
    {
        isUnion = false;
    }
    else if (begin->getTokenType() == TokenType::UnionKeyword)
    {
        isUnion = true;
    }
    else
    {
        context.logError("Expected struct or union keyword at beginning of struct or union specifier");
        return {};
    }
    begin++;
    if (begin >= end || begin->getTokenType() != TokenType::Identifier)
    {
        context.logError(std::string("Expected identifier after ") + (isUnion ? "union" : "struct"));
        return StructOrUnionSpecifier(line, column, isUnion, "", {});
    }
    const auto& name = std::get<std::string>(begin->getValue());
    begin++;
    if (begin >= end || begin->getTokenType() != TokenType::OpenBrace)
    {
        return StructOrUnionSpecifier(line, column, isUnion, name, {});
    }
    begin++;
    std::vector<StructOrUnionSpecifier::StructDeclaration> structDeclarations;
    do
    {
        std::vector<SpecifierQualifier> specifierQualifiers;
        while (begin < end && isSpecifierQualifier(*begin, context))
        {
            auto result = parseSpecifierQualifier(begin, end, context);
            if (!result)
            {
                return {};
            }
            specifierQualifiers.push_back(std::move(*result));
        }
        if (specifierQualifiers.empty())
        {
            context.logError("Expected Specifier Qualifiers at beginning of struct declarations");
        }
        std::vector<std::pair<std::unique_ptr<Declarator>, std::int64_t>> declarators;
        bool first = true;
        do
        {
            if (first)
            {
                first = false;
            }
            else if (begin < end && begin->getTokenType() == TokenType::Comma)
            {
                begin++;
            }
            else
            {
                break;
            }
            if (begin < end && begin->getTokenType() == TokenType::Colon)
            {
                begin++;
                auto constant = parseAssignmentExpression(begin, end, context);
                if (!constant)
                {
                    return {};
                }
                Semantics::ConstantEvaluator evaluator(context.structOrUnions);
                auto value = evaluator.visit(*constant);
                if (!value)
                {
                    return {};
                }
                declarators.emplace_back(nullptr,
                                         std::visit(
                                             [](auto&& value) -> std::size_t
                                             {
                                                 using T = std::decay_t<decltype(value)>;
                                                 if constexpr (std::is_convertible_v<T, std::size_t>)
                                                 {
                                                     return value;
                                                 }
                                                 else
                                                 {
                                                     throw std::runtime_error("Invalid type of constant expression");
                                                 }
                                             },
                                             *value));
            }
            auto declarator = parseDeclarator(begin, end, context);
            if (!declarator)
            {
                return {};
            }
            if (begin < end && begin->getTokenType() == TokenType::Colon)
            {
                begin++;
                auto constant = parseAssignmentExpression(begin, end, context);
                if (!constant)
                {
                    return {};
                }
                Semantics::ConstantEvaluator evaluator(context.structOrUnions);
                auto value = evaluator.visit(*constant);
                if (!value)
                {
                    return {};
                }
                declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::visit(
                                             [](auto&& value) -> std::size_t
                                             {
                                                 using T = std::decay_t<decltype(value)>;
                                                 if constexpr (std::is_convertible_v<T, std::size_t>)
                                                 {
                                                     return value;
                                                 }
                                                 else
                                                 {
                                                     throw std::runtime_error("Invalid type of constant expression");
                                                 }
                                             },
                                             *value));
            }
            else
            {
                declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), -1);
            }
        }
        while (true);
        if (begin >= end || begin->getTokenType() != TokenType::SemiColon)
        {
            context.logError(std::string("Expected ; at the end of ") + (isUnion ? "union" : "struct")
                                 + " field declaration");
        }
        begin++;
        structDeclarations.push_back({std::move(specifierQualifiers), std::move(declarators)});
    }
    while (begin < end && begin->getTokenType() != TokenType::CloseBrace);
    if (begin >= end || begin->getTokenType() != TokenType::CloseBrace)
    {
        context.logError(std::string("Expected } at the end of ") + (isUnion ? "union" : "struct") + " definition");
    }
    else
    {
        begin++;
    }
    return StructOrUnionSpecifier(line, column, isUnion, name, std::move(structDeclarations));
}

std::optional<OpenCL::Syntax::SpecifierQualifier>
OpenCL::Parser::parseSpecifierQualifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                        OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto currToken = *begin;
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
    case TokenType::UnsignedKeyword: begin++; [[fallthrough]];
    case TokenType::Identifier: begin++;
    default: break;
    }
    auto line = currToken.getLine();
    auto column = currToken.getColumn();
    switch (currToken.getTokenType())
    {
    case TokenType::ConstKeyword: return SpecifierQualifier{TypeQualifier::Const};
    case TokenType::RestrictKeyword: return SpecifierQualifier{TypeQualifier::Restrict};
    case TokenType::VolatileKeyword: return SpecifierQualifier{TypeQualifier::Volatile};
    case TokenType::VoidKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Void)};
    case TokenType::CharKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Char)};
    case TokenType::ShortKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Short)};
    case TokenType::IntKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Int)};
    case TokenType::LongKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Long)};
    case TokenType::FloatKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Float)};
    case TokenType::DoubleKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Double)};
    case TokenType::SignedKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Signed)};
    case TokenType::UnsignedKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(line, column, TypeSpecifier::PrimitiveTypeSpecifier::Unsigned)};
    case TokenType::UnionKeyword:
    case TokenType::StructKeyword:
    {
        auto expected = parseStructOrUnionSpecifier(begin, end, context);
        if (expected)
        {
            auto name = expected->getIdentifier();
            bool isDefinition = !expected->getStructDeclarations().empty();
            auto result =
                TypeSpecifier(line, column, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)));
            if (isDefinition)
            {
                auto type = Semantics::declaratorsToType({std::cref(result)});
                if (!type)
                {
                    return {};
                }
                context.structOrUnions.emplace(name, std::get<Semantics::RecordType>(type->get()));
            }
            return SpecifierQualifier{std::move(result)};
        }
        else
        {
            return {};
        }
    }
    case TokenType::EnumKeyword:
    {
        auto expected = parseEnumSpecifier(begin, end, context);
        if (expected)
        {
            return SpecifierQualifier{
                TypeSpecifier(line, column, std::make_unique<EnumSpecifier>(std::move(*expected)))};
        }
        else
        {
            return {};
        }
    }
    case TokenType::Identifier:
    {
        auto name = std::get<std::string>(currToken.getValue());
        if (!context.isInScope(name) && context.isTypedef(name))
        {
            return Syntax::SpecifierQualifier{TypeSpecifier(line, column, name)};
        }
        else if (context.isTypedef(name))
        {
            context.logError(
                "\"" + name + "\" is a typedef but cannot be used as such because another symbol overshadows it");
            return {};
        }
        break;
    }
    default: break;
    }
    context.logError("Invalid token for declaration specifier");
    return {};
}

std::optional<OpenCL::Syntax::Declarator>
OpenCL::Parser::parseDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == TokenType::Asterisk)
    {
        auto result = parsePointer(begin, end, context);
        if (!result)
        {
            return {};
        }
        pointers.push_back(std::move(*result));
    }
    auto directDeclarator = parseDirectDeclarator(begin, end, context);
    if (!directDeclarator)
    {
        return {};
    }
    return Declarator(line, column, std::move(pointers), std::move(*directDeclarator));
}

namespace
{
    std::optional<DirectDeclaratorNoStaticOrAsterisk> parseDirectDeclaratorNoStaticOrAsterisk(
        DirectDeclarator& declarator, OpenCL::Parser::Tokens::const_iterator& begin,
        OpenCL::Parser::Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
    {
        if (begin >= end)
        {
            context.logError("Unexpected end of tokens");
            return {};
        }
        auto line = begin->getLine();
        auto column = begin->getColumn();
        if (begin >= end || begin->getTokenType() != TokenType::OpenSquareBracket)
        {
            context.logError("Expected [");
        }
        else
        {
            begin++;
        }
        std::vector<TypeQualifier> typeQualifiers;
        while (begin < end
            && (begin->getTokenType() == TokenType::ConstKeyword || begin->getTokenType() == TokenType::RestrictKeyword
                || begin->getTokenType() == TokenType::VolatileKeyword))
        {
            switch (begin->getTokenType())
            {
            case TokenType::ConstKeyword: typeQualifiers.push_back(TypeQualifier::Const);
                break;
            case TokenType::RestrictKeyword: typeQualifiers.push_back(TypeQualifier::Restrict);
                break;
            case TokenType::VolatileKeyword: typeQualifiers.push_back(TypeQualifier::Volatile);
                break;
            default: break;
            }
            begin++;
        }
        auto assignment = OpenCL::Parser::parseAssignmentExpression(begin, end, context);
        if (begin >= end || begin->getTokenType() != TokenType::CloseSquareBracket)
        {
            context.logError("Expected ] to close [ in direct declarator");
        }
        else
        {
            begin++;
        }
        return DirectDeclaratorNoStaticOrAsterisk(
            line, column, std::make_unique<DirectDeclarator>(std::move(declarator)), std::move(typeQualifiers),
            assignment ? std::make_unique<AssignmentExpression>(std::move(*assignment)) : nullptr);
    }

    std::optional<DirectDeclaratorStatic>
    parseDirectDeclaratorStatic(DirectDeclarator& declarator, OpenCL::Parser::Tokens::const_iterator& begin,
                                OpenCL::Parser::Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
    {
        if (begin >= end)
        {
            context.logError("Unexpected end of tokens");
            return {};
        }
        auto line = begin->getLine();
        auto column = begin->getColumn();
        if (begin >= end || begin->getTokenType() != TokenType::OpenSquareBracket)
        {
            context.logError("Expected [");
        }
        else
        {
            begin++;
        }
        bool wasStatic = false;
        if (begin < end && begin->getTokenType() == TokenType::StaticKeyword)
        {
            wasStatic = true;
            begin++;
        }
        std::vector<TypeQualifier> typeQualifiers;
        while (begin < end
            && (begin->getTokenType() == TokenType::ConstKeyword || begin->getTokenType() == TokenType::RestrictKeyword
                || begin->getTokenType() == TokenType::VolatileKeyword))
        {
            switch (begin->getTokenType())
            {
            case TokenType::ConstKeyword: typeQualifiers.push_back(TypeQualifier::Const);
                break;
            case TokenType::RestrictKeyword: typeQualifiers.push_back(TypeQualifier::Restrict);
                break;
            case TokenType::VolatileKeyword: typeQualifiers.push_back(TypeQualifier::Volatile);
                break;
            default: break;
            }
            begin++;
        }
        if (begin < end && begin->getTokenType() == TokenType::StaticKeyword)
        {
            if (wasStatic)
            {
                context.logError("static appearing twice in direct declarator");
            }
            begin++;
        }
        auto assignmentExpression = OpenCL::Parser::parseAssignmentExpression(begin, end, context);
        if (!assignmentExpression)
        {
            return {};
        }
        if (begin >= end || begin->getTokenType() != TokenType::CloseSquareBracket)
        {
            context.logError("Expected ]");
        }
        else
        {
            begin++;
        }
        return DirectDeclaratorStatic(line, column, std::make_unique<DirectDeclarator>(std::move(declarator)),
                                      std::move(typeQualifiers), std::move(*assignmentExpression));
    }

    std::optional<DirectDeclaratorAsterisk>
    parseDirectDeclaratorAsterisk(DirectDeclarator& declarator, OpenCL::Parser::Tokens::const_iterator& begin,
                                  OpenCL::Parser::Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
    {
        if (begin >= end || begin->getTokenType() != TokenType::OpenSquareBracket)
        {
            context.logError("Expected [");
        }
        auto line = begin < end ? begin->getLine() : 0;
        auto column = begin < end ? begin->getColumn() : 0;
        begin++;
        std::vector<TypeQualifier> typeQualifiers;
        while (begin < end
            && (begin->getTokenType() == TokenType::ConstKeyword || begin->getTokenType() == TokenType::RestrictKeyword
                || begin->getTokenType() == TokenType::VolatileKeyword))
        {
            switch (begin->getTokenType())
            {
            case TokenType::ConstKeyword: typeQualifiers.push_back(TypeQualifier::Const);
                break;
            case TokenType::RestrictKeyword: typeQualifiers.push_back(TypeQualifier::Restrict);
                break;
            case TokenType::VolatileKeyword: typeQualifiers.push_back(TypeQualifier::Volatile);
                break;
            default: break;
            }
            begin++;
        }
        if (begin >= end || begin->getTokenType() != TokenType::Asterisk)
        {
            context.logError("Expected *");
        }
        if (begin >= end || begin->getTokenType() != TokenType::CloseSquareBracket)
        {
            context.logError("Expected ]");
        }
        else
        {
            begin++;
        }
        return DirectDeclaratorAsterisk(line, column, std::move(declarator), std::move(typeQualifiers));
    }
} // namespace

std::optional<DirectDeclarator> OpenCL::Parser::parseDirectDeclarator(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    std::unique_ptr<DirectDeclarator> directDeclarator;
    while (begin < end
        && (begin->getTokenType() == TokenType::Identifier || begin->getTokenType() == TokenType::OpenParenthese
            || begin->getTokenType() == TokenType::OpenSquareBracket))
    {
        switch (begin->getTokenType())
        {
        case TokenType::Identifier:
        {
            auto line = begin->getLine();
            auto column = begin->getColumn();
            auto currToken = *begin;
            begin++;
            directDeclarator =
                std::make_unique<DirectDeclarator>(line, column, std::get<std::string>(currToken.getValue()));
            break;
        }
        case TokenType::OpenParenthese:
        {
            auto line = begin->getLine();
            auto column = begin->getColumn();
            begin++;
            if (directDeclarator)
            {
                if (begin < end && isDeclarationSpecifier(*begin, context))
                {
                    auto parameterTypeList = parseParameterTypeList(begin, end, context);
                    if (!parameterTypeList)
                    {
                        return {};
                    }
                    directDeclarator = std::make_unique<DirectDeclarator>(
                        line, column,
                        DirectDeclaratorParentheseParameters(line, column, std::move(*directDeclarator),
                                                             std::move(*parameterTypeList)));
                }
                else
                {
                    std::vector<std::string> identifiers;
                    while (begin < end && begin->getTokenType() == TokenType::Identifier)
                    {
                        identifiers.push_back(std::get<std::string>(begin->getValue()));
                        begin++;
                        if (begin < end && begin->getTokenType() == TokenType::Comma)
                        {
                            begin++;
                        }
                        else if (begin < end && begin->getTokenType() != TokenType::CloseParenthese)
                        {
                            context.logError("Expected , to separate identifiers");
                        }
                    }
                    directDeclarator = std::make_unique<DirectDeclarator>(
                        line, column,
                        DirectDeclaratorParentheseIdentifiers(line, column, std::move(*directDeclarator),
                                                              std::move(identifiers)));
                }
            }
            else
            {
                auto declarator = parseDeclarator(begin, end, context);
                if (!declarator)
                {
                    return {};
                }
                directDeclarator = std::make_unique<DirectDeclarator>(
                    line, column, std::make_unique<Declarator>(std::move(*declarator)));
            }
            if (begin >= end || begin->getTokenType() != TokenType::CloseParenthese)
            {
                context.logError("Expected ) ");
            }
            else
            {
                begin++;
            }
            break;
        }
        case TokenType::OpenSquareBracket:
        {
            if (!directDeclarator)
            {
                context.logError("Expected declarator before [");
                return {};
            }
            auto line = directDeclarator->getLine();
            auto column = directDeclarator->getColumn();
            begin++;
            if (begin >= end)
            {
                context.logError("Expected ] to match [");
                return {};
            }
            if (std::any_of(begin, std::find_if(begin, end, [](const auto& token)
            { return token.getTokenType() == TokenType::CloseSquareBracket; }), [](const auto& token)
                            {
                                return token.getTokenType() == TokenType::Asterisk;
                            }))
            {
                auto asterisk = parseDirectDeclaratorAsterisk(*directDeclarator, begin, end, context);
                if (!asterisk)
                {
                    return {};
                }
                directDeclarator = std::make_unique<DirectDeclarator>(line, column, std::move(*asterisk));
            }
            else if (std::any_of(begin, std::find_if(begin, end, [](const auto& token)
            { return token.getTokenType() == TokenType::CloseSquareBracket; }), [](const auto& token)
                                 {
                                     return token.getTokenType() == TokenType::StaticKeyword;
                                 }))
            {
                auto staticDecl = parseDirectDeclaratorStatic(*directDeclarator, begin, end, context);
                if (!staticDecl)
                {
                    return {};
                }
                directDeclarator = std::make_unique<DirectDeclarator>(line, column, std::move(*staticDecl));
            }
            else
            {
                auto noStaticDecl = parseDirectDeclaratorNoStaticOrAsterisk(*directDeclarator, begin, end, context);
                if (!noStaticDecl)
                {
                    return {};
                }
                directDeclarator = std::make_unique<DirectDeclarator>(line, column, std::move(*noStaticDecl));
            }
            break;
        }
        default:break;
        }
    }
    if (!directDeclarator)
    {
        context.logError("Expected declarator");
        return {};
    }
    return std::move(*directDeclarator);
}

std::optional<ParameterTypeList>
OpenCL::Parser::parseParameterTypeList(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                       OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    auto parameterList = parseParameterList(begin, end, context);
    if (!parameterList)
    {
        return {};
    }
    bool hasEllipse = false;
    if (begin < end && begin->getTokenType() == TokenType::Comma)
    {
        begin++;
        if (begin >= end || begin->getTokenType() != TokenType::Ellipse)
        {
            context.logError("Expected another parameter after ,");
        }
        else
        {
            begin++;
            hasEllipse = true;
        }
    }
    return ParameterTypeList(line, column, std::move(*parameterList), hasEllipse);
}

std::optional<ParameterList> OpenCL::Parser::parseParameterList(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end,
                                                                OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    std::vector<ParameterDeclaration> parameterDeclarations;
    bool first = true;
    while (begin < end)
    {
        auto before = begin;
        if (first)
        {
            first = false;
        }
        else if (begin->getTokenType() == TokenType::Comma)
        {
            begin++;
        }
        else
        {
            break;
        }
        std::vector<DeclarationSpecifier> declarationSpecifiers;
        while (begin < end && isDeclarationSpecifier(*begin, context))
        {
            auto result = parseDeclarationSpecifier(begin, end, context);
            if (!result)
            {
                return {};
            }
            declarationSpecifiers.push_back(std::move(*result));
        }
        if (declarationSpecifiers.empty())
        {
            begin = before;
            break;
        }
        auto result = std::find_if(begin, end, [](const Token& token)
        {
            switch (token.getTokenType())
            {
            case TokenType::Asterisk:
            case TokenType::ConstKeyword:
            case TokenType::VolatileKeyword:
            case TokenType::RestrictKeyword: return false;
            default: break;
            }
            return true;
        });
        if (result == end)
        {
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers), std::unique_ptr<AbstractDeclarator>());
            continue;
        }

        if (result->getTokenType() == TokenType::OpenSquareBracket)
        {
            auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
            if (!abstractDeclarator)
            {
                return {};
            }
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                               std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator)));
        }
        else if (result->getTokenType() == TokenType::Identifier)
        {
            auto declarator = parseDeclarator(begin, end, context);
            if (!declarator)
            {
                return {};
            }
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                               std::make_unique<Declarator>(std::move(*declarator)));
        }
        else if (result->getTokenType() == TokenType::OpenParenthese)
        {
            while (result->getTokenType() == TokenType::OpenParenthese)
            {
                // Ambigious
                result++;
                if (result->getTokenType() == TokenType::Identifier)
                {
                    auto declarator = parseDeclarator(begin, end, context);
                    if (!declarator)
                    {
                        declarationSpecifiers.pop_back();
                        return {};
                    }
                    parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                                       std::make_unique<Declarator>(std::move(*declarator)));
                    break;
                }
                else if (result->getTokenType() != TokenType::OpenParenthese)
                {
                    auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
                    if (!abstractDeclarator)
                    {
                        declarationSpecifiers.pop_back();
                        return {};
                    }
                    parameterDeclarations.emplace_back(
                        std::move(declarationSpecifiers),
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
        context.logError("Expected at least one parameter declaration");
    }
    return ParameterList(line, column, std::move(parameterDeclarations));
}

std::optional<Pointer> OpenCL::Parser::parsePointer(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                    ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    if (begin->getTokenType() != TokenType::Asterisk)
    {
        context.logError("Expected * at the beginning of pointer");
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    begin++;
    std::vector<TypeQualifier> typeQualifier;
    while (begin < end
        && (begin->getTokenType() == TokenType::ConstKeyword || begin->getTokenType() == TokenType::RestrictKeyword
            || begin->getTokenType() == TokenType::VolatileKeyword))
    {
        switch (begin->getTokenType())
        {
        case TokenType::ConstKeyword: typeQualifier.push_back(TypeQualifier::Const);
            break;
        case TokenType::RestrictKeyword: typeQualifier.push_back(TypeQualifier::Restrict);
            break;
        case TokenType::VolatileKeyword: typeQualifier.push_back(TypeQualifier::Volatile);
            break;
        default: break;
        }
        begin++;
    }
    return Pointer(line, column, std::move(typeQualifier));
}

std::optional<AbstractDeclarator>
OpenCL::Parser::parseAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin, Tokens::const_iterator end,
                                        OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == TokenType::Asterisk)
    {
        auto result = parsePointer(begin, end, context);
        if (!result)
        {
            return {};
        }
        pointers.push_back(std::move(*result));
    }
    auto result = parseDirectAbstractDeclarator(begin, end, context);
    if (!result)
    {
        return {};
    }
    return AbstractDeclarator(line, column, std::move(pointers), std::move(*result));
}

std::optional<DirectAbstractDeclarator>
OpenCL::Parser::parseDirectAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin,
                                              Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    std::unique_ptr<DirectAbstractDeclarator> directAbstractDeclarator;
    while (begin < end)
    {
        switch (begin->getTokenType())
        {
        case TokenType::OpenParenthese:
        {
            auto line = directAbstractDeclarator ? directAbstractDeclarator->getLine() : begin->getLine();
            auto colunn = directAbstractDeclarator ? directAbstractDeclarator->getColumn() : begin->getColumn();
            begin++;
            if (begin < end && isDeclarationSpecifier(*begin, context))
            {
                auto parameterTypeList = parseParameterTypeList(begin, end, context);
                if (!parameterTypeList)
                {
                    return {};
                }
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                    line, colunn,
                    DirectAbstractDeclaratorParameterTypeList(
                        line, colunn, std::move(directAbstractDeclarator),
                        std::make_unique<ParameterTypeList>(std::move(*parameterTypeList))));
            }
            else if (begin < end && (begin->getTokenType() == TokenType::OpenParenthese
                || begin->getTokenType() == TokenType::OpenSquareBracket
                || begin->getTokenType() == TokenType::Asterisk))
            {
                auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
                if (!abstractDeclarator)
                {
                    return {};
                }
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                    line, colunn, std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator)));
            }
            else
            {
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                    line, colunn,
                    DirectAbstractDeclaratorParameterTypeList(line, colunn, std::move(directAbstractDeclarator),
                                                              nullptr));
            }
            if (begin >= end || begin->getTokenType() != TokenType::CloseParenthese)
            {
                context.logError("Expected ) to match (");
            }
            else
            {
                begin++;
            }
            break;
        }
        case TokenType::OpenSquareBracket:
        {
            auto line = directAbstractDeclarator ? directAbstractDeclarator->getLine() : begin->getLine();
            auto colunn = directAbstractDeclarator ? directAbstractDeclarator->getColumn() : begin->getColumn();
            begin++;
            if (begin < end && begin->getTokenType() == TokenType::Asterisk)
            {
                begin++;
                directAbstractDeclarator =
                    std::make_unique<DirectAbstractDeclarator>(line, colunn, std::move(directAbstractDeclarator));
            }
            else
            {
                if (begin < end && begin->getTokenType() != TokenType::CloseSquareBracket)
                {
                    auto assignment = parseAssignmentExpression(begin, end, context);
                    if (!assignment)
                    {
                        return {};
                    }
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                        line, colunn,
                        DirectAbstractDeclaratorAssignmentExpression(
                            line, colunn, std::move(directAbstractDeclarator),
                            std::make_unique<AssignmentExpression>(std::move(*assignment))));
                }
                else
                {
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                        line, colunn,
                        DirectAbstractDeclaratorAssignmentExpression(line, colunn,
                                                                     std::move(directAbstractDeclarator), nullptr));
                }
            }
            if (begin >= end || begin->getTokenType() != TokenType::CloseSquareBracket)
            {
                context.logError("Expected ] to match [");
            }
            break;
        }
        default:goto Exit;
        }
    }
Exit:
    if (!directAbstractDeclarator)
    {
        context.logError("Invalid tokens for direct abstract declarator");
        return {};
    }
    return std::move(*directAbstractDeclarator);
}

std::optional<EnumSpecifier> OpenCL::Parser::parseEnumSpecifier(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end,
                                                                OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto line = begin->getLine();
    auto colunn = begin->getColumn();
    if (begin->getTokenType() != TokenType::EnumKeyword)
    {
        context.logError("Expected enum keyword at begin of enum specifier");
        return {};
    }
    begin++;
    if (begin < end && begin->getTokenType() == TokenType::OpenBrace)
    {
        auto declaration = parseEnumDeclaration(begin, end, context);
        if (!declaration)
        {
            return {};
        }
        return EnumSpecifier(line, colunn, std::move(*declaration));
    }
    else if (begin >= end || begin->getTokenType() != TokenType::Identifier)
    {
        context.logError("Expected Identifier or { after enum");
    }
    else
    {
        begin++;
    }
    if (begin < end && begin->getTokenType() == TokenType::OpenBrace)
    {
        auto declaration = parseEnumDeclaration(begin, end, context);
        if (!declaration)
        {
            return {};
        }
        return EnumSpecifier(line, colunn, std::move(*declaration));
    }
    begin++;
    auto name = std::string();
    if (begin < end)
    {
        name = std::get<std::string>(begin->getValue());
    }
    else
    {
        context.logError("Expected name for enum specifier");
    }
    begin++;
    return EnumSpecifier(line, colunn, std::move(name));
}

std::optional<EnumDeclaration> OpenCL::Parser::parseEnumDeclaration(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    begin++;
    std::string name;
    if (begin < end && begin->getTokenType() == TokenType::Identifier)
    {
        name = std::get<std::string>(begin->getValue());
        begin++;
    }
    if (begin >= end || begin->getTokenType() != TokenType::OpenBrace)
    {
        context.logError("Expected { after enum declaration");
    }
    else
    {
        begin++;
    }
    std::vector<std::pair<std::string, std::int32_t>> values;
    do
    {
        std::string valueName;
        if (begin >= end || begin->getTokenType() != TokenType::Identifier)
        {
            context.logError("Expected Identifier in enum value list");
            return {};
        }
        else
        {
            valueName = std::get<std::string>(begin->getValue());
            begin++;
        }
        std::int32_t value = values.empty() ? 0 : values.back().second + 1;
        if (begin < end && begin->getTokenType() == TokenType::Assignment)
        {
            begin++;
            auto constant = parseAssignmentExpression(begin, end, context);
            if (!constant)
            {
                return {};
            }
            Semantics::ConstantEvaluator evaluator(context.structOrUnions);
            auto constValue = evaluator.visit(*constant);
            if (!constValue)
            {
                return {};
            }
            value = std::visit(
                [](auto&& value) -> std::int32_t
                {
                    using T = std::decay_t<decltype(value)>;
                    if constexpr (std::is_same_v<T, void*>)
                    {
                        return (std::int32_t)(std::intptr_t)value;
                    }
                    else
                    {
                        return value;
                    }
                },
                *constValue);
        }
        if (begin < end && begin->getTokenType() == TokenType::Comma)
        {
            begin++;
        }
        else if (begin >= end && begin->getTokenType() != TokenType::CloseBrace)
        {
            context.logError("Expected , after non final value in enum list");
        }
        values.emplace_back(valueName, value);
    }
    while (begin < end && begin->getTokenType() != TokenType::CloseBrace);
    if (begin < end)
    {
        begin++;
    }
    else
    {
        context.logError("Expected } at the end of enum definition");
    }
    return EnumDeclaration(line, column, std::move(name), values);
}

std::optional<Syntax::FunctionDefinition>
OpenCL::Parser::parseFunctionDefinition(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                        ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    std::vector<DeclarationSpecifier> declarationSpecifiers;
    while (begin < end && isDeclarationSpecifier(*begin, context))
    {
        auto result = parseDeclarationSpecifier(begin, end, context);
        if (!result)
        {
            return {};
        }
        declarationSpecifiers.push_back(std::move(*result));
    }
    if (declarationSpecifiers.empty())
    {
        context.logError("Expected declaration specifiers at beginning of function definition");
    }
    auto declarator = parseDeclarator(begin, end, context);
    if (!declarator)
    {
        return {};
    }
    std::vector<Declaration> declarations;
    while (begin < end && isDeclarationSpecifier(*begin, context))
    {
        auto result = parseDeclaration(begin, end, context);
        if (!result)
        {
            return {};
        }
        declarations.push_back(std::move(*result));
    }

    context.addToScope(Semantics::declaratorToName(*declarator));
    context.pushScope();
    if (auto* paramters =
        std::get_if<DirectDeclaratorParentheseParameters>(&declarator->getDirectDeclarator().getVariant()))
    {
        auto& parameterDeclarations = paramters->getParameterTypeList().getParameterList().getParameterDeclarations();
        for (auto&[specifier, paramDeclarator] :
            parameterDeclarations)
        {
            if (parameterDeclarations.size() == 1 && specifier.size() == 1
                && std::holds_alternative<TypeSpecifier>(specifier[0]))
            {
                auto* primitive
                    = std::get_if<TypeSpecifier::PrimitiveTypeSpecifier>(&std::get<TypeSpecifier>(specifier[0])
                        .getVariant());
                if (primitive && *primitive == TypeSpecifier::PrimitiveTypeSpecifier::Void)
                {
                    break;
                }
            }

            if (std::holds_alternative<std::unique_ptr<AbstractDeclarator>>(paramDeclarator))
            {
                context.logError("Parameter name omitted");
                return {};
            }
            auto& decl = std::get<std::unique_ptr<Declarator>>(paramDeclarator);
            auto visitor = [](auto self, auto&& value) -> std::string
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<std::string, T>)
                {
                    (void)self;
                    return value;
                }
                else if constexpr (std::is_same_v<T, std::unique_ptr<Declarator>>)
                {
                    return std::visit([&](auto&& value) -> std::string
                                      { return self(value); },
                                      value->getDirectDeclarator().getVariant());
                }
                else if constexpr (
                    !std::is_same_v<
                        T,
                        DirectDeclaratorParentheseIdentifiers>
                        && !std::is_same_v<T, DirectDeclaratorParentheseParameters>)
                {
                    return std::visit([&](auto&& value) -> std::string
                                      { return self(value); },
                                      value.getDirectDeclarator().getVariant());
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
    else if (auto* identifierList =
        std::get_if<DirectDeclaratorParentheseIdentifiers>(&declarator->getDirectDeclarator().getVariant()))
    {
        for (auto& iter : identifierList->getIdentifiers())
        {
            context.addToScope(iter);
        }
    }
    auto compoundStatement = parseCompoundStatement(begin, end, context);
    context.popScope();
    if (!compoundStatement)
    {
        return {};
    }

    return FunctionDefinition(line, column, std::move(declarationSpecifiers), std::move(*declarator),
                              std::move(declarations), std::move(*compoundStatement));
}

std::optional<CompoundStatement>
OpenCL::Parser::parseCompoundStatement(OpenCL::Parser::Tokens::const_iterator& begin, Tokens::const_iterator end,
                                       OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto line = begin->getLine();
    auto column = begin->getColumn();
    if (begin >= end || begin->getTokenType() != TokenType::OpenBrace)
    {
        context.logError("Expected { at start of Compound Statement");
    }
    else
    {
        begin++;
    }
    std::vector<CompoundItem> items;
    context.pushScope();
    while (begin < end && begin->getTokenType() != TokenType::CloseBrace)
    {
        auto result = parseCompoundItem(begin, end, context);
        if (!result)
        {
            return {};
        }
        items.push_back(std::move(*result));
    }
    context.popScope();
    if (begin >= end || begin->getTokenType() != TokenType::CloseBrace)
    {
        context.logError("Expected } at end of Compound Statement");
    }
    else
    {
        begin++;
    }
    return CompoundStatement(line, column, std::move(items));
}

std::optional<CompoundItem> OpenCL::Parser::parseCompoundItem(Tokens::const_iterator& begin,
                                                              Tokens::const_iterator end,
                                                              ParsingContext& context)
{
    if (begin >= end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    //backtracking
    auto prevErrorCount = context.getErrors().size();
    if (auto declaration = parseDeclaration(curr, end, context); declaration
        && prevErrorCount == context.getErrors().size())
    {
        begin = curr;
        return CompoundItem(line, column, std::move(*declaration));
    }
    else
    {
        auto copy = std::vector(context.getErrors().begin() + prevErrorCount, context.getErrors().end());
        context.getErrors().resize(prevErrorCount);
        auto pos = curr;
        auto statement = parseStatement(curr = begin, end, context);
        if (!statement || context.getErrors().size() != prevErrorCount)
        {
            bool statementWentFurther = std::distance(begin, curr) > std::distance(begin, pos);
            begin = statementWentFurther ? curr : pos;
            return {};
        }
        begin = curr;
        return CompoundItem(line, column, std::move(*statement));
    }
}

std::optional<Initializer>
OpenCL::Parser::parseInitializer(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    if (curr->getTokenType() != TokenType::OpenBrace)
    {
        auto assignment = parseAssignmentExpression(begin, end, context);
        if (!assignment)
        {
            return {};
        }
        return Initializer(curr->getLine(), curr->getColumn(), std::move(*assignment));
    }
    else
    {
        curr++;
        auto initializerList = parseInitializerList(curr, end, context);
        if (!initializerList)
        {
            return {};
        }
        if (curr == end || (curr->getTokenType() != TokenType::CloseBrace && curr->getTokenType() != TokenType::Comma))
        {
            context.logError("Expected } after initializer list");
            return {};
        }
        if (curr->getTokenType() == TokenType::Comma)
        {
            curr++;
        }
        if (curr == end || curr->getTokenType() != TokenType::CloseBrace)
        {
            context.logError("Expected } after initializer list");
            return {};
        }
        curr++;
        begin = curr;
        return Initializer{curr->getLine(), curr->getColumn(), std::move(*initializerList)};
    }
}

std::optional<InitializerList> OpenCL::Parser::parseInitializerList(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
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
        while (curr->getTokenType() == TokenType::OpenSquareBracket || curr->getTokenType() == TokenType::Dot)
        {
            if (curr->getTokenType() == TokenType::OpenSquareBracket)
            {
                curr++;
                auto constant = parseAssignmentExpression(curr, end, context);
                if (!constant)
                {
                    if (vector.empty())
                    {
                        return {};
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
                        context.logError("Expected ] to close designator in initializer list");
                        return {};
                    }
                    else
                    {
                        curr = before;
                        goto Exit;
                    }
                }
                curr++;
                Semantics::ConstantEvaluator evaluator(context.structOrUnions);
                auto constValue = evaluator.visit(*constant);
                if (!constValue)
                {
                    return {};
                }
                variants.emplace_back(std::visit(
                    [](auto&& value) -> std::size_t
                    {
                        using T = std::decay_t<decltype(value)>;
                        if constexpr (std::is_convertible_v<T, std::size_t>)
                        {
                            return value;
                        }
                        else
                        {
                            throw std::runtime_error("Invalid type of constant expression");
                        }
                    },
                    *constValue));
            }
            else if (curr->getTokenType() == TokenType::Dot)
            {
                curr++;
                if (curr->getTokenType() != TokenType::Identifier)
                {
                    if (vector.empty())
                    {
                        context.logError("Expected identifier following dot in designation of initializer list");
                        return {};
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
                context.logError("Expected = after designators");
                return {};
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
                return {};
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

std::optional<Statement> OpenCL::Parser::parseStatement(Tokens::const_iterator& begin,
                                                        Tokens::const_iterator end, ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto result = [&context, &curr, end]() -> std::optional<Statement>
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
                return {};
            }
            return Statement{
                line, column,
                ReturnStatement(curentToken.getLine(), curentToken.getColumn(), std::move(*expression))};
        }
        case TokenType::IfKeyword:
        {
            curr++;
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                context.logError("Expected ( after if");
                return {};
            }
            auto expression = parseExpression(curr, end, context);
            if (!expression)
            {
                return {};
            }
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                context.logError("Expected ) at the end of if statement");
                return {};
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
                return Statement{line, column,
                                 IfStatement(line, column, std::move(*expression),
                                             std::make_unique<Statement>(std::move(*statement)),
                                             std::make_unique<Statement>(std::move(*elseStatement)))};
            }
            else
            {
                return Statement{line, column,
                                 IfStatement(line, column, std::move(*expression),
                                             std::make_unique<Statement>(std::move(*statement)))};
            }
        }
        case TokenType::OpenBrace:
        {
            auto compoundStatement = parseCompoundStatement(curr, end, context);
            if (!compoundStatement)
            {
                return {};
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
                context.logError("Expected ( after for");
                return {};
            }
            auto blockitem = parseCompoundItem(curr, end, context);
            if (!blockitem)
            {
                return {};
            }

            std::unique_ptr<Expression> control;
            {
                if (std::holds_alternative<Declaration>(blockitem->getVariant())
                    || curr->getTokenType() != TokenType::SemiColon)
                {
                    auto expression = parseExpression(curr, end, context);
                    if (!expression)
                    {
                        return {};
                    }
                    if (curr == end || curr->getTokenType() != TokenType::SemiColon)
                    {
                        context.logError("Expected ; after control part of for loop header");
                        return {};
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
                        return {};
                    }
                    if (curr == end || curr->getTokenType() != TokenType::CloseParenthese)
                    {
                        context.logError("Expected ) after control part of for loop header");
                        return {};
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
                return Statement(
                    line, column,
                    ForDeclarationStatement(line, column, std::make_unique<Statement>(std::move(*statement)),
                                            std::move(*declaration), std::move(control), std::move(post)));
            }
            else if (auto expressionStatement = std::get_if<ExpressionStatement>(
                &std::get<Statement>(blockitem->getVariant()).getVariant()))
            {
                return Statement(line, column,
                                 ForStatement(line, column, std::make_unique<Statement>(std::move(*statement)),
                                              expressionStatement->moveOptionalExpression(), std::move(control),
                                              std::move(post)));
            }
            else
            {
                context.logError("Invalid expression or declaration for initial part of for loop header");
                return {};
            }
        }
        case TokenType::WhileKeyword:
        {
            curr++;
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                context.logError("Expected ( after while");
                return {};
            }
            auto expression = parseExpression(curr, end, context);
            if (!expression)
            {
                return {};
            }
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                context.logError("Expected ) after expression in while");
                return {};
            }
            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return statement;
            }
            return Statement(line, column,
                             HeadWhileStatement(line, column, std::move(*expression),
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
                context.logError("Expected while after do");
            }
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::OpenParenthese)
            {
                context.logError("Expected ( after while");
            }
            auto expression = parseExpression(curr, end, context);
            if (!expression)
            {
                return {};
            }
            curentToken = *curr;
            curr++;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                context.logError("Expected ) after expression in while");
            }
            return Statement(line, column,
                             FootWhileStatement(line, column, std::make_unique<Statement>(std::move(*statement)),
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
                context.logError("Expected ( after switch keyword");
            }
            curr++;
            auto expression = parseExpression(curr, end, context);
            if (!expression)
            {
                return {};
            }
            curentToken = *curr;
            if (curentToken.getTokenType() != TokenType::CloseParenthese)
            {
                context.logError("Expected ) after expression in switch ");
            }
            curr++;
            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return statement;
            }
            return Statement(line, column,
                             SwitchStatement(line, column, std::move(*expression),
                                             std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::DefaultKeyword:
        {
            curr++;
            curentToken = *curr;
            if (curentToken.getTokenType() != TokenType::Colon)
            {
                context.logError("Expected : after default");
            }
            curr++;
            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return {};
            }
            return Statement(line, column,
                             DefaultStatement(line, column, std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::CaseKeyword:
        {
            curr++;
            auto expression = parseAssignmentExpression(curr, end, context);
            if (!expression)
            {
                return {};
            }
            curentToken = *curr;
            if (curentToken.getTokenType() != TokenType::Colon)
            {
                context.logError("Expected : after constant expression of case");
            }
            curr++;
            Semantics::ConstantEvaluator evaluator(context.structOrUnions);
            auto statement = parseStatement(curr, end, context);
            if (!statement)
            {
                return statement;
            }
            auto constValue = evaluator.visit(*expression);
            if (!constValue)
            {
                return {};
            }
            return Statement(
                line, column,
                CaseStatement(line, column, *constValue, std::make_unique<Statement>(std::move(*statement))));
        }
        case TokenType::GotoKeyword:
        {
            curr++;
            if (curr->getTokenType() != TokenType::Identifier)
            {
                context.logError("Expected identifier following goto keyword");
                return {};
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
                    return {};
                }
                return Statement(
                    line, column,
                    ExpressionStatement(line, column, std::make_unique<Expression>(std::move(*expression))));
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
        context.logError("Statement not terminated with ;");
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

std::optional<Expression> OpenCL::Parser::parseExpression(Tokens::const_iterator& begin,
                                                          Tokens::const_iterator end, ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    std::vector<AssignmentExpression> expressions;
    auto assignment = parseAssignmentExpression(curr, end, context);
    if (!assignment)
    {
        return {};
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

std::optional<Syntax::AssignmentExpression>
OpenCL::Parser::parseAssignmentExpression(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                          ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto before = curr;
    auto unary = parseUnaryExpression(curr, end, context);
    //TODO: backtracking
    if (unary)
    {
        if (isAssignment(curr->getTokenType()))
        {
            auto currentToken = *curr;
            curr++;
            auto assignment = parseAssignmentExpression(curr, end, context);
            if (!assignment)
            {
                return {};
            }
            begin = curr;
            return AssignmentExpression(
                line, column,
                AssignmentExpressionAssignment(
                    line, column, std::move(*unary),
                    [assignment = currentToken.getTokenType()]
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
                        default: throw std::runtime_error("Invalid token for assignment");
                        }
                    }(),
                    std::make_unique<AssignmentExpression>(std::move(*assignment))));
        }
        else
        {
            curr = before;
        }
    }
    auto cond = parseConditionalExpression(curr, end, context);
    if (!cond)
    {
        return {};
    }
    begin = curr;
    return AssignmentExpression(line, column, std::move(*cond));
}

std::optional<ConditionalExpression> OpenCL::Parser::parseConditionalExpression(Tokens::const_iterator& begin,
                                                                                Tokens::const_iterator end,
                                                                                ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto logicalOrExperssion = parseLogicalOrExpression(curr, end, context);
    if (!logicalOrExperssion)
    {
        return {};
    }
    if (curr != end)
    {
        if (curr->getTokenType() == TokenType::QuestionMark)
        {
            curr++;
            auto optionalExpression = parseExpression(curr, end, context);
            if (!optionalExpression)
            {
                return {};
            }
            if (curr->getTokenType() != TokenType::Colon)
            {
                context.logError("Expected : to match ?");
                return {};
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

std::optional<LogicalOrExpression> OpenCL::Parser::parseLogicalOrExpression(Tokens::const_iterator& begin,
                                                                            Tokens::const_iterator end,
                                                                            ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto logicalAnd = parseLogicalAndExpression(curr, end, context);
    if (!logicalAnd)
    {
        return {};
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

std::optional<LogicalAndExpression> OpenCL::Parser::parseLogicalAndExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseBitOrExpression(curr, end, context);
    if (!result)
    {
        return {};
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

std::optional<BitOrExpression> OpenCL::Parser::parseBitOrExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseBitXorExpression(curr, end, context);
    if (!result)
    {
        return {};
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

std::optional<BitXorExpression> OpenCL::Parser::parseBitXorExpression(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }

    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseBitAndExpression(curr, end, context);
    if (!result)
    {
        return {};
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

std::optional<BitAndExpression> OpenCL::Parser::parseBitAndExpression(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseEqualityExpression(curr, end, context);
    if (!result)
    {
        return {};
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

std::optional<EqualityExpression> OpenCL::Parser::parseEqualityExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end,
                                                                          ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseRelationalExpression(curr, end, context);
    if (!result)
    {
        return {};
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
        relationalExpressions.emplace_back(token == TokenType::Equal ? EqualityExpression::EqualityOperator::Equal :
                                           EqualityExpression::EqualityOperator::NotEqual,
                                           std::move(*newRelational));
    }

    begin = curr;
    return EqualityExpression(line, column, std::move(*result), std::move(relationalExpressions));
}

std::optional<RelationalExpression> OpenCL::Parser::parseRelationalExpression(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseShiftExpression(curr, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<std::pair<RelationalExpression::RelationalOperator, ShiftExpression>> list;
    while (curr != end
        && (curr->getTokenType() == TokenType::LessThan || curr->getTokenType() == TokenType::LessThanOrEqual
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

        list.emplace_back(
            [token]() -> RelationalExpression::RelationalOperator
            {
                switch (token)
                {
                case TokenType::LessThan: return RelationalExpression::RelationalOperator::LessThan;
                case TokenType::LessThanOrEqual: return RelationalExpression::RelationalOperator::LessThanOrEqual;
                case TokenType::GreaterThan: return RelationalExpression::RelationalOperator::GreaterThan;
                case TokenType::GreaterThanOrEqual:return RelationalExpression::RelationalOperator::GreaterThanOrEqual;
                default: throw std::runtime_error("Invalid token for relational LogicalOrExpression");
                }
            }(),
            std::move(*newShift));
    }

    begin = curr;
    return RelationalExpression(line, column, std::move(*result), std::move(list));
}

std::optional<ShiftExpression> OpenCL::Parser::parseShiftExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseAdditiveExpression(curr, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<std::pair<ShiftExpression::ShiftOperator, AdditiveExpression>> list;
    while (curr != end
        && (curr->getTokenType() == TokenType::ShiftRight || curr->getTokenType() == TokenType::ShiftLeft))
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
        list.emplace_back(token == TokenType::ShiftRight ? ShiftExpression::ShiftOperator::Right :
                          ShiftExpression::ShiftOperator::Left,
                          std::move(*newAdd));
    }

    begin = curr;
    return ShiftExpression(line, column, std::move(*result), std::move(list));
}

std::optional<AdditiveExpression> OpenCL::Parser::parseAdditiveExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end,
                                                                          ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseTerm(curr, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<std::pair<AdditiveExpression::BinaryDashOperator, Term>> list;
    while (curr != end && (curr->getTokenType() == TokenType::Addition || curr->getTokenType() == TokenType::Negation))
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
        list.emplace_back(token == TokenType::Addition ? AdditiveExpression::BinaryDashOperator::BinaryPlus :
                          AdditiveExpression::BinaryDashOperator::BinaryMinus,
                          std::move(*newTerm));
    }

    begin = curr;
    return AdditiveExpression(line, column, std::move(*result), std::move(list));
}

std::optional<Term> OpenCL::Parser::parseTerm(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                              ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto line = curr->getLine();
    auto column = curr->getColumn();
    auto result = parseCastExpression(curr, end, context);
    if (!result)
    {
        return {};
    }

    std::vector<std::pair<Term::BinaryDotOperator, CastExpression>> list;
    while (curr != end
        && (curr->getTokenType() == TokenType::Asterisk || curr->getTokenType() == TokenType::Division
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
        list.emplace_back(
            [token]
            {
                switch (token)
                {
                case TokenType::Asterisk: return Term::BinaryDotOperator::BinaryMultiply;
                case TokenType::Division: return Term::BinaryDotOperator::BinaryDivide;
                case TokenType::Modulo: return Term::BinaryDotOperator::BinaryRemainder;
                default: throw std::runtime_error("Invalid token");
                }
            }(),
            std::move(*newCast));
    }

    begin = curr;
    return Term(line, column, std::move(*result), std::move(list));
}

std::optional<TypeName> OpenCL::Parser::parseTypeName(Tokens::const_iterator& begin,
                                                      Tokens::const_iterator end,
                                                      OpenCL::Parser::ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
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
        context.logError("Expected at least one specifier qualifier at beginning of typename");
        return {};
    }

    if (auto abstractDec = parseAbstractDeclarator(curr, end, context))
    {
        begin = curr;
        return TypeName(line, column, std::move(specifierQualifiers),
                        std::make_unique<AbstractDeclarator>(std::move(*abstractDec)));
    }
    begin = curr;
    return TypeName(line, column, std::move(specifierQualifiers), nullptr);
}

std::optional<CastExpression> OpenCL::Parser::parseCastExpression(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end,
                                                                  ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
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
                    return CastExpression(
                        line, column,
                        std::pair{std::move(*typeName), std::make_unique<CastExpression>(std::move(*cast))});
                }
            }
        }
    }
    auto unary = parseUnaryExpression(curr, end, context);
    if (!unary)
    {
        return {};
    }
    begin = curr;
    return CastExpression(line, column, std::move(*unary));
}

std::optional<UnaryExpression> OpenCL::Parser::parseUnaryExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
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
                return {};
            }
            if (curr->getTokenType() != TokenType::CloseParenthese)
            {
                context.logError("Expected Close Parenthese after type in sizeof");
                return {};
            }
            curr++;
            begin = curr;
            return UnaryExpression(line, column,
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
            return UnaryExpression(
                line, column,
                UnaryExpressionSizeOf(line, column, std::make_unique<UnaryExpression>(std::move(*unary))));
        }
    }
    else if (curr->getTokenType() == TokenType::Increment || curr->getTokenType() == TokenType::Decrement
        || curr->getTokenType() == TokenType::Ampersand || curr->getTokenType() == TokenType::Asterisk
        || curr->getTokenType() == TokenType::Addition || curr->getTokenType() == TokenType::Negation
        || curr->getTokenType() == TokenType::LogicalNegation
        || curr->getTokenType() == TokenType::BitWiseNegation)
    {
        auto token = curr->getTokenType();
        curr++;
        auto op = [token]
        {
            switch (token)
            {
            case TokenType::Increment: return UnaryExpressionUnaryOperator::UnaryOperator::Increment;
            case TokenType::Decrement: return UnaryExpressionUnaryOperator::UnaryOperator::Decrement;
            case TokenType::Ampersand: return UnaryExpressionUnaryOperator::UnaryOperator::Ampersand;
            case TokenType::Asterisk: return UnaryExpressionUnaryOperator::UnaryOperator::Asterisk;
            case TokenType::Addition: return UnaryExpressionUnaryOperator::UnaryOperator::Plus;
            case TokenType::Negation: return UnaryExpressionUnaryOperator::UnaryOperator::Minus;
            case TokenType::LogicalNegation: return UnaryExpressionUnaryOperator::UnaryOperator::BitNot;
            case TokenType::BitWiseNegation: return UnaryExpressionUnaryOperator::UnaryOperator::LogicalNot;
            default: throw std::runtime_error("Invalid token");
            }
        }();
        auto unary = parseUnaryExpression(curr, end, context);
        if (!unary)
        {
            return unary;
        }
        begin = curr;
        return UnaryExpression(
            line, column,
            UnaryExpressionUnaryOperator(line, column, op, std::make_unique<UnaryExpression>(std::move(*unary))));
    }

    auto postFix = parsePostFixExpression(curr, end, context);
    if (!postFix)
    {
        return {};
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
        case TokenType::Decrement: return true;
        default: break;
        }
        return false;
    }
} // namespace

std::optional<PostFixExpression> OpenCL::Parser::parsePostFixExpression(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end,
                                                                        ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    std::stack<std::unique_ptr<PostFixExpression>> stack;
    while (curr != end && isPostFixExpression(*curr))
    {
        auto before = curr;
        if (curr->getTokenType() == TokenType::Identifier || curr->getTokenType() == TokenType::Literal)
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
                    return {};
                }
            }
            stack.push(std::make_unique<PostFixExpression>(
                line, column, PostFixExpressionPrimaryExpression(line, column, std::move(*newPrimary))));
        }
        else if (curr->getTokenType() == TokenType::OpenParenthese && stack.empty())
        {
            curr++;
            if (auto type = parseTypeName(curr, end, context))
            {
                if (curr->getTokenType() != TokenType::CloseParenthese)
                {
                    context.logError("Expected ) after type name in type initializer");
                    return {};
                }
                curr++;
                if (curr->getTokenType() != TokenType::OpenBrace)
                {
                    return {};
                    context.logError("Expected { after type around parenthesis");
                }
                curr++;
                auto initializer = parseInitializerList(curr, end, context);
                if (!initializer)
                {
                    return {};
                }
                if (curr->getTokenType() == TokenType::Comma)
                {
                    curr++;
                }
                if (curr->getTokenType() != TokenType::CloseBrace)
                {
                    context.logError("Expected { after type around parenthesis");
                    return {};
                }
                curr++;
                auto line = type->getLine();
                auto column = type->getColumn();
                stack.push(std::make_unique<PostFixExpression>(
                    line, column,
                    PostFixExpressionTypeInitializer(line, column, std::move(*type), std::move(*initializer))));
            }
            else
            {
                curr = before;
                auto line = curr->getLine();
                auto column = curr->getColumn();
                auto primary = parsePrimaryExpression(curr, end, context);
                if (!primary)
                {
                    return {};
                }
                stack.push(std::make_unique<PostFixExpression>(
                    line, column, PostFixExpressionPrimaryExpression(line, column, std::move(*primary))));
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
            stack.push(std::make_unique<PostFixExpression>(
                line, column,
                PostFixExpressionFunctionCall(line, column, std::move(postExpression),
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
            stack.push(std::make_unique<PostFixExpression>(
                line, column,
                PostFixExpressionSubscript(line, column, std::move(postExpression), std::move(*expression))));
        }
        else if (curr->getTokenType() == TokenType::Increment)
        {
            curr++;
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = curr->getLine();
            auto column = curr->getColumn();
            stack.push(std::make_unique<PostFixExpression>(
                line, column, PostFixExpressionIncrement(line, column, std::move(postExpression))));
        }
        else if (curr->getTokenType() == TokenType::Decrement)
        {
            auto postExpression = std::move(stack.top());
            stack.pop();
            auto line = curr->getLine();
            auto column = curr->getColumn();
            stack.push(std::make_unique<PostFixExpression>(
                line, column, PostFixExpressionDecrement(line, column, std::move(postExpression))));
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
            stack.push(std::make_unique<PostFixExpression>(
                line, column, PostFixExpressionDot(line, column, std::move(postExpression), name)));
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
            stack.push(std::make_unique<PostFixExpression>(
                line, column, PostFixExpressionArrow(line, column, std::move(postExpression), name)));
        }
    }

    if (stack.size() != 1)
    {
        context.logError("Invalid amount of post fix expressions");
        return {};
    }
    auto ret = std::move(*stack.top());
    stack.pop();
    begin = curr;
    return ret;
}

std::optional<PrimaryExpression> OpenCL::Parser::parsePrimaryExpression(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end,
                                                                        ParsingContext& context)
{
    if (begin == end)
    {
        context.logError("Unexpected end of tokens");
        return {};
    }
    auto curr = begin;
    auto currToken = *curr;
    curr++;
    auto line = currToken.getLine();
    auto column = currToken.getColumn();
    if (currToken.getTokenType() == TokenType::Identifier)
    {
        begin = curr;
        return PrimaryExpression(line, column, PrimaryExpressionIdentifier(line, column,
                                                                           std::get<std::string>(currToken
                                                                                                     .getValue())));
    }
    else if (currToken.getTokenType() == TokenType::Literal)
    {
        begin = curr;
        return PrimaryExpression(
            line, column,
            PrimaryExpressionConstant(
                line, column,
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
    else if (currToken.getTokenType() == TokenType::OpenParenthese)
    {
        auto expression = parseExpression(curr, end, context);
        if (!expression)
        {
            return {};
        }
        if (curr->getTokenType() != TokenType::CloseParenthese)
        {
            context.logError("Expected Close Parentheses after expression in primary expression");
        }
        curr++;
        begin = curr;
        return PrimaryExpression(line, column, PrimaryExpressionParenthese(line, column, std::move(*expression)));
    }
    else
    {
        context.logError("Invalid token for primary expression");
        return {};
    }
}

bool Parser::ParsingContext::isTypedef(const std::string& name) const
{
    for (auto& iter : typedefs)
    {
        if (iter.count(name))
        {
            return true;
        }
    }
    return false;
}

void Parser::ParsingContext::logError(const std::string& error)
{
    m_errors.push_back(error);
}

std::vector<std::string>& Parser::ParsingContext::getErrors()
{
    return m_errors;
}
