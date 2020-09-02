#include "Parser.hpp"

#include <cld/Common/ScopeExit.hpp>

#include <algorithm>
#include <unordered_set>

#include "ErrorMessages.hpp"
#include "ParserUtil.hpp"
#include "SemanticUtil.hpp"
#include "Semantics.hpp"

using namespace cld::Syntax;

namespace
{
std::vector<DeclarationSpecifier> parseDeclarationSpecifierList(cld::Lexer::CTokenIterator& begin,
                                                                cld::Lexer::CTokenIterator end,
                                                                cld::Parser::Context& context)
{
    bool seenTypeSpecifier = false;
    std::vector<DeclarationSpecifier> declarationSpecifiers;
    do
    {
        auto result = parseDeclarationSpecifier(begin, end,
                                                context.withRecoveryTokens(cld::Parser::firstDeclarationSpecifierSet));
        if (result)
        {
            if (!seenTypeSpecifier && std::holds_alternative<TypeSpecifier>(*result))
            {
                seenTypeSpecifier = true;
            }
            declarationSpecifiers.push_back(std::move(*result));
        }
    } while (begin < end && cld::Parser::firstIsInDeclarationSpecifier(*begin, context)
             && (begin->getTokenType() != cld::Lexer::TokenType::Identifier || !seenTypeSpecifier));
    return declarationSpecifiers;
}

} // namespace

std::vector<cld::Syntax::SpecifierQualifier>
    cld::Parser::parseSpecifierQualifierList(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    bool seenTypeSpecifier = false;
    std::vector<SpecifierQualifier> specifierQualifiers;
    do
    {
        auto result =
            parseSpecifierQualifier(begin, end, context.withRecoveryTokens(cld::Parser::firstSpecifierQualifierSet));
        if (result)
        {
            if (!seenTypeSpecifier && std::holds_alternative<TypeSpecifier>(*result))
            {
                seenTypeSpecifier = true;
            }
            specifierQualifiers.push_back(std::move(*result));
        }
    } while (begin < end && firstIsInSpecifierQualifier(*begin, context)
             && (begin->getTokenType() != Lexer::TokenType::Identifier || !seenTypeSpecifier));
    return specifierQualifiers;
}

TranslationUnit cld::Parser::parseTranslationUnit(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                  Context& context)
{
    std::vector<Syntax::ExternalDeclaration> global;
#ifdef LLVM_ENABLE_EXCEPTIONS
    try
    {
#endif
        while (begin < end)
        {
            auto result = parseExternalDeclaration(begin, end, context.withRecoveryTokens(firstExternalDeclarationSet));
            if (result)
            {
                global.push_back(std::move(*result));
            }
        }
#ifdef LLVM_ENABLE_EXCEPTIONS
    }
    catch (const FatalParserError&)
    {
    }
#endif
    return Syntax::TranslationUnit(std::move(global));
}

std::optional<cld::Syntax::ExternalDeclaration>
    cld::Parser::parseExternalDeclaration(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;

    auto declarationSpecifiers = parseDeclarationSpecifierList(
        begin, end,
        context.withRecoveryTokens(firstDeclaratorSet | Context::fromTokenTypes(Lexer::TokenType::SemiColon)));
    bool isTypedef = std::any_of(declarationSpecifiers.begin(), declarationSpecifiers.end(),
                                 [](const DeclarationSpecifier& declarationSpecifier) {
                                     auto* storage = std::get_if<StorageClassSpecifier>(&declarationSpecifier);
                                     if (!storage)
                                     {
                                         return false;
                                     }
                                     return storage->getSpecifier() == StorageClassSpecifier::Typedef;
                                 });

    if (begin == end || begin->getTokenType() == Lexer::TokenType::SemiColon)
    {
        expect(Lexer::TokenType::SemiColon, begin, end, context);
        return Declaration(start, begin, std::move(declarationSpecifiers), {});
    }

    auto declarator = parseDeclarator(
        begin, end,
        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma, Lexer::TokenType::SemiColon,
                                                           Lexer::TokenType::OpenBrace, Lexer::TokenType::Assignment)));
    if (begin == end)
    {
        expect(Lexer::TokenType::SemiColon, begin, end, context);
        return {};
    }

    if (begin->getTokenType() == Lexer::TokenType::OpenBrace || firstIsInDeclaration(*begin, context))
    {
        std::vector<Declaration> declarations;
        while (begin < end && firstIsInDeclaration(*begin, context))
        {
            auto result = parseDeclaration(
                begin, end,
                context.withRecoveryTokens(firstDeclarationSet | Context::fromTokenTypes(Lexer::TokenType::OpenBrace)));
            if (result)
            {
                declarations.push_back(std::move(*result));
            }
        }

        context.pushScope();
        if (!declarator)
        {
            parseCompoundStatement(begin, end, context, false);
            context.popScope();
            return {};
        }

        const DirectDeclaratorParenthesesParameters* parameters = nullptr;
        const DirectDeclaratorParenthesesIdentifiers* identifierList = nullptr;

        for (auto& iter :
             Semantics::RecursiveVisitor(declarator->getDirectDeclarator(), Semantics::DIRECT_DECL_NEXT_FN))
        {
            cld::match(
                iter,
                [&](const DirectDeclaratorParenthesesParameters& dd) {
                    parameters = &dd;
                    identifierList = nullptr;
                },
                [&](const DirectDeclaratorParenthesesIdentifiers& dd) {
                    parameters = nullptr;
                    identifierList = &dd;
                },
                [](const DirectDeclaratorIdentifier&) {},
                [&](const DirectDeclaratorParentheses& parentheses) {
                    if (!parentheses.getDeclarator().getPointers().empty())
                    {
                        parameters = nullptr;
                        identifierList = nullptr;
                    }
                },
                [&](const auto&) {
                    parameters = nullptr;
                    identifierList = nullptr;
                });
        }

        if (identifierList)
        {
            for (auto& iter : identifierList->getIdentifiers())
            {
                context.addToScope(iter->getText(), {start, begin, iter});
            }
        }
        else if (parameters)
        {
            auto& parameterDeclarations = parameters->getParameterTypeList().getParameters();
            std::unordered_set<std::string_view> addedByParameters;
            for (auto& iter : parameterDeclarations)
            {
                if (parameterDeclarations.size() == 1 && iter.declarationSpecifiers.size() == 1
                    && std::holds_alternative<TypeSpecifier>(iter.declarationSpecifiers[0]))
                {
                    auto* primitive = std::get_if<TypeSpecifier::PrimitiveTypeSpecifier>(
                        &cld::get<TypeSpecifier>(iter.declarationSpecifiers[0]).getVariant());
                    if (primitive && *primitive == TypeSpecifier::PrimitiveTypeSpecifier::Void)
                    {
                        break;
                    }
                }

                //
                // Any parameters that overshadow typedefs need to also affect later parameters
                //
                for (auto& specifier : iter.declarationSpecifiers)
                {
                    auto* typeSpecifier = std::get_if<TypeSpecifier>(&specifier);
                    if (!typeSpecifier)
                    {
                        continue;
                    }
                    auto* identifier = std::get_if<std::string_view>(&typeSpecifier->getVariant());
                    if (!identifier)
                    {
                        continue;
                    }
                    if (addedByParameters.count(*identifier))
                    {
                        auto* loc = context.getLocationOf(*identifier);
                        context.log(Errors::Parser::EXPECTED_TYPENAME_INSTEAD_OF_N.args(
                            *typeSpecifier->begin(), context.getSourceInterface(), *typeSpecifier->begin()));
                        if (loc)
                        {
                            context.log(Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args(
                                *loc->identifier, context.getSourceInterface(), *loc->identifier));
                        }
                    }
                }

                if (std::holds_alternative<std::unique_ptr<AbstractDeclarator>>(iter.declarator))
                {
                    if (iter.declarationSpecifiers.empty())
                    {
                        continue;
                    }

                    context.log(Errors::Parser::MISSING_PARAMETER_NAME.args(iter, context.getSourceInterface(), iter));

                    if (iter.declarationSpecifiers.size() == 1
                        && std::holds_alternative<TypeSpecifier>(iter.declarationSpecifiers[0])
                        && std::holds_alternative<std::string_view>(
                            cld::get<TypeSpecifier>(iter.declarationSpecifiers[0]).getVariant()))
                    {
                        auto& typeSpecifier = cld::get<TypeSpecifier>(iter.declarationSpecifiers[0]);
                        auto name = cld::get<std::string_view>(typeSpecifier.getVariant());
                        auto* loc = context.getLocationOf(name);
                        if (loc)
                        {
                            context.log(Notes::IDENTIFIER_IS_TYPEDEF.args(
                                *loc->identifier, context.getSourceInterface(), *loc->identifier));
                        }
                    }
                    continue;
                }
                auto& decl = cld::get<std::unique_ptr<Declarator>>(iter.declarator);
                const auto* loc = Semantics::declaratorToLoc(*decl);
                context.addToScope(loc->getText(), {start, begin, loc});
                addedByParameters.insert(loc->getText());
            }
        }
        auto compoundStatement = parseCompoundStatement(begin, end, context, false);
        context.popScope();

        if (!declarator || !compoundStatement)
        {
            return {};
        }
        context.addToScope(Semantics::declaratorToLoc(*declarator)->getText(),
                           {start, compoundStatement->begin(), Semantics::declaratorToLoc(*declarator)});
        return FunctionDefinition(start, begin, std::move(declarationSpecifiers), std::move(*declarator),
                                  std::move(declarations), std::move(*compoundStatement));
    }

    std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>> initDeclarators;
    if (!isTypedef && declarator)
    {
        const auto* loc = Semantics::declaratorToLoc(*declarator);
        context.addToScope(loc->getText(), {start, begin, loc});
    }
    if (begin->getTokenType() == Lexer::TokenType::Assignment)
    {
        begin++;
        auto initializer = parseInitializer(
            begin, end,
            context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma, Lexer::TokenType::SemiColon)));
        if (declarator && initializer)
        {
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::make_unique<Initializer>(std::move(*initializer)));
        }
        else if (declarator)
        {
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
        }
    }
    else if (declarator)
    {
        initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
    }

    while (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
    {
        begin++;
        declarator =
            parseDeclarator(begin, end,
                            context.withRecoveryTokens(Context::fromTokenTypes(
                                Lexer::TokenType::Comma, Lexer::TokenType::SemiColon, Lexer::TokenType::Assignment)));
        if (!isTypedef && declarator)
        {
            const auto* loc = Semantics::declaratorToLoc(*declarator);
            context.addToScope(loc->getText(), {start, begin, loc});
        }
        if (begin == end || begin->getTokenType() != Lexer::TokenType::Assignment)
        {
            if (declarator)
            {
                initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
            }
            continue;
        }

        begin++;
        auto initializer = parseInitializer(
            begin, end,
            context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma, Lexer::TokenType::SemiColon)));
        if (declarator && initializer)
        {
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::make_unique<Initializer>(std::move(*initializer)));
        }
        else if (declarator)
        {
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
        }
    }

    if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
    {
        context.skipUntil(begin, end);
        return {};
    }

    if (isTypedef)
    {
        for (auto& [initDeclarator, init] : initDeclarators)
        {
            const auto* loc = Semantics::declaratorToLoc(*initDeclarator);
            context.addTypedef(loc->getText(), {start, begin, loc});
        }
    }

    return Declaration(start, begin, std::move(declarationSpecifiers), std::move(initDeclarators));
}

std::optional<cld::Syntax::Declaration> cld::Parser::parseDeclaration(Lexer::CTokenIterator& begin,
                                                                      Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;

    bool declaratorMightActuallyBeTypedef = false;
    auto declarationSpecifiers = parseDeclarationSpecifierList(
        begin, end,
        context.withRecoveryTokens(firstDeclaratorSet | Context::fromTokenTypes(Lexer::TokenType::SemiColon)));
    bool isTypedef = std::any_of(declarationSpecifiers.begin(), declarationSpecifiers.end(),
                                 [](const DeclarationSpecifier& declarationSpecifier) {
                                     auto* storage = std::get_if<StorageClassSpecifier>(&declarationSpecifier);
                                     if (!storage)
                                     {
                                         return false;
                                     }
                                     return storage->getSpecifier() == StorageClassSpecifier::Typedef;
                                 });
    if (begin < end
        && std::none_of(
            declarationSpecifiers.begin(), declarationSpecifiers.end(),
            [](const DeclarationSpecifier& specifier) { return std::holds_alternative<TypeSpecifier>(specifier); })
        && begin->getTokenType() == Lexer::TokenType::Identifier && context.isTypedef(begin->getText()))
    {
        declaratorMightActuallyBeTypedef = true;
    }

    if (begin == end || begin->getTokenType() == Lexer::TokenType::SemiColon)
    {
        expect(Lexer::TokenType::SemiColon, begin, end, context);
        return Declaration(start, begin, std::move(declarationSpecifiers), {});
    }

    std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>> initDeclarators;
    bool first = true;
    do
    {
        if (first)
        {
            first = false;
        }
        else if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        else
        {
            break;
        }
        auto declarator =
            parseDeclarator(begin, end,
                            context.withRecoveryTokens(Context::fromTokenTypes(
                                Lexer::TokenType::Comma, Lexer::TokenType::SemiColon, Lexer::TokenType::Assignment)));
        if (!isTypedef && declarator)
        {
            const auto* loc = Semantics::declaratorToLoc(*declarator);
            context.addToScope(loc->getText(), {start, begin, loc});
        }
        if (begin == end || begin->getTokenType() != Lexer::TokenType::Assignment)
        {
            if (declarator)
            {
                initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
            }
        }
        else
        {
            begin++;
            auto initializer = parseInitializer(begin, end,
                                                context.withRecoveryTokens(Context::fromTokenTypes(
                                                    Lexer::TokenType::SemiColon, Lexer::TokenType::Comma)));
            if (declarator && initializer)
            {
                initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                             std::make_unique<Initializer>(std::move(*initializer)));
            }
            else if (declarator)
            {
                initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
            }
        }
    } while (true);

    if (declaratorMightActuallyBeTypedef && initDeclarators.size() == 1)
    {
        auto* loc = context.getLocationOf(Semantics::declaratorToLoc(*initDeclarators[0].first)->getText());
        if (loc)
        {
            if (!expect(Lexer::TokenType::SemiColon, begin, end, context, [&] {
                    return Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args(
                        *loc->identifier, context.getSourceInterface(), *loc->identifier);
                }))
            {
                context.skipUntil(begin, end);
                return {};
            }
        }
        else
        {
            if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
            {
                context.skipUntil(begin, end);
                return {};
            }
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            context.skipUntil(begin, end);
            return {};
        }
    }

    if (isTypedef)
    {
        for (auto& [declator, init] : initDeclarators)
        {
            const auto* loc = Semantics::declaratorToLoc(*declator);
            context.addTypedef(loc->getText(), {start, begin, loc});
        }
    }

    return Declaration(start, begin, std::move(declarationSpecifiers), std::move(initDeclarators));
}

std::optional<cld::Syntax::DeclarationSpecifier>
    cld::Parser::parseDeclarationSpecifier(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (begin < end)
    {
        auto currToken = *begin;
        switch (begin->getTokenType())
        {
            case Lexer::TokenType::TypedefKeyword:
            case Lexer::TokenType::ExternKeyword:
            case Lexer::TokenType::StaticKeyword:
            case Lexer::TokenType::AutoKeyword:
            case Lexer::TokenType::RegisterKeyword:
            case Lexer::TokenType::ConstKeyword:
            case Lexer::TokenType::RestrictKeyword:
            case Lexer::TokenType::VolatileKeyword:
            case Lexer::TokenType::InlineKeyword:
            case Lexer::TokenType::VoidKeyword:
            case Lexer::TokenType::CharKeyword:
            case Lexer::TokenType::ShortKeyword:
            case Lexer::TokenType::IntKeyword:
            case Lexer::TokenType::UnderlineBool:
            case Lexer::TokenType::LongKeyword:
            case Lexer::TokenType::FloatKeyword:
            case Lexer::TokenType::DoubleKeyword:
            case Lexer::TokenType::SignedKeyword:
            case Lexer::TokenType::UnsignedKeyword: begin++;
            default: break;
        }
        switch (currToken.getTokenType())
        {
            case Lexer::TokenType::TypedefKeyword:
                return DeclarationSpecifier{StorageClassSpecifier(start, start + 1, StorageClassSpecifier::Typedef)};
            case Lexer::TokenType::ExternKeyword:
                return DeclarationSpecifier{StorageClassSpecifier(start, start + 1, StorageClassSpecifier::Extern)};
            case Lexer::TokenType::StaticKeyword:
                return DeclarationSpecifier{StorageClassSpecifier(start, start + 1, StorageClassSpecifier::Static)};
            case Lexer::TokenType::AutoKeyword:
                return DeclarationSpecifier{StorageClassSpecifier(start, start + 1, StorageClassSpecifier::Auto)};
            case Lexer::TokenType::RegisterKeyword:
                return DeclarationSpecifier{StorageClassSpecifier(start, start + 1, StorageClassSpecifier::Register)};
            case Lexer::TokenType::ConstKeyword:
                return DeclarationSpecifier{TypeQualifier(start, start + 1, TypeQualifier::Const)};
            case Lexer::TokenType::RestrictKeyword:
                return DeclarationSpecifier{TypeQualifier(start, start + 1, TypeQualifier::Restrict)};
            case Lexer::TokenType::VolatileKeyword:
                return DeclarationSpecifier{TypeQualifier(start, start + 1, TypeQualifier::Volatile)};
            case Lexer::TokenType::InlineKeyword: return DeclarationSpecifier{FunctionSpecifier{start, start + 1}};
            case Lexer::TokenType::VoidKeyword:
                return Syntax::DeclarationSpecifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Void)};
            case Lexer::TokenType::CharKeyword:
                return Syntax::DeclarationSpecifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Char)};
            case Lexer::TokenType::ShortKeyword:
                return Syntax::DeclarationSpecifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Short)};
            case Lexer::TokenType::IntKeyword:
                return Syntax::DeclarationSpecifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Int)};
            case Lexer::TokenType::LongKeyword:
                return Syntax::DeclarationSpecifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Long)};
            case Lexer::TokenType::FloatKeyword:
                return Syntax::DeclarationSpecifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Float)};
            case Lexer::TokenType::DoubleKeyword:
                return Syntax::DeclarationSpecifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Double)};
            case Lexer::TokenType::UnderlineBool:
                return Syntax::DeclarationSpecifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Bool)};
            case Lexer::TokenType::SignedKeyword:
                return Syntax::DeclarationSpecifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Signed)};
            case Lexer::TokenType::UnsignedKeyword:
                return Syntax::DeclarationSpecifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Unsigned)};
            case Lexer::TokenType::UnionKeyword:
            case Lexer::TokenType::StructKeyword:
            {
                auto expected = parseStructOrUnionSpecifier(begin, end, context);
                if (!expected)
                {
                    context.skipUntil(begin, end);
                    return {};
                }
                return DeclarationSpecifier{TypeSpecifier(
                    start, begin, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)))};
            }
            case Lexer::TokenType::EnumKeyword:
            {
                auto expected = parseEnumSpecifier(begin, end, context);
                if (!expected)
                {
                    context.skipUntil(begin, end);
                    return {};
                }
                else
                {
                    return DeclarationSpecifier{
                        TypeSpecifier(start, begin, std::make_unique<EnumSpecifier>(std::move(*expected)))};
                }
            }
            case Lexer::TokenType::Identifier:
            {
                auto name = begin->getText();
                if (context.isTypedefInScope(name))
                {
                    return Syntax::DeclarationSpecifier{TypeSpecifier(start, ++begin, name)};
                }
                break;
            }
            default: break;
        }
    }
    if (begin != end)
    {
        context.log(Errors::Parser::EXPECTED_STORAGE_SPECIFIER_OR_TYPENAME_BEFORE_N.args(
            *begin, context.getSourceInterface(), *begin));
    }
    else
    {
        context.log(Errors::Parser::EXPECTED_STORAGE_SPECIFIER_OR_TYPENAME.args(
            diag::after(*(begin - 1)), context.getSourceInterface(), *(begin - 1)));
    }
    context.skipUntil(begin, end);
    return {};
}

std::optional<cld::Syntax::StructOrUnionSpecifier>
    cld::Parser::parseStructOrUnionSpecifier(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    bool isUnion;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::StructKeyword)
    {
        begin++;
        isUnion = false;
    }
    else if (begin < end && begin->getTokenType() == Lexer::TokenType::UnionKeyword)
    {
        begin++;
        isUnion = true;
    }
    else
    {
        context.log(Errors::Parser::EXPECTED_N_OR_N_INSTEAD_OF_N.args(*start, context.getSourceInterface(),
                                                                      Lexer::TokenType::StructKeyword,
                                                                      Lexer::TokenType::UnionKeyword, *begin));
        context.skipUntil(begin, end);
        return {};
    }

    if (begin == end)
    {
        context.log(Errors::Parser::EXPECTED_N_OR_N_AFTER_N.args(
            diag::after(*(begin - 1)), context.getSourceInterface(), Lexer::TokenType::Identifier,
            Lexer::TokenType::OpenBrace, *(begin - 1)));
        return {};
    }

    const auto* name = begin->getTokenType() == Lexer::TokenType::Identifier ? begin : nullptr;
    if (name)
    {
        begin++;
    }

    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        if (!name)
        {
            expect(Lexer::TokenType::Identifier, begin, end, context);
            context.skipUntil(begin, end);
            return {};
        }
        return StructOrUnionSpecifier(start, begin, isUnion, name, {});
    }

    const auto* openBrace = begin;
    begin++;
    std::vector<StructOrUnionSpecifier::StructDeclaration> structDeclarations;
    while (begin < end
           && (firstIsInSpecifierQualifier(*begin, context) || begin->getTokenType() == Lexer::TokenType::Identifier))
    {
        auto specifierQualifiers = parseSpecifierQualifierList(
            begin, end,
            context.withRecoveryTokens(firstDeclaratorSet | Context::fromTokenTypes(Lexer::TokenType::Colon)));

        std::vector<std::pair<std::unique_ptr<Declarator>, std::optional<ConstantExpression>>> declarators;
        bool first = true;
        do
        {
            if (first)
            {
                first = false;
            }
            else if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
            {
                begin++;
            }
            else
            {
                break;
            }
            if (begin < end && begin->getTokenType() == Lexer::TokenType::Colon)
            {
                begin++;
                auto constant = parseConditionalExpression(begin, end,
                                                           context.withRecoveryTokens(Context::fromTokenTypes(
                                                               Lexer::TokenType::Comma, Lexer::TokenType::SemiColon)));
                declarators.emplace_back(nullptr, std::move(constant));
                continue;
            }
            auto declarator =
                parseDeclarator(begin, end,
                                context.withRecoveryTokens(Context::fromTokenTypes(
                                    Lexer::TokenType::Comma, Lexer::TokenType::SemiColon, Lexer::TokenType::Colon)));
            if (begin < end && begin->getTokenType() == Lexer::TokenType::Colon)
            {
                begin++;
                auto constant = parseConditionalExpression(begin, end,
                                                           context.withRecoveryTokens(Context::fromTokenTypes(
                                                               Lexer::TokenType::Comma, Lexer::TokenType::SemiColon)));
                if (declarator)
                {
                    declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), std::move(constant));
                }
            }
            else if (declarator)
            {
                declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::optional<ConstantExpression>{});
            }
        } while (true);
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            context.skipUntil(begin, end,
                              firstDeclarationSpecifierSet | Context::fromTokenTypes(Lexer::TokenType::CloseBrace));
        }
        structDeclarations.push_back({std::move(specifierQualifiers), std::move(declarators)});
    }
    if (!expect(Lexer::TokenType::CloseBrace, begin, end, context))
    {
        context.skipUntil(begin, end);
    }
    if (structDeclarations.empty())
    {
        if (isUnion)
        {
            context.log(Errors::Parser::UNION_REQUIRES_AT_LEAST_ONE_FIELD.args(
                *openBrace, context.getSourceInterface(), std::forward_as_tuple(*openBrace, *(begin - 1))));
        }
        else
        {
            context.log(Errors::Parser::STRUCT_REQUIRES_AT_LEAST_ONE_FIELD.args(
                *openBrace, context.getSourceInterface(), std::forward_as_tuple(*openBrace, *(begin - 1))));
        }
        return {};
    }
    return StructOrUnionSpecifier(start, begin, isUnion, name, std::move(structDeclarations));
}

std::optional<cld::Syntax::SpecifierQualifier>
    cld::Parser::parseSpecifierQualifier(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (begin < end)
    {
        auto currToken = *begin;
        switch (currToken.getTokenType())
        {
            case Lexer::TokenType::ConstKeyword:
            case Lexer::TokenType::RestrictKeyword:
            case Lexer::TokenType::VolatileKeyword:
            case Lexer::TokenType::VoidKeyword:
            case Lexer::TokenType::CharKeyword:
            case Lexer::TokenType::ShortKeyword:
            case Lexer::TokenType::IntKeyword:
            case Lexer::TokenType::LongKeyword:
            case Lexer::TokenType::FloatKeyword:
            case Lexer::TokenType::UnderlineBool:
            case Lexer::TokenType::DoubleKeyword:
            case Lexer::TokenType::SignedKeyword:
            case Lexer::TokenType::UnsignedKeyword: begin++;
            default: break;
        }
        switch (currToken.getTokenType())
        {
            case Lexer::TokenType::ConstKeyword:
                return SpecifierQualifier{TypeQualifier(start, start + 1, TypeQualifier::Const)};
            case Lexer::TokenType::RestrictKeyword:
                return SpecifierQualifier{TypeQualifier(start, start + 1, TypeQualifier::Restrict)};
            case Lexer::TokenType::VolatileKeyword:
                return SpecifierQualifier{TypeQualifier(start, start + 1, TypeQualifier::Volatile)};
            case Lexer::TokenType::VoidKeyword:
                return Syntax::SpecifierQualifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Void)};
            case Lexer::TokenType::CharKeyword:
                return Syntax::SpecifierQualifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Char)};
            case Lexer::TokenType::ShortKeyword:
                return Syntax::SpecifierQualifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Short)};
            case Lexer::TokenType::IntKeyword:
                return Syntax::SpecifierQualifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Int)};
            case Lexer::TokenType::LongKeyword:
                return Syntax::SpecifierQualifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Long)};
            case Lexer::TokenType::FloatKeyword:
                return Syntax::SpecifierQualifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Float)};
            case Lexer::TokenType::UnderlineBool:
                return Syntax::SpecifierQualifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Bool)};
            case Lexer::TokenType::DoubleKeyword:
                return Syntax::SpecifierQualifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Double)};
            case Lexer::TokenType::SignedKeyword:
                return Syntax::SpecifierQualifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Signed)};
            case Lexer::TokenType::UnsignedKeyword:
                return Syntax::SpecifierQualifier{
                    TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Unsigned)};
            case Lexer::TokenType::UnionKeyword:
            case Lexer::TokenType::StructKeyword:
            {
                auto expected = parseStructOrUnionSpecifier(begin, end, context);
                if (!expected)
                {
                    context.skipUntil(begin, end);
                    return {};
                }
                return SpecifierQualifier{TypeSpecifier(
                    start, begin, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)))};
            }
            case Lexer::TokenType::EnumKeyword:
            {
                auto expected = parseEnumSpecifier(begin, end, context);
                if (!expected)
                {
                    context.skipUntil(begin, end);
                    return {};
                }
                return SpecifierQualifier{
                    TypeSpecifier(start, begin, std::make_unique<EnumSpecifier>(std::move(*expected)))};
            }
            case Lexer::TokenType::Identifier:
            {
                auto name = begin->getText();
                if (context.isTypedefInScope(name))
                {
                    return Syntax::SpecifierQualifier{TypeSpecifier(start, ++begin, name)};
                }
                else if (context.isTypedef(name))
                {
                    auto* loc = context.getLocationOf(begin->getText());
                    CLD_ASSERT(loc);
                    context.log(Errors::Parser::EXPECTED_TYPENAME_INSTEAD_OF_N.args(
                        *begin, context.getSourceInterface(), *begin));
                    context.log(Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args(
                        *loc->identifier, context.getSourceInterface(), *loc->identifier));
                    context.skipUntil(begin, end);
                    return {};
                }
                break;
            }
            default: break;
        }
    }
    if (begin < end)
    {
        context.log(Errors::Parser::EXPECTED_TYPENAME_BEFORE_N.args(*begin, context.getSourceInterface(), *begin));
    }
    else
    {
        context.log(Errors::Parser::EXPECTED_TYPENAME.args(diag::after(*(begin - 1)), context.getSourceInterface(),
                                                           *(begin - 1)));
    }
    context.skipUntil(begin, end);
    return {};
}

std::optional<cld::Syntax::Declarator> cld::Parser::parseDeclarator(Lexer::CTokenIterator& begin,
                                                                    Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == Lexer::TokenType::Asterisk)
    {
        auto result = parsePointer(begin, end, context.withRecoveryTokens(firstDeclaratorSet));
        pointers.push_back(std::move(result));
    }
    auto directDeclarator = parseDirectDeclarator(begin, end, context);
    if (!directDeclarator)
    {
        return {};
    }
    return Declarator(start, begin, std::move(pointers), std::move(*directDeclarator));
}

std::optional<cld::Syntax::DirectDeclarator>
    cld::Parser::parseDirectDeclarator(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    std::unique_ptr<DirectDeclarator> directDeclarator;

    const auto* start = begin;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
    {
        const auto* currToken = begin;
        begin++;
        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorIdentifier(start, begin, currToken));
    }
    else if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenParentheses)
    {
        auto scope = context.parenthesesEntered(begin);
        const auto* openPpos = begin;
        begin++;
        auto declarator = parseDeclarator(
            begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
        if (declarator)
        {
            directDeclarator = std::make_unique<DirectDeclarator>(
                DirectDeclaratorParentheses(start, begin, std::make_unique<Declarator>(std::move(*declarator))));
        }
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                    [&] { return Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos); }))
        {
            context.skipUntil(
                begin, end,
                Context::fromTokenTypes(Lexer::TokenType::OpenParentheses, Lexer::TokenType::OpenSquareBracket));
        }
    }
    else
    {
        if (begin == end)
        {
            context.log(Errors::Parser::EXPECTED_N_OR_N.args(diag::after(*(begin - 1)), context.getSourceInterface(),
                                                             Lexer::TokenType::OpenParentheses,
                                                             Lexer::TokenType::Identifier, *(begin - 1)));
        }
        else
        {
            context.log(Errors::Parser::EXPECTED_N_OR_N_INSTEAD_OF_N.args(*begin, context.getSourceInterface(),
                                                                          Lexer::TokenType::OpenParentheses,
                                                                          Lexer::TokenType::Identifier, *begin));
        }
        context.skipUntil(
            begin, end,
            Context::fromTokenTypes(Lexer::TokenType::OpenParentheses, Lexer::TokenType::OpenSquareBracket));
    }

    while (begin < end
           && (begin->getTokenType() == Lexer::TokenType::OpenParentheses
               || begin->getTokenType() == Lexer::TokenType::OpenSquareBracket))
    {
        switch (begin->getTokenType())
        {
            case Lexer::TokenType::OpenParentheses:
            {
                auto scope = context.parenthesesEntered(begin);
                const auto* openPpos = begin;
                auto checkForClose = std::optional{cld::ScopeExit([&] {
                    if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context, [&] {
                            return Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos);
                        }))
                    {
                        context.skipUntil(begin, end,
                                          Context::fromTokenTypes(Lexer::TokenType::OpenParentheses,
                                                                  Lexer::TokenType::OpenSquareBracket));
                    }
                })};
                begin++;
                if (begin < end && firstIsInParameterTypeList(*begin, context))
                {
                    auto parameterTypeList = parseParameterTypeList(
                        begin, end,
                        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
                    checkForClose.reset();
                    if (directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorParenthesesParameters(
                            start, begin, std::move(*directDeclarator), std::move(parameterTypeList)));
                    }
                    break;
                }
                if (begin == end)
                {
                    break;
                }

                std::vector<Lexer::CTokenIterator> identifiers;
                if (begin->getTokenType() == Lexer::TokenType::Identifier)
                {
                    identifiers.push_back(begin);
                    begin++;
                    while (begin < end
                           && (begin->getTokenType() == Lexer::TokenType::Comma
                               || begin->getTokenType() == Lexer::TokenType::Identifier))
                    {
                        if (!expect(Lexer::TokenType::Comma, begin, end, context))
                        {
                            context.skipUntil(begin, end,
                                              Context::fromTokenTypes(Lexer::TokenType::Identifier,
                                                                      Lexer::TokenType::CloseParentheses));
                        }
                        std::string_view name;
                        if (!expectIdentifier(begin, end, context, name))
                        {
                            context.skipUntil(
                                begin, end,
                                Context::fromTokenTypes(Lexer::TokenType::Comma, Lexer::TokenType::CloseParentheses));
                            continue;
                        }

                        if (!context.isTypedef(name))
                        {
                            identifiers.push_back(begin - 1);
                            continue;
                        }

                        context.log(Errors::Parser::EXPECTED_N_INSTEAD_OF_TYPENAME.args(
                            *(begin - 1), context.getSourceInterface(), Lexer::TokenType::Identifier, *(begin - 1)));
                        if (auto* loc = context.getLocationOf(name))
                        {
                            context.log(Notes::IDENTIFIER_IS_TYPEDEF.args(
                                *loc->identifier, context.getSourceInterface(), *loc->identifier));
                        }
                    }
                }
                checkForClose.reset();
                if (directDeclarator)
                {
                    directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorParenthesesIdentifiers(
                        start, begin, std::move(*directDeclarator), std::move(identifiers)));
                }
                break;
            }
            case Lexer::TokenType::OpenSquareBracket:
            {
                auto scope = context.squareBracketEntered(begin);
                const auto* openPpos = begin;
                auto checkForClose = std::optional{cld::ScopeExit([&] {
                    if (!expect(Lexer::TokenType::CloseSquareBracket, begin, end, context, [&] {
                            return Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos);
                        }))
                    {
                        context.skipUntil(begin, end,
                                          Context::fromTokenTypes(Lexer::TokenType::OpenParentheses,
                                                                  Lexer::TokenType::OpenSquareBracket));
                    }
                })};
                begin++;
                if (begin == end)
                {
                    return {};
                }

                if (begin->getTokenType() == Lexer::TokenType::StaticKeyword)
                {
                    const auto* staticLoc = begin++;
                    std::vector<TypeQualifier> typeQualifiers;
                    while (begin < end
                           && (begin->getTokenType() == Lexer::TokenType::ConstKeyword
                               || begin->getTokenType() == Lexer::TokenType::RestrictKeyword
                               || begin->getTokenType() == Lexer::TokenType::VolatileKeyword))
                    {
                        switch (begin->getTokenType())
                        {
                            case Lexer::TokenType::ConstKeyword:
                                typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Const);
                                break;
                            case Lexer::TokenType::RestrictKeyword:
                                typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Restrict);
                                break;
                            case Lexer::TokenType::VolatileKeyword:
                                typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Volatile);
                                break;
                            default: CLD_UNREACHABLE;
                        }
                        begin++;
                    }
                    auto assignmentExpression = cld::Parser::parseAssignmentExpression(
                        begin, end,
                        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseSquareBracket)));
                    checkForClose.reset();
                    if (assignmentExpression && directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(
                            DirectDeclaratorStatic(start, begin, std::move(directDeclarator), staticLoc,
                                                   std::move(typeQualifiers), std::move(*assignmentExpression)));
                    }
                    break;
                }

                std::vector<TypeQualifier> typeQualifiers;
                while (begin < end
                       && (begin->getTokenType() == Lexer::TokenType::ConstKeyword
                           || begin->getTokenType() == Lexer::TokenType::RestrictKeyword
                           || begin->getTokenType() == Lexer::TokenType::VolatileKeyword))
                {
                    switch (begin->getTokenType())
                    {
                        case Lexer::TokenType::ConstKeyword:
                            typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Const);
                            break;
                        case Lexer::TokenType::RestrictKeyword:
                            typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Restrict);
                            break;
                        case Lexer::TokenType::VolatileKeyword:
                            typeQualifiers.emplace_back(begin, begin + 1, TypeQualifier::Volatile);
                            break;
                        default: CLD_UNREACHABLE;
                    }
                    begin++;
                }
                if (begin == end)
                {
                    break;
                }

                if (begin->getTokenType() == Lexer::TokenType::StaticKeyword)
                {
                    const auto* staticLoc = begin++;
                    auto assignment = cld::Parser::parseAssignmentExpression(
                        begin, end,
                        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseSquareBracket)));
                    checkForClose.reset();
                    if (assignment && directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(
                            DirectDeclaratorStatic(start, begin, std::move(directDeclarator), staticLoc,
                                                   std::move(typeQualifiers), std::move(*assignment)));
                    }
                }
                else if (begin->getTokenType() == Lexer::TokenType::Asterisk)
                {
                    const auto* asterisk = begin++;
                    checkForClose.reset();
                    if (directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorAsterisk(
                            start, begin, std::move(*directDeclarator), std::move(typeQualifiers), asterisk));
                    }
                }
                else if (firstIsInAssignmentExpression(*begin, context))
                {
                    auto assignment = cld::Parser::parseAssignmentExpression(
                        begin, end,
                        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseSquareBracket)));
                    checkForClose.reset();
                    if (assignment && directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorNoStaticOrAsterisk(
                            start, begin, std::move(directDeclarator), std::move(typeQualifiers),
                            std::make_unique<AssignmentExpression>(std::move(*assignment))));
                    }
                }
                else
                {
                    checkForClose.reset();
                    if (directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorNoStaticOrAsterisk(
                            start, begin, std::move(directDeclarator), std::move(typeQualifiers), nullptr));
                    }
                }
                break;
            }
            default: break;
        }
    }
    if (!directDeclarator)
    {
        return {};
    }
    return std::move(*directDeclarator);
}

cld::Syntax::ParameterTypeList cld::Parser::parseParameterTypeList(Lexer::CTokenIterator& begin,
                                                                   Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    auto parameterList =
        parseParameterList(begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
    bool hasEllipse = false;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
    {
        begin++;
        if (begin == end || begin->getTokenType() != Lexer::TokenType::Ellipse)
        {
            context.log(Errors::Parser::EXPECTED_PARAMETER_AFTER_N.args(diag::after(*(begin - 1)),
                                                                        context.getSourceInterface(), *(begin - 1)));
        }
        else
        {
            begin++;
            hasEllipse = true;
        }
    }
    return ParameterTypeList(start, begin, std::move(parameterList), hasEllipse);
}

cld::Syntax::ParameterList cld::Parser::parseParameterList(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                           Context& context)
{
    const auto* start = begin;
    std::vector<ParameterDeclaration> parameterDeclarations;
    bool first = true;
    while (begin < end)
    {
        if (first)
        {
            first = false;
        }
        else if (begin->getTokenType() == Lexer::TokenType::Comma
                 && (begin + 1 == end || (begin + 1)->getTokenType() != Lexer::TokenType::Ellipse))
        {
            begin++;
        }
        else
        {
            break;
        }
        const auto* parameterBegin = begin;
        auto declarationSpecifiers = parseDeclarationSpecifierList(
            begin, end,
            context.withRecoveryTokens(firstAbstractDeclaratorSet | firstDeclaratorSet
                                       | Context::fromTokenTypes(Lexer::TokenType::Comma)));

        // Skip past everything that is part of the pointer declaration inside of the (possibly abstract) declarator
        const auto* result = std::find_if(begin, end, [](const Lexer::CToken& token) {
            switch (token.getTokenType())
            {
                case Lexer::TokenType::Asterisk:
                case Lexer::TokenType::ConstKeyword:
                case Lexer::TokenType::VolatileKeyword:
                case Lexer::TokenType::RestrictKeyword: return false;
                default: break;
            }
            return true;
        });
        if (result == end)
        {
            begin = result;
            parameterDeclarations.push_back(ParameterDeclaration{
                {parameterBegin, begin}, std::move(declarationSpecifiers), std::unique_ptr<AbstractDeclarator>{}});
            continue;
        }

        switch (result->getTokenType())
        {
            default:
            {
                if (result == begin)
                {
                    parameterDeclarations.push_back({{parameterBegin, begin},
                                                     std::move(declarationSpecifiers),
                                                     std::unique_ptr<AbstractDeclarator>()});
                    break;
                }
                [[fallthrough]];
            }
            case Lexer::TokenType::OpenSquareBracket:
            {
                auto abstractDeclarator = parseAbstractDeclarator(
                    begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
                parameterDeclarations.push_back({{parameterBegin, begin},
                                                 std::move(declarationSpecifiers),
                                                 std::make_unique<AbstractDeclarator>(std::move(abstractDeclarator))});
                break;
            }
            case Lexer::TokenType::Identifier:
            {
                auto declarator = parseDeclarator(
                    begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
                if (declarator)
                {
                    parameterDeclarations.push_back({{parameterBegin, begin},
                                                     std::move(declarationSpecifiers),
                                                     std::make_unique<Declarator>(std::move(*declarator))});
                }
                break;
            }
            case Lexer::TokenType::OpenParentheses:
            {
                while (result < end && result->getTokenType() == Lexer::TokenType::OpenParentheses)
                {
                    // Ambiguous
                    result++;
                    result = std::find_if(result, end, [](const Lexer::CToken& token) {
                        switch (token.getTokenType())
                        {
                            case Lexer::TokenType::Asterisk:
                            case Lexer::TokenType::ConstKeyword:
                            case Lexer::TokenType::VolatileKeyword:
                            case Lexer::TokenType::RestrictKeyword: return false;
                            default: break;
                        }
                        return true;
                    });
                    if (result < end && result->getTokenType() == Lexer::TokenType::Identifier)
                    {
                        auto declarator = parseDeclarator(
                            begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
                        if (declarator)
                        {
                            parameterDeclarations.push_back({{parameterBegin, begin},
                                                             std::move(declarationSpecifiers),
                                                             std::make_unique<Declarator>(std::move(*declarator))});
                        }
                        break;
                    }
                    else if (result < end && result->getTokenType() != Lexer::TokenType::OpenParentheses)
                    {
                        auto abstractDeclarator = parseAbstractDeclarator(
                            begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
                        parameterDeclarations.push_back(
                            {{parameterBegin, begin},
                             std::move(declarationSpecifiers),
                             std::make_unique<AbstractDeclarator>(std::move(abstractDeclarator))});
                        break;
                    }
                }
                break;
            }
        }
    }
    if (first)
    {
        context.log(Errors::Parser::PARAMETER_LIST_REQUIRES_AT_LEAST_ONE_PARAMETER.args(
            *begin, context.getSourceInterface(), *begin));
    }
    return ParameterList(start, begin, std::move(parameterDeclarations));
}

cld::Syntax::Pointer cld::Parser::parsePointer(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                               Context& context)
{
    const auto* start = begin;
    if (!expect(Lexer::TokenType::Asterisk, begin, end, context))
    {
        context.skipUntil(begin, end,
                          Context::fromTokenTypes(Lexer::TokenType::ConstKeyword, Lexer::TokenType::RestrictKeyword,
                                                  Lexer::TokenType::VolatileKeyword));
    }
    std::vector<TypeQualifier> typeQualifier;
    while (begin < end
           && (begin->getTokenType() == Lexer::TokenType::ConstKeyword
               || begin->getTokenType() == Lexer::TokenType::RestrictKeyword
               || begin->getTokenType() == Lexer::TokenType::VolatileKeyword))
    {
        switch (begin->getTokenType())
        {
            case Lexer::TokenType::ConstKeyword:
                typeQualifier.emplace_back(begin, begin + 1, TypeQualifier::Const);
                break;
            case Lexer::TokenType::RestrictKeyword:
                typeQualifier.emplace_back(begin, begin + 1, TypeQualifier::Restrict);
                break;
            case Lexer::TokenType::VolatileKeyword:
                typeQualifier.emplace_back(begin, begin + 1, TypeQualifier::Volatile);
                break;
            default: CLD_UNREACHABLE;
        }
        begin++;
    }
    return Pointer(start, begin, std::move(typeQualifier));
}

cld::Syntax::AbstractDeclarator cld::Parser::parseAbstractDeclarator(Lexer::CTokenIterator& begin,
                                                                     Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == Lexer::TokenType::Asterisk)
    {
        auto result = parsePointer(begin, end, context.withRecoveryTokens(firstAbstractDeclaratorSet));
        pointers.push_back(std::move(result));
    }
    if (begin < end ? !firstIsInDirectAbstractDeclarator(*begin, context) && !pointers.empty() : !pointers.empty())
    {
        return AbstractDeclarator(start, begin, std::move(pointers), {});
    }
    auto result = parseDirectAbstractDeclarator(begin, end, context);
    return AbstractDeclarator(start, begin, std::move(pointers),
                              result ? std::move(*result) : std::optional<DirectAbstractDeclarator>{});
}

std::optional<cld::Syntax::DirectAbstractDeclarator>
    cld::Parser::parseDirectAbstractDeclarator(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                               Context& context)
{
    std::unique_ptr<DirectAbstractDeclarator> directAbstractDeclarator;
    bool first = true;
    const auto* start = begin;
    while (begin < end
           && (begin->getTokenType() == Lexer::TokenType::OpenParentheses
               || begin->getTokenType() == Lexer::TokenType::OpenSquareBracket))
    {
        switch (begin->getTokenType())
        {
            case Lexer::TokenType::OpenParentheses:
            {
                auto scope = context.parenthesesEntered(begin);
                const auto* openPpos = begin;
                auto closeParenth = std::optional{cld::ScopeExit([&] {
                    if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context, [&] {
                            return Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos);
                        }))
                    {
                        context.skipUntil(begin, end,
                                          Context::fromTokenTypes(Lexer::TokenType::OpenParentheses,
                                                                  Lexer::TokenType::OpenSquareBracket));
                    }
                })};
                begin++;
                if (begin < end && firstIsInDeclarationSpecifier(*begin, context))
                {
                    auto parameterTypeList = parseParameterTypeList(
                        begin, end,
                        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
                    closeParenth.reset();
                    directAbstractDeclarator =
                        std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParameterTypeList(
                            start, begin, std::move(directAbstractDeclarator),
                            std::make_unique<ParameterTypeList>(std::move(parameterTypeList))));
                }
                else if (begin < end && first && firstIsInAbstractDeclarator(*begin, context))
                {
                    auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
                    closeParenth.reset();
                    directAbstractDeclarator =
                        std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParentheses(
                            start, begin, std::make_unique<AbstractDeclarator>(std::move(abstractDeclarator))));
                }
                else
                {
                    closeParenth.reset();
                    directAbstractDeclarator =
                        std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParameterTypeList(
                            start, begin, std::move(directAbstractDeclarator), nullptr));
                }

                break;
            }
            case Lexer::TokenType::OpenSquareBracket:
            {
                auto scope = context.squareBracketEntered(begin);
                const auto* openPpos = begin;
                auto closeParenth = std::optional{cld::ScopeExit([&] {
                    if (!expect(Lexer::TokenType::CloseSquareBracket, begin, end, context, [&] {
                            return Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos);
                        }))
                    {
                        context.skipUntil(begin, end,
                                          Context::fromTokenTypes(Lexer::TokenType::OpenParentheses,
                                                                  Lexer::TokenType::OpenSquareBracket));
                    }
                })};
                begin++;
                if (begin < end && begin->getTokenType() == Lexer::TokenType::Asterisk)
                {
                    const auto* asterisk = begin++;
                    closeParenth.reset();
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                        DirectAbstractDeclaratorAsterisk(start, begin, std::move(directAbstractDeclarator), asterisk));
                    break;
                }

                if (begin < end && firstIsInAssignmentExpression(*begin, context))
                {
                    auto assignment = parseAssignmentExpression(
                        begin, end,
                        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
                    closeParenth.reset();
                    if (assignment)
                    {
                        directAbstractDeclarator =
                            std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorAssignmentExpression(
                                start, begin, std::move(directAbstractDeclarator),
                                std::make_unique<AssignmentExpression>(std::move(*assignment))));
                    }
                }
                else
                {
                    closeParenth.reset();
                    directAbstractDeclarator =
                        std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorAssignmentExpression(
                            start, begin, std::move(directAbstractDeclarator), nullptr));
                }

                break;
            }
            default: break;
        }
        first = false;
    }
    if (first)
    {
        if (begin == end)
        {
            context.log(Errors::Parser::EXPECTED_N_OR_N.args(diag::after(*(begin - 1)), context.getSourceInterface(),
                                                             Lexer::TokenType::OpenParentheses,
                                                             Lexer::TokenType::OpenSquareBracket, *(begin - 1)));
        }
        else
        {
            context.log(Errors::Parser::EXPECTED_N_OR_N_INSTEAD_OF_N.args(*begin, context.getSourceInterface(),
                                                                          Lexer::TokenType::OpenParentheses,
                                                                          Lexer::TokenType::OpenSquareBracket, *begin));
        }
        context.skipUntil(begin, end);
        return {};
    }
    if (!directAbstractDeclarator)
    {
        return {};
    }
    return std::move(*directAbstractDeclarator);
}

std::optional<cld::Syntax::EnumSpecifier> cld::Parser::parseEnumSpecifier(Lexer::CTokenIterator& begin,
                                                                          Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (!expect(Lexer::TokenType::EnumKeyword, begin, end, context))
    {
        context.skipUntil(begin, end,
                          Context::fromTokenTypes(Lexer::TokenType::OpenBrace, Lexer::TokenType::Identifier));
    }
    const Lexer::CToken* name = nullptr;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
    {
        name = begin;
        begin++;
    }
    else if (begin == end)
    {
        context.log(Errors::Parser::EXPECTED_N_AFTER_N.args(diag::after(*(begin - 1)), context.getSourceInterface(),
                                                            Lexer::TokenType::Identifier, *(begin - 1)));
        context.skipUntil(begin, end);
        return {};
    }

    const auto* openPpos = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        if (!name)
        {
            expect(Lexer::TokenType::Identifier, begin, end, context);
            context.skipUntil(begin, end);
            return {};
        }
        return EnumSpecifier(start, begin, name);
    }
    else
    {
        begin++;
    }

    bool inLoop = false;
    std::vector<std::pair<Lexer::CTokenIterator, std::optional<ConstantExpression>>> values;
    while (
        begin < end
        && (begin->getTokenType() == Lexer::TokenType::Identifier || begin->getTokenType() == Lexer::TokenType::Comma))
    {
        inLoop = true;
        const auto* thisValueStart = begin;
        if (!expect(Lexer::TokenType::Identifier, begin, end, context))
        {
            context.skipUntil(begin, end,
                              Context::fromTokenTypes(Lexer::TokenType::Assignment, Lexer::TokenType::Comma,
                                                      Lexer::TokenType::CloseBrace));
        }
        else
        {
            values.emplace_back(thisValueStart, std::optional<ConstantExpression>{});
        }

        if (begin < end && begin->getTokenType() == Lexer::TokenType::Assignment)
        {
            begin++;
            auto constant = parseConditionalExpression(begin, end,
                                                       context.withRecoveryTokens(Context::fromTokenTypes(
                                                           Lexer::TokenType::CloseBrace, Lexer::TokenType::Comma)));
            if (thisValueStart->getTokenType() == Lexer::TokenType::Identifier)
            {
                values.back().second = std::move(constant);
            }
        }

        if (thisValueStart->getTokenType() == Lexer::TokenType::Identifier)
        {
            context.addToScope(thisValueStart->getText(), {thisValueStart, begin, thisValueStart});
        }

        if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        else if (begin == end || begin->getTokenType() != Lexer::TokenType::CloseBrace)
        {
            if (begin == end)
            {
                context.log(Errors::Parser::EXPECTED_N.args(diag::after(*(begin - 1)), context.getSourceInterface(),
                                                            Lexer::TokenType::CloseBrace, *(begin - 1)));
                context.log(Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos));
                return {};
            }
            else
            {
                context.log(Errors::Parser::EXPECTED_N_INSTEAD_OF_N.args(*begin, context.getSourceInterface(),
                                                                         Lexer::TokenType::Comma, *begin));
                context.skipUntil(begin, end,
                                  Context::fromTokenTypes(Lexer::TokenType::Identifier, Lexer::TokenType::CloseBrace));
            }
        }
    }
    if (begin < end)
    {
        begin++;
    }
    else
    {
        expect(Lexer::TokenType::CloseBrace, begin, end, context);
        return {};
    }
    if (!inLoop)
    {
        context.log(Errors::Parser::ENUM_REQUIRES_AT_LEAST_ONE_VALUE.args(
            *openPpos, context.getSourceInterface(), std::forward_as_tuple(*openPpos, *(begin - 1))));
    }
    return EnumSpecifier(start, begin, EnumDeclaration(start, begin, name, std::move(values)));
}

std::optional<cld::Syntax::CompoundStatement> cld::Parser::parseCompoundStatement(Lexer::CTokenIterator& begin,
                                                                                  Lexer::CTokenIterator end,
                                                                                  cld::Parser::Context& context,
                                                                                  bool pushScope)
{
    const auto* start = begin;
    auto scope = context.braceEntered(begin);
    bool braceSeen = true;
    if (!expect(Lexer::TokenType::OpenBrace, begin, end, context))
    {
        braceSeen = false;
        context.skipUntil(begin, end, firstCompoundItem | Context::fromTokenTypes(Lexer::TokenType::CloseBrace));
    }
    std::vector<CompoundItem> items;
    if (pushScope)
    {
        context.pushScope();
    }
    while (begin < end && firstIsInCompoundItem(*begin, context))
    {
        auto result = parseCompoundItem(
            begin, end,
            context.withRecoveryTokens(firstCompoundItem | Context::fromTokenTypes(Lexer::TokenType::CloseBrace)));
        if (result)
        {
            items.push_back(std::move(*result));
        }
    }
    if (pushScope)
    {
        context.popScope();
    }
    if (braceSeen)
    {
        if (!expect(Lexer::TokenType::CloseBrace, begin, end, context,
                    [&] { return Notes::TO_MATCH_N_HERE.args(*start, context.getSourceInterface(), *start); }))
        {
            context.skipUntil(begin, end);
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::CloseBrace, begin, end, context))
        {
            context.skipUntil(begin, end);
        }
    }
    return CompoundStatement(start, begin, std::move(items));
}

std::optional<cld::Syntax::CompoundItem> cld::Parser::parseCompoundItem(Lexer::CTokenIterator& begin,
                                                                        Lexer::CTokenIterator end, Context& context)
{
    if (firstIsInDeclarationSpecifier(*begin, context)
        && !(begin < end && begin->getTokenType() == Lexer::TokenType::Identifier && begin + 1 < end
             && (begin + 1)->getTokenType() == Lexer::TokenType::Colon))
    {
        auto declaration = parseDeclaration(begin, end, context);
        if (!declaration)
        {
            return {};
        }
        return CompoundItem(std::move(*declaration));
    }

    auto statement = parseStatement(begin, end, context);
    if (!statement)
    {
        return {};
    }
    return CompoundItem(std::move(*statement));
}

std::optional<cld::Syntax::Initializer> cld::Parser::parseInitializer(Lexer::CTokenIterator& begin,
                                                                      Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        auto assignment = parseAssignmentExpression(begin, end, context);
        if (!assignment)
        {
            return {};
        }
        return Initializer(start, begin, std::move(*assignment));
    }

    auto scope = context.braceEntered(begin);
    begin++;
    auto initializerList = parseInitializerList(
        begin, end,
        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseBrace, Lexer::TokenType::Comma)));
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
    {
        begin++;
    }
    if (!expect(Lexer::TokenType::CloseBrace, begin, end, context))
    {
        context.skipUntil(begin, end);
    }
    if (!initializerList)
    {
        return {};
    }
    return Initializer{start, begin, std::move(*initializerList)};
}

std::optional<cld::Syntax::InitializerList>
    cld::Parser::parseInitializerList(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    typename InitializerList::vector vector;
    bool first = true;
    do
    {
        if (first)
        {
            first = false;
        }
        else
        {
            expect(Lexer::TokenType::Comma, begin, end, context);
        }

        Syntax::InitializerList::DesignatorList designation;
        bool hasDesignation = false;
        while (begin < end
               && (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket
                   || begin->getTokenType() == Lexer::TokenType::Dot))
        {
            hasDesignation = true;
            if (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket)
            {
                auto scope = context.squareBracketEntered(begin);
                const auto* openPpos = begin;
                begin++;
                auto constant = parseConditionalExpression(
                    begin, end,
                    context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseSquareBracket)));
                if (constant)
                {
                    designation.emplace_back(std::move(*constant));
                }
                if (!expect(Lexer::TokenType::CloseSquareBracket, begin, end, context, [&] {
                        return Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos);
                    }))
                {
                    context.skipUntil(begin, end,
                                      Context::fromTokenTypes(Lexer::TokenType::Assignment,
                                                              Lexer::TokenType::OpenSquareBracket,
                                                              Lexer::TokenType::Dot));
                }
            }
            else
            {
                const auto* token = ++begin;
                if (!expect(Lexer::TokenType::Identifier, begin, end, context))
                {
                    context.skipUntil(begin, end,
                                      Context::fromTokenTypes(Lexer::TokenType::Assignment,
                                                              Lexer::TokenType::OpenSquareBracket,
                                                              Lexer::TokenType::Dot));
                }
                designation.emplace_back(token);
            }
        }
        if (hasDesignation)
        {
            if (!expect(Lexer::TokenType::Assignment, begin, end, context))
            {
                context.skipUntil(begin, end, firstInitializerSet);
            }
        }
        auto initializer = parseInitializer(begin, end, context);
        if (!initializer)
        {
            continue;
        }
        vector.push_back({std::move(*initializer), std::move(designation)});
    } while (begin < end && begin + 1 < end
             && (firstIsInInitializerList(begin->getTokenType() == Lexer::TokenType::Comma ? *(begin + 1) : *begin,
                                          context)));

    return InitializerList{start, begin, std::move(vector)};
}

std::optional<cld::Syntax::Statement> cld::Parser::parseStatement(Lexer::CTokenIterator& begin,
                                                                  Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (begin != end)
    {
        switch (begin->getTokenType())
        {
            case Lexer::TokenType::ReturnKeyword:
            {
                auto ret = parseReturnStatement(begin, end, context);
                return Statement(std::move(ret));
            }
            case Lexer::TokenType::IfKeyword:
            {
                auto ifStat = parseIfStatement(begin, end, context);
                if (!ifStat)
                {
                    return {};
                }
                return Statement(std::move(*ifStat));
            }
            case Lexer::TokenType::SwitchKeyword:
            {
                auto switchStat = parseSwitchStatement(begin, end, context);
                if (!switchStat)
                {
                    return {};
                }
                return Statement(std::move(*switchStat));
            }
            case Lexer::TokenType::OpenBrace:
            {
                auto compoundStatement = parseCompoundStatement(begin, end, context, false);
                if (!compoundStatement)
                {
                    return {};
                }
                return Statement{std::move(*compoundStatement)};
            }
            case Lexer::TokenType::ForKeyword:
            {
                auto forStat = parseForStatement(begin, end, context);
                if (!forStat)
                {
                    return {};
                }
                return Statement(std::move(*forStat));
            }
            case Lexer::TokenType::WhileKeyword:
            {
                auto headWhile = parseHeadWhileStatement(begin, end, context);
                if (!headWhile)
                {
                    return {};
                }
                return Statement(std::move(*headWhile));
            }
            case Lexer::TokenType::DoKeyword:
            {
                auto doWhile = parseFootWhileStatement(begin, end, context);
                if (!doWhile)
                {
                    return {};
                }
                return Statement(std::move(*doWhile));
            }
            case Lexer::TokenType::BreakKeyword:
            {
                begin++;
                if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
                {
                    context.skipUntil(begin, end);
                    return {};
                }
                return Statement(BreakStatement(start, begin));
            }
            case Lexer::TokenType::ContinueKeyword:
            {
                begin++;
                if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
                {
                    context.skipUntil(begin, end);
                    return {};
                }
                return Statement(ContinueStatement(start, begin));
            }
            case Lexer::TokenType::DefaultKeyword:
            {
                const auto* defaultToken = begin++;
                const auto* colonToken = begin;
                if (!expect(Lexer::TokenType::Colon, begin, end, context))
                {
                    context.skipUntil(begin, end, firstStatementSet);
                }
                auto statement = parseStatement(begin, end, context);
                if (!statement)
                {
                    return {};
                }
                return Statement(DefaultStatement(start, begin, defaultToken, colonToken,
                                                  std::make_unique<Statement>(std::move(*statement))));
            }
            case Lexer::TokenType::CaseKeyword:
            {
                const auto* caseToken = begin++;
                auto expression = parseConditionalExpression(
                    begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Colon)));
                const auto* colonToken = begin;
                if (!expect(Lexer::TokenType::Colon, begin, end, context))
                {
                    context.skipUntil(begin, end, firstStatementSet);
                }
                auto statement = parseStatement(begin, end, context);
                if (!statement || !expression)
                {
                    return {};
                }
                return Statement(CaseStatement(start, begin, caseToken, std::move(*expression), colonToken,
                                               std::make_unique<Statement>(std::move(*statement))));
            }
            case Lexer::TokenType::GotoKeyword:
            {
                const auto* id = ++begin;
                if (!expect(Lexer::TokenType::Identifier, begin, end, context))
                {
                    id = nullptr;
                    if (begin < end && begin + 1 < end && (begin + 1)->getTokenType() == Lexer::TokenType::SemiColon)
                    {
                        begin++;
                    }
                    else
                    {
                        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::SemiColon));
                    }
                }
                if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
                {
                    context.skipUntil(begin, end);
                }
                return Statement(GotoStatement(start, begin, id));
            }
            case Lexer::TokenType::Identifier:
            {
                if (begin + 1 < end && (begin + 1)->getTokenType() == Lexer::TokenType::Colon)
                {
                    const auto* name = begin;
                    begin += 2;
                    auto statement = parseStatement(begin, end, context);
                    if (!statement)
                    {
                        return {};
                    }
                    return Statement(LabelStatement(start, begin, name, std::move(*statement)));
                }
                [[fallthrough]];
            }
            default:
            {
                break;
            }
        }
    }
    if (begin != end && begin->getTokenType() != Lexer::TokenType::SemiColon)
    {
        auto expression = parseExpression(
            begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::SemiColon)));
        std::optional<Message> note;
        if (start + 1 == begin && start->getTokenType() == Lexer::TokenType::Identifier
            && context.isTypedef(start->getText()) && begin->getTokenType() == Lexer::TokenType::Identifier)
        {
            auto* loc = context.getLocationOf(start->getText());
            if (loc)
            {
                if (!expect(Lexer::TokenType::SemiColon, begin, end, context, [&] {
                        return Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args(
                            *loc->identifier, context.getSourceInterface(), *loc->identifier);
                    }))
                {
                    context.skipUntil(begin, end);
                }
            }
            else
            {
                if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
                {
                    context.skipUntil(begin, end);
                }
            }
        }
        else
        {
            if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
            {
                context.skipUntil(begin, end);
            }
        }
        return Statement(ExpressionStatement(start, begin, std::make_unique<Expression>(std::move(expression))));
    }
    else
    {
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            context.skipUntil(begin, end);
        }
        return Statement(ExpressionStatement(start, begin));
    }
}

std::optional<cld::Syntax::HeadWhileStatement>
    cld::Parser::parseHeadWhileStatement(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (!expect(Lexer::TokenType::WhileKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
    }
    std::optional<Lexer::CTokenIterator> openPpos;
    if (!expect(Lexer::TokenType::OpenParentheses, begin, end, context))
    {
        context.skipUntil(begin, end, firstExpressionSet);
    }
    else
    {
        openPpos = begin - 1;
    }
    auto expression = parseExpression(
        begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
    std::optional<Message> note;
    if (openPpos)
    {
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                    [&] { return Notes::TO_MATCH_N_HERE.args(**openPpos, context.getSourceInterface(), **openPpos); }))
        {
            context.skipUntil(begin, end, firstStatementSet);
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context))
        {
            context.skipUntil(begin, end, firstStatementSet);
        }
    }
    auto statement = parseStatement(begin, end, context);
    if (!statement)
    {
        return {};
    }
    return HeadWhileStatement(start, begin, std::move(expression), std::make_unique<Statement>(std::move(*statement)));
}

std::optional<cld::Syntax::FootWhileStatement>
    cld::Parser::parseFootWhileStatement(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    std::optional<Lexer::CTokenIterator> doPos;
    if (!expect(Lexer::TokenType::DoKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, firstStatementSet);
    }
    else
    {
        doPos = start;
    }
    auto statement =
        parseStatement(begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::WhileKeyword)));
    if (doPos)
    {
        if (!expect(Lexer::TokenType::WhileKeyword, begin, end, context,
                    [&] { return Notes::TO_MATCH_N_HERE.args(**doPos, context.getSourceInterface(), **doPos); }))
        {
            context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::WhileKeyword, begin, end, context))
        {
            context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
        }
    }
    std::optional<Lexer::CTokenIterator> openPpos;
    if (!expect(Lexer::TokenType::OpenParentheses, begin, end, context))
    {
        context.skipUntil(begin, end, firstExpressionSet);
    }
    else
    {
        openPpos = begin - 1;
    }
    auto expression = parseExpression(
        begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
    if (openPpos)
    {
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                    [&] { return Notes::TO_MATCH_N_HERE.args(**openPpos, context.getSourceInterface(), **openPpos); }))
        {
            context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::SemiColon));
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context))
        {
            context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::SemiColon));
        }
    }
    if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
    {
        context.skipUntil(begin, end);
    }

    if (!statement)
    {
        return {};
    }
    return FootWhileStatement(start, begin, std::make_unique<Statement>(std::move(*statement)), std::move(expression));
}

cld::Syntax::ReturnStatement cld::Parser::parseReturnStatement(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end,
                                                               Context& context)
{
    const auto* start = begin;
    if (!expect(Lexer::TokenType::ReturnKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, firstExpressionSet | Context::fromTokenTypes(Lexer::TokenType::SemiColon));
    }
    if (begin < end && begin->getTokenType() == Lexer::TokenType::SemiColon)
    {
        expect(Lexer::TokenType::SemiColon, begin, end, context);
        return ReturnStatement(start, begin, nullptr);
    }
    else if (begin == end || !firstIsInExpression(*begin, context))
    {
        expect(Lexer::TokenType::SemiColon, begin, end, context);
        context.skipUntil(begin, end);
        return ReturnStatement(start, begin, nullptr);
    }
    auto expression =
        parseExpression(begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::SemiColon)));
    if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
    {
        context.skipUntil(begin, end);
    }
    return ReturnStatement(start, begin, std::make_unique<Expression>(std::move(expression)));
}

std::optional<cld::Syntax::IfStatement> cld::Parser::parseIfStatement(Lexer::CTokenIterator& begin,
                                                                      Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (!expect(Lexer::TokenType::IfKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
    }
    std::optional<Lexer::CTokenIterator> openPpos;
    if (!expect(Lexer::TokenType::OpenParentheses, begin, end, context))
    {
        context.skipUntil(begin, end, firstExpressionSet);
    }
    else
    {
        openPpos = begin - 1;
    }
    auto expression = parseExpression(
        begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
    if (openPpos)
    {
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                    [&] { return Notes::TO_MATCH_N_HERE.args(**openPpos, context.getSourceInterface(), **openPpos); }))
        {
            context.skipUntil(begin, end, firstStatementSet);
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context))
        {
            context.skipUntil(begin, end, firstStatementSet);
        }
    }
    auto statement =
        parseStatement(begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::ElseKeyword)));
    if (!statement && (begin == end || begin->getTokenType() != Lexer::TokenType::ElseKeyword))
    {
        return {};
    }

    if (begin < end && begin->getTokenType() == Lexer::TokenType::ElseKeyword)
    {
        begin++;
        auto elseStatement = parseStatement(begin, end, context);
        if (!statement || !elseStatement)
        {
            return {};
        }
        return IfStatement(start, begin, std::move(expression), std::make_unique<Statement>(std::move(*statement)),
                           std::make_unique<Statement>(std::move(*elseStatement)));
    }
    else
    {
        return IfStatement(start, begin, std::move(expression), std::make_unique<Statement>(std::move(*statement)));
    }
}

std::optional<cld::Syntax::SwitchStatement>
    cld::Parser::parseSwitchStatement(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (!expect(Lexer::TokenType::SwitchKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
    }
    std::optional<Lexer::CTokenIterator> openPpos;
    if (!expect(Lexer::TokenType::OpenParentheses, begin, end, context))
    {
        context.skipUntil(begin, end, firstExpressionSet);
    }
    else
    {
        openPpos = begin - 1;
    }
    auto expression = parseExpression(
        begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
    if (openPpos)
    {
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                    [&] { return Notes::TO_MATCH_N_HERE.args(**openPpos, context.getSourceInterface(), **openPpos); }))
        {
            context.skipUntil(begin, end, firstStatementSet);
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context))
        {
            context.skipUntil(begin, end, firstStatementSet);
        }
    }
    auto statement = parseStatement(begin, end, context);
    if (!statement)
    {
        return {};
    }
    return SwitchStatement(start, begin, std::move(expression), std::make_unique<Statement>(std::move(*statement)));
}

std::optional<cld::Syntax::ForStatement> cld::Parser::parseForStatement(Lexer::CTokenIterator& begin,
                                                                        Lexer::CTokenIterator end, Context& context)
{
    const auto* start = begin;
    if (!expect(Lexer::TokenType::ForKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
    }
    const auto* openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParentheses, begin, end, context))
    {
        context.skipUntil(begin, end,
                          firstExpressionSet | firstDeclarationSet
                              | Context::fromTokenTypes(Lexer::TokenType::SemiColon));
    }
    if (begin == end)
    {
        context.log(Errors::Parser::EXPECTED_EXPRESSION_OR_DECLARATION.args(
            diag::after(*(begin - 1)), context.getSourceInterface(), *(begin - 1)));
        return {};
    }

    std::variant<Declaration, std::unique_ptr<Expression>> initial{nullptr};
    if (firstIsInDeclaration(*begin, context))
    {
        auto decl = parseDeclaration(
            begin, end,
            context.withRecoveryTokens(firstExpressionSet | Context::fromTokenTypes(Lexer::TokenType::SemiColon)));
        if (decl)
        {
            initial = std::move(*decl);
        }
    }
    else if (begin->getTokenType() != Lexer::TokenType::SemiColon)
    {
        auto exp = parseExpression(
            begin, end,
            context.withRecoveryTokens(firstExpressionSet | Context::fromTokenTypes(Lexer::TokenType::SemiColon)));
        initial = std::make_unique<Expression>(std::move(exp));
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            context.skipUntil(begin, end, firstExpressionSet | Context::fromTokenTypes(Lexer::TokenType::SemiColon));
        }
    }
    else
    {
        begin++;
    }

    std::unique_ptr<Expression> controlling;
    if (begin == end)
    {
        context.log(Errors::Parser::EXPECTED_EXPRESSION.args(diag::after(*(begin - 1)), context.getSourceInterface(),
                                                             *(begin - 1)));
        return {};
    }
    else if (begin->getTokenType() != Lexer::TokenType::SemiColon)
    {
        auto exp = parseExpression(
            begin, end,
            context.withRecoveryTokens(firstExpressionSet | Context::fromTokenTypes(Lexer::TokenType::SemiColon)));
        controlling = std::make_unique<Expression>(std::move(exp));
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            context.skipUntil(begin, end,
                              firstExpressionSet | Context::fromTokenTypes(Lexer::TokenType::CloseParentheses));
        }
    }
    else
    {
        begin++;
    }

    std::unique_ptr<Expression> post;
    if (begin == end)
    {
        context.log(Errors::Parser::EXPECTED_EXPRESSION.args(diag::after(*(begin - 1)), context.getSourceInterface(),
                                                             *(begin - 1)));
        return {};
    }
    else if (begin->getTokenType() != Lexer::TokenType::CloseParentheses)
    {
        auto exp = parseExpression(begin, end, context);
        post = std::make_unique<Expression>(std::move(exp));
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                    [&] { return Notes::TO_MATCH_N_HERE.args(*openPpos, context.getSourceInterface(), *openPpos); }))
        {
            context.skipUntil(begin, end, firstStatementSet);
        }
    }
    else
    {
        begin++;
    }

    auto stat = parseStatement(begin, end, context);
    if (!stat)
    {
        return {};
    }
    return ForStatement(start, begin, std::make_unique<Statement>(std::move(*stat)), std::move(initial),
                        std::move(controlling), std::move(post));
}
