#include "Parser.hpp"

#include <CompilerCore/C/Semantics.hpp>

#include <algorithm>
#include <unordered_set>

#include "ParserUtil.hpp"
#include "SemanticUtil.hpp"

using namespace cld::Syntax;

namespace
{
std::vector<DeclarationSpecifier> parseDeclarationSpecifierList(cld::Lexer::TokenIterator& begin,
                                                                cld::Lexer::TokenIterator end,
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
    cld::Parser::parseSpecifierQualifierList(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
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

TranslationUnit cld::Parser::parseTranslationUnit(Lexer::TokenIterator& begin, Lexer::TokenIterator end,
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
    cld::Parser::parseExternalDeclaration(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    auto start = begin;

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
    else if (begin->getTokenType() == Lexer::TokenType::OpenBrace || firstIsInDeclaration(*begin, context))
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
        if (declarator)
        {
            auto [parameters, parameterDepth] =
                Semantics::findRecursivelyWithDepth<Syntax::DirectDeclaratorParentheseParameters>(
                    declarator->getDirectDeclarator(), [](auto&& value) -> const Syntax::DirectDeclarator* {
                        using T = std::decay_t<decltype(value)>;
                        if constexpr (std::is_same_v<T, Syntax::DirectDeclaratorParenthese>)
                        {
                            return &value.getDeclarator().getDirectDeclarator();
                        }
                        else if constexpr (!std::is_same_v<T, Syntax::DirectDeclaratorIdentifier>)
                        {
                            return &value.getDirectDeclarator();
                        }
                        else
                        {
                            return nullptr;
                        }
                    });

            auto [identifierList, identiferDepth] =
                Semantics::findRecursivelyWithDepth<Syntax::DirectDeclaratorParentheseIdentifiers>(
                    declarator->getDirectDeclarator(), [](auto&& value) -> const Syntax::DirectDeclarator* {
                        using T = std::decay_t<decltype(value)>;
                        if constexpr (std::is_same_v<T, Syntax::DirectDeclaratorParenthese>)
                        {
                            return &value.getDeclarator().getDirectDeclarator();
                        }
                        else if constexpr (!std::is_same_v<T, Syntax::DirectDeclaratorIdentifier>)
                        {
                            return &value.getDirectDeclarator();
                        }
                        else
                        {
                            return nullptr;
                        }
                    });

            if (parameters && (!identifierList || parameterDepth > identiferDepth))
            {
                auto& parameterDeclarations =
                    parameters->getParameterTypeList().getParameterList().getParameterDeclarations();
                std::unordered_set<std::string> addedByParameters;
                for (auto& [specifiers, paramDeclarator] : parameterDeclarations)
                {
                    if (parameterDeclarations.size() == 1 && specifiers.size() == 1
                        && std::holds_alternative<TypeSpecifier>(specifiers[0]))
                    {
                        auto* primitive = std::get_if<TypeSpecifier::PrimitiveTypeSpecifier>(
                            &cld::get<TypeSpecifier>(specifiers[0]).getVariant());
                        if (primitive && *primitive == TypeSpecifier::PrimitiveTypeSpecifier::Void)
                        {
                            break;
                        }
                    }

                    //
                    // Any parameters that overshadow typedefs need to also affect later parameters
                    //
                    for (auto& iter : specifiers)
                    {
                        auto* typeSpecifier = std::get_if<TypeSpecifier>(&iter);
                        if (!typeSpecifier)
                        {
                            continue;
                        }
                        auto* identifier = std::get_if<std::string>(&typeSpecifier->getVariant());
                        if (!identifier)
                        {
                            continue;
                        }
                        if (addedByParameters.count(*identifier))
                        {
                            auto* loc = context.getLocationOf(*identifier);
                            std::vector<Message> messages;
                            messages.push_back(Message::error(ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                                                  "typename", '\'' + *identifier + '\''),
                                                              typeSpecifier->begin(), typeSpecifier->end(),
                                                              {PointAt(typeSpecifier->begin(), typeSpecifier->end())}));
                            if (loc)
                            {
                                messages.push_back(Message::note(
                                    Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args('\'' + *identifier + '\''),
                                    loc->begin, loc->end, {PointAt(loc->identifier, loc->identifier + 1)}));
                            }
                            context.log(std::move(messages));
                        }
                    }

                    if (auto* abstractDecl = std::get_if<std::unique_ptr<AbstractDeclarator>>(&paramDeclarator))
                    {
                        if (specifiers.empty())
                        {
                            continue;
                        }
                        std::vector<Message> notes;
                        if (specifiers.size() == 1 && std::holds_alternative<TypeSpecifier>(specifiers[0])
                            && std::holds_alternative<std::string>(cld::get<TypeSpecifier>(specifiers[0]).getVariant()))
                        {
                            auto& typeSpecifier = cld::get<TypeSpecifier>(specifiers[0]);
                            const auto& name = cld::get<std::string>(typeSpecifier.getVariant());
                            auto* loc = context.getLocationOf(name);
                            if (loc)
                            {
                                notes.push_back(Message::note(Notes::IDENTIFIER_IS_TYPEDEF.args('\'' + name + '\''),
                                                              loc->begin, loc->end,
                                                              {Underline(loc->identifier, loc->identifier + 1)}));
                            }
                        }
                        notes.insert(
                            notes.begin(),
                            Message::error(
                                ErrorMessages::Parser::MISSING_PARAMETER_NAME, start, begin,
                                {Underline(nodeFromNodeDerivedVariant(specifiers.back()).begin(),
                                           *abstractDecl ? (*abstractDecl)->end() :
                                                           nodeFromNodeDerivedVariant(specifiers.back()).end())}));
                        context.log(std::move(notes));
                        continue;
                    }
                    auto& decl = cld::get<std::unique_ptr<Declarator>>(paramDeclarator);
                    auto result = Semantics::declaratorToName(*decl);
                    context.addToScope(result, {start, begin, Semantics::declaratorToLoc(*decl)});
                    addedByParameters.insert(result);
                }
            }
            else if (identifierList)
            {
                for (auto& [name, loc] : identifierList->getIdentifiers())
                {
                    context.addToScope(name, {start, begin, loc});
                }
            }
        }
        auto compoundStatement = parseCompoundStatement(begin, end, context, false);
        context.popScope();

        if (!declarator || !compoundStatement)
        {
            return {};
        }
        context.addToScope(Semantics::declaratorToName(*declarator),
                           {start, compoundStatement->begin(), Semantics::declaratorToLoc(*declarator)});
        return FunctionDefinition(start, begin, std::move(declarationSpecifiers), std::move(*declarator),
                                  std::move(declarations), std::move(*compoundStatement));
    }
    else
    {
        std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>> initDeclarators;
        if (!isTypedef && declarator)
        {
            context.addToScope(Semantics::declaratorToName(*declarator),
                               {start, begin, Semantics::declaratorToLoc(*declarator)});
        }
        if (begin->getTokenType() == Lexer::TokenType::Assignment)
        {
            begin++;
            auto initializer = parseInitializer(begin, end,
                                                context.withRecoveryTokens(Context::fromTokenTypes(
                                                    Lexer::TokenType::Comma, Lexer::TokenType::SemiColon)));
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
            declarator = parseDeclarator(
                begin, end,
                context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma, Lexer::TokenType::SemiColon,
                                                                   Lexer::TokenType::Assignment)));
            if (!isTypedef && declarator)
            {
                context.addToScope(Semantics::declaratorToName(*declarator),
                                   {start, begin, Semantics::declaratorToLoc(*declarator)});
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
                                                        Lexer::TokenType::Comma, Lexer::TokenType::SemiColon)));
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
                context.addTypedef(Semantics::declaratorToName(*initDeclarator),
                                   {start, begin, Semantics::declaratorToLoc(*initDeclarator)});
            }
        }

        return Declaration(start, begin, std::move(declarationSpecifiers), std::move(initDeclarators));
    }
}

std::optional<cld::Syntax::Declaration> cld::Parser::parseDeclaration(Lexer::TokenIterator& begin,
                                                                      Lexer::TokenIterator end, Context& context)
{
    auto start = begin;

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
        && begin->getTokenType() == Lexer::TokenType::Identifier
        && context.isTypedef(cld::get<std::string>(begin->getValue())))
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
            context.addToScope(Semantics::declaratorToName(*declarator),
                               {start, begin, Semantics::declaratorToLoc(*declarator)});
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
        auto* loc = context.getLocationOf(Semantics::declaratorToName(*initDeclarators[0].first));
        std::vector<Message> notes;
        if (loc)
        {
            notes.push_back(Message::note(Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args(
                                              '\'' + Semantics::declaratorToName(*initDeclarators[0].first) + '\''),
                                          loc->begin, loc->end, {Underline(loc->identifier, loc->identifier + 1)}));
        }
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context, std::move(notes)))
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

    if (isTypedef)
    {
        for (auto& [declator, init] : initDeclarators)
        {
            context.addTypedef(Semantics::declaratorToName(*declator),
                               {start, begin, Semantics::declaratorToLoc(*declator)});
        }
    }

    return Declaration(start, begin, std::move(declarationSpecifiers), std::move(initDeclarators));
}

std::optional<cld::Syntax::DeclarationSpecifier>
    cld::Parser::parseDeclarationSpecifier(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
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
                auto name = cld::get<std::string>(begin->getValue());
                if (context.isTypedefInScope(name))
                {
                    return Syntax::DeclarationSpecifier{TypeSpecifier(start, ++begin, name)};
                }
                break;
            }
            default: break;
        }
    }
    if (begin < end)
    {
        context.log(
            {Message::error(ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args(
                                "storage specifier or typename", '\'' + to_string(begin->getRepresentation()) + '\''),
                            begin, {PointAt(begin, begin + 1)})});
    }
    else
    {
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("storage specifier or typename"), begin,
                                    {PointAt(begin - 1, begin)})});
    }
    context.skipUntil(begin, end);
    return {};
}

std::optional<cld::Syntax::StructOrUnionSpecifier>
    cld::Parser::parseStructOrUnionSpecifier(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
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
        context.log(
            {Message::error(ErrorMessages::Parser::EXPECTED_N.args(Format::List(", ", " or ", "struct", "union")),
                            start, begin + 1, {PointAt(begin, begin + 1)})});
        context.skipUntil(begin, end);
        return {};
    }

    if (begin == end)
    {
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args(
                                        Format::List(", ", " or ", "identifier", "'{'"), isUnion ? "union" : "struct"),
                                    start, end, {PointAt(end - 1, end)})});
        return {};
    }

    auto name = begin->getTokenType() == Lexer::TokenType::Identifier ? cld::get<std::string>(begin->getValue()) : "";
    if (!name.empty())
    {
        begin++;
    }

    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        if (name.empty())
        {
            expect(Lexer::TokenType::Identifier, begin, end, context);
            context.skipUntil(begin, end);
            return {};
        }
        return StructOrUnionSpecifier(start, begin, isUnion, name, {});
    }

    auto openBrace = begin;
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
        context.log({Message::error(
            ErrorMessages::Parser::N_REQUIRES_AT_LEAST_ONE_N.args(isUnion ? "union" : "struct", "field"), start, begin,
            {Underline(openBrace, begin)})});
        return {};
    }
    return StructOrUnionSpecifier(start, begin, isUnion, name, std::move(structDeclarations));
}

std::optional<cld::Syntax::SpecifierQualifier>
    cld::Parser::parseSpecifierQualifier(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
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
                auto name = cld::get<std::string>(begin->getValue());
                if (context.isTypedefInScope(name))
                {
                    return Syntax::SpecifierQualifier{TypeSpecifier(start, ++begin, name)};
                }
                else if (context.isTypedef(name))
                {
                    auto* loc = context.getLocationOf(cld::get<std::string>(begin->getValue()));
                    context.log(
                        {Message::error(ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                            "typename", '\'' + to_string(begin->getRepresentation()) + '\''),
                                        start, {PointAt(begin, begin + 1)}),
                         Message::note(Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args(
                                           '\'' + to_string(begin->getRepresentation()) + '\''),
                                       loc->begin, loc->end, {Underline(loc->identifier, loc->identifier + 1)})});
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
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args(
                                        "typename", '\'' + to_string(begin->getRepresentation()) + '\''),
                                    begin, {PointAt(begin, begin + 1)})});
    }
    else
    {
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("typename"), begin - 1,
                                    {PointAt(begin - 1, begin)})});
    }
    context.skipUntil(begin, end);
    return {};
}

std::optional<cld::Syntax::Declarator> cld::Parser::parseDeclarator(Lexer::TokenIterator& begin,
                                                                    Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
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
    cld::Parser::parseDirectDeclarator(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    std::unique_ptr<DirectDeclarator> directDeclarator;

    auto start = begin;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
    {
        auto currToken = begin;
        begin++;
        directDeclarator = std::make_unique<DirectDeclarator>(
            DirectDeclaratorIdentifier(start, begin, cld::get<std::string>(currToken->getValue()), currToken));
    }
    else if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenParentheses)
    {
        context.parenthesesEntered(begin);
        auto openPpos = begin;
        begin++;
        auto declarator = parseDeclarator(
            begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
        if (declarator)
        {
            directDeclarator = std::make_unique<DirectDeclarator>(
                DirectDeclaratorParenthese(start, begin, std::make_unique<Declarator>(std::move(*declarator))));
        }
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), openPpos, {PointAt(openPpos, openPpos + 1)})}))
        {
            context.skipUntil(
                begin, end,
                Context::fromTokenTypes(Lexer::TokenType::OpenParentheses, Lexer::TokenType::OpenSquareBracket));
        }
        context.parenthesesLeft();
    }
    else
    {
        if (begin == end)
        {
            context.log({Message::error(
                ErrorMessages::Parser::EXPECTED_N.args(cld::Format::List(", ", " or ", "'('", "identifier")), begin,
                {InsertAfter(begin - 1)})});
        }
        else
        {
            context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                            cld::Format::List(", ", " or ", "'('", "identifier"),
                                            '\'' + to_string(begin->getRepresentation()) + '\''),
                                        begin, {PointAt(begin, begin + 1)})});
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
                context.parenthesesEntered(begin);
                auto openPpos = begin;
                begin++;
                if (begin < end && firstIsInParameterTypeList(*begin, context))
                {
                    auto parameterTypeList = parseParameterTypeList(
                        begin, end,
                        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
                    if (directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorParentheseParameters(
                            start, begin, std::move(*directDeclarator), std::move(parameterTypeList)));
                    }
                }
                else if (begin < end)
                {
                    std::vector<std::pair<std::string, Lexer::TokenIterator>> identifiers;
                    if (begin->getTokenType() == Lexer::TokenType::Identifier)
                    {
                        identifiers.emplace_back(cld::get<std::string>(begin->getValue()), begin);
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
                            std::string name;
                            if (!expect(Lexer::TokenType::Identifier, begin, end, context, name))
                            {
                                context.skipUntil(begin, end,
                                                  Context::fromTokenTypes(Lexer::TokenType::Comma,
                                                                          Lexer::TokenType::CloseParentheses));
                            }
                            else
                            {
                                if (context.isTypedef(name))
                                {
                                    std::vector<Message> notes = {Message::error(
                                        ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args("identifier", "typename"),
                                        start, begin, {Underline(begin - 1, begin)})};
                                    if (auto* loc = context.getLocationOf(name))
                                    {
                                        notes.push_back(Message::note(
                                            Notes::IDENTIFIER_IS_TYPEDEF.args('\'' + name + '\''), loc->begin, loc->end,
                                            {Underline(loc->identifier, loc->identifier + 1)}));
                                    }
                                    context.log(std::move(notes));
                                }
                                else
                                {
                                    identifiers.emplace_back(name, begin);
                                }
                            }
                        }
                    }
                    if (directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorParentheseIdentifiers(
                            start, begin, std::move(*directDeclarator), std::move(identifiers)));
                    }
                }
                if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                            {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), openPpos,
                                           {PointAt(openPpos, openPpos + 1)})}))
                {
                    context.skipUntil(begin, end,
                                      Context::fromTokenTypes(Lexer::TokenType::OpenParentheses,
                                                              Lexer::TokenType::OpenSquareBracket));
                }
                context.parenthesesLeft();
                break;
            }
            case Lexer::TokenType::OpenSquareBracket:
            {
                context.squareBracketEntered(begin);
                auto openPpos = begin;
                begin++;
                if (begin == end)
                {
                    expect(Lexer::TokenType::CloseSquareBracket, begin, end, context,
                           {Message::note(Notes::TO_MATCH_N_HERE.args("'['"), openPpos,
                                          {PointAt(openPpos, openPpos + 1)})});
                    context.squareBracketLeft();
                    return {};
                }

                if (begin->getTokenType() == Lexer::TokenType::StaticKeyword)
                {
                    begin++;
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
                    if (assignmentExpression && directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(
                            DirectDeclaratorStatic(start, begin, std::move(directDeclarator), std::move(typeQualifiers),
                                                   std::move(*assignmentExpression)));
                    }
                }
                else
                {
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
                    if (begin < end)
                    {
                        if (begin->getTokenType() == Lexer::TokenType::StaticKeyword)
                        {
                            begin++;
                            auto assignmentExpression = cld::Parser::parseAssignmentExpression(
                                begin, end,
                                context.withRecoveryTokens(
                                    Context::fromTokenTypes(Lexer::TokenType::CloseSquareBracket)));
                            if (assignmentExpression && directDeclarator)
                            {
                                directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorStatic(
                                    start, begin, std::move(directDeclarator), std::move(typeQualifiers),
                                    std::move(*assignmentExpression)));
                            }
                        }
                        else if (begin->getTokenType() == Lexer::TokenType::Asterisk)
                        {
                            begin++;
                            if (directDeclarator)
                            {
                                directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorAsterisk(
                                    start, begin, std::move(*directDeclarator), std::move(typeQualifiers)));
                            }
                        }
                        else if (firstIsInAssignmentExpression(*begin, context))
                        {
                            auto assignment = cld::Parser::parseAssignmentExpression(
                                begin, end,
                                context.withRecoveryTokens(
                                    Context::fromTokenTypes(Lexer::TokenType::CloseSquareBracket)));
                            if (assignment && directDeclarator)
                            {
                                directDeclarator =
                                    std::make_unique<DirectDeclarator>(DirectDeclaratorNoStaticOrAsterisk(
                                        start, begin, std::move(directDeclarator), std::move(typeQualifiers),
                                        std::make_unique<AssignmentExpression>(std::move(*assignment))));
                            }
                        }
                        else if (directDeclarator)
                        {
                            directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorNoStaticOrAsterisk(
                                start, begin, std::move(directDeclarator), std::move(typeQualifiers), nullptr));
                        }
                    }
                }

                if (!expect(Lexer::TokenType::CloseSquareBracket, begin, end, context,
                            {Message::note(Notes::TO_MATCH_N_HERE.args("'['"), openPpos,
                                           {PointAt(openPpos, openPpos + 1)})}))
                {
                    context.skipUntil(begin, end,
                                      Context::fromTokenTypes(Lexer::TokenType::OpenParentheses,
                                                              Lexer::TokenType::OpenSquareBracket));
                    if (begin == end
                        || (begin->getTokenType() != Lexer::TokenType::OpenParentheses
                            && begin->getTokenType() != Lexer::TokenType::OpenSquareBracket))
                    {
                        context.squareBracketLeft();
                        return {};
                    }
                }
                context.squareBracketLeft();
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

cld::Syntax::ParameterTypeList cld::Parser::parseParameterTypeList(Lexer::TokenIterator& begin,
                                                                   Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
    auto parameterList =
        parseParameterList(begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
    bool hasEllipse = false;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
    {
        begin++;
        if (begin == end || begin->getTokenType() != Lexer::TokenType::Ellipse)
        {
            context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("parameter", "','"), start,
                                        {PointAt(begin - 1, begin)})});
        }
        else
        {
            begin++;
            hasEllipse = true;
        }
    }
    return ParameterTypeList(start, begin, std::move(parameterList), hasEllipse);
}

cld::Syntax::ParameterList cld::Parser::parseParameterList(Lexer::TokenIterator& begin, Lexer::TokenIterator end,
                                                           Context& context)
{
    auto start = begin;
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
        auto declarationSpecifiers = parseDeclarationSpecifierList(
            begin, end,
            context.withRecoveryTokens(firstAbstractDeclaratorSet | firstDeclaratorSet
                                       | Context::fromTokenTypes(Lexer::TokenType::Comma)));

        // Skip past everything that is part of the pointer declaration inside of the (possibly abstract) declarator
        auto result = std::find_if(begin, end, [](const Lexer::Token& token) {
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
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers), std::unique_ptr<AbstractDeclarator>());
            continue;
        }

        if (result->getTokenType() == Lexer::TokenType::OpenSquareBracket)
        {
            auto abstractDeclarator = parseAbstractDeclarator(
                begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                               std::make_unique<AbstractDeclarator>(std::move(abstractDeclarator)));
        }
        else if (result->getTokenType() == Lexer::TokenType::Identifier)
        {
            auto declarator = parseDeclarator(
                begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
            if (declarator)
            {
                parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                                   std::make_unique<Declarator>(std::move(*declarator)));
            }
        }
        else if (result->getTokenType() == Lexer::TokenType::OpenParentheses)
        {
            while (result < end && result->getTokenType() == Lexer::TokenType::OpenParentheses)
            {
                // Ambigious
                result++;
                result = std::find_if(result, end, [](const Lexer::Token& token) {
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
                        parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                                           std::make_unique<Declarator>(std::move(*declarator)));
                    }
                    break;
                }
                else if (result < end && result->getTokenType() != Lexer::TokenType::OpenParentheses)
                {
                    auto abstractDeclarator = parseAbstractDeclarator(
                        begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Comma)));
                    parameterDeclarations.emplace_back(
                        std::move(declarationSpecifiers),
                        std::make_unique<AbstractDeclarator>(std::move(abstractDeclarator)));
                    break;
                }
            }
        }
        else
        {
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers), std::unique_ptr<AbstractDeclarator>());
        }
    }
    if (first)
    {
        context.log(
            {Message::error(ErrorMessages::Parser::N_REQUIRES_AT_LEAST_ONE_N.args("parameter list", "parameter"), begin,
                            {PointAt(begin, end)})});
    }
    return ParameterList(start, begin, std::move(parameterDeclarations));
}

cld::Syntax::Pointer cld::Parser::parsePointer(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
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

cld::Syntax::AbstractDeclarator cld::Parser::parseAbstractDeclarator(Lexer::TokenIterator& begin,
                                                                     Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
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
    cld::Parser::parseDirectAbstractDeclarator(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    std::unique_ptr<DirectAbstractDeclarator> directAbstractDeclarator;
    bool first = true;
    auto start = begin;
    while (begin < end
           && (begin->getTokenType() == Lexer::TokenType::OpenParentheses
               || begin->getTokenType() == Lexer::TokenType::OpenSquareBracket))
    {
        switch (begin->getTokenType())
        {
            case Lexer::TokenType::OpenParentheses:
            {
                context.parenthesesEntered(begin);
                auto openPpos = begin;
                begin++;
                if (begin < end && firstIsInDeclarationSpecifier(*begin, context))
                {
                    auto parameterTypeList = parseParameterTypeList(
                        begin, end,
                        context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
                    directAbstractDeclarator =
                        std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParameterTypeList(
                            start, begin, std::move(directAbstractDeclarator),
                            std::make_unique<ParameterTypeList>(std::move(parameterTypeList))));
                }
                else if (begin < end && first && firstIsInAbstractDeclarator(*begin, context))
                {
                    auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
                    directAbstractDeclarator =
                        std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParenthese(
                            start, begin, std::make_unique<AbstractDeclarator>(std::move(abstractDeclarator))));
                }
                else
                {
                    directAbstractDeclarator =
                        std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParameterTypeList(
                            start, begin, std::move(directAbstractDeclarator), nullptr));
                }
                if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                            {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), openPpos,
                                           {PointAt(openPpos, openPpos + 1)})}))
                {
                    context.skipUntil(begin, end,
                                      Context::fromTokenTypes(Lexer::TokenType::OpenParentheses,
                                                              Lexer::TokenType::OpenSquareBracket));
                }
                context.parenthesesLeft();
                break;
            }
            case Lexer::TokenType::OpenSquareBracket:
            {
                context.squareBracketEntered(begin);
                auto openPpos = begin;
                begin++;
                if (begin < end && begin->getTokenType() == Lexer::TokenType::Asterisk)
                {
                    begin++;
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                        DirectAbstractDeclaratorAsterisk(start, begin, std::move(directAbstractDeclarator)));
                }
                else
                {
                    if (begin < end && firstIsInAssignmentExpression(*begin, context))
                    {
                        auto assignment = parseAssignmentExpression(
                            begin, end,
                            context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseParentheses)));
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
                        directAbstractDeclarator =
                            std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorAssignmentExpression(
                                start, begin, std::move(directAbstractDeclarator), nullptr));
                    }
                }

                if (!expect(Lexer::TokenType::CloseSquareBracket, begin, end, context,
                            {Message::note(Notes::TO_MATCH_N_HERE.args("'['"), openPpos,
                                           {PointAt(openPpos, openPpos + 1)})}))
                {
                    context.skipUntil(begin, end,
                                      Context::fromTokenTypes(Lexer::TokenType::OpenParentheses,
                                                              Lexer::TokenType::OpenSquareBracket));
                }
                context.squareBracketLeft();
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
            context.log(
                {Message::error(ErrorMessages::Parser::EXPECTED_N.args(cld::Format::List(", ", " or ", "'('", "'['")),
                                begin, {InsertAfter(begin - 1)})});
        }
        else
        {
            context.log({Message::error(
                ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                    cld::Format::List(", ", " or ", "'('", "'['"), '\'' + to_string(begin->getRepresentation()) + '\''),
                start, {InsertAfter(begin)})});
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

std::optional<cld::Syntax::EnumSpecifier> cld::Parser::parseEnumSpecifier(Lexer::TokenIterator& begin,
                                                                          Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::EnumKeyword, begin, end, context))
    {
        context.skipUntil(begin, end,
                          Context::fromTokenTypes(Lexer::TokenType::OpenBrace, Lexer::TokenType::Identifier));
    }
    std::string name;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
    {
        name = cld::get<std::string>(begin->getValue());
        begin++;
    }
    else if (begin == end)
    {
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("identifier", "enum"), start, begin,
                                    {InsertAfter(begin - 1)})});
        context.skipUntil(begin, end);
        return {};
    }

    auto openPpos = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        if (name.empty())
        {
            expect(Lexer::TokenType::Identifier, begin, end, context);
            context.skipUntil(begin, end);
            return {};
        }
        return EnumSpecifier(start, begin, std::move(name));
    }
    else
    {
        begin++;
    }

    bool inLoop = false;
    std::vector<std::pair<std::string, std::optional<ConstantExpression>>> values;
    while (
        begin < end
        && (begin->getTokenType() == Lexer::TokenType::Identifier || begin->getTokenType() == Lexer::TokenType::Comma))
    {
        inLoop = true;
        auto thisValueStart = begin;
        std::string valueName;
        if (!expect(Lexer::TokenType::Identifier, begin, end, context, valueName))
        {
            context.skipUntil(begin, end,
                              Context::fromTokenTypes(Lexer::TokenType::Assignment, Lexer::TokenType::Comma,
                                                      Lexer::TokenType::CloseBrace));
        }
        else
        {
            values.emplace_back(valueName, std::optional<ConstantExpression>{});
        }

        if (begin < end && begin->getTokenType() == Lexer::TokenType::Assignment)
        {
            begin++;
            auto constant = parseConditionalExpression(begin, end,
                                                       context.withRecoveryTokens(Context::fromTokenTypes(
                                                           Lexer::TokenType::CloseBrace, Lexer::TokenType::Comma)));
            if (!valueName.empty())
            {
                values.back().second = std::move(constant);
            }
        }

        if (!valueName.empty())
        {
            context.addToScope(valueName, {thisValueStart, begin, thisValueStart});
        }

        if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        else if (begin == end || begin->getTokenType() != Lexer::TokenType::CloseBrace)
        {
            if (begin == end)
            {
                context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("'}'"), start, begin,
                                            {InsertAfter(begin - 1)})});
                return {};
            }
            else
            {
                context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                                "','", '\'' + to_string(begin->getRepresentation()) + '\''),
                                            start, begin, {PointAt(begin, begin + 1)})});
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
        context.log({Message::error(ErrorMessages::Parser::N_REQUIRES_AT_LEAST_ONE_N.args("enum", "value"), start,
                                    begin, {Underline(openPpos, begin)})});
    }
    return EnumSpecifier(start, begin, EnumDeclaration(start, begin, std::move(name), std::move(values)));
}

std::optional<cld::Syntax::CompoundStatement> cld::Parser::parseCompoundStatement(Lexer::TokenIterator& begin,
                                                                                  Lexer::TokenIterator end,
                                                                                  cld::Parser::Context& context,
                                                                                  bool pushScope)
{
    auto start = begin;
    context.braceEntered(begin);
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
    auto additional =
        std::vector{Message::note(Notes::TO_MATCH_N_HERE.args("'{'"), start, begin,
                                  {PointAt(start == end ? start - 1 : start, start == end ? start : start + 1)})};
    if (!braceSeen)
    {
        additional.clear();
    }
    if (!expect(Lexer::TokenType::CloseBrace, begin, end, context, std::move(additional)))
    {
        context.skipUntil(begin, end);
    }
    context.braceLeft();
    return CompoundStatement(start, begin, std::move(items));
}

std::optional<cld::Syntax::CompoundItem> cld::Parser::parseCompoundItem(Lexer::TokenIterator& begin,
                                                                        Lexer::TokenIterator end, Context& context)
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
    else
    {
        auto statement = parseStatement(begin, end, context);
        if (!statement)
        {
            return {};
        }
        return CompoundItem(std::move(*statement));
    }
}

std::optional<cld::Syntax::Initializer> cld::Parser::parseInitializer(Lexer::TokenIterator& begin,
                                                                      Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        auto assignment = parseAssignmentExpression(begin, end, context);
        if (!assignment)
        {
            return {};
        }
        return Initializer(start, begin, std::move(*assignment));
    }
    else
    {
        context.braceEntered(begin);
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
        context.braceLeft();
        if (!initializerList)
        {
            return {};
        }
        return Initializer{start, begin, std::move(*initializerList)};
    }
}

std::optional<cld::Syntax::InitializerList>
    cld::Parser::parseInitializerList(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
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

        std::vector<std::variant<ConstantExpression, std::string>> designation;
        bool hasDesignation = false;
        while (begin < end
               && (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket
                   || begin->getTokenType() == Lexer::TokenType::Dot))
        {
            hasDesignation = true;
            if (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket)
            {
                context.squareBracketEntered(begin);
                auto openPpos = begin;
                begin++;
                auto constant = parseConditionalExpression(
                    begin, end,
                    context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::CloseSquareBracket)));
                designation.emplace_back(std::move(constant));
                if (!expect(Lexer::TokenType::CloseSquareBracket, begin, end, context,
                            {Message::note(Notes::TO_MATCH_N_HERE.args("'['"), openPpos,
                                           {PointAt(openPpos, openPpos + 1)})}))
                {
                    context.skipUntil(begin, end,
                                      Context::fromTokenTypes(Lexer::TokenType::Assignment,
                                                              Lexer::TokenType::OpenSquareBracket,
                                                              Lexer::TokenType::Dot));
                }
                context.squareBracketLeft();
            }
            else
            {
                begin++;
                std::string name;
                if (!expect(Lexer::TokenType::Identifier, begin, end, context, name))
                {
                    context.skipUntil(begin, end,
                                      Context::fromTokenTypes(Lexer::TokenType::Assignment,
                                                              Lexer::TokenType::OpenSquareBracket,
                                                              Lexer::TokenType::Dot));
                }
                designation.emplace_back(std::move(name));
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

std::optional<cld::Syntax::Statement> cld::Parser::parseStatement(Lexer::TokenIterator& begin, Lexer::TokenIterator end,
                                                                  Context& context)
{
    auto start = begin;
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
                begin++;
                if (!expect(Lexer::TokenType::Colon, begin, end, context))
                {
                    context.skipUntil(begin, end, firstStatementSet);
                }
                auto statement = parseStatement(begin, end, context);
                if (!statement)
                {
                    return {};
                }
                return Statement(DefaultStatement(start, begin, std::make_unique<Statement>(std::move(*statement))));
            }
            case Lexer::TokenType::CaseKeyword:
            {
                begin++;
                auto expression = parseConditionalExpression(
                    begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::Colon)));
                if (!expect(Lexer::TokenType::Colon, begin, end, context))
                {
                    context.skipUntil(begin, end, firstStatementSet);
                }
                auto statement = parseStatement(begin, end, context);
                if (!statement)
                {
                    return {};
                }
                return Statement(CaseStatement(start, begin, std::move(expression),
                                               std::make_unique<Statement>(std::move(*statement))));
            }
            case Lexer::TokenType::GotoKeyword:
            {
                begin++;
                std::string name;
                if (!expect(Lexer::TokenType::Identifier, begin, end, context, name))
                {
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
                return Statement(GotoStatement(start, begin, name));
            }
            case Lexer::TokenType::Identifier:
            {
                if (begin + 1 < end && (begin + 1)->getTokenType() == Lexer::TokenType::Colon)
                {
                    const auto& name = cld::get<std::string>(begin->getValue());
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
        std::vector<Message> notes;
        if (start + 1 == begin && start->getTokenType() == Lexer::TokenType::Identifier
            && context.isTypedef(cld::get<std::string>(start->getValue()))
            && begin->getTokenType() == Lexer::TokenType::Identifier)
        {
            auto* loc = context.getLocationOf(cld::get<std::string>(start->getValue()));
            if (loc)
            {
                notes.push_back(Message::note(Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args(
                                                  '\'' + to_string(start->getRepresentation()) + '\''),
                                              loc->begin, loc->end, {Underline(loc->identifier, loc->identifier + 1)}));
            }
        }
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context, std::move(notes)))
        {
            context.skipUntil(begin, end);
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
    cld::Parser::parseHeadWhileStatement(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::WhileKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
    }
    std::optional<Lexer::TokenIterator> openPpos;
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
    std::vector<Message> note;
    if (openPpos)
    {
        note = {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), *openPpos, {PointAt(*openPpos, *openPpos + 1)})};
    }
    if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context, std::move(note)))
    {
        context.skipUntil(begin, end, firstStatementSet);
    }
    auto statement = parseStatement(begin, end, context);
    if (!statement)
    {
        return {};
    }
    return HeadWhileStatement(start, begin, std::move(expression), std::make_unique<Statement>(std::move(*statement)));
}

std::optional<cld::Syntax::FootWhileStatement>
    cld::Parser::parseFootWhileStatement(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
    auto doPos = begin;
    if (!expect(Lexer::TokenType::DoKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, firstStatementSet);
    }
    auto statement =
        parseStatement(begin, end, context.withRecoveryTokens(Context::fromTokenTypes(Lexer::TokenType::WhileKeyword)));
    if (!expect(Lexer::TokenType::WhileKeyword, begin, end, context,
                {Message::note(Notes::TO_MATCH_N_HERE.args("'do'"), doPos, {PointAt(doPos, doPos + 1)})}))
    {
        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
    }
    std::optional<Lexer::TokenIterator> openPpos;
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
    std::vector<Message> notes;
    if (openPpos)
    {
        notes = {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), *openPpos, {PointAt(*openPpos, *openPpos + 1)})};
    }
    if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context, std::move(notes)))
    {
        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::SemiColon));
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

cld::Syntax::ReturnStatement cld::Parser::parseReturnStatement(Lexer::TokenIterator& begin, Lexer::TokenIterator end,
                                                               Context& context)
{
    auto start = begin;
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

std::optional<cld::Syntax::IfStatement> cld::Parser::parseIfStatement(Lexer::TokenIterator& begin,
                                                                      Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::IfKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
    }
    std::optional<Lexer::TokenIterator> openPpos;
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
    std::vector<Message> note;
    if (openPpos)
    {
        note = {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), *openPpos, {PointAt(*openPpos, *openPpos + 1)})};
    }
    if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context, std::move(note)))
    {
        context.skipUntil(begin, end, firstStatementSet);
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
    cld::Parser::parseSwitchStatement(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::SwitchKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
    }
    std::optional<Lexer::TokenIterator> openPpos;
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
    std::vector<Message> note;
    if (openPpos)
    {
        note = {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), *openPpos, {PointAt(*openPpos, *openPpos + 1)})};
    }
    if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context, std::move(note)))
    {
        context.skipUntil(begin, end, firstStatementSet);
    }
    auto statement = parseStatement(begin, end, context);
    if (!statement)
    {
        return {};
    }
    return SwitchStatement(start, begin, std::move(expression), std::make_unique<Statement>(std::move(*statement)));
}

std::optional<cld::Syntax::ForStatement> cld::Parser::parseForStatement(Lexer::TokenIterator& begin,
                                                                        Lexer::TokenIterator end, Context& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::ForKeyword, begin, end, context))
    {
        context.skipUntil(begin, end, Context::fromTokenTypes(Lexer::TokenType::OpenParentheses));
    }
    auto openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParentheses, begin, end, context))
    {
        context.skipUntil(begin, end,
                          firstExpressionSet | firstDeclarationSet
                              | Context::fromTokenTypes(Lexer::TokenType::SemiColon));
    }
    if (begin == end)
    {
        context.log({Message::error(
            ErrorMessages::Parser::EXPECTED_N.args(Format::List(", ", " or ", "expression", "declaration")), start,
            begin, {InsertAfter(begin - 1)})});
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
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("expression"), start, begin,
                                    {InsertAfter(begin - 1)})});
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
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("expression"), start, begin,
                                    {InsertAfter(begin - 1)})});
        return {};
    }
    else if (begin->getTokenType() != Lexer::TokenType::CloseParentheses)
    {
        auto exp = parseExpression(begin, end, context);
        post = std::make_unique<Expression>(std::move(exp));
        if (!expect(Lexer::TokenType::CloseParentheses, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), openPpos, {PointAt(openPpos, openPpos + 1)})}))
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
