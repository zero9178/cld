#include "Parser.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>
#include <unordered_set>

#include "ConstantEvaluator.hpp"
#include "ParserUtil.hpp"
#include "SemanticUtil.hpp"

using namespace OpenCL::Syntax;

namespace
{
    std::vector<DeclarationSpecifier> parseDeclarationSpecifierList(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                    OpenCL::Parser::Tokens::const_iterator end,
                                                                    OpenCL::Parser::Context& context,
                                                                    OpenCL::Parser::InRecoverySet recoverySet)
    {
        bool seenTypeSpecifier = false;
        std::vector<DeclarationSpecifier> declarationSpecifiers;
        do
        {
            auto result = parseDeclarationSpecifier(
                begin, end, context, [&context, seenTypeSpecifier, recoverySet](const OpenCL::Lexer::Token& token) {
                    return (OpenCL::Parser::firstIsInDeclarationSpecifier(token, context)
                            && (token.getTokenType() != OpenCL::Lexer::TokenType::Identifier || !seenTypeSpecifier))
                           || recoverySet(token);
                });
            if (result)
            {
                if (!seenTypeSpecifier && std::holds_alternative<TypeSpecifier>(*result))
                {
                    seenTypeSpecifier = true;
                }
                declarationSpecifiers.push_back(std::move(*result));
            }
        } while (begin < end && OpenCL::Parser::firstIsInDeclarationSpecifier(*begin, context)
                 && (begin->getTokenType() != OpenCL::Lexer::TokenType::Identifier || !seenTypeSpecifier));
        return declarationSpecifiers;
    }
} // namespace

std::vector<SpecifierQualifier> OpenCL::Parser::parseSpecifierQualifierList(Tokens::const_iterator& begin,
                                                                            Tokens::const_iterator end,
                                                                            Context& context, InRecoverySet recoverySet)
{
    bool seenTypeSpecifier = false;
    std::vector<SpecifierQualifier> specifierQualifiers;
    do
    {
        auto result = parseSpecifierQualifier(
            begin, end, context, [&context, seenTypeSpecifier, recoverySet](const Lexer::Token& token) {
                return (firstIsInSpecifierQualifier(token, context)
                        && (token.getTokenType() != Lexer::TokenType::Identifier || !seenTypeSpecifier))
                       || recoverySet(token);
            });
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

TranslationUnit OpenCL::Parser::parseTranslationUnit(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                     Context& context)
{
    std::vector<Syntax::ExternalDeclaration> global;
    while (begin < end)
    {
        auto result = parseExternalDeclaration(begin, end, context, [&context](const Lexer::Token& token) {
            return firstIsInExternalDeclaration(token, context);
        });
        if (result)
        {
            global.push_back(std::move(*result));
        }
    }
    return Syntax::TranslationUnit(std::move(global));
}

std::optional<ExternalDeclaration> OpenCL::Parser::parseExternalDeclaration(Tokens::const_iterator& begin,
                                                                            Tokens::const_iterator end,
                                                                            Context& context, InRecoverySet recoverySet)
{
    auto start = begin;

    auto declarationSpecifiers =
        parseDeclarationSpecifierList(begin, end, context, [&context, recoverySet](const Lexer::Token& token) {
            return firstIsInDeclarator(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon
                   || recoverySet(token);
        });
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
        expect(Lexer::TokenType::SemiColon, start, begin, end, context);
        return Declaration(start, begin, std::move(declarationSpecifiers), {});
    }

    auto declarator = parseDeclarator(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::Comma || token.getTokenType() == Lexer::TokenType::SemiColon
               || token.getTokenType() == Lexer::TokenType::OpenBrace
               || token.getTokenType() == Lexer::TokenType::Assignment || recoverySet(token);
    });
    if (begin == end)
    {
        expect(Lexer::TokenType::SemiColon, start, begin, end, context);
        return {};
    }
    else if (begin->getTokenType() == Lexer::TokenType::OpenBrace || firstIsInDeclaration(*begin, context))
    {
        std::vector<Declaration> declarations;
        while (begin < end && firstIsInDeclaration(*begin, context))
        {
            auto result = parseDeclaration(begin, end, context, [&context, recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::OpenBrace || firstIsInDeclaration(token, context)
                       || recoverySet(token);
            });
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
                            &std::get<TypeSpecifier>(specifiers[0]).getVariant());
                        if (primitive && *primitive == TypeSpecifier::PrimitiveTypeSpecifier::Void)
                        {
                            break;
                        }
                    }

                    /**
                     * Any parameters that overshadow typedefs need to also affect later parameters
                     */
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
                            messages.push_back(Message::error(
                                ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args("typename",
                                                                                    '\'' + *identifier + '\''),
                                context.getLineStart(typeSpecifier->begin()), context.getLineEnd(typeSpecifier->end()),
                                Modifier(typeSpecifier->begin(), typeSpecifier->end(), Modifier::PointAtBeginning)));
                            if (loc)
                            {
                                messages.push_back(Message::note(
                                    Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args('\'' + *identifier + '\''),
                                    context.getLineStart(loc->begin), context.getLineEnd(loc->end),
                                    Modifier(loc->identifier, loc->identifier + 1)));
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
                            && std::holds_alternative<std::string>(std::get<TypeSpecifier>(specifiers[0]).getVariant()))
                        {
                            auto& typeSpecifier = std::get<TypeSpecifier>(specifiers[0]);
                            const auto& name = std::get<std::string>(typeSpecifier.getVariant());
                            auto* loc = context.getLocationOf(name);
                            if (loc)
                            {
                                notes.push_back(Message::note(Notes::IDENTIFIER_IS_TYPEDEF.args('\'' + name + '\''),
                                                              context.getLineStart(loc->begin),
                                                              context.getLineEnd(loc->end),
                                                              Modifier(loc->identifier, loc->identifier + 1)));
                            }
                        }
                        notes.insert(notes.begin(),
                                     Message::error(ErrorMessages::Parser::MISSING_PARAMETER_NAME,
                                                    context.getLineStart(start), context.getLineEnd(begin),
                                                    Modifier(nodeFromNodeDerivedVariant(specifiers.back()).begin(),
                                                             *abstractDecl ?
                                                                 (*abstractDecl)->end() :
                                                                 nodeFromNodeDerivedVariant(specifiers.back()).end())));
                        context.log(std::move(notes));
                        continue;
                    }
                    auto& decl = std::get<std::unique_ptr<Declarator>>(paramDeclarator);
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
        auto compoundStatement = parseCompoundStatement(begin, end, context, recoverySet, false);
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
            auto initializer = parseInitializer(begin, end, context, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::Comma
                       || token.getTokenType() == Lexer::TokenType::SemiColon || recoverySet(token);
            });
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
            declarator = parseDeclarator(begin, end, context, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::Comma
                       || token.getTokenType() == Lexer::TokenType::SemiColon
                       || token.getTokenType() == Lexer::TokenType::Assignment || recoverySet(token);
            });
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
                auto initializer = parseInitializer(begin, end, context, [recoverySet](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::Comma
                           || token.getTokenType() == Lexer::TokenType::SemiColon || recoverySet(token);
                });
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

        if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
        {
            skipUntil(begin, end, recoverySet);
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

std::optional<Declaration> OpenCL::Parser::parseDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                            Context& context, InRecoverySet recoverySet)
{
    auto start = begin;

    bool declaratorMightActuallyBeTypedef = false;
    auto declarationSpecifiers =
        parseDeclarationSpecifierList(begin, end, context, [&context, recoverySet](const Lexer::Token& token) {
            return firstIsInDeclarator(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon
                   || recoverySet(token);
        });
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
        && context.isTypedef(std::get<std::string>(begin->getValue())))
    {
        declaratorMightActuallyBeTypedef = true;
    }

    if (begin == end || begin->getTokenType() == Lexer::TokenType::SemiColon)
    {
        expect(Lexer::TokenType::SemiColon, start, begin, end, context);
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
        auto declarator = parseDeclarator(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::Comma
                   || token.getTokenType() == Lexer::TokenType::SemiColon
                   || token.getTokenType() == Lexer::TokenType::Assignment || recoverySet(token);
        });
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
            auto initializer = parseInitializer(begin, end, context, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::SemiColon
                       || token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
            });
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
                                          context.getLineStart(loc->begin), context.getLineEnd(loc->end),
                                          Modifier(loc->identifier, loc->identifier + 1)));
        }
        if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context, std::move(notes)))
        {
            skipUntil(begin, end, recoverySet);
            return {};
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
        {
            skipUntil(begin, end, recoverySet);
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

std::optional<DeclarationSpecifier> OpenCL::Parser::parseDeclarationSpecifier(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              OpenCL::Parser::Context& context,
                                                                              InRecoverySet recoverySet)
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
                auto expected = parseStructOrUnionSpecifier(begin, end, context, recoverySet);
                if (!expected)
                {
                    skipUntil(begin, end, recoverySet);
                    return {};
                }
                return DeclarationSpecifier{TypeSpecifier(
                    start, begin, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)))};
            }
            case Lexer::TokenType::EnumKeyword:
            {
                auto expected = parseEnumSpecifier(begin, end, context, recoverySet);
                if (!expected)
                {
                    skipUntil(begin, end, recoverySet);
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
                auto name = std::get<std::string>(begin->getValue());
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
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("storage specifier or typename",
                                                                                    '\'' + begin->emitBack() + '\''),
                                    context.getLineStart(start), context.getLineEnd(begin),
                                    Modifier{begin, begin + 1, Modifier::PointAtBeginning})});
    }
    else
    {
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("storage specifier or typename"),
                                    context.getLineStart(start), context.getLineEnd(begin),
                                    Modifier{begin - 1, begin, Modifier::InsertAtEnd})});
    }
    skipUntil(begin, end, recoverySet);
    return {};
}

std::optional<StructOrUnionSpecifier> OpenCL::Parser::parseStructOrUnionSpecifier(Tokens::const_iterator& begin,
                                                                                  Tokens::const_iterator end,
                                                                                  OpenCL::Parser::Context& context,
                                                                                  InRecoverySet recoverySet)
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
                            context.getLineStart(start), context.getLineEnd(begin + 1),
                            Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning))});
        skipUntil(begin, end, recoverySet);
        return {};
    }

    if (begin == end)
    {
        context.log(
            {Message::error(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args(
                                Format::List(", ", " or ", "identifier", "'{'"), isUnion ? "union" : "struct"),
                            context.getLineStart(start), end, Modifier(end - 1, end, Modifier::Action::InsertAtEnd))});
        return {};
    }

    auto name = begin->getTokenType() == Lexer::TokenType::Identifier ? std::get<std::string>(begin->getValue()) : "";
    if (!name.empty())
    {
        begin++;
    }

    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        if (name.empty())
        {
            expect(Lexer::TokenType::Identifier, start, begin, end, context);
            skipUntil(begin, end, recoverySet);
            return {};
        }
        return StructOrUnionSpecifier(start, begin, isUnion, name, {});
    }

    auto openBrace = begin;
    begin++;
    std::vector<StructOrUnionSpecifier::StructDeclaration> structDeclarations;
    while (begin < end && begin->getTokenType() != Lexer::TokenType::CloseBrace)
    {
        auto specifierQualifiers =
            parseSpecifierQualifierList(begin, end, context, [&context](const Lexer::Token& token) {
                return firstIsInDeclarator(token, context) || token.getTokenType() == Lexer::TokenType::Colon;
            });

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
                auto constant =
                    parseAssignmentExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::Comma
                               || token.getTokenType() == Lexer::TokenType::SemiColon || recoverySet(token);
                    });
                if (constant)
                {
                    declarators.emplace_back(nullptr, std::move(*constant));
                }
                continue;
            }
            auto declarator = parseDeclarator(begin, end, context, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::Comma
                       || token.getTokenType() == Lexer::TokenType::SemiColon
                       || token.getTokenType() == Lexer::TokenType::Colon || recoverySet(token);
            });
            if (begin < end && begin->getTokenType() == Lexer::TokenType::Colon)
            {
                begin++;
                auto constant =
                    parseAssignmentExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::Comma
                               || token.getTokenType() == Lexer::TokenType::SemiColon || recoverySet(token);
                    });
                if (constant && declarator)
                {
                    declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                             std::move(*constant));
                }
            }
            else if (declarator)
            {
                declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::optional<ConstantExpression>{});
            }
        } while (true);
        if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
        {
            skipUntil(begin, end, [&context, recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::CloseBrace
                       || firstIsInDeclarationSpecifier(token, context) || recoverySet(token);
            });
        }
        structDeclarations.push_back({std::move(specifierQualifiers), std::move(declarators)});
    }
    if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context))
    {
        skipUntil(begin, end, recoverySet);
    }
    if (structDeclarations.empty())
    {
        context.log({Message::error(
            ErrorMessages::Parser::N_REQUIRES_AT_LEAST_ONE_N.args(isUnion ? "union" : "struct", "field"),
            context.getLineStart(start), context.getLineEnd(begin), Modifier(openBrace, begin))});
        return {};
    }
    return StructOrUnionSpecifier(start, begin, isUnion, name, std::move(structDeclarations));
}

std::optional<SpecifierQualifier> OpenCL::Parser::parseSpecifierQualifier(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end,
                                                                          OpenCL::Parser::Context& context,
                                                                          InRecoverySet recoverySet)
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
                auto expected = parseStructOrUnionSpecifier(begin, end, context, recoverySet);
                if (!expected)
                {
                    skipUntil(begin, end, recoverySet);
                    return {};
                }
                return SpecifierQualifier{TypeSpecifier(
                    start, begin, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)))};
            }
            case Lexer::TokenType::EnumKeyword:
            {
                auto expected = parseEnumSpecifier(begin, end, context, recoverySet);
                if (!expected)
                {
                    skipUntil(begin, end, recoverySet);
                    return {};
                }
                return SpecifierQualifier{
                    TypeSpecifier(start, begin, std::make_unique<EnumSpecifier>(std::move(*expected)))};
            }
            case Lexer::TokenType::Identifier:
            {
                auto name = std::get<std::string>(begin->getValue());
                if (context.isTypedefInScope(name))
                {
                    return Syntax::SpecifierQualifier{TypeSpecifier(start, ++begin, name)};
                }
                else if (context.isTypedef(name))
                {
                    auto* loc = context.getLocationOf(std::get<std::string>(begin->getValue()));
                    context.log(
                        {Message::error(ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                            "typename", '\'' + begin->emitBack() + '\''),
                                        context.getLineStart(start), context.getLineEnd(end),
                                        Modifier{begin, begin + 1, Modifier::PointAtBeginning}),
                         Message::note(Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args('\'' + begin->emitBack() + '\''),
                                       context.getLineStart(loc->begin), context.getLineEnd(loc->end),
                                       Modifier(loc->identifier, loc->identifier + 1))});
                    skipUntil(begin, end, recoverySet);
                    return {};
                }
                break;
            }
            default: break;
        }
    }
    if (begin < end)
    {
        context.log({Message::error(
            ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("typename", '\'' + begin->emitBack() + '\''),
            context.getLineStart(start), context.getLineEnd(begin),
            Modifier{begin, begin + 1, Modifier::PointAtBeginning})});
    }
    else
    {
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("typename"), context.getLineStart(start),
                                    end, Modifier{begin - 1, begin, Modifier::PointAtBeginning})});
    }
    skipUntil(begin, end, recoverySet);
    return {};
}

std::optional<Declarator> OpenCL::Parser::parseDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                          OpenCL::Parser::Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == Lexer::TokenType::Asterisk)
    {
        auto result = parsePointer(begin, end, context, [&context, recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::Asterisk || firstIsInDirectDeclarator(token, context)
                   || recoverySet(token);
        });
        pointers.push_back(std::move(result));
    }
    auto directDeclarator = parseDirectDeclarator(begin, end, context, recoverySet);
    if (!directDeclarator)
    {
        return {};
    }
    return Declarator(start, begin, std::move(pointers), std::move(*directDeclarator));
}

std::optional<DirectDeclarator> OpenCL::Parser::parseDirectDeclarator(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end, Context& context,
                                                                      InRecoverySet recoverySet)
{
    std::unique_ptr<DirectDeclarator> directDeclarator;

    auto start = begin;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
    {
        auto currToken = begin;
        begin++;
        directDeclarator = std::make_unique<DirectDeclarator>(
            DirectDeclaratorIdentifier(start, begin, std::get<std::string>(currToken->getValue()), currToken));
    }
    else if (begin < end && begin->getTokenType() == Lexer::TokenType::OpenParenthese)
    {
        auto openPpos = begin;
        begin++;
        auto declarator = parseDeclarator(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
        });
        if (declarator)
        {
            directDeclarator = std::make_unique<DirectDeclarator>(
                DirectDeclaratorParenthese(start, begin, std::make_unique<Declarator>(std::move(*declarator))));
        }
        if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(openPpos),
                                   context.getLineEnd(openPpos),
                                   Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
        {
            skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::OpenParenthese
                       || token.getTokenType() == Lexer::TokenType::OpenSquareBracket || recoverySet(token);
            });
        }
    }
    else
    {
        if (begin == end)
        {
            context.log({Message::error(
                ErrorMessages::Parser::EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier")),
                context.getLineStart(start), end, Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd))});
        }
        else
        {
            context.log({Message::error(
                ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                    OpenCL::Format::List(", ", " or ", "'('", "identifier"), '\'' + begin->emitBack() + '\''),
                context.getLineStart(start), context.getLineEnd(begin),
                Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning))});
        }
        skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::OpenParenthese
                   || token.getTokenType() == Lexer::TokenType::OpenSquareBracket || recoverySet(token);
        });
    }

    while (begin < end
           && (begin->getTokenType() == Lexer::TokenType::OpenParenthese
               || begin->getTokenType() == Lexer::TokenType::OpenSquareBracket))
    {
        switch (begin->getTokenType())
        {
            case Lexer::TokenType::OpenParenthese:
            {
                auto openPpos = begin;
                begin++;
                if (begin < end && firstIsInParameterTypeList(*begin, context))
                {
                    auto parameterTypeList =
                        parseParameterTypeList(begin, end, context, [recoverySet](const Lexer::Token& token) {
                            return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
                        });
                    if (directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorParentheseParameters(
                            start, begin, std::move(*directDeclarator), std::move(parameterTypeList)));
                    }
                }
                else if (begin < end)
                {
                    std::vector<std::pair<std::string, Tokens::const_iterator>> identifiers;
                    if (begin->getTokenType() == Lexer::TokenType::Identifier)
                    {
                        identifiers.emplace_back(std::get<std::string>(begin->getValue()), begin);
                        begin++;
                        while (begin < end
                               && (begin->getTokenType() == Lexer::TokenType::Comma
                                   || begin->getTokenType() == Lexer::TokenType::Identifier))
                        {
                            if (!expect(Lexer::TokenType::Comma, start, begin, end, context))
                            {
                                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                                    return token.getTokenType() == Lexer::TokenType::Identifier
                                           || token.getTokenType() == Lexer::TokenType::CloseParenthese
                                           || recoverySet(token);
                                });
                            }
                            std::string name;
                            if (!expect(Lexer::TokenType::Identifier, start, begin, end, context, {}, &name))
                            {
                                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                                    return token.getTokenType() == Lexer::TokenType::Comma
                                           || token.getTokenType() == Lexer::TokenType::CloseParenthese
                                           || recoverySet(token);
                                });
                            }
                            else
                            {
                                if (context.isTypedef(name))
                                {
                                    std::vector<Message> notes = {Message::error(
                                        ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args("identifier", "typename"),
                                        context.getLineStart(start), context.getLineEnd(begin),
                                        Modifier(begin - 1, begin))};
                                    if (auto* loc = context.getLocationOf(name))
                                    {
                                        notes.push_back(Message::note(
                                            Notes::IDENTIFIER_IS_TYPEDEF.args('\'' + name + '\''),
                                            context.getLineStart(loc->begin), context.getLineEnd(loc->end),
                                            Modifier(loc->identifier, loc->identifier + 1, Modifier::Underline)));
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
                if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context,
                            {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(openPpos),
                                           context.getLineEnd(openPpos),
                                           Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
                {
                    skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::OpenParenthese
                               || token.getTokenType() == Lexer::TokenType::OpenSquareBracket || recoverySet(token);
                    });
                }
                break;
            }
            case Lexer::TokenType::OpenSquareBracket:
            {
                auto openPpos = begin;
                begin++;
                if (begin == end)
                {
                    expect(Lexer::TokenType::CloseSquareBracket, start, begin, end, context,
                           {Message::note(Notes::TO_MATCH_N_HERE.args("'['"), context.getLineStart(openPpos),
                                          context.getLineEnd(openPpos),
                                          Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))});
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
                            default: OPENCL_UNREACHABLE;
                        }
                        begin++;
                    }
                    auto assignmentExpression = OpenCL::Parser::parseAssignmentExpression(
                        begin, end, context, [recoverySet](const Lexer::Token& token) {
                            return token.getTokenType() == Lexer::TokenType::CloseSquareBracket || recoverySet(token);
                        });
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
                            default: OPENCL_UNREACHABLE;
                        }
                        begin++;
                    }
                    if (begin < end)
                    {
                        if (begin->getTokenType() == Lexer::TokenType::StaticKeyword)
                        {
                            begin++;
                            auto assignmentExpression = OpenCL::Parser::parseAssignmentExpression(
                                begin, end, context, [recoverySet](const Lexer::Token& token) {
                                    return token.getTokenType() == Lexer::TokenType::CloseSquareBracket
                                           || recoverySet(token);
                                });
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
                            auto assignment = OpenCL::Parser::parseAssignmentExpression(
                                begin, end, context, [recoverySet](const Lexer::Token& token) {
                                    return token.getTokenType() == Lexer::TokenType::CloseSquareBracket
                                           || recoverySet(token);
                                });
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

                if (!expect(Lexer::TokenType::CloseSquareBracket, start, begin, end, context,
                            {Message::note(Notes::TO_MATCH_N_HERE.args("'['"), context.getLineStart(openPpos),
                                           context.getLineEnd(openPpos),
                                           Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
                {
                    skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::OpenParenthese
                               || token.getTokenType() == Lexer::TokenType::OpenSquareBracket || recoverySet(token);
                    });
                    if (begin == end
                        || (begin->getTokenType() != Lexer::TokenType::OpenParenthese
                            && begin->getTokenType() != Lexer::TokenType::OpenSquareBracket))
                    {
                        return {};
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

ParameterTypeList OpenCL::Parser::parseParameterTypeList(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                         OpenCL::Parser::Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    auto parameterList = parseParameterList(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
    });
    bool hasEllipse = false;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
    {
        begin++;
        if (begin == end || begin->getTokenType() != Lexer::TokenType::Ellipse)
        {
            context.log(
                {Message::error(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("parameter", "','"),
                                context.getLineStart(start), end, Modifier(begin - 1, begin, Modifier::PointAtEnd))});
        }
        else
        {
            begin++;
            hasEllipse = true;
        }
    }
    return ParameterTypeList(start, begin, std::move(parameterList), hasEllipse);
}

ParameterList OpenCL::Parser::parseParameterList(OpenCL::Parser::Tokens::const_iterator& begin,
                                                 Tokens::const_iterator end, OpenCL::Parser::Context& context,
                                                 InRecoverySet recoverySet)
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
        auto declarationSpecifiers =
            parseDeclarationSpecifierList(begin, end, context, [recoverySet, &context](const Lexer::Token& token) {
                return firstIsInAbstractDeclarator(token, context) || firstIsInDeclarator(token, context)
                       || token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
            });

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
            auto abstractDeclarator =
                parseAbstractDeclarator(begin, end, context, [recoverySet](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
                });
            parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                               std::make_unique<AbstractDeclarator>(std::move(abstractDeclarator)));
        }
        else if (result->getTokenType() == Lexer::TokenType::Identifier)
        {
            auto declarator = parseDeclarator(begin, end, context, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
            });
            if (declarator)
            {
                parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                                   std::make_unique<Declarator>(std::move(*declarator)));
            }
        }
        else if (result->getTokenType() == Lexer::TokenType::OpenParenthese)
        {
            while (result < end && result->getTokenType() == Lexer::TokenType::OpenParenthese)
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
                    auto declarator = parseDeclarator(begin, end, context, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
                    });
                    if (declarator)
                    {
                        parameterDeclarations.emplace_back(std::move(declarationSpecifiers),
                                                           std::make_unique<Declarator>(std::move(*declarator)));
                    }
                    break;
                }
                else if (result < end && result->getTokenType() != Lexer::TokenType::OpenParenthese)
                {
                    auto abstractDeclarator =
                        parseAbstractDeclarator(begin, end, context, [recoverySet](const Lexer::Token& token) {
                            return token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
                        });
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
        context.log({Message::error(
            ErrorMessages::Parser::N_REQUIRES_AT_LEAST_ONE_N.args("parameter list", "parameter"),
            context.getLineStart(begin), context.getLineEnd(begin), Modifier(begin, end, Modifier::PointAtBeginning))});
    }
    return ParameterList(start, begin, std::move(parameterDeclarations));
}

Pointer OpenCL::Parser::parsePointer(Tokens::const_iterator& begin, Tokens::const_iterator end, Context& context,
                                     InRecoverySet recoverySet)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::Asterisk, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::ConstKeyword
                   || token.getTokenType() == Lexer::TokenType::RestrictKeyword
                   || token.getTokenType() == Lexer::TokenType::VolatileKeyword || recoverySet(token);
        });
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
            default: OPENCL_UNREACHABLE;
        }
        begin++;
    }
    return Pointer(start, begin, std::move(typeQualifier));
}

AbstractDeclarator OpenCL::Parser::parseAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin,
                                                           Tokens::const_iterator end, OpenCL::Parser::Context& context,
                                                           InRecoverySet recoverySet)
{
    auto start = begin;
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == Lexer::TokenType::Asterisk)
    {
        auto result = parsePointer(begin, end, context, [&context, recoverySet](const Lexer::Token& token) {
            return firstIsInPointer(token, context) || firstIsInDirectAbstractDeclarator(token, context)
                   || recoverySet(token);
        });
        pointers.push_back(std::move(result));
    }
    if (begin < end ? !firstIsInDirectAbstractDeclarator(*begin, context) && !pointers.empty() : !pointers.empty())
    {
        return AbstractDeclarator(start, begin, std::move(pointers), {});
    }
    auto result = parseDirectAbstractDeclarator(begin, end, context, recoverySet);
    return AbstractDeclarator(start, begin, std::move(pointers),
                              result ? std::move(*result) : std::optional<DirectAbstractDeclarator>{});
}

std::optional<DirectAbstractDeclarator>
    OpenCL::Parser::parseDirectAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin,
                                                  Tokens::const_iterator end, OpenCL::Parser::Context& context,
                                                  InRecoverySet recoverySet)
{
    std::unique_ptr<DirectAbstractDeclarator> directAbstractDeclarator;
    bool first = true;
    auto start = begin;
    while (begin < end
           && (begin->getTokenType() == Lexer::TokenType::OpenParenthese
               || begin->getTokenType() == Lexer::TokenType::OpenSquareBracket))
    {
        switch (begin->getTokenType())
        {
            case Lexer::TokenType::OpenParenthese:
            {
                auto openPpos = begin;
                begin++;
                if (begin < end && firstIsInDeclarationSpecifier(*begin, context))
                {
                    auto parameterTypeList =
                        parseParameterTypeList(begin, end, context, [recoverySet](const Lexer::Token& token) {
                            return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
                        });
                    directAbstractDeclarator =
                        std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParameterTypeList(
                            start, begin, std::move(directAbstractDeclarator),
                            std::make_unique<ParameterTypeList>(std::move(parameterTypeList))));
                }
                else if (begin < end && first && firstIsInAbstractDeclarator(*begin, context))
                {
                    auto abstractDeclarator =
                        parseAbstractDeclarator(begin, end, context, [recoverySet](const Lexer::Token& token) {
                            return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
                        });
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
                if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context,
                            {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(openPpos),
                                           context.getLineEnd(openPpos),
                                           Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
                {
                    skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::OpenParenthese
                               || token.getTokenType() == Lexer::TokenType::OpenSquareBracket || recoverySet(token);
                    });
                }
                break;
            }
            case Lexer::TokenType::OpenSquareBracket:
            {
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
                        auto assignment =
                            parseAssignmentExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
                                return token.getTokenType() == Lexer::TokenType::CloseSquareBracket
                                       || recoverySet(token);
                            });
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

                if (!expect(Lexer::TokenType::CloseSquareBracket, start, begin, end, context,
                            {Message::note(Notes::TO_MATCH_N_HERE.args("'['"), context.getLineStart(openPpos),
                                           context.getLineEnd(openPpos),
                                           Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
                {
                    skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::OpenParenthese
                               || token.getTokenType() == Lexer::TokenType::OpenSquareBracket || recoverySet(token);
                    });
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
            context.log({Message::error(
                ErrorMessages::Parser::EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "'('", "'['")),
                context.getLineStart(start), begin, Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd))});
        }
        else
        {
            context.log(
                {Message::error(ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                    OpenCL::Format::List(", ", " or ", "'('", "'['"), '\'' + begin->emitBack() + '\''),
                                context.getLineStart(start), context.getLineEnd(begin),
                                Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning))});
        }
        skipUntil(begin, end, recoverySet);
        return {};
    }
    if (!directAbstractDeclarator)
    {
        return {};
    }
    return std::move(*directAbstractDeclarator);
}

std::optional<EnumSpecifier> OpenCL::Parser::parseEnumSpecifier(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end,
                                                                OpenCL::Parser::Context& context,
                                                                InRecoverySet recoverySet)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::EnumKeyword, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::OpenBrace
                   || token.getTokenType() == Lexer::TokenType::Identifier || recoverySet(token);
        });
    }
    std::string name;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
    {
        name = std::get<std::string>(begin->getValue());
        begin++;
    }
    else if (begin == end)
    {
        context.log(
            {Message::error(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("identifier", "enum"),
                            context.getLineStart(start), begin, Modifier(begin - 1, begin, Modifier::InsertAtEnd))});
        skipUntil(begin, end, recoverySet);
        return {};
    }

    auto openPpos = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        if (name.empty())
        {
            expect(Lexer::TokenType::Identifier, start, begin, end, context);
            skipUntil(begin, end, recoverySet);
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
    while (begin < end && begin->getTokenType() != Lexer::TokenType::CloseBrace)
    {
        inLoop = true;
        auto thisValueStart = begin;
        std::string valueName;
        if (!expect(Lexer::TokenType::Identifier, start, begin, end, context, {}, &valueName))
        {
            skipUntil(begin, end, [](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::Assignment
                       || token.getTokenType() == Lexer::TokenType::Comma
                       || token.getTokenType() == Lexer::TokenType::CloseBrace;
            });
        }
        else
        {
            values.emplace_back(valueName, std::optional<ConstantExpression>{});
        }

        if (begin < end && begin->getTokenType() == Lexer::TokenType::Assignment)
        {
            begin++;
            auto constant = parseAssignmentExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::Comma
                       || token.getTokenType() == Lexer::TokenType::CloseBrace || recoverySet(token);
            });
            if (constant && !valueName.empty())
            {
                values.back().second = std::move(*constant);
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
                context.log(
                    {Message::error(ErrorMessages::Parser::EXPECTED_N.args("'}'"), context.getLineStart(start),
                                    context.getLineEnd(begin), Modifier(begin - 1, begin, Modifier::InsertAtEnd))});
                return {};
            }
            else
            {
                context.log({Message::error(
                    ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args("','", '\'' + begin->emitBack() + '\''),
                    context.getLineStart(start), context.getLineEnd(begin),
                    Modifier(begin, begin + 1, Modifier::PointAtBeginning))});
                skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::Identifier
                           || token.getTokenType() == Lexer::TokenType::CloseBrace || recoverySet(token);
                });
            }
        }
    }
    if (begin < end)
    {
        begin++;
    }
    else
    {
        expect(Lexer::TokenType::CloseBrace, start, begin, end, context);
        return {};
    }
    if (!inLoop)
    {
        context.log({Message::error(ErrorMessages::Parser::N_REQUIRES_AT_LEAST_ONE_N.args("enum", "value"),
                                    context.getLineStart(start), context.getLineEnd(begin), Modifier(openPpos, end))});
    }
    return EnumSpecifier(start, begin, EnumDeclaration(start, begin, std::move(name), std::move(values)));
}

std::optional<CompoundStatement> OpenCL::Parser::parseCompoundStatement(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end,
                                                                        OpenCL::Parser::Context& context,
                                                                        InRecoverySet recoverySet, bool pushScope)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::OpenBrace, start, begin, end, context))
    {
        skipUntil(begin, end, [&context, recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseBrace || firstIsInCompoundItem(token, context)
                   || recoverySet(token);
        });
    }
    std::vector<CompoundItem> items;
    if (pushScope)
    {
        context.pushScope();
    }
    while (begin < end && firstIsInCompoundItem(*begin, context))
    {
        auto result = parseCompoundItem(begin, end, context, [&context, recoverySet](const Lexer::Token& token) {
            return firstIsInCompoundItem(token, context) || token.getTokenType() == Lexer::TokenType::CloseBrace
                   || recoverySet(token);
        });
        if (result)
        {
            items.push_back(std::move(*result));
        }
    }
    if (pushScope)
    {
        context.popScope();
    }
    if (!expect(
            Lexer::TokenType::CloseBrace, start, begin, end, context,
            {Message::note(Notes::TO_MATCH_N_HERE.args("'{'"), context.getLineStart(start), context.getLineEnd(begin),
                           Modifier(start == end ? start - 1 : start, start == end ? start : start + 1,
                                    Modifier::PointAtBeginning))}))
    {
        skipUntil(begin, end, recoverySet);
    }
    return CompoundStatement(start, begin, std::move(items));
}

std::optional<CompoundItem> OpenCL::Parser::parseCompoundItem(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                              Context& context, InRecoverySet recoverySet)
{
    if (firstIsInDeclarationSpecifier(*begin, context)
        && !(begin < end && begin->getTokenType() == Lexer::TokenType::Identifier && begin + 1 < end
             && (begin + 1)->getTokenType() == Lexer::TokenType::Colon))
    {
        auto declaration = parseDeclaration(begin, end, context, recoverySet);
        if (!declaration)
        {
            return {};
        }
        return CompoundItem(std::move(*declaration));
    }
    else
    {
        auto statement = parseStatement(begin, end, context, recoverySet);
        if (!statement)
        {
            return {};
        }
        return CompoundItem(std::move(*statement));
    }
}

std::optional<Initializer> OpenCL::Parser::parseInitializer(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                            Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    if (begin == end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        auto assignment = parseAssignmentExpression(begin, end, context, recoverySet);
        if (!assignment)
        {
            return {};
        }
        return Initializer(start, begin, std::move(*assignment));
    }
    else
    {
        begin++;
        auto initializerList = parseInitializerList(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::Comma
                   || token.getTokenType() == Lexer::TokenType::CloseBrace || recoverySet(token);
        });
        if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        if (!expect(Lexer::TokenType::CloseBrace, start, begin, end, context))
        {
            skipUntil(begin, end, recoverySet);
        }
        if (!initializerList)
        {
            return {};
        }
        return Initializer{start, begin, std::move(*initializerList)};
    }
}

std::optional<InitializerList> OpenCL::Parser::parseInitializerList(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end, Context& context,
                                                                    InRecoverySet recoverySet)
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
            expect(Lexer::TokenType::Comma, start, begin, end, context);
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
                auto openPpos = begin;
                begin++;
                auto constant =
                    parseAssignmentExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::CloseSquareBracket || recoverySet(token);
                    });
                if (constant)
                {
                    designation.emplace_back(std::move(*constant));
                }
                if (!expect(Lexer::TokenType::CloseSquareBracket, start, begin, end, context,
                            {Message::note(Notes::TO_MATCH_N_HERE.args("'['"), context.getLineStart(openPpos),
                                           context.getLineEnd(openPpos),
                                           Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
                {
                    skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::Assignment
                               || token.getTokenType() == Lexer::TokenType::OpenSquareBracket
                               || token.getTokenType() == Lexer::TokenType::Dot || recoverySet(token);
                    });
                }
            }
            else
            {
                begin++;
                std::string name;
                if (!expect(Lexer::TokenType::Identifier, start, begin, end, context, {}, &name))
                {
                    skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::Assignment
                               || token.getTokenType() == Lexer::TokenType::OpenSquareBracket
                               || token.getTokenType() == Lexer::TokenType::Dot || recoverySet(token);
                    });
                }
                designation.emplace_back(std::move(name));
            }
        }
        if (hasDesignation)
        {
            if (!expect(Lexer::TokenType::Assignment, start, begin, end, context))
            {
                skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
                    return firstIsInInitializer(token, context) || recoverySet(token);
                });
            }
        }
        auto initializer = parseInitializer(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::Comma || recoverySet(token);
        });
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

std::optional<Statement> OpenCL::Parser::parseStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                        Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    if (begin != end)
    {
        switch (begin->getTokenType())
        {
            case Lexer::TokenType::ReturnKeyword:
            {
                auto ret = parseReturnStatement(begin, end, context, recoverySet);
                return Statement(std::move(ret));
            }
            case Lexer::TokenType::IfKeyword:
            {
                auto ifStat = parseIfStatement(begin, end, context, recoverySet);
                if (!ifStat)
                {
                    return {};
                }
                return Statement(std::move(*ifStat));
            }
            case Lexer::TokenType::SwitchKeyword:
            {
                auto switchStat = parseSwitchStatement(begin, end, context, recoverySet);
                if (!switchStat)
                {
                    return {};
                }
                return Statement(std::move(*switchStat));
            }
            case Lexer::TokenType::OpenBrace:
            {
                auto compoundStatement = parseCompoundStatement(begin, end, context, recoverySet);
                if (!compoundStatement)
                {
                    return {};
                }
                return Statement{std::move(*compoundStatement)};
            }
            case Lexer::TokenType::ForKeyword:
            {
                auto forStat = parseForStatement(begin, end, context, recoverySet);
                if (!forStat)
                {
                    return {};
                }
                return Statement(std::move(*forStat));
            }
            case Lexer::TokenType::WhileKeyword:
            {
                auto headWhile = parseHeadWhileStatement(begin, end, context, recoverySet);
                if (!headWhile)
                {
                    return {};
                }
                return Statement(std::move(*headWhile));
            }
            case Lexer::TokenType::DoKeyword:
            {
                auto doWhile = parseFootWhileStatement(begin, end, context, recoverySet);
                if (!doWhile)
                {
                    return {};
                }
                return Statement(std::move(*doWhile));
            }
            case Lexer::TokenType::BreakKeyword:
            {
                begin++;
                if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
                {
                    skipUntil(begin, end, recoverySet);
                    return {};
                }
                return Statement(BreakStatement(start, begin));
            }
            case Lexer::TokenType::ContinueKeyword:
            {
                begin++;
                if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
                {
                    skipUntil(begin, end, recoverySet);
                    return {};
                }
                return Statement(ContinueStatement(start, begin));
            }
            case Lexer::TokenType::DefaultKeyword:
            {
                begin++;
                if (!expect(Lexer::TokenType::Colon, start, begin, end, context))
                {
                    skipUntil(begin, end, [&context, recoverySet](const Lexer::Token& token) {
                        return firstIsInStatement(token, context) || recoverySet(token);
                    });
                }
                auto statement = parseStatement(begin, end, context, recoverySet);
                if (!statement)
                {
                    return {};
                }
                return Statement(DefaultStatement(start, begin, std::make_unique<Statement>(std::move(*statement))));
            }
            case Lexer::TokenType::CaseKeyword:
            {
                begin++;
                auto expression =
                    parseAssignmentExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::Colon || recoverySet(token);
                    });
                if (!expect(Lexer::TokenType::Colon, start, begin, end, context))
                {
                    skipUntil(begin, end, [&context, recoverySet](const Lexer::Token& token) {
                        return firstIsInStatement(token, context) || recoverySet(token);
                    });
                }
                auto statement = parseStatement(begin, end, context, recoverySet);
                if (!statement || !expression)
                {
                    return {};
                }
                return Statement(CaseStatement(start, begin, std::move(*expression),
                                               std::make_unique<Statement>(std::move(*statement))));
            }
            case Lexer::TokenType::GotoKeyword:
            {
                begin++;
                std::string name;
                if (!expect(Lexer::TokenType::Identifier, start, begin, end, context, {}, &name))
                {
                    skipUntil(begin, end, [](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::SemiColon;
                    });
                }
                if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
                {
                    skipUntil(begin, end, recoverySet);
                }
                return Statement(GotoStatement(start, begin, name));
            }
            case Lexer::TokenType::Identifier:
            {
                if (begin + 1 < end && (begin + 1)->getTokenType() == Lexer::TokenType::Colon)
                {
                    const auto& name = std::get<std::string>(begin->getValue());
                    begin += 2;
                    auto statement = parseStatement(begin, end, context, recoverySet);
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
        auto expression = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::SemiColon || recoverySet(token);
        });
        std::vector<Message> notes;
        if (start + 1 == begin && start->getTokenType() == Lexer::TokenType::Identifier
            && context.isTypedef(std::get<std::string>(start->getValue()))
            && begin->getTokenType() == Lexer::TokenType::Identifier)
        {
            auto* loc = context.getLocationOf(std::get<std::string>(start->getValue()));
            if (loc)
            {
                notes.push_back(
                    Message::note(Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args('\'' + start->emitBack() + '\''),
                                  context.getLineStart(loc->begin), context.getLineEnd(loc->end),
                                  Modifier(loc->identifier, loc->identifier + 1, Modifier::Underline)));
            }
        }
        if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context, std::move(notes)))
        {
            skipUntil(begin, end, recoverySet);
        }
        return Statement(ExpressionStatement(start, begin, std::make_unique<Expression>(std::move(expression))));
    }
    else
    {
        if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
        {
            skipUntil(begin, end, recoverySet);
        }
        return Statement(ExpressionStatement(start, begin));
    }
}

std::optional<HeadWhileStatement> OpenCL::Parser::parseHeadWhileStatement(
    std::vector<OpenCL::Lexer::Token, std::allocator<OpenCL::Lexer::Token>>::const_iterator& begin,
    std::vector<OpenCL::Lexer::Token, std::allocator<OpenCL::Lexer::Token>>::const_iterator end,
    OpenCL::Parser::Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::WhileKeyword, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::OpenParenthese || recoverySet(token);
        });
    }
    std::optional<Tokens::const_iterator> openPpos;
    if (!expect(Lexer::TokenType::OpenParenthese, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            return firstIsInExpression(token, context) || recoverySet(token);
        });
    }
    else
    {
        openPpos = begin - 1;
    }
    auto expression = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
    });
    std::vector<Message> note;
    if (openPpos)
    {
        note = {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(*openPpos),
                              context.getLineEnd(*openPpos),
                              Modifier(*openPpos, *openPpos + 1, Modifier::PointAtBeginning))};
    }
    if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context, std::move(note)))
    {
        skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            return firstIsInStatement(token, context) || recoverySet(token);
        });
    }
    auto statement = parseStatement(begin, end, context, recoverySet);
    if (!statement)
    {
        return {};
    }
    return HeadWhileStatement(start, begin, std::move(expression), std::make_unique<Statement>(std::move(*statement)));
}

std::optional<FootWhileStatement>
    OpenCL::Parser::parseFootWhileStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                            std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                            OpenCL::Parser::Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    auto doPos = begin;
    if (!expect(Lexer::TokenType::DoKeyword, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            return firstIsInStatement(token, context) || recoverySet(token);
        });
    }
    auto statement = parseStatement(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::WhileKeyword || recoverySet(token);
    });
    if (!expect(Lexer::TokenType::WhileKeyword, start, begin, end, context,
                {Message::note(Notes::TO_MATCH_N_HERE.args("'do'"), context.getLineStart(doPos),
                               context.getLineEnd(doPos), Modifier(doPos, doPos + 1, Modifier::PointAtBeginning))}))
    {
        skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::OpenParenthese || recoverySet(token);
        });
    }
    std::optional<Tokens::const_iterator> openPpos;
    if (!expect(Lexer::TokenType::OpenParenthese, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            return firstIsInExpression(token, context) || recoverySet(token);
        });
    }
    else
    {
        openPpos = begin - 1;
    }
    auto expression = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
    });
    std::vector<Message> notes;
    if (openPpos)
    {
        notes = {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(*openPpos),
                               context.getLineEnd(*openPpos),
                               Modifier(*openPpos, *openPpos + 1, Modifier::PointAtBeginning))};
    }
    if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context, std::move(notes)))
    {
        skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::SemiColon || recoverySet(token);
        });
    }
    if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
    {
        skipUntil(begin, end, recoverySet);
    }

    if (!statement)
    {
        return {};
    }
    return FootWhileStatement(start, begin, std::make_unique<Statement>(std::move(*statement)), std::move(expression));
}

ReturnStatement OpenCL::Parser::parseReturnStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                                     std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                                     OpenCL::Parser::Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::ReturnKeyword, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::SemiColon || firstIsInExpression(token, context)
                   || recoverySet(token);
        });
    }
    if (begin < end && begin->getTokenType() == Lexer::TokenType::SemiColon)
    {
        expect(Lexer::TokenType::SemiColon, start, begin, end, context);
        return ReturnStatement(start, begin, nullptr);
    }
    else if (begin == end || !firstIsInExpression(*begin, context))
    {
        expect(Lexer::TokenType::SemiColon, start, begin, end, context);
        skipUntil(begin, end, recoverySet);
        return ReturnStatement(start, begin, nullptr);
    }
    auto expression = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::SemiColon || recoverySet(token);
    });
    if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
    {
        skipUntil(begin, end, recoverySet);
    }
    return ReturnStatement(start, begin, std::make_unique<Expression>(std::move(expression)));
}

std::optional<IfStatement> OpenCL::Parser::parseIfStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                                            std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                                            OpenCL::Parser::Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::IfKeyword, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::OpenParenthese || recoverySet(token);
        });
    }
    std::optional<Tokens::const_iterator> openPpos;
    if (!expect(Lexer::TokenType::OpenParenthese, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            return firstIsInExpression(token, context) || recoverySet(token);
        });
    }
    else
    {
        openPpos = begin - 1;
    }
    auto expression = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
    });
    std::vector<Message> note;
    if (openPpos)
    {
        note = {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(*openPpos),
                              context.getLineEnd(*openPpos),
                              Modifier(*openPpos, *openPpos + 1, Modifier::PointAtBeginning))};
    }
    if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context, std::move(note)))
    {
        skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            return firstIsInStatement(token, context) || recoverySet(token);
        });
    }
    auto statement = parseStatement(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::ElseKeyword || recoverySet(token);
    });
    if (!statement && (begin == end || begin->getTokenType() != Lexer::TokenType::ElseKeyword))
    {
        return {};
    }

    if (begin < end && begin->getTokenType() == Lexer::TokenType::ElseKeyword)
    {
        begin++;
        auto elseStatement = parseStatement(begin, end, context, recoverySet);
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

std::optional<SwitchStatement>
    OpenCL::Parser::parseSwitchStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                         std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                         OpenCL::Parser::Context& context, InRecoverySet recoverySet)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::SwitchKeyword, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::OpenParenthese || recoverySet(token);
        });
    }
    std::optional<Tokens::const_iterator> openPpos;
    if (!expect(Lexer::TokenType::OpenParenthese, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            return firstIsInExpression(token, context) || recoverySet(token);
        });
    }
    else
    {
        openPpos = begin - 1;
    }
    auto expression = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
        return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
    });
    std::vector<Message> note;
    if (openPpos)
    {
        note = {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(*openPpos),
                              context.getLineEnd(*openPpos),
                              Modifier(*openPpos, *openPpos + 1, Modifier::PointAtBeginning))};
    }
    if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context, std::move(note)))
    {
        skipUntil(begin, end, [&context, recoverySet](const Lexer::Token& token) {
            return firstIsInStatement(token, context) || recoverySet(token);
        });
    }
    auto statement = parseStatement(begin, end, context, recoverySet);
    if (!statement)
    {
        return {};
    }
    return SwitchStatement(start, begin, std::move(expression), std::make_unique<Statement>(std::move(*statement)));
}

std::optional<ForStatement> OpenCL::Parser::parseForStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                                              std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                                              OpenCL::Parser::Context& context,
                                                              InRecoverySet recoverySet)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::ForKeyword, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::OpenParenthese || recoverySet(token);
        });
    }
    auto openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParenthese, start, begin, end, context))
    {
        skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
            return firstIsInExpression(token, context) || firstIsInDeclaration(token, context)
                   || token.getTokenType() == Lexer::TokenType::SemiColon || recoverySet(token);
        });
    }
    if (begin == end)
    {
        context.log({Message::error(
            ErrorMessages::Parser::EXPECTED_N.args(Format::List(", ", " or ", "expression", "declaration")),
            context.getLineStart(start), begin, Modifier(begin - 1, begin, Modifier::InsertAtEnd))});
        return {};
    }

    std::variant<Declaration, std::unique_ptr<Expression>> initial{nullptr};
    if (firstIsInDeclaration(*begin, context))
    {
        auto decl = parseDeclaration(begin, end, context, [&context, recoverySet](const Lexer::Token& token) {
            return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon
                   || recoverySet(token);
        });
        if (decl)
        {
            initial = std::move(*decl);
        }
    }
    else if (begin->getTokenType() != Lexer::TokenType::SemiColon)
    {
        auto exp = parseExpression(begin, end, context, [&context, recoverySet](const Lexer::Token& token) {
            return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon
                   || recoverySet(token);
        });
        initial = std::make_unique<Expression>(std::move(exp));
        if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
        {
            skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon
                       || recoverySet(token);
            });
        }
    }
    else
    {
        begin++;
    }

    std::unique_ptr<Expression> controlling;
    if (begin == end)
    {
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("expression"), context.getLineStart(start),
                                    context.getLineEnd(begin), Modifier(begin - 1, begin, Modifier::InsertAtEnd))});
        return {};
    }
    else if (begin->getTokenType() != Lexer::TokenType::SemiColon)
    {
        auto exp = parseExpression(begin, end, context, [&context, recoverySet](const Lexer::Token& token) {
            return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon
                   || recoverySet(token);
        });
        controlling = std::make_unique<Expression>(std::move(exp));
        if (!expect(Lexer::TokenType::SemiColon, start, begin, end, context))
        {
            skipUntil(begin, end, [&context, recoverySet](const Lexer::Token& token) {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::CloseParenthese
                       || recoverySet(token);
            });
        }
    }
    else
    {
        begin++;
    }

    std::unique_ptr<Expression> post;
    if (begin == end)
    {
        context.log({Message::error(ErrorMessages::Parser::EXPECTED_N.args("expression"), context.getLineStart(start),
                                    begin, Modifier(begin - 1, begin, Modifier::InsertAtEnd))});
        return {};
    }
    else if (begin->getTokenType() != Lexer::TokenType::CloseParenthese)
    {
        auto exp = parseExpression(begin, end, context, [recoverySet](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseParenthese || recoverySet(token);
        });
        post = std::make_unique<Expression>(std::move(exp));
        if (!expect(Lexer::TokenType::CloseParenthese, start, begin, end, context,
                    {Message::note(Notes::TO_MATCH_N_HERE.args("'('"), context.getLineStart(openPpos),
                                   context.getLineEnd(openPpos),
                                   Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning))}))
        {
            skipUntil(begin, end, [recoverySet, &context](const Lexer::Token& token) {
                return firstIsInStatement(token, context) || recoverySet(token);
            });
        }
    }
    else
    {
        begin++;
    }

    auto stat = parseStatement(begin, end, context, recoverySet);
    if (!stat)
    {
        return {};
    }
    return ForStatement(start, begin, std::make_unique<Statement>(std::move(*stat)), std::move(initial),
                        std::move(controlling), std::move(post));
}
