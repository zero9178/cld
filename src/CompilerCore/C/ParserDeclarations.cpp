#include "Parser.hpp"

#include "ParserUtil.hpp"
#include "ConstantEvaluator.hpp"

#include <algorithm>

using namespace OpenCL::Syntax;

namespace
{
    std::optional<std::vector<DeclarationSpecifier>> parseDeclarationSpecifierList(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                                   OpenCL::Parser::Tokens::const_iterator end,
                                                                                   OpenCL::Parser::ParsingContext& context)
    {
        bool seenTypeSpecifier = false;
        std::vector<DeclarationSpecifier> declarationSpecifiers;
        while (begin < end && OpenCL::Parser::firstIsInDeclarationSpecifier(*begin, context)
            && (begin->getTokenType() != OpenCL::Lexer::TokenType::Identifier || !seenTypeSpecifier))
        {
            auto result = parseDeclarationSpecifier(begin, end, context);
            if (!result)
            {
                begin = std::find_if(begin, end, [&context](const OpenCL::Lexer::Token& token)
                {
                    return firstIsInDeclarationSpecifier(token, context) || firstIsInDeclarator(token, context)
                        || token.getTokenType() == OpenCL::Lexer::TokenType::SemiColon
                        || firstIsInAbstractDeclarator(token, context);
                });
                if (begin == end)
                {
                    return {};
                }
                continue;
            }
            if (!seenTypeSpecifier && std::holds_alternative<TypeSpecifier>(*result))
            {
                seenTypeSpecifier = true;
            }
            declarationSpecifiers.push_back(std::move(*result));
        }
        return declarationSpecifiers;
    }

    std::optional<std::vector<SpecifierQualifier>> parseSpecifierQualifierList(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                               OpenCL::Parser::Tokens::const_iterator end,
                                                                               OpenCL::Parser::ParsingContext& context)
    {
        bool seenTypeSpecifier = false;
        std::vector<SpecifierQualifier> specifierQualifiers;
        while (begin < end && firstIsInSpecifierQualifier(*begin, context)
            && (begin->getTokenType() != OpenCL::Lexer::TokenType::Identifier || !seenTypeSpecifier))
        {
            auto result = parseSpecifierQualifier(begin, end, context);
            if (!result)
            {
                begin = std::find_if(begin, end, [&context](const OpenCL::Lexer::Token& token)
                {
                    return firstIsInSpecifierQualifier(token, context) || firstIsInDeclarator(token, context)
                        || token.getTokenType() == OpenCL::Lexer::TokenType::Colon
                        || firstIsInAbstractDeclarator(token, context)
                        || token.getTokenType() == OpenCL::Lexer::TokenType::CloseParenthese;
                });
                if (begin == end)
                {
                    return {};
                }
                continue;
            }
            if (!seenTypeSpecifier && std::holds_alternative<TypeSpecifier>(*result))
            {
                seenTypeSpecifier = true;
            }
            specifierQualifiers.push_back(std::move(*result));
        }
        return specifierQualifiers;
    }
}

TranslationUnit
OpenCL::Parser::parseTranslationUnit(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                     ParsingContext& context)
{
    std::vector<Syntax::ExternalDeclaration> global;
    while (begin < end)
    {
        auto result = parseExternalDeclaration(begin, end, context);
        if (result)
        {
            global.push_back(std::move(*result));
        }
        else
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInExternalDeclaration(token, context);
            });
        }
    }
    return Syntax::TranslationUnit(std::move(global));
}

std::optional<ExternalDeclaration>
OpenCL::Parser::parseExternalDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                         ParsingContext& context)
{
    auto start = begin;
    auto dslock = context.setDiagnosticStart(start);

    bool isTypedef = false;
    bool declaratorMightActuallyBeTypedef = false;
    auto declarationSpecifiers = parseDeclarationSpecifierList(begin, end, context);
    if (!declarationSpecifiers)
    {
        begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
        {
            return firstIsInDeclarator(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
        });
        if (begin == end)
        {
            return {};
        }
    }
    else if (declarationSpecifiers->empty())
    {
        std::vector<Message::Note> notes;
        if (begin->getTokenType() == Lexer::TokenType::Identifier
            && context.isInScope(std::get<std::string>(begin->getValue()))
            && context.isTypedef(std::get<std::string>(begin->getValue())))
        {
            auto* loc = context.getLocationOf(std::get<std::string>(begin->getValue()));
            notes.push_back({Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION
                                 .args('\'' + begin->emitBack() + '\''),
                             loc->begin,
                             loc->end,
                             Modifier(loc->identifier, loc->identifier + 1, Modifier::Action::Underline)});
        }
        context.logError(ErrorMessages::EXPECTED_N_BEFORE_N
                             .args("storage specifier or typename", '\'' + begin->emitBack() + '\''),
                         findSemicolonOrEOL(start, end),
                         Modifier{begin, begin + 1, Modifier::PointAtBeginning}, std::move(notes));
    }
    else
    {
        isTypedef = std::any_of(declarationSpecifiers->begin(),
                                declarationSpecifiers->end(),
                                [](const DeclarationSpecifier& declarationSpecifier)
                                {
                                    auto* storage = std::get_if<StorageClassSpecifier>(&declarationSpecifier);
                                    if (!storage)
                                    {
                                        return false;
                                    }
                                    return storage->getSpecifier() == StorageClassSpecifier::Typedef;
                                });
        if (begin < end && std::none_of(declarationSpecifiers->begin(), declarationSpecifiers->end(),
                                        [](const DeclarationSpecifier& specifier)
                                        {
                                            return std::holds_alternative<TypeSpecifier>(specifier);
                                        }) && begin->getTokenType() == Lexer::TokenType::Identifier
            && context.isInScope(std::get<std::string>(begin->getValue()))
            && context.isTypedef(std::get<std::string>(begin->getValue())))
        {
            declaratorMightActuallyBeTypedef = true;
        }
    }

    if (begin >= end || begin->getTokenType() == Lexer::TokenType::SemiColon)
    {
        if (begin < end)
        {
            begin++;
        }
        else if (declarationSpecifiers && std::any_of(declarationSpecifiers->begin(),
                                                      declarationSpecifiers->end(),
                                                      [](const DeclarationSpecifier& specifier)
                                                      {
                                                          auto* typeSpecifier = std::get_if<TypeSpecifier>(&specifier);
                                                          if (!typeSpecifier)
                                                          {
                                                              return false;
                                                          }
                                                          return !std::holds_alternative<std::string>(typeSpecifier
                                                                                                          ->getVariant())
                                                              && !std::holds_alternative<TypeSpecifier::PrimitiveTypeSpecifier>(
                                                                  typeSpecifier->getVariant());
                                                      }))
        {
            expect(Lexer::TokenType::SemiColon, begin, end, context);
        }
        else
        {
            expect(Lexer::TokenType::Identifier, begin, end, context);
        }
        return Declaration(start,
                           begin,
                           declarationSpecifiers ? std::move(*declarationSpecifiers)
                                                 : std::vector<Syntax::DeclarationSpecifier>{},
                           {});
    }

    auto declarator = parseDeclarator(begin, end, context);
    if (!declarator)
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token)
        {
            return token.getTokenType() == Lexer::TokenType::Comma
                || token.getTokenType() == Lexer::TokenType::SemiColon
                || token.getTokenType() == Lexer::TokenType::OpenBrace;
        });
        if (begin == end)
        {
            return {};
        }
    }
    if (begin >= end)
    {
        expect(Lexer::TokenType::SemiColon, begin, end, context);
        std::vector<std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>> initializer;
        initializer.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
        return Declaration(start,
                           begin,
                           declarationSpecifiers ? std::move(*declarationSpecifiers)
                                                 : std::vector<Syntax::DeclarationSpecifier>{},
                           std::move(initializer));
    }
    else if (begin->getTokenType() == Lexer::TokenType::OpenBrace || firstIsInDeclaration(*begin, context))
    {
        std::vector<Declaration> declarations;
        while (begin < end && firstIsInDeclaration(*begin, context))
        {
            auto result = parseDeclaration(begin, end, context);
            if (!result)
            {
                begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
                {
                    return token.getTokenType() == Lexer::TokenType::OpenBrace || firstIsInDeclaration(token, context);
                });
                if (begin == end)
                {
                    return {};
                }
            }
            else
            {
                declarations.push_back(std::move(*result));
            }
        }

        context.pushScope();
        if (auto* paramters =
            std::get_if<DirectDeclaratorParentheseParameters>(&declarator->getDirectDeclarator()))
        {
            auto& parameterDeclarations = paramters->getParameterTypeList().getParameterList()
                                                   .getParameterDeclarations();
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

                if (auto* abstractDecl = std::get_if<std::unique_ptr<AbstractDeclarator>>(&paramDeclarator))
                {
                    context.logError(ErrorMessages::MISSING_PARAMETER_NAME,
                                     begin,
                                     Modifier(nodeFromNodeDerivedVariant(specifier.back()).begin(),
                                              *abstractDecl ? (*abstractDecl)->end() : nodeFromNodeDerivedVariant(
                                                  specifier.back()).end(),
                                              Modifier::Underline));
                    continue;
                }
                auto& decl = std::get<std::unique_ptr<Declarator>>(paramDeclarator);
                auto result = Semantics::declaratorToName(*decl);
                if (!result.empty())
                {
                    context.addToScope(result, {start, begin, Semantics::declaratorToLoc(*decl)});
                }
            }
        }
        else if (auto* identifierList =
            std::get_if<DirectDeclaratorParentheseIdentifiers>(&declarator->getDirectDeclarator()))
        {
            for (auto&[name, loc] : identifierList->getIdentifiers())
            {
                context.addToScope(name, {start, begin, loc});
            }
        }
        auto compoundStatement = parseCompoundStatement(begin, end, context, false);
        context.popScope();
        if (!compoundStatement)
        {
            return {};
        }

        context.addToScope(Semantics::declaratorToName(*declarator),
                           {start, compoundStatement->begin(), Semantics::declaratorToLoc(*declarator)});
        return FunctionDefinition(start,
                                  begin,
                                  declarationSpecifiers ? std::move(*declarationSpecifiers)
                                                        : std::vector<Syntax::DeclarationSpecifier>{},
                                  std::move(*declarator),
                                  std::move(declarations),
                                  std::move(*compoundStatement));
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
            auto initializer = parseInitializer(begin, end, context);
            if (!initializer)
            {
                return {};
            }
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::make_unique<Initializer>(std::move(*initializer)));
        }
        else if (declarator)
        {
            initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
        }

        while (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
            declarator = parseDeclarator(begin, end, context);
            if (!declarator)
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token)
                {
                    return token.getTokenType() == Lexer::TokenType::Comma
                        || token.getTokenType() == Lexer::TokenType::SemiColon;
                });
                if (begin == end)
                {
                    return {};
                }
                continue;
            }
            if (!isTypedef)
            {
                context.addToScope(Semantics::declaratorToName(*declarator),
                                   {start, begin, Semantics::declaratorToLoc(*declarator)});
            }
            if (begin >= end || begin->getTokenType() != Lexer::TokenType::Assignment)
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
        if (declaratorMightActuallyBeTypedef && initDeclarators.size() == 1)
        {
            auto* loc = context.getLocationOf(Semantics::declaratorToName(*initDeclarators[0].first));
            std::vector<Message::Note> notes;
            if (loc)
            {
                notes.push_back({Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION
                                     .args(
                                         '\'' + Semantics::declaratorToName(*initDeclarators[0].first) + '\''),
                                 loc->begin,
                                 loc->end,
                                 Modifier(loc->identifier, loc->identifier + 1, Modifier::Action::Underline)});
            }
            if (!expect(Lexer::TokenType::SemiColon,
                        begin,
                        end,
                        context,
                        std::move(notes)))
            {
                return {};
            }
        }
        else
        {
            if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
            {
                return {};
            }
        }

        if (isTypedef)
        {
            for (auto&[declator, init] : initDeclarators)
            {
                context.addTypedef(Semantics::declaratorToName(*declator));
            }
        }

        return Declaration(start,
                           begin,
                           declarationSpecifiers ? std::move(*declarationSpecifiers)
                                                 : std::vector<Syntax::DeclarationSpecifier>{},
                           std::move(initDeclarators));
    }
}

std::optional<Declaration>
OpenCL::Parser::parseDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context)
{
    auto start = begin;
    auto dslock = context.setDiagnosticStart(start);

    bool isTypedef = false;
    auto declarationSpecifiers = parseDeclarationSpecifierList(begin, end, context);
    if (!declarationSpecifiers)
    {
        begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
        {
            return firstIsInDeclarator(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
        });
        if (begin == end)
        {
            return {};
        }
    }
    else if (declarationSpecifiers->empty())
    {
        context.logError(ErrorMessages::EXPECTED_N_BEFORE_N.args("storage specifier or typename", begin->emitBack()),
                         findSemicolonOrEOL(start, end),
                         Modifier{begin, begin + 1, Modifier::PointAtBeginning});
    }
    else
    {
        isTypedef = std::any_of(declarationSpecifiers->begin(),
                                declarationSpecifiers->end(),
                                [](const DeclarationSpecifier& declarationSpecifier)
                                {
                                    auto* storage = std::get_if<StorageClassSpecifier>(&declarationSpecifier);
                                    if (!storage)
                                    {
                                        return false;
                                    }
                                    return storage->getSpecifier() == StorageClassSpecifier::Typedef;
                                });
    }

    if (begin >= end || begin->getTokenType() == Lexer::TokenType::SemiColon)
    {
        if (begin < end)
        {
            begin++;
        }
        else
        {
            expect(Lexer::TokenType::SemiColon, begin, end, context);
        }
        return Declaration(start,
                           begin,
                           declarationSpecifiers ? std::move(*declarationSpecifiers)
                                                 : std::vector<Syntax::DeclarationSpecifier>{},
                           {});
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
        auto declarator = parseDeclarator(begin, end, context);
        if (!declarator)
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token)
            {
                return token.getTokenType() == Lexer::TokenType::Comma
                    || token.getTokenType() == Lexer::TokenType::SemiColon;
            });
            if (begin == end)
            {
                return {};
            }
            continue;
        }
        if (!isTypedef)
        {
            context.addToScope(Semantics::declaratorToName(*declarator),
                               {start, begin, Semantics::declaratorToLoc(*declarator)});
        }
        if (begin >= end || begin->getTokenType() != Lexer::TokenType::Assignment)
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

    if (isTypedef)
    {
        for (auto&[declator, init] : initDeclarators)
        {
            context.addTypedef(Semantics::declaratorToName(*declator));
        }
    }

    if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
    {
        return {};
    }

    return Declaration(start,
                       begin,
                       declarationSpecifiers ? std::move(*declarationSpecifiers)
                                             : std::vector<Syntax::DeclarationSpecifier>{},
                       std::move(initDeclarators));
}

std::optional<DeclarationSpecifier>
OpenCL::Parser::parseDeclarationSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                          OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
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
    case Lexer::TokenType::Identifier:
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
        auto prevErrorCount = context.getCurrentErrorCount();
        auto expected = parseStructOrUnionSpecifier(begin, end, context);
        if (expected)
        {
            auto name = expected->getIdentifier();
            auto isDefinition = !expected->getStructDeclarations().empty();
            auto result =
                TypeSpecifier(start, begin, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)));
            if (isDefinition && prevErrorCount == context.getCurrentErrorCount())
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
    case Lexer::TokenType::EnumKeyword:
    {
        auto expected = parseEnumSpecifier(begin, end, context);
        if (expected)
        {
            return DeclarationSpecifier{
                TypeSpecifier(start, begin, std::make_unique<EnumSpecifier>(std::move(*expected)))};
        }
        else
        {
            return {};
        }
    }
    case Lexer::TokenType::Identifier:
    {
        auto name = std::get<std::string>(currToken.getValue());
        if (!context.isInScope(name) && context.isTypedef(name))
        {
            return Syntax::DeclarationSpecifier{TypeSpecifier(start, begin, name)};
        }
        else if (context.isTypedef(name))
        {
            context.logError({
                                 "\"" + name
                                     + "\" is a typedef but cannot be used as such because another symbol overshadows it"},
                             std::vector<OpenCL::Lexer::Token>::const_iterator(),
                             std::optional<Modifier>(),
                             std::vector<Message::Note>());
            return {};
        }
        break;
    }
    default: break;
    }
    context.logError("Invalid token for declaration specifier",
                     std::vector<OpenCL::Lexer::Token>::const_iterator(),
                     std::optional<Modifier>(),
                     std::vector<Message::Note>());
    return {};
}

std::optional<StructOrUnionSpecifier>
OpenCL::Parser::parseStructOrUnionSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                            OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    bool isUnion = false;
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
        context.logError(ErrorMessages::EXPECTED_N.args(Format::List(", ", " or ", "struct", "union")),
                         begin + 1,
                         Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning));
        begin = std::find_if(begin, end, [](const Lexer::Token& token)
        {
            return token.getTokenType() == Lexer::TokenType::Identifier
                || token.getTokenType() == Lexer::TokenType::OpenBrace;
        });
        if (begin >= end)
        {
            return {};
        }
    }

    if (begin >= end)
    {
        context.logError(ErrorMessages::EXPECTED_N_AFTER_N
                             .args(Format::List(", ", " or ", "identifier", "'{'"), isUnion ? "union" : "struct"),
                         end,
                         Modifier(end - 1, end, Modifier::Action::InsertAtEnd));
        return {};
    }

    auto name = begin->getTokenType() == Lexer::TokenType::Identifier ? std::get<std::string>(begin->getValue()) : "";
    if (!name.empty())
    {
        begin++;
    }

    if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        if (name.empty())
        {
            expect(Lexer::TokenType::Identifier, begin, end, context);
        }
        return StructOrUnionSpecifier(start, begin, isUnion, name, {});
    }

    auto dslock = context.setDiagnosticStart(start);
    begin++;
    std::vector<StructOrUnionSpecifier::StructDeclaration> structDeclarations;
    //C99 spec doesn't allow structs with no fields. Better to handle in semantic analysis however
    while (begin < end && begin->getTokenType() != Lexer::TokenType::CloseBrace)
    {
        auto specifierQualifiers = parseSpecifierQualifierList(begin, end, context);
        if (specifierQualifiers && specifierQualifiers->empty())
        {
            if (begin < end)
            {
                context.logError(ErrorMessages::EXPECTED_N_BEFORE_N.args("typename", '\'' + begin->emitBack() + '\''),
                                 findEOL(begin, end),
                                 Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning));
                begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
                {
                    return firstIsInDeclarator(token, context) || token.getTokenType() == Lexer::TokenType::Colon;
                });
                if (begin == end)
                {
                    return {};
                }
            }
            else
            {
                context.logError(ErrorMessages::EXPECTED_N.args("typename"),
                                 begin,
                                 Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd));
            }
        }

        std::vector<std::pair<std::unique_ptr<Declarator>, std::int64_t>> declarators;
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
                continue;
            }
            auto declarator = parseDeclarator(begin, end, context);
            if (!declarator)
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token)
                {
                    return token.getTokenType() == Lexer::TokenType::Comma
                        || token.getTokenType() == Lexer::TokenType::SemiColon;
                });
                if (begin == end)
                {
                    return {};
                }
                continue;
            }
            if (begin < end && begin->getTokenType() == Lexer::TokenType::Colon)
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
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return token.getTokenType() == Lexer::TokenType::CloseBrace
                    || firstIsInDeclarationSpecifier(token, context);
            });
        }
        structDeclarations
            .push_back({specifierQualifiers ? std::move(*specifierQualifiers) : std::vector<SpecifierQualifier>{},
                        std::move(declarators)});
    }
    if (!expect(Lexer::TokenType::CloseBrace, begin, end, context))
    {
        begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
        {
            return token.getTokenType() == Lexer::TokenType::CloseBrace
                || firstIsInDeclarationSpecifier(token, context);
        });
    }
    return StructOrUnionSpecifier(start, begin, isUnion, name, std::move(structDeclarations));
}

std::optional<SpecifierQualifier>
OpenCL::Parser::parseSpecifierQualifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                        OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
    auto currToken = *begin;
    switch (currToken.getTokenType())
    {
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
    case Lexer::TokenType::ConstKeyword:
        return SpecifierQualifier{TypeQualifier(start, start + 1, TypeQualifier::Const)};
    case Lexer::TokenType::RestrictKeyword:
        return SpecifierQualifier{TypeQualifier(start, start + 1, TypeQualifier::Restrict)};
    case Lexer::TokenType::VolatileKeyword:
        return SpecifierQualifier{TypeQualifier(start, start + 1, TypeQualifier::Volatile)};
    case Lexer::TokenType::VoidKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Void)};
    case Lexer::TokenType::CharKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Char)};
    case Lexer::TokenType::ShortKeyword:
        return Syntax::SpecifierQualifier{
            TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Short)};
    case Lexer::TokenType::IntKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Int)};
    case Lexer::TokenType::LongKeyword:
        return Syntax::SpecifierQualifier{TypeSpecifier(start, begin, TypeSpecifier::PrimitiveTypeSpecifier::Long)};
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
        if (expected)
        {
            auto name = expected->getIdentifier();
            bool isDefinition = !expected->getStructDeclarations().empty();
            auto result =
                TypeSpecifier(start, begin, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)));
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
    case Lexer::TokenType::EnumKeyword:
    {
        auto expected = parseEnumSpecifier(begin, end, context);
        if (expected)
        {
            return SpecifierQualifier{
                TypeSpecifier(start, begin, std::make_unique<EnumSpecifier>(std::move(*expected)))};
        }
        else
        {
            return {};
        }
    }
    case Lexer::TokenType::Identifier:
    {
        auto name = std::get<std::string>(currToken.getValue());
        if (!context.isInScope(name) && context.isTypedef(name))
        {
            return Syntax::SpecifierQualifier{TypeSpecifier(start, begin, name)};
        }
        else if (context.isTypedef(name))
        {
            context.logError({
                                 "\"" + name
                                     + "\" is a typedef but cannot be used as such because another symbol overshadows it"},
                             std::vector<OpenCL::Lexer::Token>::const_iterator(),
                             std::optional<Modifier>(),
                             std::vector<Message::Note>());
            return {};
        }
        break;
    }
    default: break;
    }
    context.logError({"Invalid token for declaration specifier"},
                     std::vector<OpenCL::Lexer::Token>::const_iterator(),
                     std::optional<Modifier>(),
                     std::vector<Message::Note>());
    return {};
}

std::optional<Declarator>
OpenCL::Parser::parseDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == Lexer::TokenType::Asterisk)
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
    return Declarator(start, begin, std::move(pointers), std::move(*directDeclarator));
}

std::optional<DirectDeclarator> OpenCL::Parser::parseDirectDeclarator(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end,
                                                                      ParsingContext& context)
{
    std::unique_ptr<DirectDeclarator> directDeclarator;
    while (begin < end
        && ((begin->getTokenType() == Lexer::TokenType::Identifier && !directDeclarator)
            || begin->getTokenType() == Lexer::TokenType::OpenParenthese
            || (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket && directDeclarator)))
    {
        auto start = directDeclarator ? nodeFromNodeDerivedVariant(*directDeclarator).begin() : begin;
        switch (begin->getTokenType())
        {
        case Lexer::TokenType::Identifier:
        {
            auto currToken = begin;
            begin++;
            directDeclarator =
                std::make_unique<DirectDeclarator>(DirectDeclaratorIdentifier(start,
                                                                              begin,
                                                                              std::get<std::string>(currToken
                                                                                                        ->getValue()),
                                                                              currToken));
            break;
        }
        case Lexer::TokenType::OpenParenthese:
        {
            auto openPpos = begin;
            begin++;
            if (directDeclarator)
            {
                if (begin < end && firstIsInParameterTypeList(*begin, context))
                {
                    auto parameterTypeList = parseParameterTypeList(begin, end, context);
                    if (!parameterTypeList)
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
                    else
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(
                            DirectDeclaratorParentheseParameters(start, begin, std::move(*directDeclarator),
                                                                 std::move(*parameterTypeList)));
                    }
                }
                else
                {
                    std::vector<std::pair<std::string, Tokens::const_iterator>> identifiers;
                    while (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
                    {
                        identifiers.emplace_back(std::get<std::string>(begin->getValue()), begin);
                        begin++;
                        if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
                        {
                            begin++;
                        }
                        else if (begin < end && begin->getTokenType() != Lexer::TokenType::CloseParenthese)
                        {
                            context.logError(ErrorMessages::EXPECTED_N_BEFORE_N
                                                 .args("','", '\'' + begin->emitBack() + '\''),
                                             findEOLor(begin, end, Lexer::TokenType::CloseParenthese),
                                             Modifier(begin, begin + 1, Modifier::PointAtBeginning));
                            begin = std::find_if(begin, end, [](const Lexer::Token& token)
                            {
                                return token.getTokenType() == Lexer::TokenType::CloseParenthese
                                    || token.getTokenType() == Lexer::TokenType::Identifier;
                            });
                            if (begin == end)
                            {
                                return {};
                            }
                        }
                    }
                    directDeclarator = std::make_unique<DirectDeclarator>(
                        DirectDeclaratorParentheseIdentifiers(start, begin, std::move(*directDeclarator),
                                                              std::move(identifiers)));
                }
            }
            else
            {
                auto declarator = parseDeclarator(begin, end, context);
                if (!declarator)
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
                else
                {
                    directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorParenthese(
                        start, begin, std::make_unique<Declarator>(std::move(*declarator))));
                }
            }
            if (!expect(Lexer::TokenType::CloseParenthese,
                        begin,
                        end,
                        context,
                        {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                          Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
            {
                if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenParenthese
                    || begin->getTokenType() != Lexer::TokenType::OpenSquareBracket)
                {
                    return {};
                }
            }
            break;
        }
        case Lexer::TokenType::OpenSquareBracket:
        {
            auto openPpos = begin;
            begin++;
            if (begin >= end)
            {
                expect(Lexer::TokenType::CloseSquareBracket,
                       begin,
                       end,
                       context,
                       {{Notes::TO_MATCH_N_HERE.args("'['"), start, findSemicolonOrEOL(begin, end),
                         Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}});
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
                    default: break;
                    }
                    begin++;
                }
                auto assignmentExpression = OpenCL::Parser::parseAssignmentExpression(begin, end, context);
                if (!assignmentExpression)
                {
                    begin = std::find_if(begin, end, [](const Lexer::Token& token)
                    {
                        return token.getTokenType() == Lexer::TokenType::CloseSquareBracket;
                    });
                    if (begin == end)
                    {
                        return {};
                    }
                }
                else
                {
                    directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorStatic(start,
                                                                                                 begin,
                                                                                                 std::move(
                                                                                                     directDeclarator),
                                                                                                 std::move(
                                                                                                     typeQualifiers),
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
                    default: break;
                    }
                    begin++;
                }
                if (begin < end)
                {
                    if (begin->getTokenType() == Lexer::TokenType::CloseSquareBracket)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorNoStaticOrAsterisk(start,
                                                                                                                 begin,
                                                                                                                 std::move(
                                                                                                                     directDeclarator),
                                                                                                                 std::move(
                                                                                                                     typeQualifiers),
                                                                                                                 nullptr));
                    }
                    else if (begin->getTokenType() == Lexer::TokenType::StaticKeyword)
                    {
                        begin++;
                        auto assignmentExpression = OpenCL::Parser::parseAssignmentExpression(begin, end, context);
                        if (!assignmentExpression)
                        {
                            begin = std::find_if(begin, end, [](const Lexer::Token& token)
                            {
                                return token.getTokenType() == Lexer::TokenType::CloseSquareBracket;
                            });
                            if (begin == end)
                            {
                                return {};
                            }
                        }
                        else
                        {
                            directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorStatic(start,
                                                                                                         begin,
                                                                                                         std::move(
                                                                                                             directDeclarator),
                                                                                                         std::move(
                                                                                                             typeQualifiers),
                                                                                                         std::move(*assignmentExpression)));
                        }
                    }
                    else if (begin->getTokenType() == Lexer::TokenType::Asterisk)
                    {
                        begin++;
                        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorAsterisk(start,
                                                                                                       begin,
                                                                                                       std::move(
                                                                                                           *directDeclarator),
                                                                                                       std::move(
                                                                                                           typeQualifiers)));

                    }
                    else
                    {
                        auto assignment = OpenCL::Parser::parseAssignmentExpression(begin, end, context);
                        if (!assignment)
                        {
                            begin = std::find_if(begin, end, [](const Lexer::Token& token)
                            {
                                return token.getTokenType() == Lexer::TokenType::CloseSquareBracket;
                            });
                            if (begin == end)
                            {
                                return {};
                            }
                        }
                        else
                        {
                            directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorNoStaticOrAsterisk(
                                start,
                                begin,
                                std::move(directDeclarator),
                                std::move(typeQualifiers),
                                std::make_unique<AssignmentExpression>(std::move(*assignment))));
                        }
                    }
                }
            }

            if (!expect(Lexer::TokenType::CloseSquareBracket,
                        begin,
                        end,
                        context,
                        {{Notes::TO_MATCH_N_HERE.args("'['"), start, findSemicolonOrEOL(begin, end),
                          Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
            {
                if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenParenthese
                    || begin->getTokenType() != Lexer::TokenType::OpenSquareBracket)
                {
                    return {};
                }
            }
            break;
        }
        default:break;
        }
    }
    if (!directDeclarator)
    {
        if (begin == end)
        {
            context.logError(OpenCL::Parser::ErrorMessages::EXPECTED_N
                                 .args(OpenCL::Format::List(", ", " or ", "'('", "identifier")),
                             begin,
                             Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd));
        }
        else
        {
            context.logError(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                 .args(OpenCL::Format::List(", ", " or ", "'('", "identifier"),
                                       '\'' + begin->emitBack() + '\''),
                             end, Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning));
        }
        return {};
    }
    return std::move(*directDeclarator);
}

std::optional<ParameterTypeList>
OpenCL::Parser::parseParameterTypeList(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                       OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    auto parameterList = parseParameterList(begin, end, context);
    bool hasEllipse = false;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
    {
        begin++;
        if (begin >= end || begin->getTokenType() != Lexer::TokenType::Ellipse)
        {
            context.logError(ErrorMessages::EXPECTED_N_AFTER_N.args("parameter", "','"),
                             findEOL(begin, end),
                             Modifier(begin - 1, begin, Modifier::PointAtEnd));
        }
        else
        {
            begin++;
            hasEllipse = true;
        }
    }
    if (!parameterList)
    {
        return {};
    }
    return ParameterTypeList(start, begin, std::move(*parameterList), hasEllipse);
}

std::optional<ParameterList> OpenCL::Parser::parseParameterList(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end,
                                                                OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
    std::vector<ParameterDeclaration> parameterDeclarations;
    bool first = true;
    while (begin < end)
    {
        auto before = begin;
        if (first)
        {
            first = false;
        }
        else if (begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        else
        {
            break;
        }
        auto declarationSpecifiers = parseDeclarationSpecifierList(begin, end, context);
        if (!declarationSpecifiers || declarationSpecifiers->empty())
        {
            begin = before;
            break;
        }
        auto result = std::find_if(begin, end, [](const Lexer::Token& token)
        {
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
            parameterDeclarations
                .emplace_back(std::move(*declarationSpecifiers), std::unique_ptr<AbstractDeclarator>());
            continue;
        }

        if (result->getTokenType() == Lexer::TokenType::OpenSquareBracket)
        {
            auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
            if (!abstractDeclarator)
            {
                return {};
            }
            parameterDeclarations.emplace_back(std::move(*declarationSpecifiers),
                                               std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator)));
        }
        else if (result->getTokenType() == Lexer::TokenType::Identifier)
        {
            auto declarator = parseDeclarator(begin, end, context);
            if (!declarator)
            {
                return {};
            }
            parameterDeclarations.emplace_back(std::move(*declarationSpecifiers),
                                               std::make_unique<Declarator>(std::move(*declarator)));
        }
        else if (result->getTokenType() == Lexer::TokenType::OpenParenthese)
        {
            while (result->getTokenType() == Lexer::TokenType::OpenParenthese)
            {
                // Ambigious
                result++;
                if (result->getTokenType() == Lexer::TokenType::Identifier)
                {
                    auto declarator = parseDeclarator(begin, end, context);
                    if (!declarator)
                    {
                        return {};
                    }
                    parameterDeclarations.emplace_back(std::move(*declarationSpecifiers),
                                                       std::make_unique<Declarator>(std::move(*declarator)));
                    break;
                }
                else if (result->getTokenType() != Lexer::TokenType::OpenParenthese)
                {
                    auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
                    if (!abstractDeclarator)
                    {
                        return {};
                    }
                    parameterDeclarations.emplace_back(
                        std::move(*declarationSpecifiers),
                        std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator)));
                    break;
                }
            }
        }
        else
        {
            parameterDeclarations
                .emplace_back(std::move(*declarationSpecifiers), std::unique_ptr<AbstractDeclarator>());
        }
    }
    if (parameterDeclarations.empty())
    {
        context.logError({"Expected at least one parameter declaration"},
                         std::vector<OpenCL::Lexer::Token>::const_iterator(),
                         std::optional<Modifier>(),
                         std::vector<Message::Note>());
    }
    return ParameterList(start, begin, std::move(parameterDeclarations));
}

std::optional<Pointer> OpenCL::Parser::parsePointer(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                    ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    if (begin->getTokenType() != Lexer::TokenType::Asterisk)
    {
        context.logError({"Expected * at the beginning of pointer"},
                         std::vector<OpenCL::Lexer::Token>::const_iterator(),
                         std::optional<Modifier>(),
                         std::vector<Message::Note>());
    }
    auto start = begin;
    begin++;
    std::vector<TypeQualifier> typeQualifier;
    while (begin < end
        && (begin->getTokenType() == Lexer::TokenType::ConstKeyword
            || begin->getTokenType() == Lexer::TokenType::RestrictKeyword
            || begin->getTokenType() == Lexer::TokenType::VolatileKeyword))
    {
        switch (begin->getTokenType())
        {
        case Lexer::TokenType::ConstKeyword: typeQualifier.emplace_back(begin, begin + 1, TypeQualifier::Const);
            break;
        case Lexer::TokenType::RestrictKeyword: typeQualifier.emplace_back(begin, begin + 1, TypeQualifier::Restrict);
            break;
        case Lexer::TokenType::VolatileKeyword: typeQualifier.emplace_back(begin, begin + 1, TypeQualifier::Volatile);
            break;
        default: break;
        }
        begin++;
    }
    return Pointer(start, begin, std::move(typeQualifier));
}

std::optional<AbstractDeclarator>
OpenCL::Parser::parseAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin, Tokens::const_iterator end,
                                        OpenCL::Parser::ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    auto start = begin;
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == Lexer::TokenType::Asterisk)
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
    return AbstractDeclarator(start, begin, std::move(pointers), std::move(*result));
}

std::optional<DirectAbstractDeclarator>
OpenCL::Parser::parseDirectAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin,
                                              Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
{
    std::unique_ptr<DirectAbstractDeclarator> directAbstractDeclarator;
    while (begin < end && (begin->getTokenType() == Lexer::TokenType::OpenParenthese
        || begin->getTokenType() == Lexer::TokenType::OpenSquareBracket))
    {
        auto start = directAbstractDeclarator ? nodeFromNodeDerivedVariant(*directAbstractDeclarator).begin() : begin;
        switch (begin->getTokenType())
        {
        case Lexer::TokenType::OpenParenthese:
        {
            auto openPpos = begin;
            begin++;
            if (begin < end && firstIsInDeclarationSpecifier(*begin, context))
            {
                auto parameterTypeList = parseParameterTypeList(begin, end, context);
                if (!parameterTypeList)
                {
                    return {};
                }
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                    DirectAbstractDeclaratorParameterTypeList(
                        start, begin, std::move(directAbstractDeclarator),
                        std::make_unique<ParameterTypeList>(std::move(*parameterTypeList))));
            }
            else if (begin < end && !directAbstractDeclarator && firstIsInAbstractDeclarator(*begin, context))
            {
                auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
                if (!abstractDeclarator)
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
                directAbstractDeclarator
                    = std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParenthese(
                    start, begin, std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator))));
            }
            else
            {
                directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                    DirectAbstractDeclaratorParameterTypeList(start, begin, std::move(directAbstractDeclarator),
                                                              nullptr));
            }
            if (!expect(Lexer::TokenType::CloseParenthese,
                        begin,
                        end,
                        context,
                        {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                          Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
            {
                if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenParenthese
                    || begin->getTokenType() != Lexer::TokenType::OpenSquareBracket)
                {
                    return {};
                }
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
                directAbstractDeclarator =
                    std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorAsterisk(start,
                                                                                                begin,
                                                                                                std::move(
                                                                                                    directAbstractDeclarator)));
            }
            else
            {
                if (begin < end && begin->getTokenType() != Lexer::TokenType::CloseSquareBracket)
                {
                    auto assignment = parseAssignmentExpression(begin, end, context);
                    if (!assignment)
                    {
                        begin = std::find_if(begin, end, [](const Lexer::Token& token)
                        {
                            return token.getTokenType() == Lexer::TokenType::CloseSquareBracket;
                        });
                        if (begin == end)
                        {
                            return {};
                        }
                    }
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                        DirectAbstractDeclaratorAssignmentExpression(
                            start, begin, std::move(directAbstractDeclarator),
                            std::make_unique<AssignmentExpression>(std::move(*assignment))));
                }
                else
                {
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                        DirectAbstractDeclaratorAssignmentExpression(start, begin,
                                                                     std::move(directAbstractDeclarator), nullptr));
                }
            }

            if (!expect(Lexer::TokenType::CloseSquareBracket,
                        begin,
                        end,
                        context,
                        {{Notes::TO_MATCH_N_HERE.args("'['"), start, findSemicolonOrEOL(begin, end),
                          Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
            {
                if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenParenthese
                    || begin->getTokenType() != Lexer::TokenType::OpenSquareBracket)
                {
                    return {};
                }
            }
            break;
        }
        default:break;
        }
    }
    if (!directAbstractDeclarator)
    {
        if (begin == end)
        {
            context.logError(OpenCL::Parser::ErrorMessages::EXPECTED_N
                                 .args(OpenCL::Format::List(", ", " or ", "'('", "'['")),
                             begin,
                             Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd));
        }
        else
        {
            context.logError(OpenCL::Parser::ErrorMessages::EXPECTED_N_INSTEAD_OF_N
                                 .args(OpenCL::Format::List(", ", " or ", "'('", "'['"),
                                       '\'' + begin->emitBack() + '\''),
                             end, Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning));
        }
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
        return {};
    }
    auto start = begin;
    auto dslock = context.setDiagnosticStart(start);
    if (!expect(Lexer::TokenType::EnumKeyword, begin, end, context))
    {
        return {};
    }
    std::string name;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
    {
        name = std::get<std::string>(begin->getValue());
        begin++;
    }
    else if (begin >= end)
    {
        context.logError(ErrorMessages::EXPECTED_N_AFTER_N.args("identifier", "enum"),
                         end,
                         Modifier(begin - 1, begin, Modifier::InsertAtEnd));
        return {};
    }

    if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        if (name.empty())
        {
            expect(Lexer::TokenType::Identifier, begin, end, context);
        }
        return EnumSpecifier(start, begin, std::move(name));
    }
    else
    {
        begin++;
    }

    std::vector<std::pair<std::string, std::int32_t>> values;
    while (begin < end && begin->getTokenType() != Lexer::TokenType::CloseBrace)
    {
        std::string valueName;
        if (!expect(Lexer::TokenType::Identifier, begin, end, context, {}, &valueName))
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token)
            {
                return token.getTokenType() == Lexer::TokenType::Assignment
                    || token.getTokenType() == Lexer::TokenType::Comma
                    || token.getTokenType() == Lexer::TokenType::CloseBrace;
            });
            if (begin == end)
            {
                return {};
            }
        }

        std::int32_t value = values.empty() ? 0 : values.back().second + 1;
        if (begin < end && begin->getTokenType() == Lexer::TokenType::Assignment)
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

        if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        else if (begin >= end || begin->getTokenType() != Lexer::TokenType::CloseBrace)
        {
            if (begin >= end)
            {
                context.logError(ErrorMessages::EXPECTED_N.args("'}'"),
                                 begin,
                                 Modifier(begin - 1, begin, Modifier::InsertAtEnd));
                return {};
            }
            else
            {
                context.logError(ErrorMessages::EXPECTED_N_INSTEAD_OF_N.args("','", '\'' + begin->emitBack() + '\''),
                                 findSemicolonOrEOL(begin, end),
                                 Modifier(begin, begin + 1, Modifier::PointAtBeginning));
                begin = std::find_if(begin, end, [](const Lexer::Token& token)
                {
                    return token.getTokenType() == Lexer::TokenType::Identifier
                        || token.getTokenType() == Lexer::TokenType::CloseBrace;
                });
                if (begin == end)
                {
                    return {};
                }
            }
        }
        values.emplace_back(valueName, value);
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
    return EnumSpecifier(start, begin, EnumDeclaration(start, begin, std::move(name), values));
}

std::optional<CompoundStatement>
OpenCL::Parser::parseCompoundStatement(OpenCL::Parser::Tokens::const_iterator& begin,
                                       Tokens::const_iterator end,
                                       OpenCL::Parser::ParsingContext& context,
                                       bool pushScope)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::OpenBrace, begin, end, context))
    {
        return {};
    }
    std::vector<CompoundItem> items;
    if (pushScope)
    {
        context.pushScope();
    }
    while (begin < end && begin->getTokenType() != Lexer::TokenType::CloseBrace)
    {
        auto result = parseCompoundItem(begin, end, context);
        if (!result)
        {
            if (pushScope)
            {
                context.popScope();
            }
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInCompoundItem(token, context) || token.getTokenType() == Lexer::TokenType::CloseBrace;
            });
            if (begin == end)
            {
                return {};
            }
        }
        else
        {
            items.push_back(std::move(*result));
        }
    }
    if (pushScope)
    {
        context.popScope();
    }
    if (!expect(Lexer::TokenType::CloseBrace,
                begin,
                end,
                context,
                {{Notes::TO_MATCH_N_HERE.args("'{'"), start, begin,
                  Modifier(start, start + 1, Modifier::PointAtBeginning)}}))
    {
        return {};
    }
    return CompoundStatement(start, begin, std::move(items));
}

std::optional<CompoundItem> OpenCL::Parser::parseCompoundItem(Tokens::const_iterator& begin,
                                                              Tokens::const_iterator end,
                                                              ParsingContext& context)
{
    if (begin >= end)
    {
        return {};
    }
    if (firstIsInDeclarationSpecifier(*begin, context)
        && !(begin < end && begin->getTokenType() == Lexer::TokenType::Identifier && begin + 1 < end
            && (begin + 1)->getTokenType() == Lexer::TokenType::Colon))
    {
        auto declaration = parseDeclaration(begin, end, context);
        if (declaration)
        {
            return CompoundItem(std::move(*declaration));
        }
    }
    else
    {
        auto statement = parseStatement(begin, end, context);
        if (statement)
        {
            return CompoundItem(std::move(*statement));
        }
    }
    return {};
}

std::optional<Initializer>
OpenCL::Parser::parseInitializer(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    auto start = begin;
    if (begin->getTokenType() != Lexer::TokenType::OpenBrace)
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
        begin++;
        auto initializerList = parseInitializerList(begin, end, context);
        if (!initializerList)
        {
            return {};
        }
        if (begin == end
            || (begin->getTokenType() != Lexer::TokenType::CloseBrace
                && begin->getTokenType() != Lexer::TokenType::Comma))
        {
            context.logError({"Expected } after initializer list"},
                             std::vector<OpenCL::Lexer::Token>::const_iterator(),
                             std::optional<Modifier>(),
                             std::vector<Message::Note>());
            return {};
        }
        if (begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        if (begin == end || begin->getTokenType() != Lexer::TokenType::CloseBrace)
        {
            context.logError({"Expected } after initializer list"},
                             std::vector<OpenCL::Lexer::Token>::const_iterator(),
                             std::optional<Modifier>(),
                             std::vector<Message::Note>());
            return {};
        }
        begin++;
        return Initializer{start, begin, std::move(*initializerList)};
    }
}

std::optional<InitializerList> OpenCL::Parser::parseInitializerList(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    ParsingContext& context)
{
    if (begin == end)
    {
        return {};
    }
    auto start = begin;
    typename InitializerList::vector vector;
    bool first = true;
    while (true)
    {
        auto before = begin;
        if (first)
        {
            first = false;
        }
        else if (begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        else
        {
            break;
        }
        std::vector<std::variant<std::size_t, std::string>> variants;
        while (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket
            || begin->getTokenType() == Lexer::TokenType::Dot)
        {
            if (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket)
            {
                begin++;
                auto constant = parseAssignmentExpression(begin, end, context);
                if (!constant)
                {
                    if (vector.empty())
                    {
                        return {};
                    }
                    else
                    {
                        begin = before;
                        goto Exit;
                    }
                }
                if (begin->getTokenType() != Lexer::TokenType::CloseSquareBracket)
                {
                    if (vector.empty())
                    {
                        context.logError({"Expected ] to close designator in initializer list"},
                                         std::vector<OpenCL::Lexer::Token>::const_iterator(),
                                         std::optional<Modifier>(),
                                         std::vector<Message::Note>());
                        return {};
                    }
                    else
                    {
                        begin = before;
                        goto Exit;
                    }
                }
                begin++;
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
            else if (begin->getTokenType() == Lexer::TokenType::Dot)
            {
                begin++;
                if (begin->getTokenType() != Lexer::TokenType::Identifier)
                {
                    if (vector.empty())
                    {
                        context.logError({"Expected identifier following dot in designation of initializer list"},
                                         std::vector<OpenCL::Lexer::Token>::const_iterator(),
                                         std::optional<Modifier>(),
                                         std::vector<Message::Note>());
                        return {};
                    }
                    else
                    {
                        begin = before;
                        goto Exit;
                    }
                }
                variants.emplace_back(std::get<std::string>(begin->getValue()));
                begin++;
            }
        }
        if (!variants.empty())
        {
            if (begin->getTokenType() == Lexer::TokenType::Assignment)
            {
                begin++;
            }
            else if (vector.empty())
            {
                context.logError({"Expected = after designators"},
                                 std::vector<OpenCL::Lexer::Token>::const_iterator(),
                                 std::optional<Modifier>(),
                                 std::vector<Message::Note>());
                return {};
            }
            else
            {
                begin = before;
                goto Exit;
            }
        }
        auto initializer = parseInitializer(begin, end, context);
        if (!initializer)
        {
            if (vector.empty())
            {
                return {};
            }
            else
            {
                begin = before;
                goto Exit;
            }
        }
        vector.push_back({std::move(*initializer), variants});
    }
Exit:
    return InitializerList{start, begin, std::move(vector)};
}

std::optional<Statement> OpenCL::Parser::parseStatement(Tokens::const_iterator& begin,
                                                        Tokens::const_iterator end, ParsingContext& context)
{
    auto start = begin;
    auto dslock = context.setDiagnosticStart(start);
    switch (begin->getTokenType())
    {
    case Lexer::TokenType::ReturnKeyword:
    {
        auto ret = parseReturnStatement(begin, end, context);
        return ret ? Statement(std::move(*ret)) : std::optional<Statement>{};
    }
    case Lexer::TokenType::IfKeyword:
    {
        auto ifStat = parseIfStatement(begin, end, context);
        return ifStat ? Statement(std::move(*ifStat)) : std::optional<Statement>{};
    }
    case Lexer::TokenType::SwitchKeyword:
    {
        auto switchStat = parseSwitchStatement(begin, end, context);
        return switchStat ? Statement(std::move(*switchStat)) : std::optional<Statement>{};
    }
    case Lexer::TokenType::OpenBrace:
    {
        auto compoundStatement = parseCompoundStatement(begin, end, context);
        return compoundStatement ? Statement{std::move(*compoundStatement)} : std::optional<Statement>{};
    }
    case Lexer::TokenType::ForKeyword:
    {
        auto forStat = parseForStatement(begin, end, context);
        return forStat ? Statement(std::move(*forStat)) : std::optional<Statement>{};
    }
    case Lexer::TokenType::WhileKeyword:
    {
        begin++;
        auto openPpos = begin;
        if (!expect(Lexer::TokenType::OpenParenthese, begin, end, context))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInExpression(token, context);
            });
            if (begin == end)
            {
                return {};
            }
        }
        auto expression = parseExpression(begin, end, context);
        if (!expression)
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
        if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context, {
            {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
              Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}
        }))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInStatement(token, context);
            });
            if (begin == end)
            {
                return {};
            }
        }
        auto statement = parseStatement(begin, end, context);
        if (!statement)
        {
            return statement;
        }
        return Statement(HeadWhileStatement(start, begin, std::move(*expression),
                                            std::make_unique<Statement>(std::move(*statement))));
    }
    case Lexer::TokenType::DoKeyword:
    {
        auto doPos = begin;
        begin++;
        auto statement = parseStatement(begin, end, context);
        if (!statement)
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token)
            {
                return token.getTokenType() == Lexer::TokenType::WhileKeyword;
            });
            if (begin == end)
            {
                return {};
            }
        }
        if (!expect(Lexer::TokenType::WhileKeyword,
                    begin,
                    end,
                    context,
                    {{Notes::TO_MATCH_N_HERE.args("'do'"), start, findSemicolonOrEOL(begin, end),
                      Modifier(doPos, doPos + 1, Modifier::PointAtBeginning)}}))
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token)
            {
                return token.getTokenType() == Lexer::TokenType::OpenParenthese;
            });
            if (begin == end)
            {
                return {};
            }
        }
        auto openPpos = begin;
        if (!expect(Lexer::TokenType::OpenParenthese, begin, end, context))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInExpression(token, context);
            });
            if (begin == end)
            {
                return {};
            }
        }
        auto expression = parseExpression(begin, end, context);
        if (!expression)
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
        if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context, {
            {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
              Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}
        }))
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token)
            {
                return token.getTokenType() == Lexer::TokenType::SemiColon;
            });
            if (begin == end)
            {
                return {};
            }
        }
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            return {};
        }

        if (!statement)
        {
            return {};
        }
        return Statement(FootWhileStatement(start, begin, std::make_unique<Statement>(std::move(*statement)),
                                            std::move(*expression)));
    }
    case Lexer::TokenType::BreakKeyword:
    {
        begin++;
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            return {};
        }
        return Statement(BreakStatement(start, begin));
    }
    case Lexer::TokenType::ContinueKeyword:
    {
        begin++;
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            return {};
        }
        return Statement(ContinueStatement(start, begin));
    }
    case Lexer::TokenType::DefaultKeyword:
    {
        begin++;
        if (!expect(Lexer::TokenType::Colon, begin, end, context))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInStatement(token, context);
            });
            if (begin == end)
            {
                return {};
            }
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
        auto expression = parseAssignmentExpression(begin, end, context);
        if (!expression)
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
                return firstIsInStatement(token, context);
            });
            if (begin == end)
            {
                return {};
            }
        }
        Semantics::ConstantEvaluator evaluator(context.structOrUnions);
        auto statement = parseStatement(begin, end, context);
        if (!statement || !expression)
        {
            return {};
        }
        auto constValue = evaluator.visit(*expression);
        if (!constValue)
        {
            return {};
        }
        return Statement(
            CaseStatement(start, begin, *constValue, std::make_unique<Statement>(std::move(*statement))));
    }
    case Lexer::TokenType::GotoKeyword:
    {
        begin++;
        std::string name;
        if (!expect(Lexer::TokenType::Identifier, begin, end, context, {}, &name))
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token)
            {
                return token.getTokenType() == Lexer::TokenType::SemiColon;
            });
            if (begin == end)
            {
                return {};
            }
        }
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            return {};
        }
        return Statement(GotoStatement(start, begin, name));
    }
    case Lexer::TokenType::Identifier:
    {
        if (begin + 1 < end && (begin + 1)->getTokenType() == Lexer::TokenType::Colon)
        {
            const auto& name = std::get<std::string>(begin->getValue());
            begin += 2;
            return Statement(LabelStatement(start, begin, name));
        }
        [[fallthrough]];
    }
    default:
    {
        if (begin != end && begin->getTokenType() != Lexer::TokenType::SemiColon)
        {
            auto expression = parseExpression(begin, end, context);
            if (!expression)
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token)
                {
                    return token.getTokenType() == Lexer::TokenType::SemiColon;
                });
                if (begin == end)
                {
                    return {};
                }
            }
            if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
            {
                return {};
            }
            return Statement(
                ExpressionStatement(start, begin, std::make_unique<Expression>(std::move(*expression))));
        }
        else
        {
            if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
            {
                return {};
            }
            return Statement(ExpressionStatement(start, begin));
        }
    }
    }
}

std::optional<ReturnStatement> OpenCL::Parser::parseReturnStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                                                    std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                                                    OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::ReturnKeyword, begin, end, context))
    {
        begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
        {
            return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
        });
        if (begin == end)
        {
            return {};
        }
    }
    if (begin < end && begin->getTokenType() == Lexer::TokenType::SemiColon)
    {
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            return {};
        }
        return ReturnStatement(start, begin, nullptr);
    }
    else if (begin >= end || !firstIsInExpression(*begin, context))
    {
        expect(Lexer::TokenType::SemiColon, begin, end, context);
        return {};
    }
    auto expression = parseExpression(begin, end, context);
    if (!expression)
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token)
        {
            return token.getTokenType() == Lexer::TokenType::SemiColon;
        });
        if (begin == end)
        {
            return {};
        }
    }
    if (!expect(Lexer::TokenType::SemiColon, begin, end, context) || !expression)
    {
        return {};
    }
    return ReturnStatement(start, begin, std::make_unique<Expression>(std::move(*expression)));
}

std::optional<IfStatement> OpenCL::Parser::parseIfStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                                            std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                                            OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::IfKeyword, begin, end, context))
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token)
        {
            return token.getTokenType() == Lexer::TokenType::OpenParenthese;
        });
        if (begin == end)
        {
            return {};
        }
    }
    auto openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParenthese, begin, end, context))
    {
        begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
        {
            return firstIsInExpression(token, context);
        });
        if (begin == end)
        {
            return {};
        }
    }
    auto expression = parseExpression(begin, end, context);
    if (!expression)
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
    if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context, {
        {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
          Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}
    }))
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token)
        {
            return token.getTokenType() == Lexer::TokenType::SemiColon;
        });
        if (begin == end)
        {
            return {};
        }
    }
    auto statement = parseStatement(begin, end, context);
    if ((!expression || !statement) && (begin >= end || begin->getTokenType() != Lexer::TokenType::ElseKeyword))
    {
        return {};
    }
    if (begin < end && begin->getTokenType() == Lexer::TokenType::ElseKeyword)
    {
        begin++;
        auto elseStatement = parseStatement(begin, end, context);
        if (!elseStatement || !statement || !expression)
        {
            return {};
        }
        return IfStatement(start, begin, std::move(*expression),
                           std::make_unique<Statement>(std::move(*statement)),
                           std::make_unique<Statement>(std::move(*elseStatement)));
    }
    else
    {
        return IfStatement(start, begin, std::move(*expression),
                           std::make_unique<Statement>(std::move(*statement)));
    }
}

std::optional<SwitchStatement> OpenCL::Parser::parseSwitchStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                                                    std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                                                    OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::SwitchKeyword, begin, end, context))
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token)
        {
            return token.getTokenType() == Lexer::TokenType::OpenParenthese;
        });
        if (begin == end)
        {
            return {};
        }
    }
    auto openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParenthese, begin, end, context))
    {
        begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
        {
            return firstIsInExpression(token, context);
        });
        if (begin == end)
        {
            return {};
        }
    }
    auto expression = parseExpression(begin, end, context);
    if (!expression)
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
    if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context, {
        {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
          Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}
    }))
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token)
        {
            return token.getTokenType() == Lexer::TokenType::SemiColon;
        });
        if (begin == end)
        {
            return {};
        }
    }
    auto statement = parseStatement(begin, end, context);
    if (!statement || !expression)
    {
        return {};
    }
    return SwitchStatement(start, begin, std::move(*expression),
                           std::make_unique<Statement>(std::move(*statement)));
}

std::optional<ForStatement> OpenCL::Parser::parseForStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                                              std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                                              OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::ForKeyword, begin, end, context))
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token)
        {
            return token.getTokenType() == Lexer::TokenType::OpenParenthese;
        });
        if (begin == end)
        {
            return {};
        }
    }
    auto openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParenthese, begin, end, context))
    {
        begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
        {
            return firstIsInExpression(token, context) || firstIsInDeclaration(token, context);
        });
        if (begin == end)
        {
            return {};
        }
    }
    if (begin >= end)
    {
        context.logError(ErrorMessages::EXPECTED_N_AFTER_N
                             .args(Format::List(", ", " or ", "expression", "declaration"), "'('"),
                         begin,
                         Modifier(begin - 1, begin, Modifier::PointAtEnd));
        return {};
    }
    std::variant<Declaration, std::unique_ptr<Expression>> initial{nullptr};
    if (firstIsInDeclaration(*begin, context))
    {
        auto decl = parseDeclaration(begin, end, context);
        if (!decl)
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
            });
            if (begin == end)
            {
                return {};
            }
        }
        else
        {
            initial = std::move(*decl);
        }
    }
    else if (begin->getTokenType() != Lexer::TokenType::SemiColon)
    {
        auto exp = parseExpression(begin, end, context);
        if (!exp)
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
            });
            if (begin == end)
            {
                return {};
            }
        }
        else
        {
            initial = std::make_unique<Expression>(std::move(*exp));
        }
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
            });
            if (begin == end)
            {
                return {};
            }
        }
    }
    else
    {
        begin++;
    }

    std::unique_ptr<Expression> controlling;
    if (begin >= end)
    {
        context.logError(ErrorMessages::EXPECTED_N_AFTER_N.args("expression", "';'"),
                         begin,
                         Modifier(begin - 1, begin, Modifier::PointAtEnd));
        return {};
    }
    else if (begin->getTokenType() != Lexer::TokenType::SemiColon)
    {
        auto exp = parseExpression(begin, end, context);
        if (!exp)
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
            });
            if (begin == end)
            {
                return {};
            }
        }
        else
        {
            controlling = std::make_unique<Expression>(std::move(*exp));
        }
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::CloseParenthese;
            });
            if (begin == end)
            {
                return {};
            }
        }
    }
    else
    {
        begin++;
    }

    std::unique_ptr<Expression> post;
    if (begin >= end)
    {
        context.logError(ErrorMessages::EXPECTED_N_AFTER_N.args("expression", "';'"),
                         begin,
                         Modifier(begin - 1, begin, Modifier::PointAtEnd));
        return {};
    }
    else if (begin->getTokenType() != Lexer::TokenType::CloseParenthese)
    {
        auto exp = parseExpression(begin, end, context);
        if (!exp)
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
        else
        {
            post = std::make_unique<Expression>(std::move(*exp));
        }
        if (!expect(Lexer::TokenType::CloseParenthese,
                    begin,
                    end,
                    context,
                    {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                      Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token)
            {
                return firstIsInStatement(token, context);
            });
            if (begin == end)
            {
                return {};
            }
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
    return ForStatement(start,
                        begin,
                        std::make_unique<Statement>(std::move(*stat)),
                        std::move(initial),
                        std::move(controlling),
                        std::move(post));
}
