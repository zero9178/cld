#include "Parser.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>

#include "ConstantEvaluator.hpp"
#include "ParserUtil.hpp"

using namespace OpenCL::Syntax;

namespace
{
    OpenCL::Parser::ParserResult<std::vector<DeclarationSpecifier>>
        parseDeclarationSpecifierList(OpenCL::Parser::Tokens::const_iterator& begin,
                                      OpenCL::Parser::Tokens::const_iterator end,
                                      OpenCL::Parser::ParsingContext& context)
    {
        bool seenTypeSpecifier = false;
        std::vector<DeclarationSpecifier> declarationSpecifiers;
        do
        {
            auto result = parseDeclarationSpecifier(begin, end, context);
            if (result.isError())
            {
                if (begin < end && firstIsInDeclarationSpecifier(*begin, context))
                {
                    continue;
                }
                else
                {
                    return result;
                }
            }
            else if (result)
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

    OpenCL::Parser::ParserResult<std::vector<SpecifierQualifier>>
        parseSpecifierQualifierList(OpenCL::Parser::Tokens::const_iterator& begin,
                                    OpenCL::Parser::Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
    {
        bool seenTypeSpecifier = false;
        std::vector<SpecifierQualifier> specifierQualifiers;
        do
        {
            auto result = parseSpecifierQualifier(begin, end, context);
            if (result.isError())
            {
                if (begin < end && firstIsInSpecifierQualifier(*begin, context))
                {
                    continue;
                }
                else
                {
                    return result;
                }
            }
            else if (result)
            {
                if (!seenTypeSpecifier && std::holds_alternative<TypeSpecifier>(*result))
                {
                    seenTypeSpecifier = true;
                }
                specifierQualifiers.push_back(std::move(*result));
            }
        } while (begin < end && firstIsInSpecifierQualifier(*begin, context)
                 && (begin->getTokenType() != OpenCL::Lexer::TokenType::Identifier || !seenTypeSpecifier));
        return specifierQualifiers;
    }
} // namespace

TranslationUnit OpenCL::Parser::parseTranslationUnit(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                     ParsingContext& context)
{
    std::vector<Syntax::ExternalDeclaration> global;
    while (begin < end)
    {
        auto result = parseExternalDeclaration(begin, end, context);
        if (result.isError())
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
                return firstIsInExternalDeclaration(token, context);
            });
        }
        else if (result)
        {
            global.push_back(std::move(*result));
        }
    }
    return Syntax::TranslationUnit(std::move(global));
}

OpenCL::Parser::ParserResult<ExternalDeclaration>
    OpenCL::Parser::parseExternalDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                             ParsingContext& context)
{
    auto start = begin;
    auto dslock = context.setDiagnosticStart(start);

    bool isTypedef = false;
    bool declaratorMightActuallyBeTypedef = false;
    auto declarationSpecifiers = parseDeclarationSpecifierList(begin, end, context);
    if (declarationSpecifiers.isError())
    {
        begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
            return firstIsInDeclarator(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
        });
    }
    else if (declarationSpecifiers)
    {
        isTypedef = std::any_of(declarationSpecifiers->begin(), declarationSpecifiers->end(),
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
                declarationSpecifiers->begin(), declarationSpecifiers->end(),
                [](const DeclarationSpecifier& specifier) { return std::holds_alternative<TypeSpecifier>(specifier); })
            && begin->getTokenType() == Lexer::TokenType::Identifier
            && context.isTypedef(std::get<std::string>(begin->getValue())))
        {
            declaratorMightActuallyBeTypedef = true;
        }
    }

    if (begin >= end || begin->getTokenType() == Lexer::TokenType::SemiColon)
    {
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            return Error{};
        }
        return Declaration(start, begin,
                           std::move(declarationSpecifiers).value_or(std::vector<Syntax::DeclarationSpecifier>{}), {});
    }

    auto declarator = parseDeclarator(begin, end, context);
    if (declarator.isError())
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::Comma
                   || token.getTokenType() == Lexer::TokenType::SemiColon
                   || token.getTokenType() == Lexer::TokenType::OpenBrace
                   || token.getTokenType() == Lexer::TokenType::Assignment;
        });
    }
    if (begin >= end)
    {
        expect(Lexer::TokenType::SemiColon, begin, end, context);
        return Error{};
    }
    else if (begin->getTokenType() == Lexer::TokenType::OpenBrace || firstIsInDeclaration(*begin, context))
    {
        std::vector<Declaration> declarations;
        while (begin < end && firstIsInDeclaration(*begin, context))
        {
            auto result = parseDeclaration(begin, end, context);
            if (result.isError())
            {
                begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::OpenBrace || firstIsInDeclaration(token, context);
                });
            }
            else if (result)
            {
                declarations.push_back(std::move(*result));
            }
        }

        context.pushScope();
        if (declarator)
        {
            if (auto* parameters =
                    std::get_if<DirectDeclaratorParentheseParameters>(&declarator->getDirectDeclarator()))
            {
                auto& parameterDeclarations =
                    parameters->getParameterTypeList().getParameterList().getParameterDeclarations();
                for (auto& [specifier, paramDeclarator] : parameterDeclarations)
                {
                    if (parameterDeclarations.size() == 1 && specifier.size() == 1
                        && std::holds_alternative<TypeSpecifier>(specifier[0]))
                    {
                        auto* primitive = std::get_if<TypeSpecifier::PrimitiveTypeSpecifier>(
                            &std::get<TypeSpecifier>(specifier[0]).getVariant());
                        if (primitive && *primitive == TypeSpecifier::PrimitiveTypeSpecifier::Void)
                        {
                            break;
                        }
                    }

                    if (auto* abstractDecl = std::get_if<std::unique_ptr<AbstractDeclarator>>(&paramDeclarator))
                    {
                        std::vector<Message::Note> notes;
                        if (specifier.size() == 1 && std::holds_alternative<TypeSpecifier>(specifier[0])
                            && std::holds_alternative<std::string>(std::get<TypeSpecifier>(specifier[0]).getVariant()))
                        {
                            auto& typeSpecifier = std::get<TypeSpecifier>(specifier[0]);
                            const auto& name = std::get<std::string>(typeSpecifier.getVariant());
                            auto* loc = context.getLocationOf(name);
                            if (loc)
                            {
                                notes.push_back({Notes::IDENTIFIER_IS_TYPDEF.args('\'' + name + '\''), loc->begin,
                                                 loc->end,
                                                 Modifier(loc->identifier, loc->identifier + 1, Modifier::Underline)});
                            }
                        }
                        context.logError(ErrorMessages::Parser::MISSING_PARAMETER_NAME, begin,
                                         Modifier(nodeFromNodeDerivedVariant(specifier.back()).begin(),
                                                  *abstractDecl ? (*abstractDecl)->end() :
                                                                  nodeFromNodeDerivedVariant(specifier.back()).end(),
                                                  Modifier::Underline),
                                         std::move(notes));
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
                for (auto& [name, loc] : identifierList->getIdentifiers())
                {
                    context.addToScope(name, {start, begin, loc});
                }
            }
        }
        auto compoundStatement = parseCompoundStatement(begin, end, context, false);
        context.popScope();
        if (!compoundStatement)
        {
            return compoundStatement;
        }

        if (declarator)
        {
            context.addToScope(Semantics::declaratorToName(*declarator),
                               {start, compoundStatement->begin(), Semantics::declaratorToLoc(*declarator)});
            return FunctionDefinition(start, begin,
                                      declarationSpecifiers ? std::move(*declarationSpecifiers) :
                                                              std::vector<Syntax::DeclarationSpecifier>{},
                                      std::move(*declarator), std::move(declarations), std::move(*compoundStatement));
        }
        else
        {
            return {};
        }
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
            if (initializer.isError())
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::Comma
                           || token.getTokenType() == Lexer::TokenType::SemiColon;
                });
            }
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
            declarator = parseDeclarator(begin, end, context);
            if (declarator.isError())
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::Comma
                           || token.getTokenType() == Lexer::TokenType::SemiColon
                           || token.getTokenType() == Lexer::TokenType::Assignment;
                });
            }
            if (!isTypedef && declarator)
            {
                context.addToScope(Semantics::declaratorToName(*declarator),
                                   {start, begin, Semantics::declaratorToLoc(*declarator)});
            }
            if ((begin >= end || begin->getTokenType() != Lexer::TokenType::Assignment))
            {
                if (declarator)
                {
                    initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
                }
            }
            else
            {
                begin++;
                auto initializer = parseInitializer(begin, end, context);
                if (initializer.isError())
                {
                    begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::Comma
                               || token.getTokenType() == Lexer::TokenType::SemiColon;
                    });
                }
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

        if (declaratorMightActuallyBeTypedef && initDeclarators.size() == 1)
        {
            auto* loc = context.getLocationOf(Semantics::declaratorToName(*initDeclarators[0].first));
            std::vector<Message::Note> notes;
            if (loc)
            {
                notes.push_back({Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args(
                                     '\'' + Semantics::declaratorToName(*initDeclarators[0].first) + '\''),
                                 loc->begin, loc->end,
                                 Modifier(loc->identifier, loc->identifier + 1, Modifier::Action::Underline)});
            }
            if (!expect(Lexer::TokenType::SemiColon, begin, end, context, std::move(notes)))
            {
                return Error{};
            }
        }
        else
        {
            if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
            {
                return Error{};
            }
        }

        if (isTypedef)
        {
            for (auto& [declator, init] : initDeclarators)
            {
                context.addTypedef(Semantics::declaratorToName(*declator),
                                   {start, begin, Semantics::declaratorToLoc(*declarator)});
            }
        }

        return Declaration(start, begin,
                           declarationSpecifiers ? std::move(*declarationSpecifiers) :
                                                   std::vector<Syntax::DeclarationSpecifier>{},
                           std::move(initDeclarators));
    }
}

OpenCL::Parser::ParserResult<Declaration>
    OpenCL::Parser::parseDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context)
{
    auto start = begin;
    auto dslock = context.setDiagnosticStart(start);

    bool isTypedef = false;
    bool declaratorMightActuallyBeTypedef = false;
    auto declarationSpecifiers = parseDeclarationSpecifierList(begin, end, context);
    if (declarationSpecifiers.isError())
    {
        begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
            return firstIsInDeclarator(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
        });
    }
    else if (declarationSpecifiers)
    {
        isTypedef = std::any_of(declarationSpecifiers->begin(), declarationSpecifiers->end(),
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
                declarationSpecifiers->begin(), declarationSpecifiers->end(),
                [](const DeclarationSpecifier& specifier) { return std::holds_alternative<TypeSpecifier>(specifier); })
            && begin->getTokenType() == Lexer::TokenType::Identifier
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
        else
        {
            if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
            {
                return Error{};
            }
        }
        return Declaration(start, begin,
                           declarationSpecifiers ? std::move(*declarationSpecifiers) :
                                                   std::vector<Syntax::DeclarationSpecifier>{},
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
        if (declarator.isError())
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::Comma
                       || token.getTokenType() == Lexer::TokenType::SemiColon
                       || token.getTokenType() == Lexer::TokenType::Assignment;
            });
        }
        if (!isTypedef && declarator)
        {
            context.addToScope(Semantics::declaratorToName(*declarator),
                               {start, begin, Semantics::declaratorToLoc(*declarator)});
        }
        if (begin >= end || begin->getTokenType() != Lexer::TokenType::Assignment)
        {
            if (declarator)
            {
                initDeclarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), nullptr);
            }
        }
        else
        {
            begin++;
            auto initializer = parseInitializer(begin, end, context);
            if (initializer.isError())
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::SemiColon
                           || token.getTokenType() == Lexer::TokenType::Comma;
                });
            }
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
        std::vector<Message::Note> notes;
        if (loc)
        {
            notes.push_back({Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args(
                                 '\'' + Semantics::declaratorToName(*initDeclarators[0].first) + '\''),
                             loc->begin, loc->end,
                             Modifier(loc->identifier, loc->identifier + 1, Modifier::Action::Underline)});
        }
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context, std::move(notes)))
        {
            return Error{};
        }
    }
    else
    {
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            return Error{};
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

    return Declaration(start, begin,
                       declarationSpecifiers ? std::move(*declarationSpecifiers) :
                                               std::vector<Syntax::DeclarationSpecifier>{},
                       std::move(initDeclarators));
}

OpenCL::Parser::ParserResult<DeclarationSpecifier>
    OpenCL::Parser::parseDeclarationSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                              OpenCL::Parser::ParsingContext& context)
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
                    return expected;
                }
                return DeclarationSpecifier{TypeSpecifier(
                    start, begin, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)))};
            }
            case Lexer::TokenType::EnumKeyword:
            {
                auto expected = parseEnumSpecifier(begin, end, context);
                if (!expected)
                {
                    return expected;
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
                else if (context.isTypedef(name))
                {
                    std::vector<Message::Note> notes;
                    auto* loc = context.getLocationOf(std::get<std::string>(begin->getValue()));
                    notes.push_back({Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args('\'' + begin->emitBack() + '\''),
                                     loc->begin, loc->end,
                                     Modifier(loc->identifier, loc->identifier + 1, Modifier::Action::Underline)});
                    context.logError(ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("storage specifier or typename",
                                                                                     '\'' + begin->emitBack() + '\''),
                                     findSemicolonOrEOL(start, end),
                                     Modifier{begin, begin + 1, Modifier::PointAtBeginning}, std::move(notes));
                    return Error{};
                }
                break;
            }
            default: break;
        }
    }
    if (begin < end)
    {
        context.logError(ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("storage specifier or typename",
                                                                         '\'' + begin->emitBack() + '\''),
                         findSemicolonOrEOL(start, end), Modifier{begin, begin + 1, Modifier::PointAtBeginning});
    }
    else
    {
        context.logError(ErrorMessages::Parser::EXPECTED_N.args("storage specifier or typename"),
                         findSemicolonOrEOL(start, end), Modifier{begin, begin + 1, Modifier::PointAtBeginning});
    }
    return Error{};
}

OpenCL::Parser::ParserResult<StructOrUnionSpecifier>
    OpenCL::Parser::parseStructOrUnionSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                OpenCL::Parser::ParsingContext& context)
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
        context.logError(ErrorMessages::Parser::EXPECTED_N.args(Format::List(", ", " or ", "struct", "union")),
                         begin + 1, Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning));
        return Error{};
    }

    if (begin >= end)
    {
        context.logError(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args(Format::List(", ", " or ", "identifier", "'{'"),
                                                                        isUnion ? "union" : "struct"),
                         end, Modifier(end - 1, end, Modifier::Action::InsertAtEnd));
        return Error{};
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
            return Error{};
        }
        return StructOrUnionSpecifier(start, begin, isUnion, name, {});
    }

    auto dslock = context.setDiagnosticStart(start);
    auto openBrace = begin;
    begin++;
    std::vector<StructOrUnionSpecifier::StructDeclaration> structDeclarations;
    while (begin < end && begin->getTokenType() != Lexer::TokenType::CloseBrace)
    {
        auto specifierQualifiers = parseSpecifierQualifierList(begin, end, context);
        if (!specifierQualifiers)
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
                return firstIsInDeclarator(token, context) || token.getTokenType() == Lexer::TokenType::Colon;
            });
            if (begin == end)
            {
                return {};
            }
        }

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
                auto constant = parseAssignmentExpression(begin, end, context);
                if (!constant)
                {
                    return {};
                }
                declarators.emplace_back(nullptr, std::move(*constant));
                continue;
            }
            auto declarator = parseDeclarator(begin, end, context);
            if (!declarator)
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token) {
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
                declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)), std::move(*constant));
            }
            else
            {
                declarators.emplace_back(std::make_unique<Declarator>(std::move(*declarator)),
                                         std::optional<ConstantExpression>{});
            }
        } while (true);
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::CloseBrace
                       || firstIsInDeclarationSpecifier(token, context);
            });
        }
        structDeclarations.push_back(
            {specifierQualifiers ? std::move(*specifierQualifiers) : std::vector<SpecifierQualifier>{},
             std::move(declarators)});
    }
    if (!expect(Lexer::TokenType::CloseBrace, begin, end, context))
    {
        return Error{};
    }
    if (structDeclarations.empty())
    {
        context.logError(ErrorMessages::Parser::N_REQUIRES_AT_LEAST_ONE_N.args(isUnion ? "union" : "struct", "field"),
                         begin, Modifier(openBrace, begin));
        return {};
    }
    return StructOrUnionSpecifier(start, begin, isUnion, name, std::move(structDeclarations));
}

OpenCL::Parser::ParserResult<SpecifierQualifier>
    OpenCL::Parser::parseSpecifierQualifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                            OpenCL::Parser::ParsingContext& context)
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
                    return expected;
                }
                return SpecifierQualifier{TypeSpecifier(
                    start, begin, std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected)))};
            }
            case Lexer::TokenType::EnumKeyword:
            {
                auto expected = parseEnumSpecifier(begin, end, context);
                if (!expected)
                {
                    return expected;
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
                    std::vector<Message::Note> notes;
                    auto* loc = context.getLocationOf(std::get<std::string>(begin->getValue()));
                    notes.push_back({Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args('\'' + begin->emitBack() + '\''),
                                     loc->begin, loc->end,
                                     Modifier(loc->identifier, loc->identifier + 1, Modifier::Action::Underline)});
                    context.logError(
                        ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("or typename", '\'' + begin->emitBack() + '\''),
                        findSemicolonOrEOL(start, end), Modifier{begin, begin + 1, Modifier::PointAtBeginning},
                        std::move(notes));
                    return Error{};
                }
                break;
            }
            default: break;
        }
    }
    if (begin < end)
    {
        context.logError(ErrorMessages::Parser::EXPECTED_N_BEFORE_N.args("typename", '\'' + begin->emitBack() + '\''),
                         findSemicolonOrEOL(start, end), Modifier{begin, begin + 1, Modifier::PointAtBeginning});
    }
    else
    {
        context.logError(ErrorMessages::Parser::EXPECTED_N.args("typename"), findSemicolonOrEOL(start, end),
                         Modifier{begin, begin + 1, Modifier::PointAtBeginning});
    }
    return Error{};
}

OpenCL::Parser::ParserResult<Declarator> OpenCL::Parser::parseDeclarator(Tokens::const_iterator& begin,
                                                                         Tokens::const_iterator end,
                                                                         OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == Lexer::TokenType::Asterisk)
    {
        auto result = parsePointer(begin, end, context);
        if (result.isError())
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::Asterisk || firstIsInDirectDeclarator(token, context);
            });
        }
        else if (result)
        {
            pointers.push_back(std::move(*result));
        }
    }
    auto directDeclarator = parseDirectDeclarator(begin, end, context);
    if (!directDeclarator)
    {
        return directDeclarator;
    }
    return Declarator(start, begin, std::move(pointers), std::move(*directDeclarator));
}

OpenCL::Parser::ParserResult<DirectDeclarator> OpenCL::Parser::parseDirectDeclarator(Tokens::const_iterator& begin,
                                                                                     Tokens::const_iterator end,
                                                                                     ParsingContext& context)
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
        auto declarator = parseDeclarator(begin, end, context);
        if (declarator.isError())
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::CloseParenthese;
            });
        }
        else
        {
            directDeclarator = std::make_unique<DirectDeclarator>(
                DirectDeclaratorParenthese(start, begin, std::make_unique<Declarator>(std::move(*declarator))));
        }
        if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context,
                    {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                      Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
        {
            if (begin >= end
                || (begin->getTokenType() != Lexer::TokenType::OpenParenthese
                    && begin->getTokenType() != Lexer::TokenType::OpenSquareBracket))
            {
                return Error{};
            }
        }
    }
    else
    {
        if (begin == end)
        {
            context.logError(
                ErrorMessages::Parser::EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "'('", "identifier")), begin,
                Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd));
        }
        else
        {
            context.logError(
                ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                    OpenCL::Format::List(", ", " or ", "'('", "identifier"), '\'' + begin->emitBack() + '\''),
                end, Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning));
        }
        if (begin >= end
            || (begin->getTokenType() != Lexer::TokenType::OpenParenthese
                && begin->getTokenType() != Lexer::TokenType::OpenSquareBracket))
        {
            return Error{};
        }
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
                    auto parameterTypeList = parseParameterTypeList(begin, end, context);
                    if (parameterTypeList.isError())
                    {
                        begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                            return token.getTokenType() == Lexer::TokenType::CloseParenthese;
                        });
                    }
                    else if (parameterTypeList && directDeclarator)
                    {
                        directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorParentheseParameters(
                            start, begin, std::move(*directDeclarator), std::move(*parameterTypeList)));
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
                            if (!expect(Lexer::TokenType::Comma, begin, end, context))
                            {
                                if (begin->getTokenType() != Lexer::TokenType::Identifier)
                                {
                                    break;
                                }
                            }
                            std::string name;
                            if (!expect(Lexer::TokenType::Identifier, begin, end, context, {}, &name))
                            {
                                if (begin->getTokenType() != Lexer::TokenType::Comma)
                                {
                                    break;
                                }
                            }
                            else
                            {
                                if (context.isTypedef(name))
                                {
                                    std::vector<Message::Note> notes;
                                    if (auto* loc = context.getLocationOf(name))
                                    {
                                        notes.push_back(
                                            {Notes::IDENTIFIER_IS_TYPDEF.args('\'' + name + '\''), loc->begin, loc->end,
                                             Modifier(loc->identifier, loc->identifier + 1, Modifier::Underline)});
                                    }
                                    context.logError(
                                        ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args("identifier", "typename"),
                                        findSemicolonOrEOL(begin, end), Modifier(begin - 1, begin, Modifier::Underline),
                                        std::move(notes));
                                    if (begin->getTokenType() == Lexer::TokenType::Comma)
                                    {
                                        continue;
                                    }
                                    else
                                    {
                                        break;
                                    }
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
                if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context,
                            {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                              Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
                {
                    if (begin >= end
                        || (begin->getTokenType() != Lexer::TokenType::OpenParenthese
                            && begin->getTokenType() != Lexer::TokenType::OpenSquareBracket))
                    {
                        return Error{};
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
                    expect(Lexer::TokenType::CloseSquareBracket, begin, end, context,
                           {{Notes::TO_MATCH_N_HERE.args("'['"), start, findSemicolonOrEOL(begin, end),
                             Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}});
                    return Error{};
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
                    auto assignmentExpression = OpenCL::Parser::parseAssignmentExpression(begin, end, context);
                    if (assignmentExpression.isError())
                    {
                        begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                            return token.getTokenType() == Lexer::TokenType::CloseSquareBracket;
                        });
                    }
                    else if (assignmentExpression && directDeclarator)
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
                            auto assignmentExpression = OpenCL::Parser::parseAssignmentExpression(begin, end, context);
                            if (assignmentExpression.isError())
                            {
                                begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                                    return token.getTokenType() == Lexer::TokenType::CloseSquareBracket;
                                });
                            }
                            else if (assignmentExpression && directDeclarator)
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
                            auto assignment = OpenCL::Parser::parseAssignmentExpression(begin, end, context);
                            if (assignment.isError())
                            {
                                begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                                    return token.getTokenType() == Lexer::TokenType::CloseSquareBracket;
                                });
                            }
                            else if (assignment && directDeclarator)
                            {
                                directDeclarator =
                                    std::make_unique<DirectDeclarator>(DirectDeclaratorNoStaticOrAsterisk(
                                        start, begin, std::move(directDeclarator), std::move(typeQualifiers),
                                        std::make_unique<AssignmentExpression>(std::move(*assignment))));
                            }
                        }
                        else
                        {
                            directDeclarator = std::make_unique<DirectDeclarator>(DirectDeclaratorNoStaticOrAsterisk(
                                start, begin, std::move(directDeclarator), std::move(typeQualifiers), nullptr));
                        }
                    }
                }

                if (!expect(Lexer::TokenType::CloseSquareBracket, begin, end, context,
                            {{Notes::TO_MATCH_N_HERE.args("'['"), start, findSemicolonOrEOL(begin, end),
                              Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
                {
                    if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenParenthese
                        || begin->getTokenType() != Lexer::TokenType::OpenSquareBracket)
                    {
                        return Error{};
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

OpenCL::Parser::ParserResult<ParameterTypeList>
    OpenCL::Parser::parseParameterTypeList(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                           OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    auto parameterList = parseParameterList(begin, end, context);
    if (parameterList.isError() && begin->getTokenType() != Lexer::TokenType::Comma)
    {
        return parameterList;
    }
    bool hasEllipse = false;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Comma)
    {
        begin++;
        if (begin >= end || begin->getTokenType() != Lexer::TokenType::Ellipse)
        {
            context.logError(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("parameter", "','"), findEOL(begin, end),
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

OpenCL::Parser::ParserResult<ParameterList>
    OpenCL::Parser::parseParameterList(OpenCL::Parser::Tokens::const_iterator& begin, Tokens::const_iterator end,
                                       OpenCL::Parser::ParsingContext& context)
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
                 && (begin + 1 >= end || (begin + 1)->getTokenType() != Lexer::TokenType::Ellipse))
        {
            begin++;
        }
        else
        {
            break;
        }
        auto declarationSpecifiers = parseDeclarationSpecifierList(begin, end, context);
        if (declarationSpecifiers.isError())
        {
            if (begin >= end
                || (!firstIsInAbstractDeclarator(*begin, context) && !firstIsInDeclarator(*begin, context)))
            {
                break;
            }
        }

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
            if (declarationSpecifiers)
            {
                parameterDeclarations.emplace_back(std::move(*declarationSpecifiers),
                                                   std::unique_ptr<AbstractDeclarator>());
            }
            continue;
        }

        if (result->getTokenType() == Lexer::TokenType::OpenSquareBracket)
        {
            auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
            if (abstractDeclarator.isError() && (begin >= end || begin->getTokenType() != Lexer::TokenType::Comma))
            {
                return abstractDeclarator;
            }
            else if (abstractDeclarator && declarationSpecifiers)
            {
                parameterDeclarations.emplace_back(
                    std::move(*declarationSpecifiers),
                    std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator)));
            }
        }
        else if (result->getTokenType() == Lexer::TokenType::Identifier)
        {
            auto declarator = parseDeclarator(begin, end, context);
            if (declarator.isError() && (begin >= end || begin->getTokenType() != Lexer::TokenType::Comma))
            {
                return declarator;
            }
            else if (declarator && declarationSpecifiers)
            {
                parameterDeclarations.emplace_back(std::move(*declarationSpecifiers),
                                                   std::make_unique<Declarator>(std::move(*declarator)));
            }
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
                    if (declarator.isError() && (begin >= end || begin->getTokenType() != Lexer::TokenType::Comma))
                    {
                        return declarator;
                    }
                    else if (declarationSpecifiers && declarator)
                    {
                        parameterDeclarations.emplace_back(std::move(*declarationSpecifiers),
                                                           std::make_unique<Declarator>(std::move(*declarator)));
                    }
                    break;
                }
                else if (result->getTokenType() != Lexer::TokenType::OpenParenthese)
                {
                    auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
                    if (abstractDeclarator.isError()
                        && (begin >= end || begin->getTokenType() != Lexer::TokenType::Comma))
                    {
                        return abstractDeclarator;
                    }
                    else if (abstractDeclarator && declarationSpecifiers)
                    {
                        parameterDeclarations.emplace_back(
                            std::move(*declarationSpecifiers),
                            std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator)));
                    }
                    break;
                }
            }
        }
        else if (declarationSpecifiers)
        {
            parameterDeclarations.emplace_back(std::move(*declarationSpecifiers),
                                               std::unique_ptr<AbstractDeclarator>());
        }
    }
    if (first)
    {
        context.logError(ErrorMessages::Parser::N_REQUIRES_AT_LEAST_ONE_N.args("parameter list", "parameter"),
                         findSemicolonOrEOL(begin, end), Modifier(begin, end, Modifier::PointAtBeginning));
    }
    return ParameterList(start, begin, std::move(parameterDeclarations));
}

OpenCL::Parser::ParserResult<Pointer> OpenCL::Parser::parsePointer(Tokens::const_iterator& begin,
                                                                   Tokens::const_iterator end, ParsingContext& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::Asterisk, begin, end, context))
    {
        return Error{};
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

OpenCL::Parser::ParserResult<AbstractDeclarator>
    OpenCL::Parser::parseAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin, Tokens::const_iterator end,
                                            OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    std::vector<Syntax::Pointer> pointers;
    while (begin < end && begin->getTokenType() == Lexer::TokenType::Asterisk)
    {
        auto result = parsePointer(begin, end, context);
        if (result.isError()
            && (begin >= end
                || (begin->getTokenType() != Lexer::TokenType::Asterisk
                    && !firstIsInDirectAbstractDeclarator(*begin, context))))
        {
            return result;
        }
        else if (result)
        {
            pointers.push_back(std::move(*result));
        }
    }
    if (begin < end ? !firstIsInDirectAbstractDeclarator(*begin, context) && !pointers.empty() : !pointers.empty())
    {
        return AbstractDeclarator(start, begin, std::move(pointers), {});
    }
    auto result = parseDirectAbstractDeclarator(begin, end, context);
    if (!result)
    {
        return result;
    }
    return AbstractDeclarator(start, begin, std::move(pointers), std::move(*result));
}

OpenCL::Parser::ParserResult<DirectAbstractDeclarator>
    OpenCL::Parser::parseDirectAbstractDeclarator(OpenCL::Parser::Tokens::const_iterator& begin,
                                                  Tokens::const_iterator end, OpenCL::Parser::ParsingContext& context)
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
                    auto parameterTypeList = parseParameterTypeList(begin, end, context);
                    if (parameterTypeList.isError())
                    {
                        begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                            return token.getTokenType() == Lexer::TokenType::CloseParenthese;
                        });
                    }
                    else if (parameterTypeList)
                    {
                        directAbstractDeclarator =
                            std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParameterTypeList(
                                start, begin, std::move(directAbstractDeclarator),
                                std::make_unique<ParameterTypeList>(std::move(*parameterTypeList))));
                    }
                }
                else if (begin < end && first && firstIsInAbstractDeclarator(*begin, context))
                {
                    auto abstractDeclarator = parseAbstractDeclarator(begin, end, context);
                    if (abstractDeclarator.isError())
                    {
                        begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                            return token.getTokenType() == Lexer::TokenType::CloseParenthese;
                        });
                    }
                    else if (abstractDeclarator)
                    {
                        directAbstractDeclarator =
                            std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParenthese(
                                start, begin, std::make_unique<AbstractDeclarator>(std::move(*abstractDeclarator))));
                    }
                }
                else
                {
                    directAbstractDeclarator =
                        std::make_unique<DirectAbstractDeclarator>(DirectAbstractDeclaratorParameterTypeList(
                            start, begin, std::move(directAbstractDeclarator), nullptr));
                }
                if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context,
                            {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                              Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
                {
                    if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenParenthese
                        || begin->getTokenType() != Lexer::TokenType::OpenSquareBracket)
                    {
                        return Error{};
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
                    directAbstractDeclarator = std::make_unique<DirectAbstractDeclarator>(
                        DirectAbstractDeclaratorAsterisk(start, begin, std::move(directAbstractDeclarator)));
                }
                else
                {
                    if (begin < end && firstIsInAssignmentExpression(*begin, context))
                    {
                        auto assignment = parseAssignmentExpression(begin, end, context);
                        if (assignment.isError())
                        {
                            begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                                return token.getTokenType() == Lexer::TokenType::CloseSquareBracket;
                            });
                        }
                        else if (assignment)
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
                            {{Notes::TO_MATCH_N_HERE.args("'['"), start, findSemicolonOrEOL(begin, end),
                              Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
                {
                    if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenParenthese
                        || begin->getTokenType() != Lexer::TokenType::OpenSquareBracket)
                    {
                        return Error{};
                    }
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
            context.logError(ErrorMessages::Parser::EXPECTED_N.args(OpenCL::Format::List(", ", " or ", "'('", "'['")),
                             begin, Modifier(begin - 1, begin, Modifier::Action::InsertAtEnd));
        }
        else
        {
            context.logError(ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args(
                                 OpenCL::Format::List(", ", " or ", "'('", "'['"), '\'' + begin->emitBack() + '\''),
                             end, Modifier(begin, begin + 1, Modifier::Action::PointAtBeginning));
        }
        return Error{};
    }
    if (!directAbstractDeclarator)
    {
        return {};
    }
    return std::move(*directAbstractDeclarator);
}

OpenCL::Parser::ParserResult<EnumSpecifier>
    OpenCL::Parser::parseEnumSpecifier(OpenCL::Parser::Tokens::const_iterator& begin, Tokens::const_iterator end,
                                       OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    auto dslock = context.setDiagnosticStart(start);
    if (!expect(Lexer::TokenType::EnumKeyword, begin, end, context))
    {
        return Error{};
    }
    std::string name;
    if (begin < end && begin->getTokenType() == Lexer::TokenType::Identifier)
    {
        name = std::get<std::string>(begin->getValue());
        begin++;
    }
    else if (begin >= end)
    {
        context.logError(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("identifier", "enum"), end,
                         Modifier(begin - 1, begin, Modifier::InsertAtEnd));
        return Error{};
    }

    auto openPpos = begin;
    if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        if (name.empty())
        {
            expect(Lexer::TokenType::Identifier, begin, end, context);
            return Error{};
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
        if (!expect(Lexer::TokenType::Identifier, begin, end, context, {}, &valueName))
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token) {
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
            auto constant = parseAssignmentExpression(begin, end, context);
            if (constant.isError())
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::Comma
                           || token.getTokenType() == Lexer::TokenType::CloseBrace;
                });
            }
            else if (constant)
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
        else if (begin >= end || begin->getTokenType() != Lexer::TokenType::CloseBrace)
        {
            if (begin >= end)
            {
                context.logError(ErrorMessages::Parser::EXPECTED_N.args("'}'"), begin,
                                 Modifier(begin - 1, begin, Modifier::InsertAtEnd));
                return Error{};
            }
            else
            {
                context.logError(
                    ErrorMessages::Parser::EXPECTED_N_INSTEAD_OF_N.args("','", '\'' + begin->emitBack() + '\''),
                    findSemicolonOrEOL(begin, end), Modifier(begin, begin + 1, Modifier::PointAtBeginning));
                begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::Identifier
                           || token.getTokenType() == Lexer::TokenType::CloseBrace;
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
        expect(Lexer::TokenType::CloseBrace, begin, end, context);
        return Error{};
    }
    if (!inLoop)
    {
        context.logError(ErrorMessages::Parser::N_REQUIRES_AT_LEAST_ONE_N.args("enum", "value"), end,
                         Modifier(openPpos, end));
    }
    return EnumSpecifier(start, begin, EnumDeclaration(start, begin, std::move(name), std::move(values)));
}

OpenCL::Parser::ParserResult<CompoundStatement>
    OpenCL::Parser::parseCompoundStatement(OpenCL::Parser::Tokens::const_iterator& begin, Tokens::const_iterator end,
                                           OpenCL::Parser::ParsingContext& context, bool pushScope)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::OpenBrace, begin, end, context))
    {
        return Error{};
    }
    std::vector<CompoundItem> items;
    if (pushScope)
    {
        context.pushScope();
    }
    while (begin < end && begin->getTokenType() != Lexer::TokenType::CloseBrace)
    {
        auto result = parseCompoundItem(begin, end, context);
        if (result.isError())
        {
            if (pushScope)
            {
                context.popScope();
            }
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
                return firstIsInCompoundItem(token, context) || token.getTokenType() == Lexer::TokenType::CloseBrace;
            });
        }
        else if (result)
        {
            items.push_back(std::move(*result));
        }
    }
    if (pushScope)
    {
        context.popScope();
    }
    if (!expect(Lexer::TokenType::CloseBrace, begin, end, context,
                {{Notes::TO_MATCH_N_HERE.args("'{'"), start, begin,
                  Modifier(start, start + 1, Modifier::PointAtBeginning)}}))
    {
        return Error{};
    }
    return CompoundStatement(start, begin, std::move(items));
}

OpenCL::Parser::ParserResult<CompoundItem> OpenCL::Parser::parseCompoundItem(Tokens::const_iterator& begin,
                                                                             Tokens::const_iterator end,
                                                                             ParsingContext& context)
{
    if (firstIsInDeclarationSpecifier(*begin, context)
        && !(begin < end && begin->getTokenType() == Lexer::TokenType::Identifier && begin + 1 < end
             && (begin + 1)->getTokenType() == Lexer::TokenType::Colon))
    {
        auto declaration = parseDeclaration(begin, end, context);
        if (!declaration)
        {
            return declaration;
        }
        return CompoundItem(std::move(*declaration));
    }
    else
    {
        auto statement = parseStatement(begin, end, context);
        if (!statement)
        {
            return statement;
        }
        return CompoundItem(std::move(*statement));
    }
}

OpenCL::Parser::ParserResult<Initializer>
    OpenCL::Parser::parseInitializer(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context)
{
    auto start = begin;
    if (begin >= end || begin->getTokenType() != Lexer::TokenType::OpenBrace)
    {
        auto assignment = parseAssignmentExpression(begin, end, context);
        if (!assignment)
        {
            return assignment;
        }
        return Initializer(start, begin, std::move(*assignment));
    }
    else
    {
        begin++;
        auto initializerList = parseInitializerList(begin, end, context);
        if (initializerList.isError())
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::Comma
                       || token.getTokenType() == Lexer::TokenType::CloseBrace;
            });
        }
        if (begin == end
            || (begin->getTokenType() != Lexer::TokenType::CloseBrace
                && begin->getTokenType() != Lexer::TokenType::Comma))
        {
            context.logError({"Expected } after initializer list"}, std::vector<OpenCL::Lexer::Token>::const_iterator(),
                             std::optional<Modifier>(), std::vector<Message::Note>());
            return Error{};
        }
        if (begin->getTokenType() == Lexer::TokenType::Comma)
        {
            begin++;
        }
        if (begin == end || begin->getTokenType() != Lexer::TokenType::CloseBrace)
        {
            context.logError({"Expected } after initializer list"}, std::vector<OpenCL::Lexer::Token>::const_iterator(),
                             std::optional<Modifier>(), std::vector<Message::Note>());
            return Error{};
        }
        begin++;
        if (initializerList)
        {
            return Initializer{start, begin, std::move(*initializerList)};
        }
        return {};
    }
}

OpenCL::Parser::ParserResult<InitializerList> OpenCL::Parser::parseInitializerList(Tokens::const_iterator& begin,
                                                                                   Tokens::const_iterator end,
                                                                                   ParsingContext& context)
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
        else if (!expect(Lexer::TokenType::Comma, begin, end, context))
        {
            break;
        }

        std::vector<std::variant<ConstantExpression, std::string>> designation;
        while (begin < end
               && (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket
                   || begin->getTokenType() == Lexer::TokenType::Dot))
        {
            if (begin->getTokenType() == Lexer::TokenType::OpenSquareBracket)
            {
                auto openPpos = begin;
                begin++;
                auto constant = parseAssignmentExpression(begin, end, context);
                if (constant.isError())
                {
                    begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::CloseSquareBracket;
                    });
                }
                else if (constant)
                {
                    designation.emplace_back(std::move(*constant));
                }
                if (!expect(Lexer::TokenType::CloseSquareBracket, begin, end, context,
                            {{Notes::TO_MATCH_N_HERE.args("'['"), start, findSemicolonOrEOL(begin, end),
                              Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
                {
                    continue;
                }
            }
            else if (begin->getTokenType() == Lexer::TokenType::Dot)
            {
                begin++;
                std::string name;
                if (!expect(Lexer::TokenType::Identifier, begin, findSemicolonOrEOL(begin, end), context, {}, &name))
                {
                    continue;
                }
                designation.emplace_back(std::move(name));
            }
        }
        if (!designation.empty())
        {
            if (!expect(Lexer::TokenType::Assignment, begin, findSemicolonOrEOL(begin, end), context))
            {
                continue;
            }
        }
        auto initializer = parseInitializer(begin, end, context);
        if (!initializer)
        {
            continue;
        }
        vector.push_back({std::move(*initializer), std::move(designation)});
    } while (begin < end && begin->getTokenType() == Lexer::TokenType::Comma);

    return InitializerList{start, begin, std::move(vector)};
}

OpenCL::Parser::ParserResult<Statement>
    OpenCL::Parser::parseStatement(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context)
{
    auto start = begin;
    auto dslock = context.setDiagnosticStart(start);
    switch (begin->getTokenType())
    {
        case Lexer::TokenType::ReturnKeyword:
        {
            auto ret = parseReturnStatement(begin, end, context);
            if (!ret)
            {
                return ret;
            }
            return Statement(std::move(*ret));
        }
        case Lexer::TokenType::IfKeyword:
        {
            auto ifStat = parseIfStatement(begin, end, context);
            if (!ifStat)
            {
                return ifStat;
            }
            return Statement(std::move(*ifStat));
        }
        case Lexer::TokenType::SwitchKeyword:
        {
            auto switchStat = parseSwitchStatement(begin, end, context);
            if (!switchStat)
            {
                return switchStat;
            }
            return Statement(std::move(*switchStat));
        }
        case Lexer::TokenType::OpenBrace:
        {
            auto compoundStatement = parseCompoundStatement(begin, end, context);
            if (!compoundStatement)
            {
                return compoundStatement;
            }
            return Statement{std::move(*compoundStatement)};
        }
        case Lexer::TokenType::ForKeyword:
        {
            auto forStat = parseForStatement(begin, end, context);
            if (!forStat)
            {
                return forStat;
            }
            return Statement(std::move(*forStat));
        }
        case Lexer::TokenType::WhileKeyword:
        {
            auto headWhile = parseHeadWhileStatement(begin, end, context);
            if (!headWhile)
            {
                return headWhile;
            }
            return Statement(std::move(*headWhile));
        }
        case Lexer::TokenType::DoKeyword:
        {
            auto doWhile = parseFootWhileStatement(begin, end, context);
            if (!doWhile)
            {
                return doWhile;
            }
            return Statement(std::move(*doWhile));
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
                begin = std::find_if(
                    begin, end, [&context](const Lexer::Token& token) { return firstIsInStatement(token, context); });
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
                begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                    return token.getTokenType() == Lexer::TokenType::Colon;
                });
                if (begin == end)
                {
                    return {};
                }
            }
            if (!expect(Lexer::TokenType::Colon, begin, end, context))
            {
                begin = std::find_if(
                    begin, end, [&context](const Lexer::Token& token) { return firstIsInStatement(token, context); });
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
            return Statement(CaseStatement(start, begin, std::move(*expression),
                                           std::make_unique<Statement>(std::move(*statement))));
        }
        case Lexer::TokenType::GotoKeyword:
        {
            begin++;
            std::string name;
            if (!expect(Lexer::TokenType::Identifier, begin, end, context, {}, &name))
            {
                begin = std::find_if(begin, end, [](const Lexer::Token& token) {
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
                    begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                        return token.getTokenType() == Lexer::TokenType::SemiColon;
                    });
                    if (begin == end)
                    {
                        return {};
                    }
                }
                std::vector<Message::Note> notes;
                if (start + 1 == begin && start->getTokenType() == Lexer::TokenType::Identifier
                    && context.isTypedef(std::get<std::string>(start->getValue()))
                    && begin->getTokenType() == Lexer::TokenType::Identifier)
                {
                    auto* loc = context.getLocationOf(std::get<std::string>(start->getValue()));
                    if (loc)
                    {
                        notes.push_back(Message::Note{
                            Notes::TYPEDEF_OVERSHADOWED_BY_DECLARATION.args('\'' + start->emitBack() + '\''),
                            loc->begin, loc->end, Modifier(loc->identifier, loc->identifier + 1, Modifier::Underline)});
                    }
                }
                if (!expect(Lexer::TokenType::SemiColon, begin, findSemicolonOrEOL(begin, end), context,
                            std::move(notes))
                    || !expression)
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

OpenCL::Parser::ParserResult<HeadWhileStatement> OpenCL::Parser::parseHeadWhileStatement(
    std::vector<OpenCL::Lexer::Token, std::allocator<OpenCL::Lexer::Token>>::const_iterator& begin,
    std::vector<OpenCL::Lexer::Token, std::allocator<OpenCL::Lexer::Token>>::const_iterator end,
    OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    begin++;
    auto openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParenthese, begin, end, context))
    {
        return Error{};
    }
    auto expression = parseExpression(begin, end, context);
    if (expression.isError())
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseParenthese;
        });
    }
    if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context,
                {{{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                   Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}}))
    {
        return Error{};
    }
    auto statement = parseStatement(begin, end, context);
    if (!statement)
    {
        return statement;
    }
    if (!expression)
    {
        return {};
    }
    return HeadWhileStatement(start, begin, std::move(*expression), std::make_unique<Statement>(std::move(*statement)));
}

OpenCL::Parser::ParserResult<FootWhileStatement> OpenCL::Parser::parseFootWhileStatement(
    std::vector<OpenCL::Lexer::Token, std::allocator<OpenCL::Lexer::Token>>::const_iterator& begin,
    std::vector<OpenCL::Lexer::Token, std::allocator<OpenCL::Lexer::Token>>::const_iterator end,
    OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    auto doPos = begin;
    begin++;
    auto statement = parseStatement(begin, end, context);
    if (statement.isError())
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::WhileKeyword;
        });
    }
    if (!expect(Lexer::TokenType::WhileKeyword, begin, end, context,
                {{Notes::TO_MATCH_N_HERE.args("'do'"), start, findSemicolonOrEOL(begin, end),
                  Modifier(doPos, doPos + 1, Modifier::PointAtBeginning)}}))
    {
        return Error{};
    }
    auto openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParenthese, begin, end, context))
    {
        return {};
    }
    auto expression = parseExpression(begin, end, context);
    if (expression.isError())
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseParenthese;
        });
    }
    if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context,
                {{{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                   Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}}))
    {
        return Error{};
    }
    if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
    {
        return Error{};
    }

    if (!statement || !expression)
    {
        return {};
    }
    return FootWhileStatement(start, begin, std::make_unique<Statement>(std::move(*statement)), std::move(*expression));
}

OpenCL::Parser::ParserResult<ReturnStatement>
    OpenCL::Parser::parseReturnStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                         std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                         OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::ReturnKeyword, begin, end, context))
    {
        return Error{};
    }
    if (begin < end && begin->getTokenType() == Lexer::TokenType::SemiColon)
    {
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            return Error{};
        }
        return ReturnStatement(start, begin, nullptr);
    }
    else if (begin >= end || !firstIsInExpression(*begin, context))
    {
        expect(Lexer::TokenType::SemiColon, begin, end, context);
        return Error{};
    }
    auto expression = parseExpression(begin, end, context);
    if (expression.isError())
    {
        begin = std::find_if(
            begin, end, [](const Lexer::Token& token) { return token.getTokenType() == Lexer::TokenType::SemiColon; });
    }
    if (!expect(Lexer::TokenType::SemiColon, begin, end, context) || !expression)
    {
        return Error{};
    }
    if (!expression)
    {
        return {};
    }
    return ReturnStatement(start, begin, std::make_unique<Expression>(std::move(*expression)));
}

OpenCL::Parser::ParserResult<IfStatement>
    OpenCL::Parser::parseIfStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                     std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                     OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::IfKeyword, begin, end, context))
    {
        return Error{};
    }
    auto openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParenthese, begin, end, context))
    {
        return Error{};
    }
    auto expression = parseExpression(begin, end, context);
    if (expression.isError())
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseParenthese;
        });
    }
    if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context,
                {{{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                   Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}}))
    {
        return Error{};
    }
    auto statement = parseStatement(begin, end, context);
    if (!statement && (begin >= end || begin->getTokenType() != Lexer::TokenType::ElseKeyword))
    {
        return statement;
    }

    if (begin < end && begin->getTokenType() == Lexer::TokenType::ElseKeyword)
    {
        begin++;
        auto elseStatement = parseStatement(begin, end, context);
        if (!elseStatement)
        {
            return elseStatement;
        }
        if (!expression || !statement)
        {
            return {};
        }
        return IfStatement(start, begin, std::move(*expression), std::make_unique<Statement>(std::move(*statement)),
                           std::make_unique<Statement>(std::move(*elseStatement)));
    }
    else if (expression && statement)
    {
        return IfStatement(start, begin, std::move(*expression), std::make_unique<Statement>(std::move(*statement)));
    }
    return {};
}

OpenCL::Parser::ParserResult<SwitchStatement>
    OpenCL::Parser::parseSwitchStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                         std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                         OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::SwitchKeyword, begin, end, context))
    {
        return Error{};
    }
    auto openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParenthese, begin, end, context))
    {
        return {};
    }
    auto expression = parseExpression(begin, end, context);
    if (expression.isError())
    {
        begin = std::find_if(begin, end, [](const Lexer::Token& token) {
            return token.getTokenType() == Lexer::TokenType::CloseParenthese;
        });
    }
    if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context,
                {{{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                   Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}}))
    {
        return Error{};
    }
    auto statement = parseStatement(begin, end, context);
    if (!statement)
    {
        return statement;
    }
    if (!expression)
    {
        return {};
    }
    return SwitchStatement(start, begin, std::move(*expression), std::make_unique<Statement>(std::move(*statement)));
}

OpenCL::Parser::ParserResult<ForStatement>
    OpenCL::Parser::parseForStatement(std::vector<OpenCL::Lexer::Token>::const_iterator& begin,
                                      std::vector<OpenCL::Lexer::Token>::const_iterator end,
                                      OpenCL::Parser::ParsingContext& context)
{
    auto start = begin;
    if (!expect(Lexer::TokenType::ForKeyword, begin, end, context))
    {
        return Error{};
    }
    auto openPpos = begin;
    if (!expect(Lexer::TokenType::OpenParenthese, begin, end, context))
    {
        return Error{};
    }
    if (begin >= end)
    {
        context.logError(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args(
                             Format::List(", ", " or ", "expression", "declaration"), "'('"),
                         begin, Modifier(begin - 1, begin, Modifier::PointAtEnd));
        return Error{};
    }

    std::variant<Declaration, std::unique_ptr<Expression>> initial{nullptr};
    if (firstIsInDeclaration(*begin, context))
    {
        auto decl = parseDeclaration(begin, end, context);
        if (decl.isError())
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
            });
        }
        else if (decl)
        {
            initial = std::move(*decl);
        }
    }
    else if (begin->getTokenType() != Lexer::TokenType::SemiColon)
    {
        auto exp = parseExpression(begin, end, context);
        if (exp.isError())
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
            });
        }
        else if (exp)
        {
            initial = std::make_unique<Expression>(std::move(*exp));
        }
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            return Error{};
        }
    }
    else
    {
        begin++;
    }

    std::unique_ptr<Expression> controlling;
    if (begin >= end)
    {
        context.logError(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("expression", "';'"), begin,
                         Modifier(begin - 1, begin, Modifier::PointAtEnd));
        return Error{};
    }
    else if (begin->getTokenType() != Lexer::TokenType::SemiColon)
    {
        auto exp = parseExpression(begin, end, context);
        if (exp.isError())
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::SemiColon;
            });
        }
        else if (exp)
        {
            controlling = std::make_unique<Expression>(std::move(*exp));
        }
        if (!expect(Lexer::TokenType::SemiColon, begin, end, context))
        {
            begin = std::find_if(begin, end, [&context](const Lexer::Token& token) {
                return firstIsInExpression(token, context) || token.getTokenType() == Lexer::TokenType::CloseParenthese;
            });
        }
    }
    else
    {
        begin++;
    }

    std::unique_ptr<Expression> post;
    if (begin >= end)
    {
        context.logError(ErrorMessages::Parser::EXPECTED_N_AFTER_N.args("expression", "';'"), begin,
                         Modifier(begin - 1, begin, Modifier::PointAtEnd));
        return Error{};
    }
    else if (begin->getTokenType() != Lexer::TokenType::CloseParenthese)
    {
        auto exp = parseExpression(begin, end, context);
        if (exp.isError())
        {
            begin = std::find_if(begin, end, [](const Lexer::Token& token) {
                return token.getTokenType() == Lexer::TokenType::CloseParenthese;
            });
        }
        else if (exp)
        {
            post = std::make_unique<Expression>(std::move(*exp));
        }
        if (!expect(Lexer::TokenType::CloseParenthese, begin, end, context,
                    {{Notes::TO_MATCH_N_HERE.args("'('"), start, findSemicolonOrEOL(begin, end),
                      Modifier(openPpos, openPpos + 1, Modifier::PointAtBeginning)}}))
        {
            return Error{};
        }
    }
    else
    {
        begin++;
    }

    auto stat = parseStatement(begin, end, context);
    if (!stat)
    {
        return stat;
    }
    return ForStatement(start, begin, std::make_unique<Statement>(std::move(*stat)), std::move(initial),
                        std::move(controlling), std::move(post));
}
