#include "SemanticAnalysis.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>
#include <array>
#include <optional>
#include <utility>

#include "ConstantEvaluator.hpp"
#include "ErrorMessages.hpp"

void OpenCL::Semantics::SemanticAnalysis::logError(const OpenCL::Message& message)
{
    if (m_reporter)
    {
        (*m_reporter) << message;
    }
}

OpenCL::Semantics::TranslationUnit OpenCL::Semantics::SemanticAnalysis::visit(const Syntax::TranslationUnit& node)
{
    std::vector<TranslationUnit::Variant> globals;
    for (auto& iter : node.getGlobals())
    {
        auto result = match(
            iter,
            [this, &globals](const Syntax::FunctionDefinition& function) -> std::optional<TranslationUnit::Variant> {
                auto result = visit(function);
                if (result && result->hasPrototype())
                {
                    globals.emplace_back(Declaration(Type(false, false, "", result->getType()), result->getLinkage(),
                                                     Lifetime::Static, result->getName()));
                }
                return result ? std::optional<TranslationUnit::Variant>(*result) :
                                std::optional<TranslationUnit::Variant>{};
            },
            [this, &globals](const Syntax::Declaration& declaration) -> std::optional<TranslationUnit::Variant> {
                auto result = visit(declaration);
                globals.insert(globals.end(), result.begin(), result.end());
                return std::optional<TranslationUnit::Variant>{};
            });
        if (result)
        {
            globals.push_back(std::move(*result));
        }
    }
    return TranslationUnit(std::move(globals));
}

namespace
{
    template <class T, class InputIterator>
    bool declarationSpecifierHas(InputIterator&& begin, InputIterator&& end, const T& value)
    {
        return std::any_of(begin, end, [&value](const OpenCL::Syntax::DeclarationSpecifier& declarationSpecifier) {
            auto* t = std::get_if<T>(&declarationSpecifier);
            if (!t)
            {
                return false;
            }
            return *t == value;
        });
    }

    template <class T, class InputIterator, class Predicate>
    bool declarationSpecifierHasIf(InputIterator&& begin, InputIterator&& end, Predicate&& predicate)
    {
        return std::any_of(begin, end, [&predicate](const OpenCL::Syntax::DeclarationSpecifier& declarationSpecifier) {
            auto* t = std::get_if<T>(&declarationSpecifier);
            if (!t)
            {
                return false;
            }
            return predicate(*t);
        });
    }
} // namespace

std::optional<OpenCL::Semantics::FunctionDefinition>
    OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::FunctionDefinition& node)
{
    auto name = declaratorToName(node.getDeclarator());
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        if (auto* storage = std::get_if<Syntax::StorageClassSpecifier>(&iter))
        {
            if (storageClassSpecifier)
            {
                logError(
                    {ErrorMessages::Semantics::ONLY_ONE_STORAGE_SPECIFIER,
                     node.begin(),
                     node.getCompoundStatement().begin(),
                     Modifier(storage->begin(), storage->end(), Modifier::Underline),
                     {{Notes::PREVIOUS_STORAGE_SPECIFIER_HERE, node.begin(), node.getCompoundStatement().begin(),
                       Modifier(storageClassSpecifier->begin(), storageClassSpecifier->end(), Modifier::Underline)}}});
                continue;
            }
            if (storage->getSpecifier() != Syntax::StorageClassSpecifier::Static
                && storage->getSpecifier() != Syntax::StorageClassSpecifier::Extern)
            {
                logError({ErrorMessages::Semantics::ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION, node.begin(),
                          node.getCompoundStatement().begin(),
                          Modifier(storage->begin(), storage->end(), Modifier::Underline)});
                continue;
            }
            storageClassSpecifier = storage;
        }
    }
    auto type = declaratorsToType({node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end()},
                                  node.getDeclarator(), node.getDeclarations());
    if (!std::holds_alternative<Semantics::FunctionType>(type.get()))
    {
        logError({ErrorMessages::Semantics::EXPECTED_PARAMETER_LIST_IN_FUNCTION_DEFINITION, node.begin(),
                  node.getCompoundStatement().begin(),
                  Modifier(node.getDeclarator().begin(), node.getDeclarator().end(), Modifier::Underline)});
        return {};
    }
    const auto& functionRP = std::get<Semantics::FunctionType>(type.get());

    const Syntax::DirectDeclaratorParentheseParameters* paramterTypeList = matchWithSelf(
        node.getDeclarator().getDirectDeclarator(),
        [](auto&&, const Syntax::DirectDeclaratorParentheseParameters& directDeclaratorParentheseParameters)
            -> const Syntax::DirectDeclaratorParentheseParameters* { return &directDeclaratorParentheseParameters; },
        [](auto&& self, const Syntax::DirectDeclaratorParenthese& directDeclaratorParenthese)
            -> const Syntax::DirectDeclaratorParentheseParameters* {
            return std::visit([&self](auto&& value) { return self(value); },
                              directDeclaratorParenthese.getDeclarator().getDirectDeclarator());
        },
        [](auto&&, auto &&) -> const Syntax::DirectDeclaratorParentheseParameters* { return nullptr; });

    const Syntax::DirectDeclaratorParentheseIdentifiers* identifierList = matchWithSelf(
        node.getDeclarator().getDirectDeclarator(),
        [](auto&&, const Syntax::DirectDeclaratorParentheseIdentifiers& directDeclaratorParentheseIdentifiers)
            -> const Syntax::DirectDeclaratorParentheseIdentifiers* { return &directDeclaratorParentheseIdentifiers; },
        [](auto&& self, const Syntax::DirectDeclaratorParenthese& directDeclaratorParenthese)
            -> const Syntax::DirectDeclaratorParentheseIdentifiers* {
            return std::visit([&self](auto&& value) { return self(value); },
                              directDeclaratorParenthese.getDeclarator().getDirectDeclarator());
        },
        [](auto&&, auto &&) -> const Syntax::DirectDeclaratorParentheseIdentifiers* { return nullptr; });

    if (!identifierList && !node.getDeclarations().empty())
    {
        logError({ErrorMessages::Semantics::DECLARATIONS_ONLY_ALLOWED_WITH_IDENTIFIER_LIST, node.begin(),
                  node.getCompoundStatement().end(),
                  Modifier(node.getDeclarations().front().begin(), node.getDeclarations().back().end(),
                           Modifier::Underline)});
    }

    std::map<std::string, Semantics::Type> declarationMap;
    if (identifierList)
    {
        for (auto& iter : node.getDeclarations())
        {
            for (auto& pair : iter.getInitDeclarators())
            {
                auto result = declaratorsToType(
                    {iter.getDeclarationSpecifiers().begin(), iter.getDeclarationSpecifiers().end()}, *pair.first);
                declarationMap.emplace(Semantics::declaratorToName(*pair.first), result);
            }
        }
    }

    pushScope();
    std::vector<Declaration> declarations;
    for (std::size_t i = 0; i < functionRP.getArguments().size(); i++)
    {
        if (paramterTypeList)
        {
            auto* declarator = std::get_if<std::unique_ptr<Syntax::Declarator>>(
                &paramterTypeList->getParameterTypeList().getParameterList().getParameterDeclarations()[i].second);
            if (!declarator)
            {
                throw std::runtime_error("Internal compiler error");
            }
            // declarator cannot be null as otherwise it'd have failed in the parser
            auto argName = declaratorToName(**declarator);
            auto& specifiers =
                paramterTypeList->getParameterTypeList().getParameterList().getParameterDeclarations()[i].first;
            declarations.emplace_back(functionRP.getArguments()[i].first, Linkage::None,
                                      declarationSpecifierHasIf<Syntax::StorageClassSpecifier>(
                                          specifiers.begin(), specifiers.end(),
                                          [](const Syntax::StorageClassSpecifier& specifier) {
                                              return specifier.getSpecifier()
                                                     == Syntax::StorageClassSpecifier::Register;
                                          }) ?
                                          Lifetime::Register :
                                          Lifetime::Automatic,
                                      functionRP.getArguments()[i].second);
            if (auto [iter, inserted] =
                    m_declarations.back().emplace(declarations.back().getName(), declarations.back());
                !inserted)
            {
                // TODO: showing previous one
                auto begin = Syntax::nodeFromNodeDerivedVariant(paramterTypeList->getParameterTypeList()
                                                                    .getParameterList()
                                                                    .getParameterDeclarations()[i]
                                                                    .first.front())
                                 .begin();
                logError({ErrorMessages::REDEFINITION_OF_SYMBOL_N.args('\'' + declarations.back().getName() + '\''),
                          node.begin(), node.getCompoundStatement().begin(), Modifier(begin, (*declarator)->end())});
            }
        }
        else if (identifierList)
        {
            auto result = declarationMap.find(identifierList->getIdentifiers()[i].first);
            if (result == declarationMap.end())
            {
                declarations.emplace_back(functionRP.getArguments()[i].first, Linkage::None, Lifetime::Automatic,
                                          functionRP.getArguments()[i].second);
            }
            else
            {
                declarations.emplace_back(result->second, Linkage::None,
                                          declarationSpecifierHasIf<Syntax::StorageClassSpecifier>(
                                              node.getDeclarations()[i].getDeclarationSpecifiers().begin(),
                                              node.getDeclarations()[i].getDeclarationSpecifiers().end(),
                                              [](const Syntax::StorageClassSpecifier& specifier) {
                                                  return specifier.getSpecifier()
                                                         == Syntax::StorageClassSpecifier::Register;
                                              }) ?
                                              Lifetime::Register :
                                              Lifetime::Automatic,
                                          functionRP.getArguments()[i].second);
            }
            if (auto [iter, inserted] =
                    m_declarations.back().emplace(declarations.back().getName(), declarations.back());
                !inserted)
            {
                // TODO: Modifier
                std::vector<Lexer::Token>::const_iterator identifierLoc;
                auto decl = std::find_if(node.getDeclarations().begin(), node.getDeclarations().end(),
                                         [&declarations, &identifierLoc](const Syntax::Declaration& declaration) {
                                             return std::any_of(declaration.getInitDeclarators().begin(),
                                                                declaration.getInitDeclarators().end(),
                                                                [&declarations, &identifierLoc](const auto& pair) {
                                                                    if (Semantics::declaratorToName(*pair.first)
                                                                        == declarations.back().getName())
                                                                    {
                                                                        identifierLoc =
                                                                            Semantics::declaratorToLoc(*pair.first);
                                                                        return true;
                                                                    }
                                                                    return false;
                                                                });
                                         });
                logError({ErrorMessages::REDEFINITION_OF_SYMBOL_N.args('\'' + declarations.back().getName() + '\''),
                          node.begin(),
                          node.getCompoundStatement().begin(),
                          {},
                          {{Notes::PREVIOUSLY_DECLARED_HERE, decl->begin(), decl->end(),
                            Modifier(identifierLoc, identifierLoc + 1)}}});
            }
        }
        else
        {
            throw std::runtime_error("Internal compiler error");
        }
    }

    // TODO:
    //    auto result = visit(node.getCompoundStatement(),false);
    //    if(result)
    //    {
    //        return result;
    //    }

    popScope();

    return FunctionDefinition(
        functionRP, name, std::move(declarations),
        storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static ?
            Linkage::Internal :
            Linkage::External,
        CompoundStatement({}));
}

std::vector<OpenCL::Semantics::Declaration>
    OpenCL::Semantics::SemanticAnalysis::visit(const OpenCL::Syntax::Declaration& node)
{
    std::vector<OpenCL::Semantics::Declaration> decls;
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        if (auto* storage = std::get_if<Syntax::StorageClassSpecifier>(&iter); storage)
        {
            if (!storageClassSpecifier)
            {
                storageClassSpecifier = storage;
            }
            else
            {
                logError(
                    {ErrorMessages::Semantics::ONLY_ONE_STORAGE_SPECIFIER,
                     node.begin(),
                     node.end(),
                     Modifier(storage->begin(), storage->end(), Modifier::Underline),
                     {{Notes::PREVIOUS_STORAGE_SPECIFIER_HERE, node.begin(), node.end(),
                       Modifier(storageClassSpecifier->begin(), storageClassSpecifier->end(), Modifier::Underline)}}});
            }
            if (m_declarations.size() == 1
                && (storage->getSpecifier() == Syntax::StorageClassSpecifier::Auto
                    || storage->getSpecifier() == Syntax::StorageClassSpecifier::Register))
            {
                logError({ErrorMessages::Semantics::DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO_OR_REGISTER, node.begin(),
                          node.end(), Modifier(storage->begin(), storage->end(), Modifier::Underline)});
            }
        }
    }
    if (node.getInitDeclarators().empty())
    {
        // TODO: Warning
        return decls;
    }
    for (auto& [declarator, initializer] : node.getInitDeclarators())
    {
        auto name = declaratorToName(*declarator);
        auto result = declaratorsToType(
            {node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end()}, *declarator);
        if (isVoid(result))
        {
            auto voidLoc = std::find_if(
                Syntax::nodeFromNodeDerivedVariant(node.getDeclarationSpecifiers().front()).begin(),
                Syntax::nodeFromNodeDerivedVariant(node.getDeclarationSpecifiers().back()).end(),
                [](const Lexer::Token& token) { return token.getTokenType() == Lexer::TokenType::VoidKeyword; });
            logError({ErrorMessages::Semantics::DECLARATION_CANNNOT_BE_VOID, node.begin(), node.end(),
                      Modifier(voidLoc, voidLoc + 1, Modifier::Underline)});
        }
        if (auto* functionType = std::get_if<FunctionType>(&result.get()))
        {
            if (declarationSpecifierHasIf<Syntax::StorageClassSpecifier>(
                    node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                    [](const Syntax::StorageClassSpecifier& storageClassSpecifier) {
                        return storageClassSpecifier.getSpecifier() != Syntax::StorageClassSpecifier::Static
                               && storageClassSpecifier.getSpecifier() != Syntax::StorageClassSpecifier::Extern;
                    }))
            {
                auto storageLoc =
                    std::find_if(node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
                                 [](const Syntax::DeclarationSpecifier& declarationSpecifier) {
                                     auto* storage = std::get_if<Syntax::StorageClassSpecifier>(&declarationSpecifier);
                                     if (!storage)
                                     {
                                         return false;
                                     }
                                     return storage->getSpecifier() != Syntax::StorageClassSpecifier::Static
                                            && storage->getSpecifier() != Syntax::StorageClassSpecifier::Extern;
                                 });
                logError({ErrorMessages::Semantics::ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DECLARATION, node.begin(),
                          node.end(),
                          Modifier(Syntax::nodeFromNodeDerivedVariant(*storageLoc).begin(),
                                   Syntax::nodeFromNodeDerivedVariant(*storageLoc).end())});
            }
            if (initializer)
            {
                logError({ErrorMessages::Semantics::FUNCTION_DECLARATION_NOT_ALLOWED_TO_HAVE_INITIALIZER, node.begin(),
                          node.end(), Modifier(initializer->begin(), initializer->end())});
            }
            if (m_declarations.size() > 1 && storageClassSpecifier
                && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static)
            {
                logError({ErrorMessages::Semantics::STATIC_ONLY_ALLOWED_FOR_FUNCTION_DECLARATION_AT_FILE_SCOPE,
                          node.begin(), node.end(),
                          Modifier(storageClassSpecifier->begin(), storageClassSpecifier->end())});
            }
            if (!functionType->hasPrototype() && !functionType->getArguments().empty())
            {
                // TODO: Recursively walk down direct declarators to find identifier list
                logError({ErrorMessages::Semantics::IDENTIFIER_LIST_NOT_ALLOWED_IN_FUNCTION_DECLARATION, node.begin(),
                          node.end()});
            }
            decls.emplace_back(
                Declaration(std::move(result),
                            storageClassSpecifier
                                    && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static ?
                                Linkage::Internal :
                                Linkage::External,
                            Lifetime::Static, std::move(name)));
            m_declarations.back().emplace(decls.back().getName(), decls.back());
        }
        else
        {
            Linkage linkage = Linkage::None;
            Lifetime lifetime = m_declarations.size() > 1 ? Lifetime::Automatic : Lifetime::Static;
            if (storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static)
            {
                if (m_declarations.size() > 1)
                {
                    lifetime = Lifetime::Static;
                }
                else
                {
                    linkage = Linkage::Internal;
                }
            }
            else if (storageClassSpecifier
                     && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Extern)
            {
                linkage = Linkage::External;
            }
            else if (storageClassSpecifier
                     && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Register)
            {
                lifetime = Lifetime::Register;
            }
            auto declaration = Declaration(std::move(result), linkage, lifetime, name);
            if (auto [prev, success] = m_declarations.back().emplace(name, declaration);
                !success
                && (std::get<Declaration>(prev->second).getLinkage() == Linkage::None || linkage == Linkage::None))
            {
                auto declLoc = declaratorToLoc(*declarator);
                // TODO: Note to show previous declaration
                logError({ErrorMessages::REDEFINITION_OF_SYMBOL_N.args(name), node.begin(), node.end(),
                          Modifier(declLoc, declLoc + 1)});
            }
            decls.emplace_back(std::move(declaration));
        }
    }
    return decls;
}

OpenCL::Semantics::Type OpenCL::Semantics::SemanticAnalysis::primitivesToType(
    std::vector<OpenCL::Lexer::Token>::const_iterator declStart,
    std::vector<OpenCL::Lexer::Token>::const_iterator declEnd,
    const std::vector<OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier>& primitives, bool isConst, bool isVolatile)
{
    enum
    {
        Void = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Void),
        Char = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Char),
        Short = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Short),
        Int = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Int),
        Long = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Long),
        Float = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Float),
        Double = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Double),
        Signed = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Signed),
        Unsigned = static_cast<std::size_t>(OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Unsigned),
    };
    auto logMoreThanN = [&](OpenCL::Lexer::TokenType tokenType, const char* amountString) -> void {
        auto result = std::find_if(
            std::find_if(declStart, declEnd,
                         [tokenType](const OpenCL::Lexer::Token& token) { return token.getTokenType() == tokenType; })
                + 1,
            declEnd, [tokenType](const OpenCL::Lexer::Token& token) { return token.getTokenType() == tokenType; });
        logError({OpenCL::ErrorMessages::Semantics::N_APPEARING_MORE_THAN_N.args(OpenCL::Lexer::tokenName(tokenType),
                                                                                 amountString),
                  declStart, declEnd, OpenCL::Modifier(result, result + 1, OpenCL::Modifier::Underline)});
    };

    std::array<std::size_t, 9> primitivesCount = {0};
    for (auto& iter : primitives)
    {
        primitivesCount[static_cast<std::size_t>(iter)]++;
        if (primitivesCount[Void] > 1)
        {
            logMoreThanN(OpenCL::Lexer::TokenType::VoidKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Char] > 1)
        {
            logMoreThanN(OpenCL::Lexer::TokenType::CharKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Short] > 1)
        {
            logMoreThanN(OpenCL::Lexer::TokenType::ShortKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Int] > 1)
        {
            logMoreThanN(OpenCL::Lexer::TokenType::IntKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Float] > 1)
        {
            logMoreThanN(OpenCL::Lexer::TokenType::FloatKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Double] > 1)
        {
            logMoreThanN(OpenCL::Lexer::TokenType::DoubleKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Signed] > 1)
        {
            logMoreThanN(OpenCL::Lexer::TokenType::SignedKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Unsigned] > 1)
        {
            logMoreThanN(OpenCL::Lexer::TokenType::UnsignedKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Long] > 2)
        {
            logMoreThanN(OpenCL::Lexer::TokenType::LongKeyword, "twice");
            return Type{};
        }
    }

    bool hasSigned = primitivesCount[Signed];
    bool hasUnsigned = primitivesCount[Unsigned];
    if (hasSigned && hasUnsigned)
    {
        auto result =
            std::find_if(std::find_if(declStart, declEnd,
                                      [](const OpenCL::Lexer::Token& token) {
                                          return token.getTokenType() == OpenCL::Lexer::TokenType::SignedKeyword
                                                 || token.getTokenType() == OpenCL::Lexer::TokenType::UnsignedKeyword;
                                      })
                             + 1,
                         declEnd, [](const OpenCL::Lexer::Token& token) {
                             return token.getTokenType() == OpenCL::Lexer::TokenType::SignedKeyword
                                    || token.getTokenType() == OpenCL::Lexer::TokenType::UnsignedKeyword;
                         });
        logError({OpenCL::ErrorMessages::Semantics::CANNOT_COMBINE_N_WITH_N.args("'unsigned'", "'signed'"), declStart,
                  declEnd, OpenCL::Modifier(result, result + 1, OpenCL::Modifier::Underline)});
        return Type{};
    }

    auto firstPrimitiveNotOf = [&](const std::vector<OpenCL::Lexer::TokenType>& tokenTypes) {
        return std::find_if(declStart, declEnd, [&tokenTypes](const OpenCL::Lexer::Token& token) {
            using namespace OpenCL::Lexer;
            return std::none_of(
                       tokenTypes.begin(), tokenTypes.end(),
                       [&token](OpenCL::Lexer::TokenType tokenType) { return token.getTokenType() == tokenType; })
                   && (token.getTokenType() == TokenType::CharKeyword || token.getTokenType() == TokenType::VoidKeyword
                       || token.getTokenType() == TokenType::ShortKeyword
                       || token.getTokenType() == TokenType::IntKeyword
                       || token.getTokenType() == TokenType::LongKeyword
                       || token.getTokenType() == TokenType::FloatKeyword
                       || token.getTokenType() == TokenType::DoubleKeyword
                       || token.getTokenType() == TokenType::SignedKeyword
                       || token.getTokenType() == TokenType::UnsignedKeyword);
        });
    };

    if (primitivesCount[Void])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i++ == Void)
                {
                    return false;
                }
                return count;
            }))
        {
            auto result = firstPrimitiveNotOf({OpenCL::Lexer::TokenType::VoidKeyword});
            logError({OpenCL::ErrorMessages::Semantics::CANNOT_COMBINE_N_WITH_N.args("'void'", " any other primitives"),
                      declStart, declEnd, OpenCL::Modifier(result, result + 1, OpenCL::Modifier::Underline)});
        }
        return OpenCL::Semantics::PrimitiveType::createVoid(isConst, isVolatile);
    }
    if (primitivesCount[Float])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i++ == Float)
                {
                    return false;
                }
                return count;
            }))
        {
            auto result = firstPrimitiveNotOf({OpenCL::Lexer::TokenType::FloatKeyword});
            logError(
                {OpenCL::ErrorMessages::Semantics::CANNOT_COMBINE_N_WITH_N.args("'float'", " any other primitives"),
                 declStart, declEnd, OpenCL::Modifier(result, result + 1, OpenCL::Modifier::Underline)});
        }
        return OpenCL::Semantics::PrimitiveType::createFloat(isConst, isVolatile);
    }
    if (primitivesCount[Double])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i++ == Double)
                {
                    return false;
                }
                return count;
            }))
        {
            auto result = firstPrimitiveNotOf({OpenCL::Lexer::TokenType::DoubleKeyword});
            logError(
                {OpenCL::ErrorMessages::Semantics::CANNOT_COMBINE_N_WITH_N.args("'double'", " any other primitives"),
                 declStart, declEnd, OpenCL::Modifier(result, result + 1, OpenCL::Modifier::Underline)});
        }
        return OpenCL::Semantics::PrimitiveType::createDouble(isConst, isVolatile);
    }
    if (primitivesCount[Char])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Char || i == Signed || i == Unsigned)
                {
                    i++;
                    return false;
                }
                i++;
                return count;
            }))
        {
            auto result =
                firstPrimitiveNotOf({OpenCL::Lexer::TokenType::CharKeyword, OpenCL::Lexer::TokenType::SignedKeyword,
                                     OpenCL::Lexer::TokenType::UnsignedKeyword});
            logError({OpenCL::ErrorMessages::Semantics::CANNOT_COMBINE_N_WITH_N.args(
                          "'char'", " any other primitives but signed and unsigned"),
                      declStart, declEnd, OpenCL::Modifier(result, result + 1, OpenCL::Modifier::Underline)});
        }
        if (hasUnsigned)
        {
            return OpenCL::Semantics::PrimitiveType::createUnsignedChar(isConst, isVolatile);
        }
        else
        {
            return OpenCL::Semantics::PrimitiveType::createChar(isConst, isVolatile);
        }
    }
    if (primitivesCount[Short])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Short || i == Signed || i == Unsigned || i == Int)
                {
                    i++;
                    return false;
                }
                i++;
                return count;
            }))
        {
            auto result =
                firstPrimitiveNotOf({OpenCL::Lexer::TokenType::ShortKeyword, OpenCL::Lexer::TokenType::SignedKeyword,
                                     OpenCL::Lexer::TokenType::UnsignedKeyword});
            logError({OpenCL::ErrorMessages::Semantics::CANNOT_COMBINE_N_WITH_N.args(
                          "'short'", " any other primitives but signed and unsigned"),
                      declStart, declEnd, OpenCL::Modifier(result, result + 1, OpenCL::Modifier::Underline)});
        }
        if (hasUnsigned)
        {
            return OpenCL::Semantics::PrimitiveType::createUnsignedShort(isConst, isVolatile);
        }
        else
        {
            return OpenCL::Semantics::PrimitiveType::createShort(isConst, isVolatile);
        }
    }
    if (primitivesCount[Long] == 1)
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Signed || i == Unsigned || i == Int || i == Long)
                {
                    i++;
                    return false;
                }
                i++;
                return count;
            }))
        {
            auto result = firstPrimitiveNotOf(
                {OpenCL::Lexer::TokenType::LongKeyword, OpenCL::Lexer::TokenType::IntKeyword,
                 OpenCL::Lexer::TokenType::SignedKeyword, OpenCL::Lexer::TokenType::UnsignedKeyword});
            logError({OpenCL::ErrorMessages::Semantics::CANNOT_COMBINE_N_WITH_N.args(
                          "'long'", " any other primitives but signed, unsigned, long and int"),
                      declStart, declEnd, OpenCL::Modifier(result, result + 1, OpenCL::Modifier::Underline)});
        }
        if (hasUnsigned)
        {
            return OpenCL::Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile);
        }
        else
        {
            return OpenCL::Semantics::PrimitiveType::createInt(isConst, isVolatile);
        }
    }
    if (primitivesCount[Long] == 2)
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Signed || i == Unsigned || i == Int || i == Long)
                {
                    i++;
                    return false;
                }
                i++;
                return count;
            }))
        {
            auto result = firstPrimitiveNotOf(
                {OpenCL::Lexer::TokenType::LongKeyword, OpenCL::Lexer::TokenType::IntKeyword,
                 OpenCL::Lexer::TokenType::SignedKeyword, OpenCL::Lexer::TokenType::UnsignedKeyword});
            logError({OpenCL::ErrorMessages::Semantics::CANNOT_COMBINE_N_WITH_N.args(
                          "'long'", " any other primitives but signed, unsigned, long and int"),
                      declStart, declEnd, OpenCL::Modifier(result, result + 1, OpenCL::Modifier::Underline)});
        }
        if (hasUnsigned)
        {
            return OpenCL::Semantics::PrimitiveType::createUnsignedLongLong(isConst, isVolatile);
        }
        else
        {
            return OpenCL::Semantics::PrimitiveType::createLongLong(isConst, isVolatile);
        }
    }
    if (primitivesCount[Int])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Signed || i == Unsigned || i == Int)
                {
                    i++;
                    return false;
                }
                i++;
                return count;
            }))
        {
            auto result =
                firstPrimitiveNotOf({OpenCL::Lexer::TokenType::ShortKeyword, OpenCL::Lexer::TokenType::SignedKeyword,
                                     OpenCL::Lexer::TokenType::UnsignedKeyword});
            logError({OpenCL::ErrorMessages::Semantics::CANNOT_COMBINE_N_WITH_N.args(
                          "'int'", " any other primitives but signed and unsigned"),
                      declStart, declEnd, OpenCL::Modifier(result, result + 1, OpenCL::Modifier::Underline)});
        }
        if (hasUnsigned)
        {
            return OpenCL::Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile);
        }
        else
        {
            return OpenCL::Semantics::PrimitiveType::createInt(isConst, isVolatile);
        }
    }
    if (hasSigned)
    {
        return OpenCL::Semantics::PrimitiveType::createInt(isConst, isVolatile);
    }
    else if (hasUnsigned)
    {
        return OpenCL::Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile);
    }
    throw std::runtime_error("Internal compiler error in " + std::string(__func__) + ":" + std::to_string(__LINE__));
}

OpenCL::Semantics::Type OpenCL::Semantics::SemanticAnalysis::declaratorsToType(
    const std::vector<DeclarationOrSpecifierQualifier>& declarationOrSpecifierQualifiers,
    OpenCL::Semantics::PossiblyAbstractQualifierRef declarator, const std::vector<Syntax::Declaration>& declarations)
{
    auto declStart = Syntax::nodeFromNodeDerivedVariant(declarationOrSpecifierQualifiers.front()).begin();
    auto declEnd = match(
        declarator,
        [&declarations](const Syntax::Declarator& realDeclarator) {
            return declarations.empty() ? realDeclarator.end() : declarations.back().end();
        },
        [&declarationOrSpecifierQualifiers, &declarations](const Syntax::AbstractDeclarator* abstractDeclarator) {
            if (declarations.empty())
            {
                return abstractDeclarator ?
                           abstractDeclarator->end() :
                           Syntax::nodeFromNodeDerivedVariant(declarationOrSpecifierQualifiers.back()).end();
            }
            else
            {
                return declarations.back().end();
            }
        });

    bool isConst = false;
    bool isVolatile = false;
    bool isRestricted = false;
    for (auto& iter : declarationOrSpecifierQualifiers)
    {
        match(iter, [&](auto&& value) {
            if (auto* typeQualifier = std::get_if<Syntax::TypeQualifier>(&value.get()))
            {
                switch (typeQualifier->getQualifier())
                {
                    case Syntax::TypeQualifier::Const:
                    {
                        isConst = true;
                        break;
                    }
                    case Syntax::TypeQualifier::Restrict:
                    {
                        if (!isRestricted)
                        {
                            isRestricted = true;
                            auto result = std::find_if(
                                declarationOrSpecifierQualifiers.begin(), declarationOrSpecifierQualifiers.end(),
                                [](const DeclarationOrSpecifierQualifier& declarationOrSpecifierQualifier) {
                                    return match(declarationOrSpecifierQualifier, [](auto&& value) {
                                        auto* typeQualifier = std::get_if<Syntax::TypeQualifier>(&value.get());
                                        if (!typeQualifier)
                                        {
                                            return false;
                                        }
                                        return typeQualifier->getQualifier() == Syntax::TypeQualifier::Restrict;
                                    });
                                });
                            logError(
                                {ErrorMessages::Semantics::ONLY_POINTERS_CAN_BE_RESTRICTED, declStart, declEnd,
                                 Modifier(Syntax::nodeFromNodeDerivedVariant(*result).begin(),
                                          Syntax::nodeFromNodeDerivedVariant(*result).end(), Modifier::Underline)});
                        }
                        break;
                    }
                    case Syntax::TypeQualifier::Volatile:
                    {
                        isVolatile = true;
                        break;
                    }
                }
            }
        });
    }

    std::vector<const Syntax::TypeSpecifier*> typeSpecifiers;
    std::transform(declarationOrSpecifierQualifiers.begin(), declarationOrSpecifierQualifiers.end(),
                   std::back_inserter(typeSpecifiers), [](auto&& value) -> const Syntax::TypeSpecifier* {
                       return match(value,
                                    [](auto&& value2) { return std::get_if<Syntax::TypeSpecifier>(&value2.get()); });
                   });
    typeSpecifiers.erase(std::remove(typeSpecifiers.begin(), typeSpecifiers.end(), nullptr), typeSpecifiers.end());

    Type baseType;
    if (typeSpecifiers.empty())
    {
        logError({ErrorMessages::Semantics::AT_LEAST_ONE_TYPE_SPECIFIER_REQUIRED, declStart, declEnd,
                  Modifier(declStart, Syntax::nodeFromNodeDerivedVariant(declarationOrSpecifierQualifiers.back()).end(),
                           Modifier::Underline)});
    }
    else
    {
        baseType = typeSpecifiersToType(declStart, declEnd, typeSpecifiers, isConst, isVolatile);
    }

    return apply(declStart, declEnd, declarator, std::move(baseType), declarations);
}

OpenCL::Semantics::Type OpenCL::Semantics::SemanticAnalysis::typeSpecifiersToType(
    std::vector<OpenCL::Lexer::Token>::const_iterator declStart,
    std::vector<OpenCL::Lexer::Token>::const_iterator declEnd,
    const std::vector<const OpenCL::Syntax::TypeSpecifier*>& typeSpecifiers, bool isConst, bool isVolatile)
{
    return match(
        typeSpecifiers[0]->getVariant(),
        [&](Syntax::TypeSpecifier::PrimitiveTypeSpecifier) -> Type {
            if (auto result =
                    std::find_if(typeSpecifiers.begin(), typeSpecifiers.end(),
                                 [](const auto pointer) {
                                     return !std::holds_alternative<Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(
                                         pointer->getVariant());
                                 });
                result != typeSpecifiers.end())
            {
                logError({ErrorMessages::Semantics::EXPECTED_ONLY_PRIMITIVES.args(
                              '\'' + typeSpecifiers[0]->begin()->emitBack() + '\''),
                          declStart, declEnd, Modifier((*result)->begin(), (*result)->end(), Modifier::Underline)});
            }
            std::vector<Syntax::TypeSpecifier::PrimitiveTypeSpecifier> primitiveTypeSpecifier;
            for (auto& iter : typeSpecifiers)
            {
                if (auto* primitive = std::get_if<Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(&iter->getVariant()))
                {
                    primitiveTypeSpecifier.push_back(*primitive);
                }
            }
            return primitivesToType(declStart, declEnd, primitiveTypeSpecifier, isConst, isVolatile);
        },
        [&](const std::unique_ptr<Syntax::StructOrUnionSpecifier>& structOrUnion) -> Type {
            if (typeSpecifiers.size() > 1)
            {
                logError({ErrorMessages::Semantics::EXPECTED_NO_FURTHER_N_AFTER_N.args(
                              "type specifiers", (structOrUnion->isUnion() ? "union specifier" : "struct specifier")),
                          declStart, declEnd,
                          Modifier(typeSpecifiers[1]->begin(), typeSpecifiers[1]->end(), Modifier::Underline)});
            }
            if (structOrUnion->getStructDeclarations().empty())
            {
                return RecordType::create(isConst, isVolatile, structOrUnion->isUnion(),
                                          structOrUnion->getIdentifier());
            }
            std::vector<std::tuple<Type, std::string, std::int64_t>> members;
            for (auto& [structSpecifiers, structDecls] : structOrUnion->getStructDeclarations())
            {
                for (auto& iter : structDecls)
                {
                    auto type = declaratorsToType(
                        {structSpecifiers.begin(), structSpecifiers.end()}, [&]() -> PossiblyAbstractQualifierRef {
                            return iter.first ? PossiblyAbstractQualifierRef{*iter.first} : nullptr;
                        }());
                    std::string name = iter.first ? declaratorToName(*iter.first) : "";
                    if (!iter.second)
                    {
                        members.emplace_back(std::move(type), std::move(name), 0);
                    }
                    else
                    {
                        auto evaluator = makeEvaluator(iter.second->begin(), iter.second->end());
                        auto result = evaluator.visit(*iter.second);
                        if (result.isUndefined())
                        {
                            members.emplace_back(std::move(type), std::move(name), 0);
                        }
                        else if (!result.isInteger())
                        {
                            logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                                      iter.second->begin(), iter.second->end(),
                                      Modifier(iter.second->begin(), iter.second->end())});
                            members.emplace_back(std::move(type), std::move(name), 0);
                        }
                        else
                        {
                            members.emplace_back(std::move(type), std::move(name), result.to<std::size_t>());
                        }
                    }
                }
            }
            return RecordType::create(isConst, isVolatile, structOrUnion->isUnion(), structOrUnion->getIdentifier(),
                                      std::move(members));
        },
        [&](const std::unique_ptr<Syntax::EnumSpecifier>& enumSpecifier) -> Type {
            if (typeSpecifiers.size() > 1)
            {
                logError({ErrorMessages::Semantics::EXPECTED_NO_FURTHER_N_AFTER_N.args("type specifiers", "enum"),
                          declStart, declEnd,
                          Modifier(typeSpecifiers[1]->begin(), typeSpecifiers[1]->end(), Modifier::Underline)});
            }
            std::vector<std::pair<std::string, std::int32_t>> values;
            auto declName = match(
                enumSpecifier->getVariant(), [](const std::string& name) { return name; },
                [&values, this](const Syntax::EnumDeclaration& declaration) {
                    for (auto& [name, expression] : declaration.getValues())
                    {
                        if (!expression)
                        {
                            values.emplace_back(name, values.empty() ? 0 : values.back().second + 1);
                        }
                        else
                        {
                            auto evaluator = makeEvaluator(expression->begin(), expression->end());
                            auto value = evaluator.visit(*expression);
                            if (value.isUndefined())
                            {
                                continue;
                            }
                            else if (!value.isInteger())
                            {
                                logError(
                                    {ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                                     expression->begin(), expression->end(),
                                     Modifier(expression->begin(), expression->end())});
                            }
                            else
                            {
                                values.emplace_back(name, value.to<std::int32_t>());
                            }
                        }
                    }
                    return declaration.getName();
                });
            return EnumType::create(isConst, isVolatile, declName, std::move(values));
        },
        [&](const std::string& typedefName) -> Type {
            auto* result = getTypedef(typedefName);
            if (!result)
            {
                throw std::runtime_error("Internal compiler error: could not find typedef in semantics");
            }
            Type copy = *result;
            copy.setName(typedefName);
            return copy;
        });
}

bool OpenCL::Semantics::SemanticAnalysis::isTypedef(const std::string& name) const
{
    for (auto iter = m_declarations.rbegin(); iter != m_declarations.rend(); iter++)
    {
        if (auto result = iter->find(name); result != iter->end() && std::holds_alternative<Type>(result->second))
        {
            return true;
        }
    }
    return false;
}

bool OpenCL::Semantics::SemanticAnalysis::isTypedefInScope(const std::string& name) const
{
    for (auto iter = m_declarations.rbegin(); iter != m_declarations.rend(); iter++)
    {
        if (auto result = iter->find(name); result != iter->end())
        {
            return std::holds_alternative<Type>(result->second);
        }
    }
    return false;
}

const OpenCL::Semantics::Type* OpenCL::Semantics::SemanticAnalysis::getTypedef(const std::string& name) const
{
    for (auto iter = m_declarations.rbegin(); iter != m_declarations.rend(); iter++)
    {
        if (auto result = iter->find(name); result != iter->end())
        {
            return std::get_if<Type>(&result->second);
        }
    }
    return nullptr;
}

OpenCL::Semantics::Type OpenCL::Semantics::SemanticAnalysis::apply(std::vector<Lexer::Token>::const_iterator declStart,
                                                                   std::vector<Lexer::Token>::const_iterator declEnd,
                                                                   PossiblyAbstractQualifierRef declarator,
                                                                   Type&& baseType,
                                                                   const std::vector<Syntax::Declaration>& declarations)
{
    return match(
        declarator,
        [&](const Syntax::AbstractDeclarator* abstractDeclarator) -> Type {
            if (!abstractDeclarator)
            {
                return baseType;
            }
            for (auto& iter : abstractDeclarator->getPointers())
            {
                auto [isConst, isVolatile, isRestricted] = getQualifiers(iter.getTypeQualifiers());
                baseType = PointerType::create(isConst, isVolatile, isRestricted, std::move(baseType));
            }
            return apply(declStart, declEnd, abstractDeclarator->getDirectAbstractDeclarator(), std::move(baseType),
                         declarations);
        },
        [&](std::reference_wrapper<const Syntax::Declarator> declarator) -> Type {
            for (auto& iter : declarator.get().getPointers())
            {
                auto [isConst, isVolatile, isRestricted] = getQualifiers(iter.getTypeQualifiers());
                baseType = PointerType::create(isConst, isVolatile, isRestricted, std::move(baseType));
            }
            return apply(declStart, declEnd, declarator.get().getDirectDeclarator(), std::move(baseType), declarations);
        });
}

std::vector<std::pair<OpenCL::Semantics::Type, std::string>>
    OpenCL::Semantics::SemanticAnalysis::parameterListToArguments(
        std::vector<OpenCL::Lexer::Token>::const_iterator declStart,
        std::vector<OpenCL::Lexer::Token>::const_iterator declEnd,
        const std::vector<OpenCL::Syntax::ParameterDeclaration>& parameterDeclarations,
        const std::vector<OpenCL::Syntax::Declaration>& declarations)
{
    std::vector<std::pair<OpenCL::Semantics::Type, std::string>> arguments;
    for (auto& pair : parameterDeclarations)
    {
        for (auto& iter : pair.first)
        {
            OpenCL::match(
                iter, [](auto&&) {},
                [this, declStart, declEnd](const OpenCL::Syntax::StorageClassSpecifier& storageClassSpecifier) {
                    if (storageClassSpecifier.getSpecifier() != OpenCL::Syntax::StorageClassSpecifier::Register)
                    {
                        logError({OpenCL::ErrorMessages::Semantics::ONLY_REGISTER_ALLOWED_IN_FUNCTION_ARGUMENT,
                                  declStart, declEnd,
                                  OpenCL::Modifier(storageClassSpecifier.begin(), storageClassSpecifier.end(),
                                                   OpenCL::Modifier::Underline)});
                    }
                },
                [this, declStart, declEnd](const OpenCL::Syntax::FunctionSpecifier& functionSpecifier) {
                    logError({OpenCL::ErrorMessages::Semantics::INLINE_ONLY_ALLOWED_IN_FRONT_OF_FUNCTION, declStart,
                              declEnd,
                              OpenCL::Modifier(functionSpecifier.begin(), functionSpecifier.end(),
                                               OpenCL::Modifier::Underline)});
                });
        }
        auto result = declaratorsToType(
            {pair.first.begin(), pair.first.end()},
            OpenCL::match(
                pair.second,
                [](const std::unique_ptr<OpenCL::Syntax::Declarator>& directDeclarator)
                    -> OpenCL::Semantics::PossiblyAbstractQualifierRef { return std::cref(*directDeclarator); },
                [](const std::unique_ptr<OpenCL::Syntax::AbstractDeclarator>& abstractDeclarator)
                    -> OpenCL::Semantics::PossiblyAbstractQualifierRef { return abstractDeclarator.get(); }),
            declarations);
        if (parameterDeclarations.size() == 1 && result == OpenCL::Semantics::PrimitiveType::createVoid(false, false)
            && !std::visit([](const auto& uniquePtr) -> bool { return uniquePtr.get(); }, pair.second))
        {
            break;
        }
        if (isVoid(result))
        {
            auto voidBegin = std::find_if(
                Syntax::nodeFromNodeDerivedVariant(pair.first.front()).begin(),
                Syntax::nodeFromNodeDerivedVariant(pair.first.back()).end(),
                [](const Lexer::Token& token) { return token.getTokenType() == Lexer::TokenType::VoidKeyword; });
            logError({OpenCL::ErrorMessages::Semantics::FUNCTION_PARAMETER_CANNOT_BE_VOID, declStart, declEnd,
                      OpenCL::Modifier(voidBegin, voidBegin + 1, OpenCL::Modifier::Underline)});
        }
        else
        {
            arguments.emplace_back(std::move(result),
                                   std::holds_alternative<std::unique_ptr<OpenCL::Syntax::Declarator>>(pair.second) ?
                                       OpenCL::Semantics::declaratorToName(
                                           *std::get<std::unique_ptr<OpenCL::Syntax::Declarator>>(pair.second)) :
                                       "");
        }
    }
    return arguments;
}
OpenCL::Semantics::ConstantEvaluator
    OpenCL::Semantics::SemanticAnalysis::makeEvaluator(std::vector<OpenCL::Lexer::Token>::const_iterator exprBegin,
                                                       std::vector<OpenCL::Lexer::Token>::const_iterator exprEnd)
{
    return Semantics::ConstantEvaluator(
        exprBegin, exprEnd,
        [this](const Syntax::TypeName& typeName) -> Type {
            return declaratorsToType(
                {typeName.getSpecifierQualifiers().begin(), typeName.getSpecifierQualifiers().end()},
                typeName.getAbstractDeclarator());
        },
        [this](const std::string& name) -> const DeclarationTypedefEnums* {
            for (auto iter = m_declarations.rbegin(); iter != m_declarations.rend(); iter++)
            {
                if (auto result = iter->find(name); result != iter->end())
                {
                    return &result->second;
                }
            }
            return nullptr;
        },
        [this](const Message& message) { logError(message); });
}

OpenCL::Semantics::Type
    OpenCL::Semantics::SemanticAnalysis::apply(std::vector<OpenCL::Lexer::Token>::const_iterator declStart,
                                               std::vector<OpenCL::Lexer::Token>::const_iterator declEnd,
                                               const OpenCL::Syntax::DirectAbstractDeclarator& abstractDeclarator,
                                               OpenCL::Semantics::Type&& baseType,
                                               const std::vector<Syntax::Declaration>& declarations)
{
    matchWithSelf(
        abstractDeclarator,
        [&](auto&&, const Syntax::DirectAbstractDeclaratorParenthese& abstractDeclarator) -> void {
            // Might need to watch out for 6.7.5.3.11 not sure yet
            baseType = apply(declStart, declEnd, &abstractDeclarator.getAbstractDeclarator(), std::move(baseType),
                             declarations);
        },
        [&](auto&& directSelf,
            const Syntax::DirectAbstractDeclaratorAssignmentExpression& directAbstractDeclaratorAssignmentExpression)
            -> void {
            auto expression = directAbstractDeclaratorAssignmentExpression.getAssignmentExpression();
            if (!expression)
            {
                baseType = AbstractArrayType::create(false, false, false, std::move(baseType));
            }
            else
            {
                bool success = true;
                auto evaluator = makeEvaluator(expression->begin(), expression->end());
                auto size = evaluator.visit(*expression);
                if (!size.isUndefined() && success)
                {
                    if (!size.isInteger())
                    {
                        logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                                  directAbstractDeclaratorAssignmentExpression.begin(),
                                  directAbstractDeclaratorAssignmentExpression.end(),
                                  Modifier(expression->begin(), expression->end())});
                        baseType = ArrayType::create(false, false, false, std::move(baseType), -1);
                    }
                    else
                    {
                        baseType = ArrayType::create(false, false, false, std::move(baseType), size.to<std::size_t>());
                    }
                }
                else
                {
                    baseType = ValArrayType::create(false, false, false, std::move(baseType));
                }
            }

            if (directAbstractDeclaratorAssignmentExpression.getDirectAbstractDeclarator())
            {
                std::visit([&directSelf](auto&& value) -> void { directSelf(value); },
                           *directAbstractDeclaratorAssignmentExpression.getDirectAbstractDeclarator());
            }
        },
        [&](auto&& directSelf, const Syntax::DirectAbstractDeclaratorAsterisk& directAbstractDeclarator) -> void {
            baseType = ValArrayType::create(false, false, false, std::move(baseType));
            if (directAbstractDeclarator.getDirectAbstractDeclarator())
            {
                std::visit([&directSelf](auto&& value) { directSelf(value); },
                           *directAbstractDeclarator.getDirectAbstractDeclarator());
            }
        },
        [&](auto&& directSelf, const Syntax::DirectAbstractDeclaratorParameterTypeList& parameterTypeList) -> void {
            std::vector<std::pair<Type, std::string>> arguments;
            if (parameterTypeList.getParameterTypeList())
            {
                auto argumentsResult = parameterListToArguments(
                    declStart, declEnd,
                    parameterTypeList.getParameterTypeList()->getParameterList().getParameterDeclarations(),
                    declarations);
                arguments = std::move(argumentsResult);
            }
            baseType = FunctionType::create(std::move(baseType), std::move(arguments),
                                            !parameterTypeList.getParameterTypeList()
                                                || parameterTypeList.getParameterTypeList()->hasEllipse(),
                                            true);
            if (parameterTypeList.getDirectAbstractDeclarator())
            {
                std::visit([&directSelf](auto&& value) -> void { directSelf(value); },
                           *parameterTypeList.getDirectAbstractDeclarator());
            }
        });
    return std::move(baseType);
}

OpenCL::Semantics::Type OpenCL::Semantics::SemanticAnalysis::apply(
    std::vector<OpenCL::Lexer::Token>::const_iterator declStart,
    std::vector<OpenCL::Lexer::Token>::const_iterator declEnd, const OpenCL::Syntax::DirectDeclarator& directDeclarator,
    OpenCL::Semantics::Type&& baseType, const std::vector<Syntax::Declaration>& declarations)
{
    matchWithSelf(
        directDeclarator, [&](auto&&, const Syntax::DirectDeclaratorIdentifier&) -> void {},
        [&](auto&&, const Syntax::DirectDeclaratorParenthese& declarator) -> void {
            // Might need to watch out
            // for 6.7.5.3.11 not sure yet
            baseType =
                apply(declStart, declEnd, std::cref(declarator.getDeclarator()), std::move(baseType), declarations);
        },
        [&](auto&& directSelf, const Syntax::DirectDeclaratorNoStaticOrAsterisk& dirWithoutStaticOrAsterisk) -> void {
            auto [isConst, isVolatile, isRestricted] = getQualifiers(dirWithoutStaticOrAsterisk.getTypeQualifiers());
            auto& expression = dirWithoutStaticOrAsterisk.getAssignmentExpression();
            if (!expression)
            {
                baseType = AbstractArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType));
            }
            else
            {
                bool success = true;
                auto evaluator = makeEvaluator(expression->begin(), expression->end());
                auto size = evaluator.visit(*expression);
                if (!size.isUndefined() && success)
                {
                    if (!size.isInteger())
                    {
                        logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                                  dirWithoutStaticOrAsterisk.begin(), dirWithoutStaticOrAsterisk.end(),
                                  Modifier(expression->begin(), expression->end())});
                        baseType = ArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType), -1);
                    }
                    else
                    {
                        baseType = ArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType),
                                                     size.to<std::size_t>());
                    }
                }
                else
                {
                    baseType = ValArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType));
                }
            }
            std::visit([&directSelf](auto&& value) -> void { directSelf(value); },
                       dirWithoutStaticOrAsterisk.getDirectDeclarator());
        },
        [&](auto&& directSelf, const Syntax::DirectDeclaratorStatic& directDeclaratorStatic) -> void {
            auto [isConst, isVolatile, isRestricted] = getQualifiers(directDeclaratorStatic.getTypeQualifiers());
            bool success = true;
            auto evaluator = makeEvaluator(directDeclaratorStatic.getAssignmentExpression().begin(),
                                           directDeclaratorStatic.getAssignmentExpression().end());
            auto size = evaluator.visit(directDeclaratorStatic.getAssignmentExpression());
            if (!size.isUndefined() && success)
            {
                if (!size.isInteger())
                {
                    logError({ErrorMessages::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                              directDeclaratorStatic.begin(), directDeclaratorStatic.end(),
                              Modifier(directDeclaratorStatic.getAssignmentExpression().begin(),
                                       directDeclaratorStatic.getAssignmentExpression().end())});
                    baseType = ArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType), -1);
                }
                else
                {
                    baseType = ArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType),
                                                 size.to<std::size_t>());
                }
            }
            else
            {
                baseType = ValArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType));
            }
            std::visit([&directSelf](auto&& value) -> void { directSelf(value); },
                       directDeclaratorStatic.getDirectDeclarator());
        },
        [&](auto&& directSelf, const Syntax::DirectDeclaratorAsterisk& directDeclaratorAsterisk) -> void {
            auto [isConst, isVolatile, isRestricted] = getQualifiers(directDeclaratorAsterisk.getTypeQualifiers());
            baseType = ValArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType));
            std::visit([&directSelf](auto&& value) -> void { directSelf(value); },
                       directDeclaratorAsterisk.getDirectDeclarator());
        },
        [&](auto&& directSelf, const Syntax::DirectDeclaratorParentheseParameters& parentheseParameters) -> void {
            auto& parameterDeclarations =
                parentheseParameters.getParameterTypeList().getParameterList().getParameterDeclarations();
            auto argumentResult = parameterListToArguments(declStart, declEnd, parameterDeclarations, declarations);
            baseType = FunctionType::create(std::move(baseType), std::move(argumentResult),
                                            parentheseParameters.getParameterTypeList().hasEllipse(), true);
            std::visit([&directSelf](auto&& value) -> void { directSelf(value); },
                       parentheseParameters.getDirectDeclarator());
        },
        [&](auto&& directSelf, const Syntax::DirectDeclaratorParentheseIdentifiers& identifiers) -> void {
            std::vector<std::pair<Type, std::string>> arguments(
                identifiers.getIdentifiers().size(), {PrimitiveType::create(false, false, false, true, 32), ""});
            using Iterator = std::vector<Lexer::Token>::const_iterator;
            struct TypeBinding
            {
                Type type;
                Iterator begin;
                Iterator end;
            };
            std::map<std::string, TypeBinding> declarationMap;
            for (auto& iter : declarations)
            {
                auto thisBegin = Syntax::nodeFromNodeDerivedVariant(iter.getDeclarationSpecifiers()[0]).begin();
                for (auto& specifiers : iter.getDeclarationSpecifiers())
                {
                    match(
                        specifiers, [](auto&&) {},
                        [this, declStart, declEnd](const Syntax::StorageClassSpecifier& storageClassSpecifier) {
                            if (storageClassSpecifier.getSpecifier() != Syntax::StorageClassSpecifier::Register)
                            {
                                logError({OpenCL::ErrorMessages::Semantics::ONLY_REGISTER_ALLOWED_IN_FUNCTION_ARGUMENT,
                                          declStart, declEnd,
                                          OpenCL::Modifier(storageClassSpecifier.begin(), storageClassSpecifier.end(),
                                                           OpenCL::Modifier::Underline)});
                            }
                        },
                        [this, declStart, declEnd](const Syntax::FunctionSpecifier& functionSpecifier) {
                            logError({OpenCL::ErrorMessages::Semantics::INLINE_ONLY_ALLOWED_IN_FRONT_OF_FUNCTION,
                                      declStart, declEnd,
                                      OpenCL::Modifier(functionSpecifier.begin(), functionSpecifier.end(),
                                                       OpenCL::Modifier::Underline)});
                        });
                }
                for (auto& pair : iter.getInitDeclarators())
                {
                    if (pair.second)
                    {
                        logError(
                            {OpenCL::ErrorMessages::Semantics::PARAMETER_IN_FUNCTION_NOT_ALLOWED_TO_HAVE_INITIALIZER,
                             declStart, declEnd,
                             Modifier(pair.second->begin(), pair.second->end(), Modifier::Underline)});
                    }
                    auto name = Semantics::declaratorToName(*pair.first);
                    auto result = declaratorsToType(
                        {iter.getDeclarationSpecifiers().begin(), iter.getDeclarationSpecifiers().end()}, *pair.first,
                        {});
                    declarationMap.insert({name, {std::move(result), thisBegin, pair.first->end()}});
                }
            }

            for (std::size_t i = 0; i < identifiers.getIdentifiers().size(); i++)
            {
                auto result = declarationMap.find(identifiers.getIdentifiers()[i].first);
                if (result == declarationMap.end())
                {
                    continue;
                }
                if (auto* primitive = std::get_if<PrimitiveType>(&result->second.type.get()))
                {
                    if (primitive->isFloatingPoint())
                    {
                        arguments[i] = {PrimitiveType::createDouble(result->second.type.isConst(),
                                                                    result->second.type.isVolatile()),
                                        result->first};
                    }
                    else if (primitive->getBitCount() == 0)
                    {
                        logError({ErrorMessages::Semantics::DECLARATION_CANNNOT_BE_VOID, declStart, declEnd,
                                  Modifier(result->second.begin, result->second.end, Modifier::Underline)});
                    }
                    else if (primitive->getBitCount() < 32)
                    {
                        arguments[i] = {
                            PrimitiveType::createInt(result->second.type.isConst(), result->second.type.isVolatile()),
                            result->first};
                    }
                    else
                    {
                        arguments[i] = {result->second.type, result->first};
                    }
                }
                else
                {
                    arguments[i] = {result->second.type, result->first};
                }
                declarationMap.erase(result);
            }

            for (auto& [name, binding] : declarationMap)
            {
                logError({ErrorMessages::Semantics::N_APPEARING_IN_N_BUT_NOT_IN_N.args(
                              '\'' + name + '\'', "declarations", "identifier list"),
                          declStart, declEnd, Modifier(binding.begin, binding.end, Modifier::Underline)});
            }
            baseType = FunctionType::create(std::move(baseType), std::move(arguments), false, false);
            std::visit(directSelf, identifiers.getDirectDeclarator());
        });
    return std::move(baseType);
}
