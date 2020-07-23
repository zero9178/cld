#include "SemanticAnalysis.hpp"

#include <Frontend/Compiler/SourceObject.hpp>
#include <Frontend/Common/Text.hpp>

#include <algorithm>
#include <array>
#include <numeric>
#include <optional>
#include <unordered_map>
#include <utility>

#include "ConstantEvaluator.hpp"
#include "ErrorMessages.hpp"
#include "SemanticUtil.hpp"

void cld::Semantics::SemanticAnalysis::log(const Message& message)
{
    if (m_reporter)
    {
        *m_reporter << message;
    }
}

cld::Semantics::TranslationUnit cld::Semantics::SemanticAnalysis::visit(const Syntax::TranslationUnit& node)
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
    return std::any_of(begin, end, [&value](const cld::Syntax::DeclarationSpecifier& declarationSpecifier) {
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
    return std::any_of(begin, end, [&predicate](const cld::Syntax::DeclarationSpecifier& declarationSpecifier) {
        auto* t = std::get_if<T>(&declarationSpecifier);
        if (!t)
        {
            return false;
        }
        return predicate(*t);
    });
}
} // namespace

std::optional<cld::Semantics::FunctionDefinition>
    cld::Semantics::SemanticAnalysis::visit(const cld::Syntax::FunctionDefinition& node)
{
    auto name = declaratorToName(node.getDeclarator());
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        if (auto* storage = std::get_if<Syntax::StorageClassSpecifier>(&iter))
        {
            if (storageClassSpecifier)
            {
                log(Errors::Semantics::ONLY_ONE_STORAGE_SPECIFIER.args(*storage, m_sourceObject, *storage));
                log(Notes::PREVIOUS_STORAGE_SPECIFIER_HERE.args(*storageClassSpecifier, m_sourceObject,
                                                                *storageClassSpecifier));
                continue;
            }
            if (storage->getSpecifier() != Syntax::StorageClassSpecifier::Static
                && storage->getSpecifier() != Syntax::StorageClassSpecifier::Extern)
            {
                log(Errors::Semantics::ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION.args(
                    *storage, m_sourceObject, *storage));
                continue;
            }
            storageClassSpecifier = storage;
        }
    }
    auto type = declaratorsToType({node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end()},
                                  node.getDeclarator(), node.getDeclarations());
    if (!std::holds_alternative<Semantics::FunctionType>(type.get()))
    {
        log(Errors::Semantics::EXPECTED_PARAMETER_LIST_IN_FUNCTION_DEFINITION.args(node.getDeclarator(), m_sourceObject,
                                                                                   node.getDeclarator()));
        return {};
    }
    const auto& functionRP = cld::get<Semantics::FunctionType>(type.get());

    auto* parameterTypeList = findRecursively<Syntax::DirectDeclaratorParentheseParameters>(
        node.getDeclarator().getDirectDeclarator(), [](auto&& value) -> const Syntax::DirectDeclarator* {
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

    auto* identifierList = findRecursively<Syntax::DirectDeclaratorParentheseIdentifiers>(
        node.getDeclarator().getDirectDeclarator(), [](auto&& value) -> const Syntax::DirectDeclarator* {
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

    if (!identifierList && !node.getDeclarations().empty())
    {
        log(Errors::Semantics::DECLARATIONS_ONLY_ALLOWED_WITH_IDENTIFIER_LIST.args(
            *node.getDeclarations().front().begin(), m_sourceObject,
            std::forward_as_tuple(*node.getDeclarations().front().begin(),
                                  *(node.getDeclarations().back().end() - 1))));
    }

    std::unordered_map<std::string, Semantics::Type> declarationMap;
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
        if (parameterTypeList)
        {
            auto* declarator = std::get_if<std::unique_ptr<Syntax::Declarator>>(
                &parameterTypeList->getParameterTypeList().getParameterList().getParameterDeclarations()[i].second);
            CLD_ASSERT(declarator);
            // declarator cannot be null as otherwise it'd have failed in the parser
            auto argName = declaratorToName(**declarator);
            auto& specifiers =
                parameterTypeList->getParameterTypeList().getParameterList().getParameterDeclarations()[i].first;
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
                const auto* begin = Syntax::nodeFromNodeDerivedVariant(parameterTypeList->getParameterTypeList()
                                                                           .getParameterList()
                                                                           .getParameterDeclarations()[i]
                                                                           .first.front())
                                        .begin();
                //                log(Errors::REDEFINITION_OF_SYMBOL_N.args(*begin, m_sourceObject,
                //                                                          std::forward_as_tuple(*begin,
                //                                                          *((*declarator)->end() - 1))));
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
                Lexer::CTokenIterator identifierLoc;
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

                // TODO:                log({Message::error(Errors::REDEFINITION_OF_SYMBOL_N.args('\'' +
                // declarations.back().getName() + '\''),
                //                                    node.begin(), node.getCompoundStatement().begin()),
                //                     Message::note(Notes::PREVIOUSLY_DECLARED_HERE, decl->begin(), decl->end(),
                //                                   {Underline(identifierLoc, identifierLoc + 1)})});
            }
        }
        else
        {
            CLD_UNREACHABLE;
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

std::vector<cld::Semantics::Declaration> cld::Semantics::SemanticAnalysis::visit(const cld::Syntax::Declaration& node)
{
    std::vector<cld::Semantics::Declaration> decls;
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
                // TODO:                log({Message::error(Errors::Semantics::ONLY_ONE_STORAGE_SPECIFIER, node.begin(),
                // node.end(),
                //                                    {Underline(storage->begin(), storage->end())}),
                //                     Message::note(Notes::PREVIOUS_STORAGE_SPECIFIER_HERE, node.begin(), node.end(),
                //                                   {Underline(storageClassSpecifier->begin(),
                //                                   storageClassSpecifier->end())})});
            }
            if (m_declarations.size() == 1
                && (storage->getSpecifier() == Syntax::StorageClassSpecifier::Auto
                    || storage->getSpecifier() == Syntax::StorageClassSpecifier::Register))
            {
                // TODO: log({Message::error(Errors::Semantics::DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO_OR_REGISTER,
                //                                    node.begin(), node.end(), {Underline(storage->begin(),
                //                                    storage->end())})});
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
            const auto* voidLoc = std::find_if(
                Syntax::nodeFromNodeDerivedVariant(node.getDeclarationSpecifiers().front()).begin(),
                Syntax::nodeFromNodeDerivedVariant(node.getDeclarationSpecifiers().back()).end(),
                [](const Lexer::CToken& token) { return token.getTokenType() == Lexer::TokenType::VoidKeyword; });
            // TODO:            log({Message::error(Errors::Semantics::DECLARATION_CANNNOT_BE_VOID, node.begin(),
            // node.end(),
            //                                {Underline(voidLoc, voidLoc + 1)})});
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
                // TODO: log({Message::error(Errors::Semantics::ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DECLARATION,
                //                                    node.begin(), node.end(),
                //                                    {Underline(Syntax::nodeFromNodeDerivedVariant(*storageLoc).begin(),
                //                                               Syntax::nodeFromNodeDerivedVariant(*storageLoc).end())})});
            }
            if (initializer)
            {
                // TODO: log({Message::error(Errors::Semantics::FUNCTION_DECLARATION_NOT_ALLOWED_TO_HAVE_INITIALIZER,
                //                                    node.begin(), node.end(), {Underline(initializer->begin(),
                //                                    initializer->end())})});
            }
            if (m_declarations.size() > 1 && storageClassSpecifier
                && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static)
            {
                // TODO:
                // log({Message::error(Errors::Semantics::STATIC_ONLY_ALLOWED_FOR_FUNCTION_DECLARATION_AT_FILE_SCOPE,
                //                                    node.begin(), node.end(),
                //                                    {Underline(storageClassSpecifier->begin(),
                //                                    storageClassSpecifier->end())})});
            }
            if (!functionType->hasPrototype() && !functionType->getArguments().empty())
            {
                // TODO: Recursively walk down direct declarators to find identifier list
                // TODO: log({Message::error(Errors::Semantics::IDENTIFIER_LIST_NOT_ALLOWED_IN_FUNCTION_DECLARATION,
                //                                    node.begin(), node.end())});
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
                && (cld::get<Declaration>(prev->second).getLinkage() == Linkage::None || linkage == Linkage::None))
            {
                auto declLoc = declaratorToLoc(*declarator);
                // TODO: Note to show previous declaration
                // TODO:                log({Message::error(Errors::REDEFINITION_OF_SYMBOL_N.args(name), node.begin(),
                // node.end(),
                //                                    {Underline(declLoc, declLoc + 1)})});
            }
            decls.emplace_back(std::move(declaration));
        }
    }
    return decls;
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::primitivesToType(
    Lexer::CTokenIterator declStart, Lexer::CTokenIterator declEnd,
    const std::vector<cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier>& primitives, bool isConst, bool isVolatile)
{
    enum
    {
        Void = static_cast<std::size_t>(cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Void),
        Char = static_cast<std::size_t>(cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Char),
        Short = static_cast<std::size_t>(cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Short),
        Int = static_cast<std::size_t>(cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Int),
        Long = static_cast<std::size_t>(cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Long),
        Float = static_cast<std::size_t>(cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Float),
        Double = static_cast<std::size_t>(cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Double),
        Signed = static_cast<std::size_t>(cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Signed),
        Unsigned = static_cast<std::size_t>(cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Unsigned),
        UnderlineBool = static_cast<std::size_t>(cld::Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Bool),
    };
    auto logMoreThanN = [&](cld::Lexer::TokenType tokenType, const char* amountString) -> void {
        const auto* result = std::find_if(
            std::find_if(declStart, declEnd,
                         [tokenType](const cld::Lexer::CToken& token) { return token.getTokenType() == tokenType; })
                + 1,
            declEnd, [tokenType](const cld::Lexer::CToken& token) { return token.getTokenType() == tokenType; });
        // TODO:        log({Message::error(
        //            cld::Errors::Semantics::N_APPEARING_MORE_THAN_N.args(cld::Lexer::tokenName(tokenType),
        //            amountString), declStart, declEnd, {Underline(result, result + 1)})});
    };

    std::array<std::size_t, 10> primitivesCount = {0};
    for (auto& iter : primitives)
    {
        primitivesCount[static_cast<std::size_t>(iter)]++;
        if (primitivesCount[Void] > 1)
        {
            logMoreThanN(cld::Lexer::TokenType::VoidKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Char] > 1)
        {
            logMoreThanN(cld::Lexer::TokenType::CharKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Short] > 1)
        {
            logMoreThanN(cld::Lexer::TokenType::ShortKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Int] > 1)
        {
            logMoreThanN(cld::Lexer::TokenType::IntKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Float] > 1)
        {
            logMoreThanN(cld::Lexer::TokenType::FloatKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Double] > 1)
        {
            logMoreThanN(cld::Lexer::TokenType::DoubleKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Signed] > 1)
        {
            logMoreThanN(cld::Lexer::TokenType::SignedKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Unsigned] > 1)
        {
            logMoreThanN(cld::Lexer::TokenType::UnsignedKeyword, "once");
            return Type{};
        }
        if (primitivesCount[Long] > 2)
        {
            logMoreThanN(cld::Lexer::TokenType::LongKeyword, "twice");
            return Type{};
        }
        if (primitivesCount[UnderlineBool] > 1)
        {
            logMoreThanN(cld::Lexer::TokenType::UnderlineBool, "once");
            return Type{};
        }
    }

    bool hasSigned = primitivesCount[Signed] == 1;
    bool hasUnsigned = primitivesCount[Unsigned] == 1;
    if (hasSigned && hasUnsigned)
    {
        auto result =
            std::find_if(std::find_if(declStart, declEnd,
                                      [](const cld::Lexer::CToken& token) {
                                          return token.getTokenType() == cld::Lexer::TokenType::SignedKeyword
                                                 || token.getTokenType() == cld::Lexer::TokenType::UnsignedKeyword;
                                      })
                             + 1,
                         declEnd, [](const cld::Lexer::CToken& token) {
                             return token.getTokenType() == cld::Lexer::TokenType::SignedKeyword
                                    || token.getTokenType() == cld::Lexer::TokenType::UnsignedKeyword;
                         });
        // TODO:        log({Message::error(cld::Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args("'unsigned'",
        // "'signed'"), declStart,
        //                            declEnd, {Underline(result, result + 1)})});
        return Type{};
    }

    auto firstPrimitiveNotOf = [&](const std::vector<cld::Lexer::TokenType>& tokenTypes) {
        return std::find_if(declStart, declEnd, [&tokenTypes](const cld::Lexer::CToken& token) {
            using namespace cld::Lexer;
            return std::none_of(tokenTypes.begin(), tokenTypes.end(),
                                [&token](cld::Lexer::TokenType tokenType) { return token.getTokenType() == tokenType; })
                   && (token.getTokenType() == TokenType::CharKeyword || token.getTokenType() == TokenType::VoidKeyword
                       || token.getTokenType() == TokenType::ShortKeyword
                       || token.getTokenType() == TokenType::IntKeyword
                       || token.getTokenType() == TokenType::LongKeyword
                       || token.getTokenType() == TokenType::FloatKeyword
                       || token.getTokenType() == TokenType::DoubleKeyword
                       || token.getTokenType() == TokenType::SignedKeyword
                       || token.getTokenType() == TokenType::UnsignedKeyword
                       || token.getTokenType() == TokenType::UnderlineBool);
        });
    };

    if (primitivesCount[Void])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i++ == Void)
                {
                    return count > 1;
                }
                return count;
            }))
        {
            auto result = firstPrimitiveNotOf({cld::Lexer::TokenType::VoidKeyword});
            // TODO:            log({Message::error(cld::Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args("'void'", " any
            // other primitives"),
            //                                declStart, declEnd, {Underline(result, result + 1)})});
        }
        return cld::Semantics::PrimitiveType::createVoid(isConst, isVolatile);
    }
    if (primitivesCount[Float])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i++ == Float)
                {
                    return count > 1;
                }
                return count;
            }))
        {
            auto result = firstPrimitiveNotOf({cld::Lexer::TokenType::FloatKeyword});
            // TODO:            log({Message::error(
            //                cld::Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args("'float'", " any other primitives"),
            //                declStart, declEnd, {Underline(result, result + 1)})});
        }
        return cld::Semantics::PrimitiveType::createFloat(isConst, isVolatile);
    }
    if (primitivesCount[Double])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Double || i == Long)
                {
                    i++;
                    return count > 1;
                }
                i++;
                return count;
            }))
        {
            auto result = firstPrimitiveNotOf({cld::Lexer::TokenType::DoubleKeyword});
            // TODO:            log({Message::error(
            //                cld::Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args("'double'", " any other primitives
            //                but long"), declStart, declEnd, {Underline(result, result + 1)})});
        }
        if (primitivesCount[Long])
        {
            return cld::Semantics::PrimitiveType::createLongDouble(isConst, isVolatile,
                                                                   m_sourceObject.getLanguageOptions());
        }
        return cld::Semantics::PrimitiveType::createDouble(isConst, isVolatile);
    }
    if (primitivesCount[Char])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Char || i == Signed || i == Unsigned)
                {
                    i++;
                    return count > 1;
                }
                i++;
                return count;
            }))
        {
            auto result = firstPrimitiveNotOf({cld::Lexer::TokenType::CharKeyword, cld::Lexer::TokenType::SignedKeyword,
                                               cld::Lexer::TokenType::UnsignedKeyword});
            // TODO:            log({Message::error(cld::Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args(
            //                                    "'char'", " any other primitives but signed and unsigned"),
            //                                declStart, declEnd, {Underline(result, result + 1)})});
        }
        if (hasUnsigned)
        {
            return cld::Semantics::PrimitiveType::createUnsignedChar(isConst, isVolatile);
        }
        else if (hasSigned)
        {
            return cld::Semantics::PrimitiveType::createSignedChar(isConst, isVolatile);
        }
        else
        {
            return cld::Semantics::PrimitiveType::createChar(isConst, isVolatile, m_sourceObject.getLanguageOptions());
        }
    }
    if (primitivesCount[Short])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Short || i == Signed || i == Unsigned || i == Int)
                {
                    i++;
                    return count > 1;
                }
                i++;
                return count;
            }))
        {
            auto result =
                firstPrimitiveNotOf({cld::Lexer::TokenType::ShortKeyword, cld::Lexer::TokenType::SignedKeyword,
                                     cld::Lexer::TokenType::UnsignedKeyword});
            // TODO:            log({Message::error(cld::Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args(
            //                                    "'short'", " any other primitives but signed and unsigned"),
            //                                declStart, declEnd, {Underline(result, result + 1)})});
        }
        if (hasUnsigned)
        {
            return cld::Semantics::PrimitiveType::createUnsignedShort(isConst, isVolatile,
                                                                      m_sourceObject.getLanguageOptions());
        }
        else
        {
            return cld::Semantics::PrimitiveType::createShort(isConst, isVolatile, m_sourceObject.getLanguageOptions());
        }
    }
    if (primitivesCount[Long] == 1)
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Signed || i == Unsigned || i == Int || i == Long)
                {
                    i++;
                    return count > 1;
                }
                i++;
                return count;
            }))
        {
            auto result =
                firstPrimitiveNotOf({cld::Lexer::TokenType::LongKeyword, cld::Lexer::TokenType::IntKeyword,
                                     cld::Lexer::TokenType::SignedKeyword, cld::Lexer::TokenType::UnsignedKeyword});
            // TODO:            log({Message::error(cld::Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args(
            //                                    "'long'", " any other primitives but signed, unsigned, long and int"),
            //                                declStart, declEnd, {Underline(result, result + 1)})});
        }
        if (hasUnsigned)
        {
            return cld::Semantics::PrimitiveType::createUnsignedLong(isConst, isVolatile,
                                                                     m_sourceObject.getLanguageOptions());
        }
        else
        {
            return cld::Semantics::PrimitiveType::createLong(isConst, isVolatile, m_sourceObject.getLanguageOptions());
        }
    }
    if (primitivesCount[Long] == 2)
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Signed || i == Unsigned || i == Int || i == Long)
                {
                    return count > (i++ == Long ? 2 : 1);
                }
                i++;
                return count;
            }))
        {
            auto result =
                firstPrimitiveNotOf({cld::Lexer::TokenType::LongKeyword, cld::Lexer::TokenType::IntKeyword,
                                     cld::Lexer::TokenType::SignedKeyword, cld::Lexer::TokenType::UnsignedKeyword});
            // TODO:            log({Message::error(cld::Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args(
            //                                    "'long'", " any other primitives but signed, unsigned, long and int"),
            //                                declStart, declEnd, {Underline(result, result + 1)})});
        }
        if (hasUnsigned)
        {
            return cld::Semantics::PrimitiveType::createUnsignedLongLong(isConst, isVolatile);
        }
        else
        {
            return cld::Semantics::PrimitiveType::createLongLong(isConst, isVolatile);
        }
    }
    if (primitivesCount[Int])
    {
        std::size_t i = 0;
        if (std::any_of(primitivesCount.begin(), primitivesCount.end(), [&i](std::size_t count) -> bool {
                if (i == Signed || i == Unsigned || i == Int)
                {
                    i++;
                    return count > 1;
                }
                i++;
                return count;
            }))
        {
            auto result =
                firstPrimitiveNotOf({cld::Lexer::TokenType::ShortKeyword, cld::Lexer::TokenType::SignedKeyword,
                                     cld::Lexer::TokenType::UnsignedKeyword});
            // TODO:            log({Message::error(cld::Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args(
            //                                    "'int'", " any other primitives but signed and unsigned"),
            //                                declStart, declEnd, {Underline(result, result + 1)})});
        }
        if (hasUnsigned)
        {
            return cld::Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile,
                                                                    m_sourceObject.getLanguageOptions());
        }
        else
        {
            return cld::Semantics::PrimitiveType::createInt(isConst, isVolatile, m_sourceObject.getLanguageOptions());
        }
    }
    if (hasSigned)
    {
        return cld::Semantics::PrimitiveType::createInt(isConst, isVolatile, m_sourceObject.getLanguageOptions());
    }
    else if (hasUnsigned)
    {
        return cld::Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile,
                                                                m_sourceObject.getLanguageOptions());
    }
    else
    {
        auto text =
            std::accumulate(primitives.begin(), primitives.end(), std::string(),
                            [](const std::string& result, Syntax::TypeSpecifier::PrimitiveTypeSpecifier specifier) {
                                switch (specifier)
                                {
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Void: return result + " void";
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Char: return result + " char";
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Short: return result + " short";
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Int: return result + " int";
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Long: return result + " long";
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Float: return result + " float";
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Double:
                                        return result + " double";
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Signed:
                                        return result + " signed";
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Unsigned:
                                        return result + " unsigned";
                                    case Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Bool: return result + " _Bool";
                                }
                                CLD_UNREACHABLE;
                            });
        // TODO:        log({Message::error(cld::Errors::Semantics::UNKNOWN_TYPE_N.args(text), declStart, declEnd,
        //                            {Underline(declStart, declEnd)})});
    }
    CLD_UNREACHABLE;
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::declaratorsToType(
    const std::vector<DeclarationOrSpecifierQualifier>& declarationOrSpecifierQualifiers,
    const cld::Semantics::PossiblyAbstractQualifierRef& declarator,
    const std::vector<Syntax::Declaration>& declarations)
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
                        if (isRestricted)
                        {
                            break;
                        }
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
                        // TODO: log({Message::error(Errors::Semantics::ONLY_POINTERS_CAN_BE_RESTRICTED, declStart,
                        // declEnd,
                        //                                            {Underline(Syntax::nodeFromNodeDerivedVariant(*result).begin(),
                        //                                                       Syntax::nodeFromNodeDerivedVariant(*result).end())})});
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
        // TODO:        log({Message::error(
        //            Errors::Semantics::AT_LEAST_ONE_TYPE_SPECIFIER_REQUIRED, declStart, declEnd,
        //            {Underline(declStart,
        //                       Syntax::nodeFromNodeDerivedVariant(declarationOrSpecifierQualifiers.back()).end())})});
    }
    else
    {
        baseType = typeSpecifiersToType(declStart, declEnd, typeSpecifiers, isConst, isVolatile);
    }

    return apply(declStart, declEnd, declarator, std::move(baseType), declarations);
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::typeSpecifiersToType(
    Lexer::CTokenIterator declStart, Lexer::CTokenIterator declEnd,
    const std::vector<const cld::Syntax::TypeSpecifier*>& typeSpecifiers, bool isConst, bool isVolatile)
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
                // TODO:                log({Message::error(
                //                    Errors::Semantics::EXPECTED_ONLY_PRIMITIVES.args(
                //                        '\'' +
                //                        to_string(typeSpecifiers[0]->begin()->getRepresentation(m_sourceObject)) +
                //                        '\''),
                //                    declStart, declEnd, {Underline((*result)->begin(), (*result)->end())})});
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
                // TODO:                log({Message::error(
                //                    Errors::Semantics::EXPECTED_NO_FURTHER_N_AFTER_N.args(
                //                        "type specifiers", (structOrUnion->isUnion() ? "union specifier" : "struct
                //                        specifier")),
                //                    declStart, declEnd, {Underline(typeSpecifiers[1]->begin(),
                //                    typeSpecifiers[1]->end())})});
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
                            // TODO:                            log({Message::error(
                            //                                Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                            //                                iter.second->begin(), iter.second->end(),
                            //                                {Underline(iter.second->begin(), iter.second->end())})});
                            members.emplace_back(std::move(type), std::move(name), 0);
                        }
                        else
                        {
                            members.emplace_back(std::move(type), std::move(name), result.toUInt());
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
                // TODO:                log({Message::error(Errors::Semantics::EXPECTED_NO_FURTHER_N_AFTER_N.args("type
                // specifiers", "enum"),
                //                                    declStart, declEnd,
                //                                    {Underline(typeSpecifiers[1]->begin(),
                //                                    typeSpecifiers[1]->end())})});
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
                                // TODO:                                log({Message::error(
                                //                                    Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                                //                                    expression->begin(), expression->end(),
                                //                                    {Underline(expression->begin(),
                                //                                    expression->end())})});
                            }
                            else
                            {
                                values.emplace_back(name, value.toInt());
                            }
                        }
                    }
                    return declaration.getName();
                });
            return EnumType::create(isConst, isVolatile, declName, std::move(values));
        },
        [&](const std::string& typedefName) -> Type {
            auto* result = getTypedef(typedefName);
            CLD_ASSERT(result);
            Type copy = *result;
            copy.setName(typedefName);
            return copy;
        });
}

bool cld::Semantics::SemanticAnalysis::isTypedef(const std::string& name) const
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

bool cld::Semantics::SemanticAnalysis::isTypedefInScope(const std::string& name) const
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

const cld::Semantics::Type* cld::Semantics::SemanticAnalysis::getTypedef(const std::string& name) const
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

cld::Semantics::Type cld::Semantics::SemanticAnalysis::apply(Lexer::CTokenIterator declStart,
                                                             Lexer::CTokenIterator declEnd,
                                                             PossiblyAbstractQualifierRef declarator, Type&& baseType,
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
            if (abstractDeclarator->getDirectAbstractDeclarator())
            {
                return apply(declStart, declEnd, *abstractDeclarator->getDirectAbstractDeclarator(),
                             std::move(baseType), declarations);
            }
            else
            {
                return baseType;
            }
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

std::vector<std::pair<cld::Semantics::Type, std::string>> cld::Semantics::SemanticAnalysis::parameterListToArguments(
    Lexer::CTokenIterator declStart, Lexer::CTokenIterator declEnd,
    const std::vector<cld::Syntax::ParameterDeclaration>& parameterDeclarations,
    const std::vector<cld::Syntax::Declaration>& declarations)
{
    std::vector<std::pair<cld::Semantics::Type, std::string>> arguments;
    for (auto& pair : parameterDeclarations)
    {
        for (auto& iter : pair.first)
        {
            cld::match(
                iter, [](auto&&) {},
                [this, declStart, declEnd](const cld::Syntax::StorageClassSpecifier& storageClassSpecifier) {
                    if (storageClassSpecifier.getSpecifier() != cld::Syntax::StorageClassSpecifier::Register)
                    {
                        // TODO: log({Message::error(cld::Errors::Semantics::ONLY_REGISTER_ALLOWED_IN_FUNCTION_ARGUMENT,
                        //                                            declStart, declEnd,
                        //                                            {Underline(storageClassSpecifier.begin(),
                        //                                            storageClassSpecifier.end())})});
                    }
                },
                [this, declStart, declEnd](const cld::Syntax::FunctionSpecifier& functionSpecifier) {
                    // TODO: log({Message::error(cld::Errors::Semantics::INLINE_ONLY_ALLOWED_IN_FRONT_OF_FUNCTION,
                    // declStart,
                    //                                        declEnd, {Underline(functionSpecifier.begin(),
                    //                                        functionSpecifier.end())})});
                });
        }
        auto result = declaratorsToType(
            {pair.first.begin(), pair.first.end()},
            cld::match(
                pair.second,
                [](const std::unique_ptr<cld::Syntax::Declarator>& directDeclarator)
                    -> cld::Semantics::PossiblyAbstractQualifierRef { return std::cref(*directDeclarator); },
                [](const std::unique_ptr<cld::Syntax::AbstractDeclarator>& abstractDeclarator)
                    -> cld::Semantics::PossiblyAbstractQualifierRef { return abstractDeclarator.get(); }),
            declarations);
        if (parameterDeclarations.size() == 1 && result == cld::Semantics::PrimitiveType::createVoid(false, false)
            && !cld::match(pair.second, [](const auto& uniquePtr) -> bool { return uniquePtr.get(); }))
        {
            break;
        }
        if (isVoid(result))
        {
            auto voidBegin = std::find_if(
                Syntax::nodeFromNodeDerivedVariant(pair.first.front()).begin(),
                Syntax::nodeFromNodeDerivedVariant(pair.first.back()).end(),
                [](const Lexer::CToken& token) { return token.getTokenType() == Lexer::TokenType::VoidKeyword; });
            // TODO:            log({Message::error(cld::Errors::Semantics::FUNCTION_PARAMETER_CANNOT_BE_VOID,
            // declStart, declEnd,
            //                                {Underline(voidBegin, voidBegin + 1)})});
        }
        else
        {
            arguments.emplace_back(
                std::move(result),
                std::holds_alternative<std::unique_ptr<cld::Syntax::Declarator>>(pair.second) ?
                    cld::Semantics::declaratorToName(*cld::get<std::unique_ptr<cld::Syntax::Declarator>>(pair.second)) :
                    "");
        }
    }
    return arguments;
}

cld::Semantics::ConstantEvaluator cld::Semantics::SemanticAnalysis::makeEvaluator(Lexer::CTokenIterator exprBegin,
                                                                                  Lexer::CTokenIterator exprEnd)
{
    return Semantics::ConstantEvaluator(
        m_sourceObject,
        [this](const Syntax::TypeName& typeName) -> Type {
            return declaratorsToType(
                {typeName.getSpecifierQualifiers().begin(), typeName.getSpecifierQualifiers().end()},
                typeName.getAbstractDeclarator());
        },
        [this](std::string_view name) -> const DeclarationTypedefEnums* {
            for (auto iter = m_declarations.rbegin(); iter != m_declarations.rend(); iter++)
            {
                if (auto result = iter->find(cld::to_string(name)); result != iter->end())
                {
                    return &result->second;
                }
            }
            return nullptr;
        },
        [this](const Message& message) { log({message}); });
}

cld::Semantics::Type
    cld::Semantics::SemanticAnalysis::apply(Lexer::CTokenIterator declStart, Lexer::CTokenIterator declEnd,
                                            const cld::Syntax::DirectAbstractDeclarator& abstractDeclarator,
                                            cld::Semantics::Type&& baseType,
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
                        // TODO:
                        // log({Message::error(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                        //                                            directAbstractDeclaratorAssignmentExpression.begin(),
                        //                                            directAbstractDeclaratorAssignmentExpression.end(),
                        //                                            {Underline(expression->begin(),
                        //                                            expression->end())})});
                        baseType = ArrayType::create(false, false, false, std::move(baseType), 0);
                    }
                    else
                    {
                        baseType = ArrayType::create(false, false, false, std::move(baseType), size.toUInt());
                    }
                }
                else
                {
                    baseType = ValArrayType::create(false, false, false, std::move(baseType));
                }
            }

            if (directAbstractDeclaratorAssignmentExpression.getDirectAbstractDeclarator())
            {
                cld::match(*directAbstractDeclaratorAssignmentExpression.getDirectAbstractDeclarator(),
                           [&directSelf](auto&& value) -> void { directSelf(value); });
            }
        },
        [&](auto&& directSelf, const Syntax::DirectAbstractDeclaratorAsterisk& directAbstractDeclarator) -> void {
            baseType = ValArrayType::create(false, false, false, std::move(baseType));
            if (directAbstractDeclarator.getDirectAbstractDeclarator())
            {
                cld::match(*directAbstractDeclarator.getDirectAbstractDeclarator(),
                           [&directSelf](auto&& value) { directSelf(value); });
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
                cld::match(*parameterTypeList.getDirectAbstractDeclarator(),
                           [&directSelf](auto&& value) -> void { directSelf(value); });
            }
        });
    return std::move(baseType);
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::apply(Lexer::CTokenIterator declStart,
                                                             Lexer::CTokenIterator declEnd,
                                                             const cld::Syntax::DirectDeclarator& directDeclarator,
                                                             cld::Semantics::Type&& baseType,
                                                             const std::vector<Syntax::Declaration>& declarations)
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
                        // TODO:
                        // log({Message::error(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                        //                                            dirWithoutStaticOrAsterisk.begin(),
                        //                                            dirWithoutStaticOrAsterisk.end(),
                        //                                            {Underline(expression->begin(),
                        //                                            expression->end())})});
                        baseType = ArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType), 0);
                    }
                    else
                    {
                        baseType =
                            ArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType), size.toUInt());
                    }
                }
                else
                {
                    baseType = ValArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType));
                }
            }
            cld::match(dirWithoutStaticOrAsterisk.getDirectDeclarator(),
                       [&directSelf](auto&& value) -> void { directSelf(value); });
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
                    // TODO:
                    // log({Message::error(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
                    //                                        directDeclaratorStatic.begin(),
                    //                                        directDeclaratorStatic.end(),
                    //                                        {Underline(directDeclaratorStatic.getAssignmentExpression().begin(),
                    //                                                   directDeclaratorStatic.getAssignmentExpression().end())})});
                    baseType = ArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType), 0);
                }
                else
                {
                    baseType = ArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType), size.toUInt());
                }
            }
            else
            {
                baseType = ValArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType));
            }
            cld::match(directDeclaratorStatic.getDirectDeclarator(),
                       [&directSelf](auto&& value) -> void { directSelf(value); });
        },
        [&](auto&& directSelf, const Syntax::DirectDeclaratorAsterisk& directDeclaratorAsterisk) -> void {
            auto [isConst, isVolatile, isRestricted] = getQualifiers(directDeclaratorAsterisk.getTypeQualifiers());
            baseType = ValArrayType::create(isConst, isVolatile, isRestricted, std::move(baseType));
            cld::match(directDeclaratorAsterisk.getDirectDeclarator(),
                       [&directSelf](auto&& value) -> void { directSelf(value); });
        },
        [&](auto&& directSelf, const Syntax::DirectDeclaratorParentheseParameters& parentheseParameters) -> void {
            auto& parameterDeclarations =
                parentheseParameters.getParameterTypeList().getParameterList().getParameterDeclarations();
            auto argumentResult = parameterListToArguments(declStart, declEnd, parameterDeclarations, declarations);
            baseType = FunctionType::create(std::move(baseType), std::move(argumentResult),
                                            parentheseParameters.getParameterTypeList().hasEllipse(), true);
            cld::match(parentheseParameters.getDirectDeclarator(),
                       [&directSelf](auto&& value) -> void { directSelf(value); });
        },
        [&](auto&& directSelf, const Syntax::DirectDeclaratorParentheseIdentifiers& identifiers) -> void {
            std::vector<std::pair<Type, std::string>> arguments(
                identifiers.getIdentifiers().size(),
                {PrimitiveType::createInt(false, false, m_sourceObject.getLanguageOptions()), ""});
            using Iterator = Lexer::CTokenIterator;
            struct TypeBinding
            {
                Type type;
                Iterator begin;
                Iterator end;
            };
            std::unordered_map<std::string, TypeBinding> declarationMap;
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
                                // TODO:                                log({Message::error(
                                //                                    cld::Errors::Semantics::ONLY_REGISTER_ALLOWED_IN_FUNCTION_ARGUMENT,
                                //                                    declStart, declEnd,
                                //                                    {Underline(storageClassSpecifier.begin(),
                                //                                    storageClassSpecifier.end())})});
                            }
                        },
                        [this, declStart, declEnd](const Syntax::FunctionSpecifier& functionSpecifier) {
                            // TODO:
                            // log({Message::error(cld::Errors::Semantics::INLINE_ONLY_ALLOWED_IN_FRONT_OF_FUNCTION,
                            //                                                declStart, declEnd,
                            //                                                {Underline(functionSpecifier.begin(),
                            //                                                functionSpecifier.end())})});
                        });
                }
                for (auto& pair : iter.getInitDeclarators())
                {
                    if (pair.second)
                    {
                        // TODO:                        log({Message::error(
                        //                            cld::Errors::Semantics::PARAMETER_IN_FUNCTION_NOT_ALLOWED_TO_HAVE_INITIALIZER,
                        //                            declStart, declEnd, {Underline(pair.second->begin(),
                        //                            pair.second->end())})});
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
                        // TODO: log({Message::error(Errors::Semantics::DECLARATION_CANNNOT_BE_VOID, declStart, declEnd,
                        //                                            {Underline(result->second.begin,
                        //                                            result->second.end)})});
                    }
                    else if (primitive->getBitCount() < 32)
                    {
                        arguments[i] = {PrimitiveType::createInt(result->second.type.isConst(),
                                                                 result->second.type.isVolatile(),
                                                                 m_sourceObject.getLanguageOptions()),
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
                // TODO:                log({Message::error(Errors::Semantics::N_APPEARING_IN_N_BUT_NOT_IN_N.args(
                //                                        '\'' + name + '\'', "declarations", "identifier list"),
                //                                    declStart, declEnd, {Underline(binding.begin, binding.end)})});
            }
            baseType = FunctionType::create(std::move(baseType), std::move(arguments), false, false);
            cld::match(identifiers.getDirectDeclarator(), directSelf);
        });
    return std::move(baseType);
}

std::tuple<bool, bool, bool>
    cld::Semantics::SemanticAnalysis::getQualifiers(const std::vector<Syntax::TypeQualifier>& typeQualifiers)
{
    bool isConst = false;
    bool isVolatile = false;
    bool isRestricted = false;
    for (auto& typeQual : typeQualifiers)
    {
        switch (typeQual.getQualifier())
        {
            case Syntax::TypeQualifier::Const: isConst = true; break;
            case Syntax::TypeQualifier::Restrict: isRestricted = true; break;
            case Syntax::TypeQualifier::Volatile: isVolatile = true; break;
            default: break;
        }
    }
    return std::tie(isConst, isVolatile, isRestricted);
}
