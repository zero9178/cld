#include "SemanticAnalysis.hpp"

#include <Frontend/Common/Text.hpp>
#include <Frontend/Compiler/SourceObject.hpp>

#include <algorithm>
#include <array>
#include <numeric>
#include <optional>
#include <unordered_map>
#include <utility>

#include <bitset2.hpp>

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
        auto result = cld::match(iter, [&](auto&& value) { return visit(value); });
        globals.insert(globals.end(), std::move_iterator(result.begin()), std::move_iterator(result.end()));
    }
    return TranslationUnit(std::move(globals));
}

std::vector<cld::Semantics::TranslationUnit::Variant>
    cld::Semantics::SemanticAnalysis::visit(const cld::Syntax::FunctionDefinition& node)
{
    //    std::vector<TranslationUnit::Variant> result;
    //    auto name = declaratorToName(node.getDeclarator());
    //    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
    //    for (auto& iter : node.getDeclarationSpecifiers())
    //    {
    //        if (auto* storage = std::get_if<Syntax::StorageClassSpecifier>(&iter))
    //        {
    //            if (storageClassSpecifier)
    //            {
    //                log(Errors::Semantics::ONLY_ONE_STORAGE_SPECIFIER.args(*storage, m_sourceInterface, *storage));
    //                log(Notes::PREVIOUS_STORAGE_SPECIFIER_HERE.args(*storageClassSpecifier, m_sourceInterface,
    //                                                                *storageClassSpecifier));
    //                continue;
    //            }
    //            if (storage->getSpecifier() != Syntax::StorageClassSpecifier::Static
    //                && storage->getSpecifier() != Syntax::StorageClassSpecifier::Extern)
    //            {
    //                log(Errors::Semantics::ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION.args(
    //                    *storage, m_sourceInterface, *storage));
    //                continue;
    //            }
    //            storageClassSpecifier = storage;
    //        }
    //    }
    //
    //    auto type = declaratorsToType(node.getDeclarationSpecifiers(), node.getDeclarator(), node.getDeclarations());
    //    if (!std::holds_alternative<Semantics::FunctionType>(type.get()))
    //    {
    //        log(Errors::Semantics::EXPECTED_PARAMETER_LIST_IN_FUNCTION_DEFINITION.args(
    //            node.getDeclarator(), m_sourceInterface, node.getDeclarator()));
    //        return {};
    //    }
    //    const auto& functionRP = cld::get<Semantics::FunctionType>(type.get());
    //
    //    auto* parameterTypeList = findRecursively<Syntax::DirectDeclaratorParenthesesParameters>(
    //        node.getDeclarator().getDirectDeclarator(), DIRECT_DECL_NEXT_FN);
    //
    //    auto* identifierList = findRecursively<Syntax::DirectDeclaratorParenthesesIdentifiers>(
    //        node.getDeclarator().getDirectDeclarator(), DIRECT_DECL_NEXT_FN);
    //
    //    if (!identifierList && !node.getDeclarations().empty())
    //    {
    //        log(Errors::Semantics::DECLARATIONS_ONLY_ALLOWED_WITH_IDENTIFIER_LIST.args(
    //            *node.getDeclarations().front().begin(), m_sourceInterface,
    //            std::forward_as_tuple(*node.getDeclarations().front().begin(),
    //                                  *(node.getDeclarations().back().end() - 1))));
    //    }
    //
    //    std::unordered_map<std::string_view, Semantics::Type> declarationMap;
    //    if (identifierList)
    //    {
    //        for (auto& iter : node.getDeclarations())
    //        {
    //            for (auto& pair : iter.getInitDeclarators())
    //            {
    //                declarationMap.emplace(Semantics::declaratorToName(*pair.first),
    //                                       declaratorsToType(iter.getDeclarationSpecifiers(), *pair.first));
    //            }
    //        }
    //    }
    //
    //    auto scope = pushScope();
    //    std::vector<Declaration> declarations;
    //    for (std::size_t i = 0; i < functionRP.getArguments().size(); i++)
    //    {
    //        if (parameterTypeList)
    //        {
    //            const auto& parameterDeclaration = parameterTypeList->getParameterTypeList().getParameters()[i];
    //            auto& declarator = cld::get<std::unique_ptr<Syntax::Declarator>>(parameterDeclaration.declarator);
    //            CLD_ASSERT(declarator);
    //            // declarator cannot be null as otherwise it'd have failed in the parser
    //            auto argName = declaratorToName(*declarator);
    //            auto& specifiers = parameterDeclaration.declarationSpecifiers;
    //            declarations.emplace_back(functionRP.getArguments()[i].first, Linkage::None, Lifetime::Automatic,
    //                                      functionRP.getArguments()[i].second);
    //            /*
    //            if (auto [iter, inserted] =
    //                    m_declarationScope.back().emplace(declarations.back().getName(), declarations.back());
    //                !inserted)
    //            {
    //                // TODO: showing previous one
    //                const auto* begin =
    //                Syntax::nodeFromVariant(parameterDeclaration.declarationSpecifiers.front()).begin();
    //                //                log(Errors::REDEFINITION_OF_SYMBOL_N.args(*begin, m_sourceInterface,
    //                //                                                          std::forward_as_tuple(*begin,
    //                //                                                          *((*declarator)->end() - 1))));
    //            }
    //             */
    //        }
    //        else if (identifierList)
    //        {
    //            auto result = declarationMap.find(identifierList->getIdentifiers()[i].first);
    //            if (result == declarationMap.end())
    //            {
    //                declarations.emplace_back(functionRP.getArguments()[i].first, Linkage::None, Lifetime::Automatic,
    //                                          functionRP.getArguments()[i].second);
    //            }
    //            else
    //            {
    //                declarations.emplace_back(result->second, Linkage::None, Lifetime::Automatic,
    //                                          functionRP.getArguments()[i].second);
    //            }
    //            /*
    //            if (auto [iter, inserted] =
    //                    m_declarationScope.back().emplace(declarations.back().getName(), declarations.back());
    //                !inserted)
    //            {
    //                // TODO: Modifier
    //                Lexer::CTokenIterator identifierLoc;
    //                auto decl = std::find_if(node.getDeclarations().begin(), node.getDeclarations().end(),
    //                                         [&declarations, &identifierLoc](const Syntax::Declaration& declaration) {
    //                                             return std::any_of(declaration.getInitDeclarators().begin(),
    //                                                                declaration.getInitDeclarators().end(),
    //                                                                [&declarations, &identifierLoc](const auto& pair)
    //                                                                {
    //                                                                    if (Semantics::declaratorToName(*pair.first)
    //                                                                        == declarations.back().getName())
    //                                                                    {
    //                                                                        identifierLoc =
    //                                                                            Semantics::declaratorToLoc(*pair.first);
    //                                                                        return true;
    //                                                                    }
    //                                                                    return false;
    //                                                                });
    //                                         });
    //
    //                // TODO:                log({Message::error(Errors::REDEFINITION_OF_SYMBOL_N.args('\'' +
    //                // declarations.back().getName() + '\''),
    //                //                                    node.begin(), node.getCompoundStatement().begin()),
    //                //                     Message::note(Notes::PREVIOUSLY_DECLARED_HERE, decl->begin(), decl->end(),
    //                //                                   {Underline(identifierLoc, identifierLoc + 1)})});
    //            }
    //             */
    //        }
    //        else
    //        {
    //            CLD_UNREACHABLE;
    //        }
    //    }
    //
    //    // TODO:
    //    //    auto result = visit(node.getCompoundStatement(),false);
    //    //    if(result)
    //    //    {
    //    //        return result;
    //    //    }
    //
    //    result.emplace_back(std::make_unique<FunctionDefinition>(
    //        functionRP, cld::to_string(name), std::move(declarations),
    //        storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static ?
    //            Linkage::Internal :
    //            Linkage::External,
    //        CompoundStatement({})));
    //
    //    return result;
}

std::vector<cld::Semantics::TranslationUnit::Variant>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::Declaration& node)
{
    std::vector<TranslationUnit::Variant> decls;
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        if (auto* storage = std::get_if<Syntax::StorageClassSpecifier>(&iter))
        {
            if (!storageClassSpecifier)
            {
                storageClassSpecifier = storage;
            }
            else
            {
                log(Errors::Semantics::ONLY_ONE_STORAGE_SPECIFIER.args(*storage, m_sourceInterface, *storage));
                log(Notes::PREVIOUS_STORAGE_SPECIFIER_HERE.args(*storageClassSpecifier, m_sourceInterface,
                                                                *storageClassSpecifier));
            }
            if (m_currentScope == 0
                && (storage->getSpecifier() == Syntax::StorageClassSpecifier::Auto
                    || storage->getSpecifier() == Syntax::StorageClassSpecifier::Register))
            {
                if (storage->getSpecifier() == Syntax::StorageClassSpecifier::Auto)
                {
                    log(Errors::Semantics::DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO.args(*storage, m_sourceInterface,
                                                                                          *storage));
                }
                else
                {
                    log(Errors::Semantics::DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_REGISTER.args(
                        *storage, m_sourceInterface, *storage));
                }
            }
        }
    }
    if (node.getInitDeclarators().empty())
    {
        bool declaresSomething = std::any_of(
            node.getDeclarationSpecifiers().begin(), node.getDeclarationSpecifiers().end(),
            [](const Syntax::DeclarationSpecifier& declarationSpecifier) {
                if (!std::holds_alternative<Syntax::TypeSpecifier>(declarationSpecifier))
                {
                    return false;
                }
                auto& typeSpec = cld::get<Syntax::TypeSpecifier>(declarationSpecifier);
                if (std::holds_alternative<std::unique_ptr<Syntax::EnumSpecifier>>(typeSpec.getVariant())
                    || std::holds_alternative<std::unique_ptr<Syntax::StructOrUnionSpecifier>>(typeSpec.getVariant()))
                {
                    return true;
                }
                return false;
            });
        if (!declaresSomething)
        {
            log(Errors::Semantics::DECLARATION_DOES_NOT_DECLARE_ANYTHING.args(node, m_sourceInterface, node));
        }
        else
        {
            declaratorsToType(node.getDeclarationSpecifiers());
        }
        return decls;
    }
    for (auto& [declarator, initializer] : node.getInitDeclarators())
    {
        const auto* loc = declaratorToLoc(*declarator);
        auto name = declaratorToName(*declarator);
        auto result = declaratorsToType(node.getDeclarationSpecifiers(), *declarator);
        if (auto* functionType = std::get_if<FunctionType>(&result.get());
            functionType
            && (!storageClassSpecifier
                || storageClassSpecifier->getSpecifier() != Syntax::StorageClassSpecifier::Typedef))
        {
            if (initializer)
            {
                log(Errors::Semantics::FUNCTION_PROTOTYPE_MUST_NOT_HAVE_AN_INITIALIZER.args(
                    *initializer, m_sourceInterface, *initializer));
            }
            Linkage linkage = Linkage::External;
            if (storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static)
            {
                linkage = Linkage::Internal;
            }
            Lifetime lifetime = Lifetime::Static;
            auto declaration =
                std::make_unique<Declaration>(std::move(result), linkage, lifetime, cld::to_string(name));
            auto [prev, notRedefinition] =
                getCurrentScope().declarations.insert({name, DeclarationInScope{loc, declaration.get()}});
            if (!notRedefinition)
            {
                if (std::holds_alternative<ConstRetType>(prev->second.declared)
                    || std::holds_alternative<Type>(prev->second.declared)
                    || (std::holds_alternative<const Declaration*>(prev->second.declared)
                        && !typesAreCompatible(declaration->getType(),
                                               cld::get<const Declaration*>(prev->second.declared)->getType()))
                    || (std::holds_alternative<const FunctionDefinition*>(prev->second.declared)
                        && !typesAreCompatible(declaration->getType(),
                                               cld::get<const FunctionDefinition*>(prev->second.declared)->getType())))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
                else if (std::holds_alternative<const Declaration*>(prev->second.declared))
                {
                    auto& otherType = cld::get<const Declaration*>(prev->second.declared)->getType();
                    auto composite = compositeType(otherType, declaration->getType());
                    declaration =
                        std::make_unique<Declaration>(std::move(composite), linkage, lifetime, cld::to_string(name));
                    prev->second.declared = declaration.get();
                    decls.push_back(std::move(declaration));
                }
                else
                {
                    decls.push_back(std::move(declaration));
                }
            }
            else
            {
                decls.push_back(std::move(declaration));
            }
        }
        else
        {
            for (auto& iter : node.getDeclarationSpecifiers())
            {
                if (auto* funcSpec = std::get_if<Syntax::FunctionSpecifier>(&iter))
                {
                    log(Errors::Semantics::INLINE_ONLY_ALLOWED_FOR_FUNCTIONS.args(*funcSpec, m_sourceInterface,
                                                                                  *funcSpec));
                }
            }
            if (std::holds_alternative<AbstractArrayType>(result.get()) && initializer)
            {
                // TODO: size deduction
            }
            bool errors = false;
            Linkage linkage = m_currentScope == 0 ? Linkage::External : Linkage::None;
            Lifetime lifetime = m_currentScope == 0 ? Lifetime::Static : Lifetime::Automatic;
            if (storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static)
            {
                if (m_currentScope == 0)
                {
                    linkage = Linkage::Internal;
                }
                lifetime = Lifetime::Static;
            }
            else if (storageClassSpecifier
                     && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Extern)
            {
                linkage = Linkage::External;
                lifetime = Lifetime::Static;
            }
            if (linkage == Linkage::None && !isCompleteType(result))
            {
                log(Errors::Semantics::DECLARATION_MUST_HAVE_A_COMPLETE_TYPE.args(*loc, m_sourceInterface, *loc,
                                                                                  result));
                errors = true;
            }
            else if (isVoid(result))
            {
                log(Errors::Semantics::DECLARATION_MUST_NOT_BE_VOID.args(*loc, m_sourceInterface, *loc));
                errors = true;
            }
            auto typeVisitor = RecursiveVisitor(result, ARRAY_TYPE_NEXT_FN);
            if (std::any_of(typeVisitor.begin(), typeVisitor.end(),
                            [](const Type& type) { return std::holds_alternative<ValArrayType>(type.get()); }))
            {
                if (m_currentScope == 0)
                {
                    log(Errors::Semantics::VARIABLE_LENGTH_ARRAY_NOT_ALLOWED_AT_FILE_SCOPE.args(*loc, m_sourceInterface,
                                                                                                *loc));
                    errors = true;
                }
                else if (linkage != Linkage::None)
                {
                    CLD_ASSERT(storageClassSpecifier);
                    log(Errors::Semantics::VARIABLE_LENGTH_ARRAY_MUST_NOT_HAVE_ANY_LINKAGE.args(
                        *loc, m_sourceInterface, *storageClassSpecifier));
                    errors = true;
                }
                else if (lifetime == Lifetime::Static)
                {
                    CLD_ASSERT(storageClassSpecifier);
                    log(Errors::Semantics::VARIABLE_LENGTH_ARRAY_MUST_NOT_HAVE_STATIC_LIFETIME.args(
                        *loc, m_sourceInterface, *storageClassSpecifier));
                    errors = true;
                }
            }
            if (std::any_of(typeVisitor.begin(), typeVisitor.end(), [](const Type& type) {
                    return cld::match(
                        type.get(), [](const ArrayType& arrayType) { return arrayType.isStatic(); },
                        [](const ValArrayType& arrayType) { return arrayType.isStatic(); },
                        [](auto&&) { return false; });
                }))
            {
                errors = true;
                auto ddVisitor = RecursiveVisitor(declarator->getDirectDeclarator(), DIRECT_DECL_NEXT_FN);
                auto ddStatic =
                    std::find_if(ddVisitor.begin(), ddVisitor.end(), [](const Syntax::DirectDeclarator& dd) {
                        return std::holds_alternative<Syntax::DirectDeclaratorStatic>(dd);
                    });
                CLD_ASSERT(ddStatic != ddVisitor.end());
                const auto* staticLoc = cld::get<Syntax::DirectDeclaratorStatic>(*ddStatic).getStaticLoc();
                log(Errors::Semantics::ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_STATIC.args(
                    *staticLoc, m_sourceInterface, *staticLoc));
            }
            if (std::any_of(typeVisitor.begin(), typeVisitor.end(), [](const Type& type) {
                    return cld::match(
                        type.get(),
                        [&](const ArrayType& arrayType) {
                            return type.isConst() || type.isVolatile() || arrayType.isRestricted();
                        },
                        [&](const ValArrayType& arrayType) {
                            return type.isConst() || type.isVolatile() || arrayType.isRestricted();
                        },
                        [](auto&&) { return false; });
                }))
            {
                errors = true;
                auto ddVisitor = RecursiveVisitor(declarator->getDirectDeclarator(), DIRECT_DECL_NEXT_FN);
                const auto* ddQual = std::accumulate(
                    ddVisitor.begin(), ddVisitor.end(), (const Syntax::Node*)nullptr,
                    [](const Syntax::Node* curr, const Syntax::DirectDeclarator& dd) {
                        if (curr)
                        {
                            return curr;
                        }
                        return cld::match(
                            dd,
                            [](const Syntax::DirectDeclaratorNoStaticOrAsterisk& array) -> const Syntax::Node* {
                                if (!array.getTypeQualifiers().empty())
                                {
                                    return &array.getTypeQualifiers()[0];
                                }
                                return nullptr;
                            },
                            [](const Syntax::DirectDeclaratorStatic& array) -> const Syntax::Node* {
                                if (!array.getTypeQualifiers().empty())
                                {
                                    return &array.getTypeQualifiers()[0];
                                }
                                return nullptr;
                            },
                            [](auto&&) -> const Syntax::Node* { return nullptr; });
                    });
                CLD_ASSERT(ddQual);
                log(Errors::Semantics::ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_QUALIFIED.args(
                    *ddQual, m_sourceInterface, *ddQual));
            }
            if (errors)
            {
                result = Type{};
            }
            if (storageClassSpecifier
                && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Typedef)
            {
                result.setName(name);
                auto [prev, noRedefinition] =
                    getCurrentScope().declarations.insert({name, DeclarationInScope{loc, result}});
                if (!noRedefinition
                    && (!std::holds_alternative<Type>(prev->second.declared)
                        || !typesAreCompatible(result, cld::get<Type>(prev->second.declared))))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
                continue;
            }
            auto declaration =
                std::make_unique<Declaration>(std::move(result), linkage, lifetime, cld::to_string(name));
            auto [prev, notRedefinition] =
                getCurrentScope().declarations.insert({name, DeclarationInScope{loc, declaration.get()}});
            if (!notRedefinition)
            {
                if (!std::holds_alternative<const Declaration*>(prev->second.declared)
                    || (declaration->getLinkage() == Linkage::None
                        && cld::get<const Declaration*>(prev->second.declared)->getLinkage() == Linkage::None)
                    || !typesAreCompatible(declaration->getType(),
                                           cld::get<const Declaration*>(prev->second.declared)->getType()))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
            }
            else
            {
                decls.push_back(std::move(declaration));
            }
        }
    }
    return decls;
}

bool cld::Semantics::SemanticAnalysis::isTypedef(std::string_view name) const
{
    auto curr = m_currentScope;
    while (curr >= 0)
    {
        auto result = m_scopes[curr].declarations.find(name);
        if (result != m_scopes[curr].declarations.end() && std::holds_alternative<Type>(result->second.declared))
        {
            return true;
        }
        curr = m_scopes[curr].previousScope;
    }

    return false;
}

bool cld::Semantics::SemanticAnalysis::isTypedefInScope(std::string_view name) const
{
    auto curr = m_currentScope;
    while (curr >= 0)
    {
        auto result = m_scopes[curr].declarations.find(name);
        if (result != m_scopes[curr].declarations.end())
        {
            return std::holds_alternative<Type>(result->second.declared);
        }
        curr = m_scopes[curr].previousScope;
    }

    return false;
}

const cld::Semantics::Type* cld::Semantics::SemanticAnalysis::getTypedef(std::string_view name) const
{
    return std::get_if<Type>(lookupDecl(name));
}

const cld::Semantics::SemanticAnalysis::DeclarationInScope::Variant*
    cld::Semantics::SemanticAnalysis::lookupDecl(std::string_view name, std::int64_t scope) const
{
    auto curr = scope;
    while (curr >= 0)
    {
        auto result = m_scopes[curr].declarations.find(name);
        if (result != m_scopes[curr].declarations.end())
        {
            return &result->second.declared;
        }
        curr = m_scopes[curr].previousScope;
    }
    return nullptr;
}

const cld::Semantics::SemanticAnalysis::TagTypeInScope::Variant*
    cld::Semantics::SemanticAnalysis::lookupType(std::string_view name, std::int64_t scope) const
{
    auto curr = scope;
    while (curr >= 0)
    {
        auto result = m_scopes[curr].types.find(name);
        if (result != m_scopes[curr].types.end())
        {
            return &result->second.tagType;
        }
        curr = m_scopes[curr].previousScope;
    }
    return nullptr;
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
    return std::make_tuple(isConst, isVolatile, isRestricted);
}

template <class T>
void cld::Semantics::SemanticAnalysis::handleParameterList(Type& type,
                                                           const Syntax::ParameterTypeList* parameterTypeList,
                                                           T&& returnTypeLoc)
{
    if (std::holds_alternative<FunctionType>(type.get()))
    {
        log(cld::Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_A_FUNCTION.args(returnTypeLoc, m_sourceInterface,
                                                                                     returnTypeLoc, type));
        type = Type{};
        return;
    }
    else if (std::holds_alternative<ArrayType>(type.get()) || std::holds_alternative<AbstractArrayType>(type.get())
             || std::holds_alternative<ValArrayType>(type.get()))
    {
        log(cld::Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_AN_ARRAY.args(returnTypeLoc, m_sourceInterface,
                                                                                   returnTypeLoc, type));
        type = Type{};
        return;
    }
    if (std::holds_alternative<FunctionType>(type.get()))
    {
        log(cld::Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_A_FUNCTION.args(returnTypeLoc, m_sourceInterface,
                                                                                     returnTypeLoc, type));
        type = Type{};
        return;
    }
    else if (std::holds_alternative<ArrayType>(type.get()) || std::holds_alternative<AbstractArrayType>(type.get())
             || std::holds_alternative<ValArrayType>(type.get()))
    {
        log(cld::Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_AN_ARRAY.args(returnTypeLoc, m_sourceInterface,
                                                                                   returnTypeLoc, type));
        type = Type{};
        return;
    }
    if (!parameterTypeList)
    {
        type = FunctionType::create(std::move(type), {}, false, true);
        return;
    }
    std::vector<std::pair<Type, std::string>> parameters;
    for (auto& iter : parameterTypeList->getParameters())
    {
        for (auto& specs : iter.declarationSpecifiers)
        {
            auto* storageSpec = std::get_if<cld::Syntax::StorageClassSpecifier>(&specs);
            if (!storageSpec)
            {
                continue;
            }
            if (storageSpec->getSpecifier() != cld::Syntax::StorageClassSpecifier::Register)
            {
                log(cld::Errors::Semantics::NO_STORAGE_CLASS_SPECIFIER_ALLOWED_IN_PARAMETER_BESIDES_REGISTER.args(
                    *storageSpec, m_sourceInterface, *storageSpec));
            }
        }
        auto paramType = cld::match(
            iter.declarator,
            [&](const std::unique_ptr<cld::Syntax::Declarator>& ptr) {
                return declaratorsToType(iter.declarationSpecifiers, *ptr);
            },
            [&](const std::unique_ptr<cld::Syntax::AbstractDeclarator>& ptr) {
                return declaratorsToType(iter.declarationSpecifiers, ptr.get());
            });
        if (isVoid(paramType) && !paramType.isConst() && !paramType.isVolatile()
            && !std::holds_alternative<std::unique_ptr<cld::Syntax::Declarator>>(iter.declarator)
            && parameterTypeList->getParameters().size() == 1 && !parameterTypeList->hasEllipse())
        {
            type = FunctionType::create(std::move(type), {}, false, false);
            return;
        }
        if (isVoid(paramType))
        {
            log(Errors::Semantics::VOID_TYPE_NOT_ALLOWED_AS_FUNCTION_PARAMETER.args(
                iter.declarationSpecifiers, m_sourceInterface, iter.declarationSpecifiers));
        }
        auto visitor = RecursiveVisitor(paramType, ARRAY_TYPE_NEXT_FN);
        auto begin = visitor.begin();
        begin++;
        auto result = std::find_if(begin, visitor.end(), [](const Type& type) {
            if (std::holds_alternative<ValArrayType>(type.get()))
            {
                return cld::get<ValArrayType>(type.get()).isStatic();
            }
            else if (std::holds_alternative<ArrayType>(type.get()))
            {
                return cld::get<ArrayType>(type.get()).isStatic();
            }
            return false;
        });
        if (result != visitor.end())
        {
            log(Errors::Semantics::STATIC_ONLY_ALLOWED_IN_OUTERMOST_ARRAY.args(iter.declarator, m_sourceInterface,
                                                                               iter.declarator));
        }
        std::string name;
        if (std::holds_alternative<std::unique_ptr<Syntax::Declarator>>(iter.declarator))
        {
            name = declaratorToName(*cld::get<std::unique_ptr<Syntax::Declarator>>(iter.declarator));
        }
        // Not transforming array types to pointers here as we might still want to use that information
        // to warn callers.
        if (std::holds_alternative<FunctionType>(paramType.get()))
        {
            paramType = PointerType::create(false, false, false, std::move(paramType));
        }
        parameters.emplace_back(std::move(paramType), std::move(name));
    }
    type = FunctionType::create(std::move(type), std::move(parameters), parameterTypeList->hasEllipse(), false);
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::declaratorsToTypeImpl(
    const std::vector<DeclarationOrSpecifierQualifier>& declarationOrSpecifierQualifiers,
    const PossiblyAbstractQualifierRef& declarator, const std::vector<Syntax::Declaration>& declarations)
{
    bool isConst = false;
    bool isVolatile = false;
    std::vector<const Syntax::TypeSpecifier*> typeSpecs;
    for (auto& iter : declarationOrSpecifierQualifiers)
    {
        if (auto* typeSpec = std::get_if<const Syntax::TypeSpecifier*>(&iter))
        {
            typeSpecs.push_back(*typeSpec);
        }
        else if (auto* typeQual = std::get_if<const Syntax::TypeQualifier*>(&iter))
        {
            switch ((*typeQual)->getQualifier())
            {
                case Syntax::TypeQualifier::Const: isConst = true; break;
                case Syntax::TypeQualifier::Volatile: isVolatile = true; break;
                case Syntax::TypeQualifier::Restrict:
                    log(Errors::Semantics::RESTRICT_CAN_ONLY_BE_APPLIED_TO_POINTERS.args(**typeQual, m_sourceInterface,
                                                                                         **typeQual));
                    break;
            }
        }
    }
    if (typeSpecs.empty())
    {
        log(Errors::Semantics::AT_LEAST_ONE_TYPE_SPECIFIER_REQUIRED.args(
            declarationOrSpecifierQualifiers, m_sourceInterface, declarationOrSpecifierQualifiers));
        return Type{};
    }
    auto type = typeSpecifiersToType(isConst, isVolatile, std::move(typeSpecs));
    if (!cld::match(declarator, [](auto&& value) -> bool { return value; }))
    {
        return type;
    }
    // whatever is in declarator it is not null
    if (std::holds_alternative<const Syntax::Declarator*>(declarator))
    {
        auto& realDecl = *cld::get<const Syntax::Declarator*>(declarator);
        for (auto& iter : realDecl.getPointers())
        {
            auto [isConst, isVolatile, restricted] = getQualifiers(iter.getTypeQualifiers());
            type = PointerType::create(isConst, isVolatile, restricted, std::move(type));
        }
        cld::matchWithSelf<void>(
            realDecl.getDirectDeclarator(), [](auto&&, const Syntax::DirectDeclaratorIdentifier&) {},
            [&](auto&& self, const Syntax::DirectDeclaratorParentheses& parentheses) {
                for (auto& iter : parentheses.getDeclarator().getPointers())
                {
                    auto [isConst, isVolatile, restricted] = getQualifiers(iter.getTypeQualifiers());
                    if (restricted && std::holds_alternative<FunctionType>(type.get()))
                    {
                        auto restrictQual =
                            std::find_if(iter.getTypeQualifiers().begin(), iter.getTypeQualifiers().end(),
                                         [](const Syntax::TypeQualifier& typeQualifier) {
                                             return typeQualifier.getQualifier() == Syntax::TypeQualifier::Restrict;
                                         });
                        CLD_ASSERT(restrictQual != iter.getTypeQualifiers().end());
                        log(Errors::Semantics::
                                ELEMENT_TYPE_OF_POINTER_WITH_RESTRICT_QUALIFIER_MUST_NOT_BE_A_FUNCTION_TYPE.args(
                                    parentheses.getDeclarator().getDirectDeclarator(), m_sourceInterface,
                                    parentheses.getDeclarator().getDirectDeclarator(), *restrictQual));
                        restricted = false;
                    }
                    type = PointerType::create(isConst, isVolatile, restricted, std::move(type));
                }
                cld::match(parentheses.getDeclarator().getDirectDeclarator(), self);
            },
            [&](auto&& self, const Syntax::DirectDeclaratorNoStaticOrAsterisk& noStaticOrAsterisk) {
                auto scope = llvm::make_scope_exit([&] { cld::match(noStaticOrAsterisk.getDirectDeclarator(), self); });
                if (std::holds_alternative<FunctionType>(type.get()))
                {
                    log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                        noStaticOrAsterisk.getDirectDeclarator(), m_sourceInterface,
                        noStaticOrAsterisk.getDirectDeclarator(), type));
                    type = Type{};
                    return;
                }
                else if (!isCompleteType(type))
                {
                    log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_BE_A_COMPLETE_TYPE.args(
                        noStaticOrAsterisk.getDirectDeclarator(), m_sourceInterface,
                        noStaticOrAsterisk.getDirectDeclarator(), type));
                    type = Type{};
                    return;
                }

                auto [isConst, isVolatile, restricted] = getQualifiers(noStaticOrAsterisk.getTypeQualifiers());
                if (!noStaticOrAsterisk.getAssignmentExpression())
                {
                    type = AbstractArrayType::create(isConst, isVolatile, restricted, std::move(type));
                    return;
                }
                auto result = evaluateConstantExpression(*noStaticOrAsterisk.getAssignmentExpression(),
                                                         ConstantEvaluator::Arithmetic);
                if (!result)
                {
                    type = ValArrayType::create(isConst, isVolatile, restricted, false, std::move(type));
                    return;
                }
                if (!result->isInteger())
                {
                    log(Errors::Semantics::ARRAY_SIZE_MUST_BE_AN_INTEGER_TYPE.args(
                        *noStaticOrAsterisk.getAssignmentExpression(), m_sourceInterface,
                        *noStaticOrAsterisk.getAssignmentExpression(), result->getType()));
                    type = Type{};
                    return;
                }
                if (cld::get<PrimitiveType>(result->getType().get()).isSigned())
                {
                    if (result->toInt() <= 0)
                    {
                        log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(
                            *noStaticOrAsterisk.getAssignmentExpression(), m_sourceInterface,
                            *noStaticOrAsterisk.getAssignmentExpression(),
                            cld::get<llvm::APSInt>(result->getValue()).toString(10)));
                        type = Type{};
                        return;
                    }
                }
                else if (result->toUInt() == 0)
                {
                    log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(
                        *noStaticOrAsterisk.getAssignmentExpression(), m_sourceInterface,
                        *noStaticOrAsterisk.getAssignmentExpression(),
                        cld::get<llvm::APSInt>(result->getValue()).toString(10)));
                    type = Type{};
                    return;
                }
                auto size = result->toUInt();
                type = ArrayType::create(isConst, isVolatile, restricted, false, std::move(type), size);
            },
            [&](auto&& self, const Syntax::DirectDeclaratorStatic& declaratorStatic) {
                auto scope = llvm::make_scope_exit([&] { cld::match(declaratorStatic.getDirectDeclarator(), self); });
                if (std::holds_alternative<FunctionType>(type.get()))
                {
                    log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                        declaratorStatic.getDirectDeclarator(), m_sourceInterface,
                        declaratorStatic.getDirectDeclarator(), type));
                    type = Type{};
                    return;
                }
                else if (!isCompleteType(type))
                {
                    log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_BE_A_COMPLETE_TYPE.args(
                        declaratorStatic.getDirectDeclarator(), m_sourceInterface,
                        declaratorStatic.getDirectDeclarator(), type));
                    type = Type{};
                    return;
                }

                auto [isConst, isVolatile, restricted] = getQualifiers(declaratorStatic.getTypeQualifiers());
                auto result = evaluateConstantExpression(declaratorStatic.getAssignmentExpression(),
                                                         ConstantEvaluator::Arithmetic);
                if (!result)
                {
                    type = ValArrayType::create(isConst, isVolatile, restricted, true, std::move(type));
                    return;
                }
                if (!result->isInteger())
                {
                    log(Errors::Semantics::ARRAY_SIZE_MUST_BE_AN_INTEGER_TYPE.args(
                        declaratorStatic.getAssignmentExpression(), m_sourceInterface,
                        declaratorStatic.getAssignmentExpression(), result->getType()));
                    type = Type{};
                    return;
                }
                if (cld::get<PrimitiveType>(result->getType().get()).isSigned())
                {
                    if (result->toInt() <= 0)
                    {
                        log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(
                            declaratorStatic.getAssignmentExpression(), m_sourceInterface,
                            declaratorStatic.getAssignmentExpression(),
                            cld::get<llvm::APSInt>(result->getValue()).toString(10)));
                        type = Type{};
                        return;
                    }
                }
                else if (result->toUInt() == 0)
                {
                    log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(
                        declaratorStatic.getAssignmentExpression(), m_sourceInterface,
                        declaratorStatic.getAssignmentExpression(),
                        cld::get<llvm::APSInt>(result->getValue()).toString(10)));
                    type = Type{};
                    return;
                }
                auto size = result->toUInt();
                type = ArrayType::create(isConst, isVolatile, restricted, true, std::move(type), size);
            },
            [&](auto&& self, const Syntax::DirectDeclaratorAsterisk& asterisk) {
                auto scope = llvm::make_scope_exit([&] { cld::match(asterisk.getDirectDeclarator(), self); });
                if (std::holds_alternative<FunctionType>(type.get()))
                {
                    log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                        asterisk.getDirectDeclarator(), m_sourceInterface, asterisk.getDirectDeclarator(), type));
                    type = Type{};
                    return;
                }
                else if (!isCompleteType(type))
                {
                    log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_BE_A_COMPLETE_TYPE.args(
                        asterisk.getDirectDeclarator(), m_sourceInterface, asterisk.getDirectDeclarator(), type));
                    type = Type{};
                    return;
                }

                auto [isConst, isVolatile, restricted] = getQualifiers(asterisk.getTypeQualifiers());
                type = ValArrayType::create(isConst, isVolatile, restricted, false, std::move(type));
            },
            [&](auto&& self, const Syntax::DirectDeclaratorParenthesesIdentifiers& identifiers) {
                auto scope = llvm::make_scope_exit([&] { cld::match(identifiers.getDirectDeclarator(), self); });
                if (std::holds_alternative<FunctionType>(type.get()))
                {
                    log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                        std::forward_as_tuple(declarationOrSpecifierQualifiers[0], identifiers.getDirectDeclarator()),
                        m_sourceInterface,
                        std::forward_as_tuple(declarationOrSpecifierQualifiers[0], identifiers.getDirectDeclarator()),
                        type));
                    type = Type{};
                    return;
                }
                else if (std::holds_alternative<ArrayType>(type.get())
                         || std::holds_alternative<AbstractArrayType>(type.get())
                         || std::holds_alternative<ValArrayType>(type.get()))
                {
                    log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_AN_ARRAY.args(
                        std::forward_as_tuple(declarationOrSpecifierQualifiers[0], identifiers.getDirectDeclarator()),
                        m_sourceInterface,
                        std::forward_as_tuple(declarationOrSpecifierQualifiers[0], identifiers.getDirectDeclarator()),
                        type));
                    type = Type{};
                    return;
                }
                if (identifiers.getIdentifiers().empty())
                {
                    type = FunctionType::create(std::move(type), {}, false, true);
                    return;
                }
                if (declarations.empty())
                {
                    log(Errors::Semantics::IDENTIFIER_LIST_ONLY_ALLOWED_AS_PART_OF_A_FUNCTION_DEFINITION.args(
                        identifiers.getIdentifiers(), m_sourceInterface, identifiers.getIdentifiers()));
                    type = FunctionType::create(std::move(type), {}, false, true);
                    return;
                }
                std::unordered_map<std::string_view, std::uint64_t> paramNames;
                std::vector<std::pair<Type, std::string>> parameters;
                std::unordered_map<std::string_view, Lexer::CTokenIterator> seenParameters;
                for (auto& iter : identifiers.getIdentifiers())
                {
                    paramNames[cld::get<std::string>(iter->getValue())] = paramNames.size();
                    parameters.emplace_back(Type{}, cld::get<std::string>(iter->getValue()));
                    seenParameters.emplace(cld::get<std::string>(iter->getValue()), nullptr);
                }
                for (auto& iter : declarations)
                {
                    for (auto& specs : iter.getDeclarationSpecifiers())
                    {
                        auto* storageSpec = std::get_if<Syntax::StorageClassSpecifier>(&specs);
                        if (!storageSpec)
                        {
                            continue;
                        }
                        if (storageSpec->getSpecifier() != Syntax::StorageClassSpecifier::Register)
                        {
                            log(Errors::Semantics::NO_STORAGE_CLASS_SPECIFIER_ALLOWED_IN_PARAMETER_BESIDES_REGISTER
                                    .args(*storageSpec, m_sourceInterface, *storageSpec));
                        }
                    }
                    for (auto& [decl, init] : iter.getInitDeclarators())
                    {
                        if (init)
                        {
                            // TODO: Error
                        }
                        if (!decl)
                        {
                            // TODO: Error
                        }
                        auto paramType = declaratorsToType(iter.getDeclarationSpecifiers(), *decl);
                        auto name = declaratorToName(*decl);
                        auto result = paramNames.find(name);
                        if (result == paramNames.end())
                        {
                            // TODO: Error
                        }
                        parameters[result->second].first = paramType;
                        seenParameters[name] = declaratorToLoc(*decl);
                    }
                }
                for (auto& [name, loc] : seenParameters)
                {
                    if (!loc)
                    {
                        // TODO: Error
                    }
                }
                type = FunctionType::create(std::move(type), std::move(parameters), false, true);
            },
            [&](auto&& self, const Syntax::DirectDeclaratorParenthesesParameters& parameterList) {
                auto scope = llvm::make_scope_exit([&] { cld::match(parameterList.getDirectDeclarator(), self); });
                handleParameterList(
                    type, &parameterList.getParameterTypeList(),
                    std::forward_as_tuple(declarationOrSpecifierQualifiers, parameterList.getDirectDeclarator()));
            });
    }
    else
    {
        auto& realDecl = *cld::get<const Syntax::AbstractDeclarator*>(declarator);
        for (auto& iter : realDecl.getPointers())
        {
            auto [isConst, isVolatile, restricted] = getQualifiers(iter.getTypeQualifiers());
            type = PointerType::create(isConst, isVolatile, restricted, std::move(type));
        }
        if (!realDecl.getDirectAbstractDeclarator())
        {
            return type;
        }
        cld::matchWithSelf<void>(
            *realDecl.getDirectAbstractDeclarator(),
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorParentheses& parentheses) {
                for (auto& iter : parentheses.getAbstractDeclarator().getPointers())
                {
                    auto [isConst, isVolatile, restricted] = getQualifiers(iter.getTypeQualifiers());
                    if (restricted && std::holds_alternative<FunctionType>(type.get()))
                    {
                        auto restrictQual =
                            std::find_if(iter.getTypeQualifiers().begin(), iter.getTypeQualifiers().end(),
                                         [](const Syntax::TypeQualifier& typeQualifier) {
                                             return typeQualifier.getQualifier() == Syntax::TypeQualifier::Restrict;
                                         });
                        CLD_ASSERT(restrictQual != iter.getTypeQualifiers().end());
                        log(Errors::Semantics::
                                ELEMENT_TYPE_OF_POINTER_WITH_RESTRICT_QUALIFIER_MUST_NOT_BE_A_FUNCTION_TYPE.args(
                                    parentheses, m_sourceInterface, parentheses, *restrictQual));
                        type = Type{};
                        continue;
                    }
                    type = PointerType::create(isConst, isVolatile, restricted, std::move(type));
                }
                if (parentheses.getAbstractDeclarator().getDirectAbstractDeclarator())
                {
                    cld::match(*parentheses.getAbstractDeclarator().getDirectAbstractDeclarator(), self);
                }
            },
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorAsterisk& asterisk) {
                auto scope = llvm::make_scope_exit([&] {
                    if (asterisk.getDirectAbstractDeclarator())
                    {
                        cld::match(*asterisk.getDirectAbstractDeclarator(), self);
                    }
                });
                if (std::holds_alternative<FunctionType>(type.get()))
                {
                    if (asterisk.getDirectAbstractDeclarator())
                    {
                        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                            *asterisk.getDirectAbstractDeclarator(), m_sourceInterface,
                            *asterisk.getDirectAbstractDeclarator(), type));
                    }
                    else
                    {
                        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                            declarationOrSpecifierQualifiers, m_sourceInterface, declarationOrSpecifierQualifiers,
                            type));
                    }
                    type = Type{};
                    return;
                }
                else if (!isCompleteType(type))
                {
                    if (asterisk.getDirectAbstractDeclarator())
                    {
                        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_BE_A_COMPLETE_TYPE.args(
                            *asterisk.getDirectAbstractDeclarator(), m_sourceInterface,
                            *asterisk.getDirectAbstractDeclarator(), type));
                    }
                    else
                    {
                        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                            declarationOrSpecifierQualifiers, m_sourceInterface, declarationOrSpecifierQualifiers,
                            type));
                    }
                    type = Type{};
                    return;
                }

                type = ValArrayType::create(false, false, false, false, std::move(type));
            },
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorAssignmentExpression& expression) {
                auto scope = llvm::make_scope_exit([&] {
                    if (expression.getDirectAbstractDeclarator())
                    {
                        cld::match(*expression.getDirectAbstractDeclarator(), self);
                    }
                });
                if (std::holds_alternative<FunctionType>(type.get()))
                {
                    if (expression.getDirectAbstractDeclarator())
                    {
                        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                            *expression.getDirectAbstractDeclarator(), m_sourceInterface,
                            *expression.getDirectAbstractDeclarator(), type));
                    }
                    else
                    {
                        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                            declarationOrSpecifierQualifiers, m_sourceInterface, declarationOrSpecifierQualifiers,
                            type));
                    }
                    type = Type{};
                    return;
                }
                else if (!isCompleteType(type))
                {
                    if (expression.getDirectAbstractDeclarator())
                    {
                        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_BE_A_COMPLETE_TYPE.args(
                            *expression.getDirectAbstractDeclarator(), m_sourceInterface,
                            *expression.getDirectAbstractDeclarator(), type));
                    }
                    else
                    {
                        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                            declarationOrSpecifierQualifiers, m_sourceInterface, declarationOrSpecifierQualifiers,
                            type));
                    }
                    type = Type{};
                    return;
                }

                if (!expression.getAssignmentExpression())
                {
                    type = AbstractArrayType::create(false, false, false, std::move(type));
                    return;
                }
                auto result =
                    evaluateConstantExpression(*expression.getAssignmentExpression(), ConstantEvaluator::Arithmetic);
                if (!result)
                {
                    type = ValArrayType::create(false, false, false, false, std::move(type));
                    return;
                }
                if (!result->isInteger())
                {
                    log(Errors::Semantics::ARRAY_SIZE_MUST_BE_AN_INTEGER_TYPE.args(
                        *expression.getAssignmentExpression(), m_sourceInterface, *expression.getAssignmentExpression(),
                        result->getType()));
                    type = Type{};
                    return;
                }
                if (cld::get<PrimitiveType>(result->getType().get()).isSigned())
                {
                    if (result->toInt() <= 0)
                    {
                        log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(
                            *expression.getAssignmentExpression(), m_sourceInterface,
                            *expression.getAssignmentExpression(),
                            cld::get<llvm::APSInt>(result->getValue()).toString(10)));
                        type = Type{};
                        return;
                    }
                }
                else if (result->toUInt() == 0)
                {
                    log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(
                        *expression.getAssignmentExpression(), m_sourceInterface, *expression.getAssignmentExpression(),
                        cld::get<llvm::APSInt>(result->getValue()).toString(10)));
                    type = Type{};
                    return;
                }
                auto size = result->toUInt();
                type = ArrayType::create(false, false, false, false, std::move(type), size);
            },
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorParameterTypeList& parameterTypeList) {
                auto scope = llvm::make_scope_exit([&] {
                    if (parameterTypeList.getDirectAbstractDeclarator())
                    {
                        cld::match(*parameterTypeList.getDirectAbstractDeclarator(), self);
                    }
                });
                handleParameterList(type, parameterTypeList.getParameterTypeList(), declarationOrSpecifierQualifiers);
            });
    }
    return type;
}

cld::Semantics::Type
    cld::Semantics::SemanticAnalysis::typeSpecifiersToType(bool isConst, bool isVolatile,
                                                           const std::vector<const Syntax::TypeSpecifier*>& typeSpec)
{
    CLD_ASSERT(!typeSpec.empty());
    if (std::holds_alternative<Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(typeSpec[0]->getVariant()))
    {
        return primitiveTypeSpecifiersToType(isConst, isVolatile, std::move(typeSpec));
    }
    if (auto* name = std::get_if<std::string>(&typeSpec[0]->getVariant()))
    {
        if (typeSpec.size() != 1)
        {
            log(Errors::Semantics::EXPECTED_NO_FURTHER_TYPE_SPECIFIERS_AFTER_TYPENAME.args(
                *typeSpec[1], m_sourceInterface, llvm::ArrayRef(typeSpec).drop_front()));
        }
        const auto* type = getTypedef(*name);
        CLD_ASSERT(type);
        auto ret = Type(isConst, isVolatile, cld::to_string(type->getTypeName()), type->get());
        ret.setName(type->getName());
        return ret;
    }
    if (auto* structOrUnionPtr =
            std::get_if<std::unique_ptr<Syntax::StructOrUnionSpecifier>>(&typeSpec[0]->getVariant()))
    {
        auto& structOrUnion = *structOrUnionPtr;
        if (typeSpec.size() != 1)
        {
            log(Errors::Semantics::EXPECTED_NO_FURTHER_TYPE_SPECIFIERS_AFTER_N.args(
                *typeSpec[1], m_sourceInterface,
                structOrUnion->isUnion() ? Lexer::TokenType::UnionKeyword : Lexer::TokenType::StructKeyword,
                llvm::ArrayRef(typeSpec).drop_front()));
        }
        if (structOrUnion->getStructDeclarations().empty())
        {
            CLD_ASSERT(structOrUnion->getIdentifierLoc());
            if (structOrUnion->isUnion())
            {
                auto [prev, notRedefined] = getCurrentScope().types.insert(
                    {cld::get<std::string>(structOrUnion->getIdentifierLoc()->getValue()),
                     TagTypeInScope{structOrUnion->getIdentifierLoc(), TagTypeInScope::UnionDecl{}}});
                if (!notRedefined && !std::holds_alternative<TagTypeInScope::UnionDecl>(prev->second.tagType)
                    && !std::holds_alternative<UnionDefinition>(prev->second.tagType))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion->getIdentifierLoc(), m_sourceInterface,
                                                              *structOrUnion->getIdentifierLoc()));
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
                return UnionType::create(isConst, isVolatile,
                                         cld::get<std::string>(structOrUnion->getIdentifierLoc()->getValue()),
                                         m_currentScope);
            }
            else
            {
                auto [prev, notRedefined] = getCurrentScope().types.insert(
                    {cld::get<std::string>(structOrUnion->getIdentifierLoc()->getValue()),
                     TagTypeInScope{structOrUnion->getIdentifierLoc(), TagTypeInScope::StructDecl{}}});
                if (!notRedefined && !std::holds_alternative<TagTypeInScope::StructDecl>(prev->second.tagType)
                    && !std::holds_alternative<StructDefinition>(prev->second.tagType))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion->getIdentifierLoc(), m_sourceInterface,
                                                              *structOrUnion->getIdentifierLoc()));
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
                return StructType::create(isConst, isVolatile,
                                          cld::get<std::string>(structOrUnion->getIdentifierLoc()->getValue()),
                                          m_currentScope);
            }
        }
        std::optional<decltype(getCurrentScope().types)::iterator> recordDefInScope;
        if (structOrUnion->getIdentifierLoc())
        {
            auto& name = cld::get<std::string>(structOrUnion->getIdentifierLoc()->getValue());
            if (structOrUnion->isUnion())
            {
                auto [prev, notRedefined] = getCurrentScope().types.insert(
                    {name, TagTypeInScope{structOrUnion->getIdentifierLoc(), TagTypeInScope::UnionDecl{}}});
                if (!notRedefined && !std::holds_alternative<TagTypeInScope::UnionDecl>(prev->second.tagType))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion->getIdentifierLoc(), m_sourceInterface,
                                                              *structOrUnion->getIdentifierLoc()));
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
                else
                {
                    recordDefInScope = prev;
                }
            }
            else
            {
                auto [prev, notRedefined] = getCurrentScope().types.insert(
                    {name, TagTypeInScope{structOrUnion->getIdentifierLoc(), TagTypeInScope::StructDecl{}}});
                if (!notRedefined && !std::holds_alternative<TagTypeInScope::StructDecl>(prev->second.tagType))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion->getIdentifierLoc(), m_sourceInterface,
                                                              *structOrUnion->getIdentifierLoc()));
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
                else
                {
                    recordDefInScope = prev;
                }
            }
        }
        std::vector<Field> fields;
        for (auto& [specifiers, declarators] : structOrUnion->getStructDeclarations())
        {
            for (auto& [declarator, size] : declarators)
            {
                if (!declarator)
                {
                    // TODO: Check if last and has size, otherwise error
                    continue;
                }
                auto type = declaratorsToType(specifiers, *declarator, {});
                auto fieldName = declaratorToName(*declarator);
                // TODO: bitfields, constraints, etc.
                (void)size;
                fields.push_back({std::make_shared<Type>(std::move(type)), cld::to_string(fieldName), {}});
            }
        }
        if (structOrUnion->getIdentifierLoc())
        {
            auto& name = cld::get<std::string>(structOrUnion->getIdentifierLoc()->getValue());
            if (structOrUnion->isUnion())
            {
                if (recordDefInScope)
                {
                    (*recordDefInScope)->second.tagType = UnionDefinition(name, std::move(fields));
                }
                return UnionType::create(isConst, isVolatile,
                                         cld::get<std::string>(structOrUnion->getIdentifierLoc()->getValue()),
                                         m_currentScope);
            }
            if (recordDefInScope)
            {
                (*recordDefInScope)->second.tagType = StructDefinition(name, std::move(fields));
            }
            return StructType::create(isConst, isVolatile,
                                      cld::get<std::string>(structOrUnion->getIdentifierLoc()->getValue()),
                                      m_currentScope);
        }

        if (structOrUnion->isUnion())
        {
            return AnonymousUnionType::create(isConst, isVolatile, std::move(fields));
        }
        else
        {
            return AnonymousStructType::create(isConst, isVolatile, std::move(fields));
        }
    }
    CLD_ASSERT(std::holds_alternative<std::unique_ptr<Syntax::EnumSpecifier>>(typeSpec[0]->getVariant()));
    if (typeSpec.size() != 1)
    {
        log(Errors::Semantics::EXPECTED_NO_FURTHER_TYPE_SPECIFIERS_AFTER_N.args(
            *typeSpec[1], m_sourceInterface, Lexer::TokenType::EnumKeyword, llvm::ArrayRef(typeSpec).drop_front()));
    }
    auto& enumDecl = cld::get<std::unique_ptr<Syntax::EnumSpecifier>>(typeSpec[0]->getVariant());
    if (auto* loc = std::get_if<Lexer::CTokenIterator>(&enumDecl->getVariant()))
    {
        auto [prev, notRedefined] = getCurrentScope().types.insert(
            {cld::get<std::string>((*loc)->getValue()), TagTypeInScope{*loc, TagTypeInScope::EnumDecl{}}});
        if (!notRedefined && !std::holds_alternative<TagTypeInScope::EnumDecl>(prev->second.tagType)
            && !std::holds_alternative<EnumDefinition>(prev->second.tagType))
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(**loc, m_sourceInterface, **loc));
            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                     *prev->second.identifier));
        }
        return EnumType::create(isConst, isVolatile, cld::get<std::string>((*loc)->getValue()), m_currentScope);
    }
    auto& enumDef = cld::get<Syntax::EnumDeclaration>(enumDecl->getVariant());
    // TODO: Type depending on values as an extension
    std::optional<decltype(getCurrentScope().types)::iterator> enumDefInScope;
    if (enumDef.getName())
    {
        auto& name = cld::get<std::string>(enumDef.getName()->getValue());
        auto [prev, notRedefined] =
            getCurrentScope().types.insert({name, TagTypeInScope{enumDef.getName(), EnumDefinition(name, Type{})}});
        if (!notRedefined)
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*enumDef.getName(), m_sourceInterface, *enumDef.getName()));
            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                     *prev->second.identifier));
        }
        else
        {
            enumDefInScope = prev;
        }
    }
    static ConstRetType one = {
        llvm::APSInt(llvm::APInt(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, 1), false),
        PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions())};
    ConstRetType nextValue = {llvm::APSInt(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, false),
                              PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions())};
    for (auto& [loc, maybeExpression] : enumDef.getValues())
    {
        ConstRetType value;
        if (maybeExpression)
        {
            auto result = evaluateConstantExpression(*maybeExpression, ConstantEvaluator::Integer);
            if (!result)
            {
                for (auto& iter : result.error())
                {
                    log(iter);
                }
                continue;
            }
            CLD_ASSERT(std::holds_alternative<llvm::APSInt>(result->getValue()));
            if (cld::get<llvm::APSInt>(result->getValue())
                    .ugt(llvm::APSInt::getMaxValue(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, true)))
            {
                auto number = cld::get<llvm::APSInt>(result->getValue()).toString(10);
                log(Errors::Semantics::VALUE_OF_ENUMERATION_CONSTANT_MUST_FIT_IN_TYPE_INT.args(
                    *loc, m_sourceInterface, *loc, *maybeExpression, number));
            }
            value = result->castTo(PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()),
                                   m_sourceInterface.getLanguageOptions());
        }
        else
        {
            value = nextValue;
        }
        nextValue = value.plus(one, m_sourceInterface.getLanguageOptions());
        auto [prevValue, notRedefined] = getCurrentScope().declarations.insert(
            {cld::get<std::string>(loc->getValue()), DeclarationInScope{loc, std::move(value)}});
        if (!notRedefined)
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prevValue->second.identifier, m_sourceInterface,
                                                     *prevValue->second.identifier));
        }
    }
    if (enumDef.getName())
    {
        auto& name = cld::get<std::string>(enumDef.getName()->getValue());
        if (enumDefInScope)
        {
            (*enumDefInScope)->second.tagType =
                EnumDefinition(name, PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()));
        }
        return EnumType::create(isConst, isVolatile, name, m_currentScope);
    }
    return AnonymousEnumType::create(isConst, isVolatile,
                                     PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()));
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::primitiveTypeSpecifiersToType(
    bool isConst, bool isVolatile, const std::vector<const Syntax::TypeSpecifier*>& typeSpecs)
{
    using PrimitiveTypeSpecifier = Syntax::TypeSpecifier::PrimitiveTypeSpecifier;
    CLD_ASSERT(std::holds_alternative<PrimitiveTypeSpecifier>(typeSpecs[0]->getVariant()));
    auto excessSpecifiersError = [this](std::string_view type, const Syntax::TypeSpecifier* typeSpec) {
        if (std::holds_alternative<std::string>(typeSpec->getVariant()))
        {
            log(Errors::Semantics::CANNOT_COMBINE_N_WITH_TYPENAME.args(*typeSpec, m_sourceInterface, type, *typeSpec));
        }
        else if (auto* structOrUnionP =
                     std::get_if<std::unique_ptr<Syntax::StructOrUnionSpecifier>>(&typeSpec->getVariant()))
        {
            log(Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args(
                *typeSpec, m_sourceInterface, type,
                (*structOrUnionP)->isUnion() ? Lexer::TokenType::UnionKeyword : Lexer::TokenType::StructKeyword,
                *typeSpec));
        }
        else if (std::holds_alternative<std::unique_ptr<Syntax::EnumSpecifier>>(typeSpec->getVariant()))
        {
            log(Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args(*typeSpec, m_sourceInterface, type,
                                                                Lexer::TokenType::EnumKeyword, *typeSpec));
        }
        else
        {
            Lexer::TokenType tokenType;
            switch (cld::get<PrimitiveTypeSpecifier>(typeSpec->getVariant()))
            {
                case PrimitiveTypeSpecifier::Void: tokenType = Lexer::TokenType::VoidKeyword; break;
                case PrimitiveTypeSpecifier::Char: tokenType = Lexer::TokenType::CharKeyword; break;
                case PrimitiveTypeSpecifier::Short: tokenType = Lexer::TokenType::ShortKeyword; break;
                case PrimitiveTypeSpecifier::Int: tokenType = Lexer::TokenType::IntKeyword; break;
                case PrimitiveTypeSpecifier::Long: tokenType = Lexer::TokenType::LongKeyword; break;
                case PrimitiveTypeSpecifier::Float: tokenType = Lexer::TokenType::FloatKeyword; break;
                case PrimitiveTypeSpecifier::Double: tokenType = Lexer::TokenType::DoubleKeyword; break;
                case PrimitiveTypeSpecifier::Signed: tokenType = Lexer::TokenType::SignedKeyword; break;
                case PrimitiveTypeSpecifier::Unsigned: tokenType = Lexer::TokenType::UnsignedKeyword; break;
                case PrimitiveTypeSpecifier::Bool: tokenType = Lexer::TokenType::UnderlineBool; break;
            }
            log(Errors::Semantics::CANNOT_COMBINE_N_WITH_N.args(*typeSpec, m_sourceInterface, type, tokenType,
                                                                *typeSpec));
        }
    };

    using BitSet = Bitset2::bitset2<11>;
    const auto table = [&]() -> std::unordered_map<BitSet, std::pair<BitSet, Type>> {
        using Tuple = std::tuple<std::underlying_type_t<PrimitiveTypeSpecifier>,
                                 std::underlying_type_t<PrimitiveTypeSpecifier>, Type>;
        std::vector<Tuple> temp = {
            {PrimitiveTypeSpecifier::Void, 0, PrimitiveType::createVoid(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Char, PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::createChar(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Char + PrimitiveTypeSpecifier::Signed, 0,
             PrimitiveType::createSignedChar(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Char + PrimitiveTypeSpecifier::Unsigned, 0,
             PrimitiveType::createUnsignedChar(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Float, 0, PrimitiveType::createFloat(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Bool, 0, PrimitiveType::createUnderlineBool(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Double, PrimitiveTypeSpecifier::Long,
             PrimitiveType::createDouble(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Double + PrimitiveTypeSpecifier::Long, 0,
             PrimitiveType::createLongDouble(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Int,
             PrimitiveTypeSpecifier::Long | PrimitiveTypeSpecifier::Short | PrimitiveTypeSpecifier::Signed
                 | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::createInt(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long,
             PrimitiveTypeSpecifier::Long | PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::createLong(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long,
             PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::createLongLong(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long
                 + PrimitiveTypeSpecifier::Signed,
             0, PrimitiveType::createLongLong(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long
                 + PrimitiveTypeSpecifier::Unsigned,
             0, PrimitiveType::createUnsignedLongLong(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Signed,
             PrimitiveTypeSpecifier::Long,
             PrimitiveType::createLong(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Unsigned,
             PrimitiveTypeSpecifier::Long,
             PrimitiveType::createUnsignedLong(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Short,
             PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::createShort(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Short + PrimitiveTypeSpecifier::Signed, 0,
             PrimitiveType::createShort(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Short + PrimitiveTypeSpecifier::Unsigned, 0,
             PrimitiveType::createUnsignedShort(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Signed,
             PrimitiveTypeSpecifier ::Long | PrimitiveTypeSpecifier::Short,
             PrimitiveType::createInt(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Unsigned,
             PrimitiveTypeSpecifier ::Long | PrimitiveTypeSpecifier::Short,
             PrimitiveType::createUnsignedInt(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Short,
             PrimitiveTypeSpecifier ::Int | PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::createShort(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Short + PrimitiveTypeSpecifier::Signed, PrimitiveTypeSpecifier ::Int,
             PrimitiveType::createShort(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Short + PrimitiveTypeSpecifier::Unsigned, PrimitiveTypeSpecifier ::Int,
             PrimitiveType::createUnsignedShort(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Long,
             PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Long | PrimitiveTypeSpecifier::Signed
                 | PrimitiveTypeSpecifier::Unsigned | PrimitiveTypeSpecifier::Double,
             PrimitiveType::createLong(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Signed,
             PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Long,
             PrimitiveType::createLong(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Unsigned,
             PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Long,
             PrimitiveType::createUnsignedLong(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long,
             PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::createLongLong(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Signed,
             PrimitiveTypeSpecifier::Int, PrimitiveType::createLongLong(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Unsigned,
             PrimitiveTypeSpecifier::Int, PrimitiveType::createUnsignedLongLong(isConst, isVolatile)},
            {PrimitiveTypeSpecifier::Signed,
             PrimitiveTypeSpecifier::Short | PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Char
                 | PrimitiveTypeSpecifier::Long,
             PrimitiveType::createInt(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
            {PrimitiveTypeSpecifier::Unsigned,
             PrimitiveTypeSpecifier::Short | PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Char
                 | PrimitiveTypeSpecifier::Long,
             PrimitiveType::createUnsignedInt(isConst, isVolatile, m_sourceInterface.getLanguageOptions())},
        };
        std::unordered_map<BitSet, std::pair<BitSet, Type>> result;
        for (auto& [state, next, type] : temp)
        {
            result.insert({BitSet(state), {BitSet(next), std::move(type)}});
        }
        return result;
    }();

    auto primTypeSpecToString = [](PrimitiveTypeSpecifier spec) -> std::string_view {
        switch (spec)
        {
            case Syntax::TypeSpecifier::Void: return "void";
            case Syntax::TypeSpecifier::Char: return "char";
            case Syntax::TypeSpecifier::Short: return "short";
            case Syntax::TypeSpecifier::Int: return "int";
            case Syntax::TypeSpecifier::Long: return "long";
            case Syntax::TypeSpecifier::Float: return "float";
            case Syntax::TypeSpecifier::Double: return "double";
            case Syntax::TypeSpecifier::Signed: return "signed";
            case Syntax::TypeSpecifier::Unsigned: return "unsigned";
            case Syntax::TypeSpecifier::Bool: return "_Bool";
        }
        CLD_UNREACHABLE;
    };

    std::string text = "'";
    text += primTypeSpecToString(cld::get<PrimitiveTypeSpecifier>(typeSpecs[0]->getVariant()));
    BitSet type(cld::get<PrimitiveTypeSpecifier>(typeSpecs[0]->getVariant()));
    for (auto& iter : llvm::ArrayRef(typeSpecs).drop_front())
    {
        CLD_ASSERT(iter);
        auto* typeSpec = std::get_if<PrimitiveTypeSpecifier>(&iter->getVariant());
        if (!typeSpec)
        {
            excessSpecifiersError(text + "'", iter);
            auto result = table.find(type);
            CLD_ASSERT(result != table.end());
            return result->second.second;
        }
        auto result = table.find(type);
        CLD_ASSERT(result != table.end());
        if ((result->second.first & BitSet(*typeSpec)).any())
        {
            type += BitSet(*typeSpec);
            text += " ";
            text += primTypeSpecToString(*typeSpec);
            continue;
        }
        excessSpecifiersError(text + "'", iter);
        return result->second.second;
    }
    auto result = table.find(type);
    CLD_ASSERT(result != table.end());
    return result->second.second;
}

bool cld::Semantics::SemanticAnalysis::isCompleteType(const Type& type) const
{
    if (isVoid(type))
    {
        return false;
    }
    if (std::holds_alternative<AbstractArrayType>(type.get()))
    {
        return false;
    }
    if (std::holds_alternative<EnumType>(type.get()))
    {
        auto& enumType = cld::get<EnumType>(type.get());
        return lookupType(enumType.getName(), enumType.getScope());
    }
    if (std::holds_alternative<StructType>(type.get()))
    {
        auto& recordType = cld::get<StructType>(type.get());
        return lookupType(recordType.getName(), recordType.getScope());
    }
    return true;
}

bool cld::Semantics::SemanticAnalysis::typesAreCompatible(const cld::Semantics::Type& lhs,
                                                          const cld::Semantics::Type& rhs) const
{
    if (lhs.isUndefined() || rhs.isUndefined())
    {
        return true;
    }
    // C99 6.7.3§9: For two qualified types to be compatible, both shall have the identically qualified version
    // of a compatible type; the order of type qualifiers within a list of specifiers or qualifiers
    // does not affect the specified type.
    if (std::tuple(lhs.isConst(), lhs.isVolatile()) != std::tuple(rhs.isConst(), rhs.isVolatile()))
    {
        return false;
    }
    if (isArray(lhs) && isArray(rhs))
    {
        const auto& lhsType = cld::match(lhs.get(), [](auto&& value) -> const Type& {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<ArrayType,
                                         T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
            {
                return value.getType();
            }
            CLD_UNREACHABLE;
        });
        const auto& rhsType = cld::match(rhs.get(), [](auto&& value) -> const Type& {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<ArrayType,
                                         T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
            {
                return value.getType();
            }
            CLD_UNREACHABLE;
        });
        if (!typesAreCompatible(lhsType, rhsType))
        {
            return false;
        }
        if (!std::holds_alternative<ArrayType>(lhs.get()) || !std::holds_alternative<ArrayType>(rhs.get()))
        {
            return true;
        }
        return cld::get<ArrayType>(lhs.get()).getSize() == cld::get<ArrayType>(rhs.get()).getSize();
    }
    if (lhs.get().index() != rhs.get().index())
    {
        return false;
    }
    if (std::holds_alternative<PointerType>(lhs.get()))
    {
        auto& lhsType = cld::get<PointerType>(lhs.get());
        auto& rhsType = cld::get<PointerType>(rhs.get());
        if (lhsType.isRestricted() != rhsType.isRestricted())
        {
            return false;
        }
        return typesAreCompatible(lhsType.getElementType(), rhsType.getElementType());
    }
    if (std::holds_alternative<FunctionType>(lhs.get()))
    {
        // C99 6.7.5.3§15:
        // (In the determination of type
        // compatibility and of a composite type, each parameter declared with function or array
        // type is taken as having the adjusted type and each parameter declared with qualified type
        // is taken as having the unqualified version of its declared type.)
        auto& lhsFtype = cld::get<FunctionType>(lhs.get());
        auto& rhsFtype = cld::get<FunctionType>(rhs.get());
        if (!typesAreCompatible(lhsFtype.getReturnType(), rhsFtype.getReturnType()))
        {
            return false;
        }
        if (lhsFtype.isKandR() || rhsFtype.isKandR())
        {
            if (lhsFtype.isKandR() && rhsFtype.isKandR())
            {
                // This case isn't even mentioned?
                // But it's kinda not possible either with the exception
                // of a pre declaration using an empty identifier list so true should be correct
                return true;
            }
            auto& kandRFunc = lhsFtype.isKandR() ? lhsFtype : rhsFtype;
            auto& paramFunc = lhsFtype.isKandR() ? rhsFtype : lhsFtype;
            // TODO: empty k&r func needs to know if it's from a definition or a declaration
            if (kandRFunc.getArguments().empty())
            {
                // C99 6.7.5.3§15:
                // If one type has a parameter type list and the other type is specified by a
                // function declarator that is not part of a function definition and that contains an empty
                // identifier list, the parameter list shall not have an ellipsis terminator and the type of each
                // parameter shall be compatible with the type that results from the application of the
                // default argument promotions
                if (paramFunc.isLastVararg())
                {
                    return false;
                }
                for (auto& iter : paramFunc.getArguments())
                {
                    auto nonQualifiedType = Type(false, false, cld::to_string(iter.first.getName()), iter.first.get());
                    auto ret = defaultArgumentPromotion(nonQualifiedType);
                    if (!typesAreCompatible(nonQualifiedType, ret))
                    {
                        return false;
                    }
                }
                return true;
            }
            // C99 6.7.5.3§15:
            // If one type has a parameter type list and the other type is
            // specified by a function definition that contains a (possibly empty) identifier list, both shall
            // agree in the number of parameters, and the type of each prototype parameter shall be
            // compatible with the type that results from the application of the default argument
            // promotions to the type of the corresponding identifier
            if (kandRFunc.getArguments().size() != paramFunc.getArguments().size())
            {
                return false;
            }
            for (std::size_t i = 0; i < kandRFunc.getArguments().size(); i++)
            {
                auto kandRType = adjustParameterType(kandRFunc.getArguments()[i].first);
                auto paramType = adjustParameterType(paramFunc.getArguments()[i].first);
                auto nonQualifiedkandR = Type(false, false, cld::to_string(kandRType.getName()), kandRType.get());
                auto nonQualifiedParam = Type(false, false, cld::to_string(paramType.getName()), paramType.get());
                if (!typesAreCompatible(defaultArgumentPromotion(nonQualifiedkandR),
                                        defaultArgumentPromotion(nonQualifiedParam)))
                {
                    return false;
                }
            }
            return true;
        }
        // C99 6.7.5.3§15:
        // Moreover, the parameter type lists, if both are present, shall agree in the number of
        // parameters and in use of the ellipsis terminator; corresponding parameters shall have
        // compatible types.
        if (lhsFtype.getArguments().size() != rhsFtype.getArguments().size())
        {
            return false;
        }
        if (lhsFtype.isLastVararg() != rhsFtype.isLastVararg())
        {
            return false;
        }
        for (std::size_t i = 0; i < lhsFtype.getArguments().size(); i++)
        {
            auto lhsType = adjustParameterType(lhsFtype.getArguments()[i].first);
            auto rhsType = adjustParameterType(rhsFtype.getArguments()[i].first);
            auto nonQualifiedLhs = Type(false, false, cld::to_string(lhsType.getName()), lhsType.get());
            auto nonQualifiedRhs = Type(false, false, cld::to_string(rhsType.getName()), rhsType.get());
            if (!typesAreCompatible(nonQualifiedLhs, nonQualifiedRhs))
            {
                return false;
            }
        }
        return true;
    }
    return lhs == rhs;
}

cld::Expected<size_t, cld::Message> cld::Semantics::SemanticAnalysis::sizeOf(const Type& type,
                                                                             llvm::ArrayRef<Lexer::CToken> loc) const
{
    using RetType = Expected<std::size_t, Message>;
    return match(
        type.get(), [](const PrimitiveType& primitiveType) -> RetType { return primitiveType.getByteCount(); },
        [&](const ArrayType& arrayType) -> RetType {
            auto result = sizeOf(arrayType.getType(), loc);
            if (!result)
            {
                return result;
            }
            return *result * arrayType.getSize();
        },
        [&](const AbstractArrayType&) -> RetType {
            return Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(loc, m_sourceInterface, type, loc);
        },
        [&](const ValArrayType&) -> RetType {
            return Errors::Semantics::SIZEOF_VAL_ARRAY_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION.args(
                loc, m_sourceInterface, loc);
        },
        [&](const FunctionType&) -> RetType {
            return Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF.args(loc, m_sourceInterface, loc);
        },
        [&](const StructType& structType) -> RetType {
            const auto* result = lookupType(structType.getName(), structType.getScope());
            if (!result || !std::holds_alternative<StructDefinition>(*result))
            {
                return Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(loc, m_sourceInterface, type, loc);
            }
            auto& recDef = cld::get<StructDefinition>(*result);
            std::size_t currentSize = 0;
            for (auto& [type, name, bits] : recDef.getFields())
            {
                (void)name;
                (void)bits;
                // TODO: Bitfield
                auto alignment = alignOf(*type, loc);
                CLD_ASSERT(alignment);
                auto rest = currentSize % *alignment;
                if (rest != 0)
                {
                    currentSize += *alignment - rest;
                }
                auto subSize = sizeOf(*type, loc);
                CLD_ASSERT(subSize);
                currentSize += *subSize;
            }
            return currentSize;
        },
        [&](const UnionType& unionType) -> RetType {
            const auto* result = lookupType(unionType.getName(), unionType.getScope());
            if (!result || !std::holds_alternative<UnionDefinition>(*result))
            {
                return Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(loc, m_sourceInterface, type, loc);
            }
            auto& recDef = cld::get<UnionDefinition>(*result);
            std::size_t maxSize = 0;
            for (auto& [type, name, bits] : recDef.getFields())
            {
                (void)name;
                (void)bits;
                // TODO: Bitfield
                auto subSize = sizeOf(*type, loc);
                CLD_ASSERT(subSize); // A struct must always have a valid size otherwise we shouldn't even have a
                // StructType
                maxSize = std::max(maxSize, *subSize);
            }
            return maxSize;
        },
        [&](const AnonymousStructType& structType) -> RetType {
            std::size_t currentSize = 0;
            for (auto& [type, name, bits] : structType.getFields())
            {
                (void)name;
                (void)bits;
                // TODO: Bitfield
                auto alignment = alignOf(*type, loc);
                CLD_ASSERT(alignment);
                auto rest = currentSize % *alignment;
                if (rest != 0)
                {
                    currentSize += *alignment - rest;
                }
                auto subSize = sizeOf(*type, loc);
                CLD_ASSERT(subSize);
                currentSize += *subSize;
            }
            return currentSize;
        },
        [&](const AnonymousUnionType& unionType) -> RetType {
            std::size_t maxSize = 0;
            for (auto& [type, name, bits] : unionType.getFields())
            {
                (void)name;
                (void)bits;
                // TODO: Bitfield
                auto subSize = sizeOf(*type, loc);
                CLD_ASSERT(subSize); // A struct must always have a valid size otherwise we shouldn't even have a
                // StructType
                maxSize = std::max(maxSize, *subSize);
            }
            return maxSize;
        },
        [&](const AnonymousEnumType& enumType) -> RetType { return sizeOf(enumType.getType(), loc); },
        [&](const EnumType& enumType) -> RetType {
            const auto* result = lookupType(enumType.getName(), enumType.getScope());
            if (!result || std::holds_alternative<EnumDefinition>(*result))
            {
                return Errors::Semantics::INCOMPLETE_TYPE_N_IN_SIZE_OF.args(loc, m_sourceInterface, type, loc);
            }
            auto& enumDef = cld::get<EnumDefinition>(*result);
            return sizeOf(enumDef.getType(), loc);
        },
        [&](const PointerType&) -> RetType {
            return std::size_t{m_sourceInterface.getLanguageOptions().sizeOfVoidStar};
        },
        [](std::monostate) -> RetType { CLD_UNREACHABLE; });
}

cld::Expected<size_t, cld::Message> cld::Semantics::SemanticAnalysis::alignOf(const cld::Semantics::Type& type,
                                                                              llvm::ArrayRef<Lexer::CToken> loc) const
{
    using RetType = Expected<std::size_t, Message>;
    return match(
        type.get(), [](const PrimitiveType& primitiveType) -> RetType { return primitiveType.getByteCount(); },
        [&](const ArrayType& arrayType) -> RetType { return alignOf(arrayType.getType(), loc); },
        [&](const AbstractArrayType&) -> RetType {
            return Errors::Semantics::INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF.args(loc, m_sourceInterface, type, loc);
        },
        [&](const ValArrayType& arrayType) -> RetType { return alignOf(arrayType.getType(), loc); },
        [&](const FunctionType&) -> RetType {
            return Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_ALIGNMENT_OF.args(loc, m_sourceInterface, loc);
        },
        [&](const StructType& recordType) -> RetType {
            const auto* result = lookupType(recordType.getName(), recordType.getScope());
            if (!result || !std::holds_alternative<StructDefinition>(*result))
            {
                return Errors::Semantics::INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF.args(loc, m_sourceInterface, type, loc);
            }
            auto& recDef = cld::get<StructDefinition>(*result);
            std::size_t currentAlignment = 0;
            for (auto& [type, name, bits] : recDef.getFields())
            {
                (void)name;
                (void)bits;
                // TODO: Bitfield
                auto alignment = alignOf(*type, loc);
                CLD_ASSERT(alignment);
                currentAlignment = std::max(currentAlignment, *alignment);
            }
            return currentAlignment;
        },
        [&](const UnionType& unionType) -> RetType {
            const auto* result = lookupType(unionType.getName(), unionType.getScope());
            if (!result || !std::holds_alternative<UnionDefinition>(*result))
            {
                return Errors::Semantics::INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF.args(loc, m_sourceInterface, type, loc);
            }
            auto& recDef = cld::get<UnionDefinition>(*result);
            auto maxElement = std::max_element(recDef.getFields().begin(), recDef.getFields().end(),
                                               [&](const Field& lhs, const Field& rhs) {
                                                   auto lhsSize = sizeOf(*lhs.type, loc);
                                                   CLD_ASSERT(lhsSize);
                                                   auto rhsSize = sizeOf(*rhs.type, loc);
                                                   CLD_ASSERT(rhsSize);
                                                   return *lhsSize < *rhsSize;
                                               });
            return alignOf(*maxElement->type, loc);
        },
        [&](const AnonymousStructType& structType) -> RetType {
            std::size_t currentAlignment = 0;
            for (auto& [type, name, bits] : structType.getFields())
            {
                (void)name;
                (void)bits;
                // TODO: Bitfield
                auto alignment = alignOf(*type, loc);
                CLD_ASSERT(alignment);
                currentAlignment = std::max(currentAlignment, *alignment);
            }
            return currentAlignment;
        },
        [&](const AnonymousUnionType& unionType) {
            auto maxElement = std::max_element(unionType.getFields().begin(), unionType.getFields().end(),
                                               [&](const Field& lhs, const Field& rhs) {
                                                   auto lhsSize = sizeOf(*lhs.type, loc);
                                                   CLD_ASSERT(lhsSize);
                                                   auto rhsSize = sizeOf(*rhs.type, loc);
                                                   CLD_ASSERT(rhsSize);
                                                   return *lhsSize < *rhsSize;
                                               });
            return alignOf(*maxElement->type, loc);
        },
        [&](const AnonymousEnumType& enumType) -> RetType { return sizeOf(enumType.getType(), loc); },
        [&](const EnumType& enumType) -> RetType {
            const auto* result = lookupType(enumType.getName(), enumType.getScope());
            if (!result || std::holds_alternative<EnumDefinition>(*result))
            {
                return Errors::Semantics::INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF.args(loc, m_sourceInterface, type, loc);
            }
            auto& enumDef = cld::get<EnumDefinition>(*result);
            return alignOf(enumDef.getType(), loc);
        },
        [&](const PointerType&) -> RetType {
            return std::size_t{m_sourceInterface.getLanguageOptions().sizeOfVoidStar};
        },
        [](std::monostate) -> RetType { CLD_UNREACHABLE; });
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::defaultArgumentPromotion(const cld::Semantics::Type& type) const
{
    if (!std::holds_alternative<PrimitiveType>(type.get()))
    {
        return type;
    }
    auto& prim = cld::get<PrimitiveType>(type.get());
    if (prim.isFloatingPoint())
    {
        if (prim.getBitCount() == 32)
        {
            return PrimitiveType::createDouble(type.isConst(), type.isVolatile());
        }
        return type;
    }
    return integerPromotion(type);
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::integerPromotion(const Type& type) const
{
    if (!std::holds_alternative<PrimitiveType>(type.get()))
    {
        return type;
    }
    auto& prim = cld::get<PrimitiveType>(type.get());
    if (prim.isFloatingPoint())
    {
        return type;
    }
    if (prim.getBitCount() == 0)
    {
        return type;
    }
    if (prim.getBitCount() < m_sourceInterface.getLanguageOptions().sizeOfInt * 8)
    {
        return PrimitiveType::createInt(type.isConst(), type.isVolatile(), m_sourceInterface.getLanguageOptions());
    }
    return type;
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::adjustParameterType(const cld::Semantics::Type& type) const
{
    if (std::holds_alternative<ArrayType>(type.get()) || std::holds_alternative<AbstractArrayType>(type.get())
        || std::holds_alternative<ValArrayType>(type.get()))
    {
        auto elementType = cld::match(type.get(), [](auto&& value) -> Type {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<ArrayType,
                                         T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
            {
                return value.getType();
            }
            CLD_UNREACHABLE;
        });
        bool restrict = cld::match(type.get(), [](auto&& value) -> bool {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<ArrayType,
                                         T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
            {
                return value.isRestricted();
            }
            CLD_UNREACHABLE;
        });
        return PointerType::create(type.isConst(), type.isVolatile(), restrict, std::move(elementType));
    }
    return type;
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::compositeType(const cld::Semantics::Type& lhs,
                                                                     const cld::Semantics::Type& rhs) const
{
    if (isArray(lhs) || isArray(rhs))
    {
        auto getElementType = [](const Type& type) -> const Type& {
            return cld::match(type.get(), [](auto&& value) -> const Type& {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<
                                  ArrayType,
                                  T> || std::is_same_v<ValArrayType, T> || std::is_same_v<AbstractArrayType, T>)
                {
                    return value.getType();
                }
                else if constexpr (std::is_same_v<PointerType, T>)
                {
                    return value.getElementType();
                }
                CLD_UNREACHABLE;
            });
        };
        if (auto* array = std::get_if<ArrayType>(&lhs.get()))
        {
            return ArrayType::create(lhs.isConst(), lhs.isVolatile(), array->isRestricted(), array->isStatic(),
                                     compositeType(array->getType(), getElementType(rhs)), array->getSize());
        }
        if (auto* array = std::get_if<ArrayType>(&rhs.get()))
        {
            return ArrayType::create(rhs.isConst(), rhs.isVolatile(), array->isRestricted(), array->isStatic(),
                                     compositeType(array->getType(), getElementType(lhs)), array->getSize());
        }
        if (auto* valArray = std::get_if<ValArrayType>(&lhs.get()))
        {
            return ValArrayType::create(lhs.isConst(), lhs.isVolatile(), valArray->isRestricted(), valArray->isStatic(),
                                        compositeType(valArray->getType(), getElementType(rhs)));
        }
        if (auto* valArray = std::get_if<ValArrayType>(&rhs.get()))
        {
            return ValArrayType::create(rhs.isConst(), rhs.isVolatile(), valArray->isRestricted(), valArray->isStatic(),
                                        compositeType(valArray->getType(), getElementType(lhs)));
        }
        auto& abstractArray = std::holds_alternative<AbstractArrayType>(lhs.get()) ? lhs : rhs;
        return AbstractArrayType::create(rhs.isConst(), rhs.isVolatile(),
                                         cld::get<AbstractArrayType>(abstractArray.get()).isRestricted(),
                                         compositeType(getElementType(lhs), getElementType(rhs)));
    }
    if (std::holds_alternative<FunctionType>(lhs.get()))
    {
        auto& lhsFtype = cld::get<FunctionType>(lhs.get());
        auto& rhsFtype = cld::get<FunctionType>(rhs.get());
        if (lhsFtype.isKandR() && !rhsFtype.isKandR())
        {
            return rhs;
        }
        if (!lhsFtype.isKandR() && rhsFtype.isKandR())
        {
            return lhs;
        }
        if (lhsFtype.isKandR() && rhsFtype.isKandR())
        {
            return rhs;
        }
        std::vector<std::pair<Type, std::string>> parameters;
        for (std::size_t i = 0; i < rhsFtype.getArguments().size(); i++)
        {
            parameters.emplace_back(compositeType(lhsFtype.getArguments()[i].first, rhsFtype.getArguments()[i].first),
                                    rhsFtype.getArguments()[i].second);
        }
        return FunctionType::create(compositeType(lhsFtype.getReturnType(), rhsFtype.getReturnType()),
                                    std::move(parameters), rhsFtype.isLastVararg(), false);
    }
    if (std::holds_alternative<PointerType>(lhs.get()))
    {
        return PointerType::create(rhs.isConst(), rhs.isVolatile(), cld::get<PointerType>(rhs.get()).isRestricted(),
                                   compositeType(cld::get<PointerType>(lhs.get()).getElementType(),
                                                 cld::get<PointerType>(rhs.get()).getElementType()));
    }
    return rhs;
}
