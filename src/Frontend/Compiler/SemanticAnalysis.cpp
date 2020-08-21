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

#include "ConstValue.hpp"
#include "ErrorMessages.hpp"
#include "SemanticUtil.hpp"

bool cld::Semantics::SemanticAnalysis::log(const Message& message)
{
    if (m_reporter)
    {
        *m_reporter << message;
    }
    return message.getSeverity() != Severity::None;
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
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        if (!std::holds_alternative<Syntax::StorageClassSpecifier>(iter))
        {
            continue;
        }
        auto& storage = cld::get<Syntax::StorageClassSpecifier>(iter);
        if (storage.getSpecifier() != Syntax::StorageClassSpecifier::Extern
            && storage.getSpecifier() != Syntax::StorageClassSpecifier::Static)
        {
            log(Errors::Semantics::ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION.args(storage, m_sourceInterface,
                                                                                             storage));
            continue;
        }
        storageClassSpecifier = &storage;
    }
    auto scope = pushScope();
    auto type = declaratorsToType(node.getDeclarationSpecifiers(), node.getDeclarator(), node.getDeclarations(), true);
    if (!std::holds_alternative<FunctionType>(type.get()))
    {
        log(Errors::Semantics::FUNCTION_DEFINITION_MUST_HAVE_FUNCTION_TYPE.args(
            std::forward_as_tuple(node.getDeclarationSpecifiers(), node.getDeclarator()), m_sourceInterface,
            std::forward_as_tuple(node.getDeclarationSpecifiers(), node.getDeclarator()), type));
        visit(node.getCompoundStatement());
        return {};
    }

    auto& ft = cld::get<FunctionType>(type.get());
    if (!isVoid(ft.getReturnType()) && !isCompleteType(ft.getReturnType()))
    {
        log(Errors::Semantics::RETURN_TYPE_OF_FUNCTION_DEFINITION_MUST_BE_A_COMPLETE_TYPE.args(
            node.getDeclarationSpecifiers(), m_sourceInterface, node.getDeclarationSpecifiers()));
    }
    const Syntax::DirectDeclaratorParenthesesParameters* parameters = nullptr;
    const Syntax::DirectDeclaratorParenthesesIdentifiers* identifierList = nullptr;

    for (auto& iter : RecursiveVisitor(node.getDeclarator().getDirectDeclarator(), DIRECT_DECL_NEXT_FN))
    {
        cld::match(
            iter,
            [&](const Syntax::DirectDeclaratorParenthesesParameters& dd) {
                parameters = &dd;
                identifierList = nullptr;
            },
            [&](const Syntax::DirectDeclaratorParenthesesIdentifiers& dd) {
                parameters = nullptr;
                identifierList = &dd;
            },
            [](const Syntax::DirectDeclaratorIdentifier&) {},
            [&](const Syntax::DirectDeclaratorParentheses& parentheses) {
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
    if (!parameters && !identifierList)
    {
        log(Errors::Semantics::FUNCTION_DEFINITION_MUST_HAVE_A_PARAMETER_LIST.args(
            std::forward_as_tuple(node.getDeclarationSpecifiers(), node.getDeclarator()), m_sourceInterface,
            std::forward_as_tuple(node.getDeclarationSpecifiers(), node.getDeclarator())));
        visit(node.getCompoundStatement());
        return {};
    }

    std::vector<std::unique_ptr<Declaration>> parameterDeclarations;
    if (parameters)
    {
        if (!node.getDeclarations().empty())
        {
            log(Errors::Semantics::FUNCTION_DEFINITION_WITH_A_PARAMETER_LIST_MUST_NOT_HAVE_DECLARATIONS_FOLLOWING_IT
                    .args(node.getDeclarations(), m_sourceInterface, node.getDeclarations()));
        }
        const auto& parameterSyntaxDecls = parameters->getParameterTypeList().getParameters();
        // Parameters that have some kind of error in their declaration should still be inserted in the function
        // type except when parameterSyntaxDecls is 0
        CLD_ASSERT(parameterSyntaxDecls.size() == ft.getArguments().size() || ft.getArguments().size() == 0);
        for (std::size_t i = 0; i < ft.getArguments().size(); i++)
        {
            // Assured by the parser which should error otherwise
            CLD_ASSERT(std::holds_alternative<std::unique_ptr<Syntax::Declarator>>(parameterSyntaxDecls[i].declarator));
            auto& declarator = *cld::get<std::unique_ptr<Syntax::Declarator>>(parameterSyntaxDecls[i].declarator);
            auto* loc = declaratorToLoc(declarator);
            Lifetime lifetime = Lifetime ::Automatic;
            for (auto& iter : parameterSyntaxDecls[i].declarationSpecifiers)
            {
                if (!std::holds_alternative<Syntax::StorageClassSpecifier>(iter))
                {
                    continue;
                }
                if (cld::get<Syntax::StorageClassSpecifier>(iter).getSpecifier()
                    == Syntax::StorageClassSpecifier::Register)
                {
                    lifetime = Lifetime ::Register;
                }
            }
            auto paramType = ft.getArguments()[i].first;
            auto& ptr = parameterDeclarations.emplace_back(
                std::make_unique<Declaration>(std::move(paramType), Linkage::None, lifetime, loc));
            auto [prev, notRedefined] =
                getCurrentScope().declarations.insert({loc->getText(), DeclarationInScope{loc, ptr.get()}});
            if (!notRedefined)
            {
                log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
        }
    }
    else
    {
        std::unordered_map<std::string_view, Type> parameterNameToType;
        for (auto& [type, name] : ft.getArguments())
        {
            parameterNameToType.emplace(name, type);
        }
        for (auto& iter : node.getDeclarations())
        {
            Lifetime lifetime = Lifetime::Automatic;
            for (auto& iter2 : iter.getDeclarationSpecifiers())
            {
                if (!std::holds_alternative<Syntax::StorageClassSpecifier>(iter2))
                {
                    continue;
                }
                if (cld::get<Syntax::StorageClassSpecifier>(iter2).getSpecifier()
                    == Syntax::StorageClassSpecifier::Register)
                {
                    lifetime = Lifetime::Register;
                }
            }
            for (auto& [decl, init] : iter.getInitDeclarators())
            {
                (void)init;
                if (!decl)
                {
                    continue;
                }
                const auto* loc = declaratorToLoc(*decl);
                auto result = parameterNameToType.find(loc->getText());
                if (result == parameterNameToType.end())
                {
                    continue;
                }
                auto& ptr = parameterDeclarations.emplace_back(
                    std::make_unique<Declaration>(result->second, Linkage::None, lifetime, loc));
                getCurrentScope().declarations.insert({loc->getText(), DeclarationInScope{loc, ptr.get()}});
                // Don't check for duplicates again. We already did that when processing the identifier list
            }
        }
    }

    const auto* loc = declaratorToLoc(node.getDeclarator());
    std::vector<TranslationUnit::Variant> result;
    auto& ptr = cld::get<std::unique_ptr<FunctionDefinition>>(result.emplace_back(std::make_unique<FunctionDefinition>(
        std::move(type), loc, std::move(parameterDeclarations),
        storageClassSpecifier ?
            (storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static ? Linkage::Internal :
                                                                                              Linkage::External) :
            Linkage::External,
        CompoundStatement({}))));
    // We are currently in block scope. Functions are always at file scope though so we can't use getCurrentScope
    auto [prev, notRedefinition] =
        m_scopes[0].declarations.insert({loc->getText(), DeclarationInScope{loc, ptr.get()}});
    if (!notRedefinition)
    {
        if (!std::holds_alternative<const Declaration*>(prev->second.declared)
            || !typesAreCompatible(ptr->getType(), cld::get<const Declaration*>(prev->second.declared)->getType(),
                                   true))
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                     *prev->second.identifier));
        }
        else
        {
            prev->second = DeclarationInScope{loc, ptr.get()};
        }
    }

    auto comp = visit(node.getCompoundStatement(), false);
    ptr->setCompoundStatement(std::move(comp));
    return result;
}

std::vector<cld::Semantics::TranslationUnit::Variant>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::Declaration& node)
{
    std::vector<TranslationUnit::Variant> decls;
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        if (!std::holds_alternative<Syntax::StorageClassSpecifier>(iter))
        {
            continue;
        }
        auto& storage = cld::get<Syntax::StorageClassSpecifier>(iter);
        if (!storageClassSpecifier)
        {
            storageClassSpecifier = &storage;
        }
        else
        {
            log(Errors::Semantics::ONLY_ONE_STORAGE_SPECIFIER.args(storage, m_sourceInterface, storage));
            log(Notes::PREVIOUS_STORAGE_SPECIFIER_HERE.args(*storageClassSpecifier, m_sourceInterface,
                                                            *storageClassSpecifier));
        }
        if (m_currentScope == 0
            && (storage.getSpecifier() == Syntax::StorageClassSpecifier::Auto
                || storage.getSpecifier() == Syntax::StorageClassSpecifier::Register))
        {
            if (storage.getSpecifier() == Syntax::StorageClassSpecifier::Auto)
            {
                log(Errors::Semantics::DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO.args(storage, m_sourceInterface,
                                                                                      storage));
            }
            else
            {
                log(Errors::Semantics::DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_REGISTER.args(storage, m_sourceInterface,
                                                                                          storage));
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
            auto declaration = std::make_unique<Declaration>(std::move(result), linkage, lifetime, loc);
            auto [prev, notRedefinition] =
                getCurrentScope().declarations.insert({loc->getText(), DeclarationInScope{loc, declaration.get()}});
            if (!notRedefinition)
            {
                if (std::holds_alternative<std::pair<ConstValue, Type>>(prev->second.declared)
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
                    declaration = std::make_unique<Declaration>(std::move(composite), linkage, lifetime, loc);
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
            if (linkage == Linkage::None && !isCompleteType(result)
                && !(std::holds_alternative<AbstractArrayType>(result.get()) && initializer))
            {
                log(Errors::Semantics::DECLARATION_MUST_HAVE_A_COMPLETE_TYPE.args(*loc, m_sourceInterface, *loc,
                                                                                  result));
                errors = true;
            }
            else if (isVoid(removeQualifiers(result)))
            {
                log(Errors::Semantics::DECLARATION_MUST_NOT_BE_VOID.args(*loc, m_sourceInterface, *loc));
                errors = true;
            }
            if (isVariablyModified(result))
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
            auto typeVisitor = RecursiveVisitor(result, ARRAY_TYPE_NEXT_FN);
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
                result.setName(loc->getText());
                auto [prev, noRedefinition] =
                    getCurrentScope().declarations.insert({loc->getText(), DeclarationInScope{loc, result}});
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
            auto declaration = std::make_unique<Declaration>(std::move(result), linkage, lifetime, loc);
            auto [prev, notRedefinition] =
                getCurrentScope().declarations.insert({loc->getText(), DeclarationInScope{loc, declaration.get()}});
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
                else
                {
                    auto& otherType = cld::get<const Declaration*>(prev->second.declared)->getType();
                    auto composite = compositeType(otherType, declaration->getType());
                    declaration = std::make_unique<Declaration>(std::move(composite), linkage, lifetime, loc);
                    prev->second.declared = declaration.get();
                }
            }
            else
            {
                if (initializer)
                {
                    std::size_t size = 0;
                    visit(*initializer, declaration->getType(), declaration->getLifetime() == Lifetime::Static, &size);
                    if (std::holds_alternative<AbstractArrayType>(declaration->getType().get()))
                    {
                        auto& prevType = declaration->getType();
                        declaration = std::make_unique<Declaration>(
                            ArrayType::create(prevType.isConst(), prevType.isVolatile(),
                                              cld::get<AbstractArrayType>(prevType.get()).isRestricted(), false,
                                              cld::get<AbstractArrayType>(prevType.get()).getType(), size),
                            linkage, lifetime, loc);
                        prev->second.declared = declaration.get();
                    }
                }
                decls.push_back(std::move(declaration));
            }
        }
    }
    return decls;
}

cld::Semantics::CompoundStatement cld::Semantics::SemanticAnalysis::visit(const Syntax::CompoundStatement& node,
                                                                          bool pushScope)
{
    std::optional<decltype(this->pushScope())> scope;
    if (pushScope)
    {
        scope.emplace(this->pushScope());
    }
    std::vector<CompoundStatement::Variant> result;
    for (auto& iter : node.getBlockItems())
    {
        auto tmp = visit(iter);
        result.insert(result.end(), std::move_iterator(tmp.begin()), std::move_iterator(tmp.end()));
    }
    return CompoundStatement(std::move(result));
}

std::vector<cld::Semantics::CompoundStatement::Variant>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::CompoundItem& node)
{
    std::vector<CompoundStatement::Variant> result;
    cld::match(
        node,
        [&](const Syntax::Declaration& declaration) {
            auto tmp = visit(declaration);
            result.reserve(result.size() + tmp.size());
            std::transform(std::move_iterator(tmp.begin()), std::move_iterator(tmp.end()), std::back_inserter(result),
                           [](TranslationUnit::Variant&& variant) {
                               CLD_ASSERT(std::holds_alternative<std::unique_ptr<Declaration>>(variant));
                               return cld::get<std::unique_ptr<Declaration>>(std::move(variant));
                           });
        },
        [&](const Syntax::Statement& statement) { result.emplace_back(visit(statement)); });
    return result;
}

cld::Semantics::Statement cld::Semantics::SemanticAnalysis::visit(const Syntax::Statement& node)
{
    return cld::match(
        node, [](const auto&) -> Statement { CLD_UNREACHABLE; },
        [&](const Syntax::ExpressionStatement& node) -> Statement {
            if (!node.getOptionalExpression())
            {
                return ExpressionStatement({});
            }
            return ExpressionStatement(visit(*node.getOptionalExpression()));
        });
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
    std::vector<std::pair<Type, std::string_view>> parameters;
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
        if (isVoid(paramType) && !std::holds_alternative<std::unique_ptr<cld::Syntax::Declarator>>(iter.declarator)
            && !cld::get<std::unique_ptr<cld::Syntax::AbstractDeclarator>>(iter.declarator)
            && parameterTypeList->getParameters().size() == 1 && !parameterTypeList->hasEllipse())
        {
            type = FunctionType::create(std::move(type), {}, false, false);
            return;
        }
        if (isVoid(removeQualifiers(paramType)))
        {
            log(Errors::Semantics::VOID_TYPE_NOT_ALLOWED_AS_FUNCTION_PARAMETER.args(
                iter.declarationSpecifiers, m_sourceInterface, iter.declarationSpecifiers));
            paramType = Type{};
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
        std::string_view name;
        if (std::holds_alternative<std::unique_ptr<Syntax::Declarator>>(iter.declarator))
        {
            name = declaratorToLoc(*cld::get<std::unique_ptr<Syntax::Declarator>>(iter.declarator))->getText();
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
    const PossiblyAbstractQualifierRef& declarator, const std::vector<Syntax::Declaration>& declarations,
    bool inFunctionDefinition)
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
    if (std::holds_alternative<const Syntax::Declarator * CLD_NON_NULL>(declarator))
    {
        bool isFunctionPrototype = false;
        const Syntax::DirectDeclaratorParenthesesIdentifiers* declarationsOwner = nullptr;
        auto& realDecl = *cld::get<const Syntax::Declarator*>(declarator);
        for (auto& iter : realDecl.getPointers())
        {
            auto [isConst, isVolatile, restricted] = getQualifiers(iter.getTypeQualifiers());
            type = PointerType::create(isConst, isVolatile, restricted, std::move(type));
        }
        for (auto& iter : RecursiveVisitor(realDecl.getDirectDeclarator(), DIRECT_DECL_NEXT_FN))
        {
            cld::match(
                iter,
                [&](const Syntax::DirectDeclaratorParenthesesIdentifiers& dd) {
                    declarationsOwner = &dd;
                    isFunctionPrototype = true;
                },
                [&](const Syntax::DirectDeclaratorParenthesesParameters&) { isFunctionPrototype = true; },
                [&](const Syntax::DirectDeclaratorParentheses& parentheses) {
                    if (!parentheses.getDeclarator().getPointers().empty())
                    {
                        isFunctionPrototype = false;
                    }
                },
                [](const Syntax::DirectDeclaratorIdentifier&) {}, [&](const auto&) { isFunctionPrototype = false; });
        }
        isFunctionPrototype = isFunctionPrototype && !inFunctionDefinition;
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
                handleArray(type, noStaticOrAsterisk.getTypeQualifiers(),
                            noStaticOrAsterisk.getAssignmentExpression().get(), false, false,
                            noStaticOrAsterisk.getDirectDeclarator());
            },
            [&](auto&& self, const Syntax::DirectDeclaratorStatic& declaratorStatic) {
                auto scope = llvm::make_scope_exit([&] { cld::match(declaratorStatic.getDirectDeclarator(), self); });
                handleArray(type, declaratorStatic.getTypeQualifiers(), &declaratorStatic.getAssignmentExpression(),
                            true, false, declaratorStatic.getDirectDeclarator());
            },
            [&](auto&& self, const Syntax::DirectDeclaratorAsterisk& asterisk) {
                auto scope = llvm::make_scope_exit([&] { cld::match(asterisk.getDirectDeclarator(), self); });
                handleArray(type, asterisk.getTypeQualifiers(), nullptr, false, true, asterisk.getDirectDeclarator());
            },
            [&](auto&& self, const Syntax::DirectDeclaratorParenthesesIdentifiers& identifiers) {
                auto scope = llvm::make_scope_exit([&] { cld::match(identifiers.getDirectDeclarator(), self); });
                std::optional<decltype(pushScope())> scope2;
                if (isFunctionPrototype)
                {
                    scope2.emplace(pushScope());
                }
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
                if (!inFunctionDefinition || declarationsOwner != &identifiers)
                {
                    log(Errors::Semantics::IDENTIFIER_LIST_ONLY_ALLOWED_AS_PART_OF_A_FUNCTION_DEFINITION.args(
                        identifiers.getIdentifiers(), m_sourceInterface, identifiers.getIdentifiers()));
                    type = FunctionType::create(std::move(type), {}, false, true);
                    return;
                }
                std::unordered_map<std::string_view, std::uint64_t> paramNames;
                std::vector<std::pair<Type, std::string_view>> parameters;
                std::unordered_map<std::string_view, Lexer::CTokenIterator> seenParameters;
                for (auto& iter : identifiers.getIdentifiers())
                {
                    auto name = iter->getText();
                    paramNames[name] = paramNames.size();
                    parameters.emplace_back(Type{}, iter->getText());
                    auto& loc = seenParameters[name];
                    if (loc != nullptr)
                    {
                        log(Errors::REDEFINITION_OF_SYMBOL_N.args(*iter, m_sourceInterface, *iter));
                        log(Notes::PREVIOUSLY_DECLARED_HERE.args(*loc, m_sourceInterface, *loc));
                    }
                    else
                    {
                        loc = iter;
                    }
                }
                for (auto& [first, second] : seenParameters)
                {
                    second = nullptr;
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
                    if (iter.getInitDeclarators().empty())
                    {
                        log(Errors::Semantics::DECLARATION_OF_IDENTIFIER_LIST_MUST_DECLARE_AT_LEAST_ONE_IDENTIFIER.args(
                            iter, m_sourceInterface, iter));
                    }
                    for (auto& [decl, init] : iter.getInitDeclarators())
                    {
                        if (init)
                        {
                            log(Errors::Semantics::DECLARATION_OF_IDENTIFIER_LIST_NOT_ALLOWED_TO_HAVE_AN_INITIALIZER
                                    .args(*init, m_sourceInterface, *init));
                        }
                        auto paramType = declaratorsToType(iter.getDeclarationSpecifiers(), *decl);
                        const auto* loc = declaratorToLoc(*decl);
                        auto result = paramNames.find(loc->getText());
                        if (result == paramNames.end())
                        {
                            log(Errors::Semantics::DECLARATION_OF_IDENTIFIER_LIST_NOT_BELONGING_TO_ANY_PARAMETER.args(
                                *loc, m_sourceInterface, *loc, identifiers.getIdentifiers()));
                            continue;
                        }
                        auto& element = seenParameters[loc->getText()];
                        if (element != nullptr)
                        {
                            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
                            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*element, m_sourceInterface, *element));
                        }
                        else
                        {
                            parameters[result->second].first = paramType;
                            element = loc;
                        }
                    }
                }
                for (auto& [name, loc] : seenParameters)
                {
                    if (!loc)
                    {
                        const auto* identifierLoc = identifiers.getIdentifiers()[paramNames[name]];
                        log(Errors::Semantics::PARAMETER_N_IN_IDENTIFIER_LIST_DOES_NOT_HAVE_A_MATCHING_DECLARATION.args(
                            *identifierLoc, m_sourceInterface, *identifierLoc));
                    }
                }
                type = FunctionType::create(std::move(type), std::move(parameters), false, true);
            },
            [&](auto&& self, const Syntax::DirectDeclaratorParenthesesParameters& parameterList) {
                auto scope = llvm::make_scope_exit([&] { cld::match(parameterList.getDirectDeclarator(), self); });
                std::optional<decltype(pushScope())> scope2;
                if (isFunctionPrototype)
                {
                    scope2.emplace(pushScope());
                }
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
                if (asterisk.getDirectAbstractDeclarator())
                {
                    handleArray(type, {}, nullptr, false, true, *asterisk.getDirectAbstractDeclarator());
                }
                else
                {
                    handleArray(type, {}, nullptr, false, true, declarationOrSpecifierQualifiers);
                }
            },
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorAssignmentExpression& expression) {
                auto scope = llvm::make_scope_exit([&] {
                    if (expression.getDirectAbstractDeclarator())
                    {
                        cld::match(*expression.getDirectAbstractDeclarator(), self);
                    }
                });
                if (expression.getDirectAbstractDeclarator())
                {
                    handleArray(type, {}, expression.getAssignmentExpression(), false, false,
                                *expression.getDirectAbstractDeclarator());
                }
                else
                {
                    handleArray(type, {}, expression.getAssignmentExpression(), false, false,
                                declarationOrSpecifierQualifiers);
                }
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
    if (auto* name = std::get_if<std::string_view>(&typeSpec[0]->getVariant()))
    {
        if (typeSpec.size() != 1)
        {
            log(Errors::Semantics::EXPECTED_NO_FURTHER_TYPE_SPECIFIERS_AFTER_TYPENAME.args(
                *typeSpec[1], m_sourceInterface, llvm::ArrayRef(typeSpec).drop_front()));
        }
        const auto* type = getTypedef(*name);
        CLD_ASSERT(type);
        auto ret = Type(isConst, isVolatile, type->get());
        if (std::tuple(type->isConst(), type->isVolatile()) == std::tuple(isConst, isVolatile))
        {
            ret.setName(type->getName());
        }
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
            auto name = structOrUnion->getIdentifierLoc()->getText();
            if (structOrUnion->isUnion())
            {
                std::uint64_t id;
                if (getUnionDefinition(name, m_currentScope | IS_SCOPE, &id))
                {
                    return UnionType::create(isConst, isVolatile, name, id);
                }
                auto [prev, notRedefined] = getCurrentScope().types.insert(
                    {name, TagTypeInScope{structOrUnion->getIdentifierLoc(), TagTypeInScope::UnionDecl{}}});
                if (!notRedefined && !std::holds_alternative<TagTypeInScope::UnionDecl>(prev->second.tagType)
                    && !std::holds_alternative<UnionDefTag>(prev->second.tagType))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion->getIdentifierLoc(), m_sourceInterface,
                                                              *structOrUnion->getIdentifierLoc()));
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
                return UnionType::create(isConst, isVolatile, name, m_currentScope | IS_SCOPE);
            }
            else
            {
                std::uint64_t id;
                if (getStructDefinition(name, m_currentScope | IS_SCOPE, &id))
                {
                    return StructType::create(isConst, isVolatile, name, id);
                }
                auto [prev, notRedefined] = getCurrentScope().types.insert(
                    {name, TagTypeInScope{structOrUnion->getIdentifierLoc(), TagTypeInScope::StructDecl{}}});
                if (!notRedefined && !std::holds_alternative<TagTypeInScope::StructDecl>(prev->second.tagType)
                    && !std::holds_alternative<StructDefTag>(prev->second.tagType))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion->getIdentifierLoc(), m_sourceInterface,
                                                              *structOrUnion->getIdentifierLoc()));
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
                return StructType::create(isConst, isVolatile, name, m_currentScope | IS_SCOPE);
            }
        }
        // Originally an iterator pointing at the std::unorderd_map was used here. But in the loops over the fields
        // an insertion into the types scope could be made which causes iterator invalidation
        // After that I instead used a pointer rehash of std::unorderd_map only causes iterator not reference
        // invalidation Then however I realised that below code can also cause reference invalidation as the scope can
        // be increased causing the move assignment on std::unordered_map which causes reference invalidation So now
        // instead we just note that our definition was valid and then do a insert_or_assign below
        bool definitionIsValid = true;
        if (structOrUnion->getIdentifierLoc())
        {
            auto name = structOrUnion->getIdentifierLoc()->getText();
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
                    definitionIsValid = false;
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
                    definitionIsValid = false;
                }
            }
        }
        std::vector<Field> fields;
        for (auto iter = structOrUnion->getStructDeclarations().begin();
             iter != structOrUnion->getStructDeclarations().end(); iter++)
        {
            auto& [specifiers, declarators] = *iter;
            for (auto iter2 = declarators.begin(); iter2 != declarators.end(); iter2++)
            {
                bool last = iter2 + 1 == declarators.end() && iter + 1 == structOrUnion->getStructDeclarations().end();
                bool first = iter2 == declarators.begin() && iter == structOrUnion->getStructDeclarations().begin();
                auto& [declarator, size] = *iter2;
                auto type = declarator ? declaratorsToType(specifiers, *declarator, {}) :
                                         declaratorsToType(specifiers, nullptr, {});
                if (isVoid(type))
                {
                    if (structOrUnion->isUnion())
                    {
                        log(Errors::Semantics::VOID_TYPE_NOT_ALLOWED_IN_UNION.args(*declarator, m_sourceInterface,
                                                                                   specifiers, *declarator));
                    }
                    else
                    {
                        log(Errors::Semantics::VOID_TYPE_NOT_ALLOWED_IN_STRUCT.args(*declarator, m_sourceInterface,
                                                                                    specifiers, *declarator));
                    }
                    type = Type{};
                }
                else if (isVariablyModified(type))
                {
                    if (structOrUnion->isUnion())
                    {
                        log(Errors::Semantics::VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_IN_UNION.args(
                            *declarator, m_sourceInterface, specifiers, *declarator));
                    }
                    else
                    {
                        log(Errors::Semantics::VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_IN_STRUCT.args(
                            *declarator, m_sourceInterface, specifiers, *declarator));
                    }
                    type = Type{};
                }
                else if (!isCompleteType(type)
                         && !(!structOrUnion->isUnion() && last && !first
                              && std::holds_alternative<AbstractArrayType>(type.get())))
                {
                    if (structOrUnion->isUnion())
                    {
                        log(Errors::Semantics::INCOMPLETE_TYPE_NOT_ALLOWED_IN_UNION.args(
                            *declarator, m_sourceInterface, type, specifiers, *declarator));
                    }
                    else
                    {
                        log(Errors::Semantics::INCOMPLETE_TYPE_NOT_ALLOWED_IN_STRUCT.args(
                            *declarator, m_sourceInterface, type, specifiers, *declarator));
                    }
                    type = Type{};
                }
                else if (std::holds_alternative<FunctionType>(type.get()))
                {
                    if (structOrUnion->isUnion())
                    {
                        log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_UNION.args(*declarator, m_sourceInterface,
                                                                                       specifiers, *declarator, type));
                    }
                    else
                    {
                        log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_STRUCT.args(*declarator, m_sourceInterface,
                                                                                        specifiers, *declarator, type));
                    }
                    type = Type{};
                }
                else if (!structOrUnion->isUnion() && hasFlexibleArrayMember(type))
                {
                    if (std::holds_alternative<StructType>(type.get()))
                    {
                        log(Errors::Semantics::STRUCT_WITH_FLEXIBLE_ARRAY_MEMBER_NOT_ALLOWED_IN_STRUCT.args(
                            specifiers, m_sourceInterface, specifiers));
                    }
                    else
                    {
                        log(Errors::Semantics::
                                UNION_WITH_STRUCT_OR_UNION_CONTAINING_A_FLEXIBLE_ARRAY_MEMBER_IS_NOT_ALLOWED_IN_STRUCT
                                    .args(specifiers, m_sourceInterface, specifiers));
                    }
                    type = Type{};
                }
                std::optional<std::uint8_t> value;
                if (size)
                {
                    bool hadValidType = true;
                    if (!std::holds_alternative<PrimitiveType>(type.get()))
                    {
                        log(Errors::Semantics::BITFIELD_MAY_ONLY_BE_OF_TYPE_INT_OR_BOOL.args(
                            specifiers, m_sourceInterface, specifiers));
                        hadValidType = false;
                    }
                    else
                    {
                        auto& primitive = cld::get<PrimitiveType>(type.get());
                        switch (primitive.getKind())
                        {
                            case PrimitiveType::Kind::Bool:
                            case PrimitiveType::Kind::Int:
                            case PrimitiveType::Kind::UnsignedInt: break;
                            default:
                            {
                                log(Errors::Semantics::BITFIELD_MAY_ONLY_BE_OF_TYPE_INT_OR_BOOL.args(
                                    specifiers, m_sourceInterface, specifiers));
                                hadValidType = false;
                            }
                        }
                    }
                    auto expr = visit(*size);
                    if (expr.isUndefined())
                    {
                        continue;
                    }
                    auto result = evaluateConstantExpression(expr);
                    if (!result)
                    {
                        for (auto& message : result.error())
                        {
                            log(message);
                        }
                        continue;
                    }
                    CLD_ASSERT(std::holds_alternative<PrimitiveType>(expr.getType().get()));
                    if (cld::get<PrimitiveType>(expr.getType().get()).isSigned() && result->toInt() < 0)
                    {
                        log(Errors::Semantics::BITFIELD_MUST_BE_OF_SIZE_ZERO_OR_GREATER.args(*size, m_sourceInterface,
                                                                                             *size, result->toInt()));
                        continue;
                    }
                    if (!hadValidType)
                    {
                        continue;
                    }
                    auto objectWidth = cld::get<PrimitiveType>(type.get()).getBitCount();
                    if (result->toUInt() > objectWidth)
                    {
                        log(Errors::Semantics::BITFIELD_MUST_NOT_HAVE_A_GREATER_WIDTH_THAN_THE_TYPE.args(
                            *size, m_sourceInterface, specifiers, objectWidth, *size, result->toUInt()));
                    }
                    if (result->toUInt() == 0 && declarator)
                    {
                        log(Errors::Semantics::BITFIELD_WITH_SIZE_ZERO_MAY_NOT_HAVE_A_NAME.args(
                            *declarator, m_sourceInterface, *declarator));
                    }
                    value = result->toUInt();
                }
                std::string_view fieldName = declarator ? declaratorToLoc(*declarator)->getText() : "";
                fields.push_back({std::make_shared<Type>(std::move(type)), fieldName, value});
            }
        }
        std::size_t currentSize = 0, currentAlignment = 0;
        if (!structOrUnion->isUnion())
        {
            for (auto iter = fields.begin(); iter != fields.end();)
            {
                if (iter->type->isUndefined())
                {
                    iter++;
                    continue;
                }
                if (iter + 1 == fields.end() && std::holds_alternative<AbstractArrayType>(iter->type->get()))
                {
                    break;
                }
                if (!iter->bitFieldSize)
                {
                    auto alignment = iter->type->getAlignOf(*this);
                    currentAlignment = std::max(currentAlignment, alignment);
                    auto rest = currentSize % alignment;
                    if (rest != 0)
                    {
                        currentSize += alignment - rest;
                    }
                    auto subSize = iter->type->getSizeOf(*this);
                    currentSize += subSize;
                    iter++;
                    continue;
                }
                bool lastWasZero = false;
                std::uint64_t storageLeft = 0;
                std::uint64_t prevSize = 0;
                for (; iter != fields.end() && iter->bitFieldSize; iter++)
                {
                    if (*iter->bitFieldSize == 0)
                    {
                        lastWasZero = true;
                        continue;
                    }
                    std::size_t size = iter->type->getSizeOf(*this);
                    if (!lastWasZero && storageLeft > *iter->bitFieldSize
                        && (!m_sourceInterface.getLanguageOptions().discreteBitfields || prevSize == size))
                    {
                        storageLeft -= *iter->bitFieldSize;
                        continue;
                    }
                    lastWasZero = false;
                    currentSize += prevSize;
                    auto alignment = iter->type->getAlignOf(*this);
                    currentAlignment = std::max(currentAlignment, alignment);
                    auto rest = currentSize % alignment;
                    if (rest != 0)
                    {
                        currentSize += alignment - rest;
                    }
                    prevSize = size;
                    storageLeft = cld::get<PrimitiveType>(iter->type->get()).getBitCount() - *iter->bitFieldSize;
                }
                currentSize += prevSize;
            }
        }
        else
        {
            for (auto& iter : fields)
            {
                if (iter.type->isUndefined())
                {
                    continue;
                }
                auto size = iter.type->getSizeOf(*this);
                if (size > currentSize)
                {
                    currentSize = size;
                    currentAlignment = iter.type->getAlignOf(*this);
                }
            }
        }
        if (currentAlignment != 0)
        {
            auto rest = currentSize % currentAlignment;
            if (rest != 0)
            {
                currentSize += currentAlignment - rest;
            }
        }
        if (structOrUnion->getIdentifierLoc())
        {
            auto name = structOrUnion->getIdentifierLoc()->getText();
            if (structOrUnion->isUnion())
            {
                m_unionDefinitions.emplace_back(name, std::move(fields), currentSize, currentAlignment);
                if (definitionIsValid)
                {
                    getCurrentScope().types.insert_or_assign(
                        name,
                        TagTypeInScope{structOrUnion->getIdentifierLoc(), UnionDefTag(m_unionDefinitions.size() - 1)});
                }
                return UnionType::create(isConst, isVolatile, structOrUnion->getIdentifierLoc()->getText(),
                                         m_unionDefinitions.size() - 1);
            }
            m_structDefinitions.emplace_back(name, std::move(fields), currentSize, currentAlignment);
            if (definitionIsValid)
            {
                getCurrentScope().types.insert_or_assign(
                    name,
                    TagTypeInScope{structOrUnion->getIdentifierLoc(), StructDefTag(m_structDefinitions.size() - 1)});
            }
            return StructType::create(isConst, isVolatile, structOrUnion->getIdentifierLoc()->getText(),
                                      m_structDefinitions.size() - 1);
        }

        if (structOrUnion->isUnion())
        {
            return AnonymousUnionType::create(isConst, isVolatile,
                                              reinterpret_cast<std::uintptr_t>(structOrUnion->begin()),
                                              std::move(fields), currentSize, currentAlignment);
        }
        else
        {
            return AnonymousStructType::create(isConst, isVolatile,
                                               reinterpret_cast<std::uintptr_t>(structOrUnion->begin()),
                                               std::move(fields), currentSize, currentAlignment);
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
        const auto* lookup = lookupType<EnumDefTag>((*loc)->getText());
        if (!lookup)
        {
            // C99 6.7.2.3:
            // A type specifier of the form
            //  enum identifier
            // without an enumerator list shall only appear after the type it specifies is complete
            log(Errors::Semantics::FORWARD_DECLARING_AN_ENUM_IS_NOT_ALLOWED.args(*typeSpec[0], m_sourceInterface,
                                                                                 *typeSpec[0]));
            return EnumType::create(isConst, isVolatile, (*loc)->getText(), m_currentScope | IS_SCOPE);
        }
        return EnumType::create(isConst, isVolatile, (*loc)->getText(), static_cast<std::uint64_t>(*lookup));
    }
    auto& enumDef = cld::get<Syntax::EnumDeclaration>(enumDecl->getVariant());
    // TODO: Type depending on values as an extension
    const ConstValue one = {llvm::APSInt(llvm::APInt(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, 1), false)};
    ConstValue nextValue = {llvm::APSInt(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, false)};
    for (auto& [loc, maybeExpression] : enumDef.getValues())
    {
        ConstValue value;
        bool validValue = true;
        if (maybeExpression)
        {
            auto expr = visit(*maybeExpression);
            auto result = evaluateConstantExpression(expr);
            if (!result || !isInteger(expr.getType()))
            {
                if (result && !isInteger(expr.getType()))
                {
                    log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                        expr, m_sourceInterface, expr));
                }
                validValue = false;
                value = ConstValue{};
                for (auto& iter : result.error())
                {
                    log(iter);
                }
            }
            else
            {
                CLD_ASSERT(std::holds_alternative<llvm::APSInt>(result->getValue()));
                auto& apInt = cld::get<llvm::APSInt>(result->getValue());
                if (apInt.ugt(llvm::APSInt::getMaxValue(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, true)
                                  .extOrTrunc(apInt.getBitWidth())))
                {
                    log(Errors::Semantics::VALUE_OF_ENUMERATION_CONSTANT_MUST_FIT_IN_TYPE_INT.args(
                        *loc, m_sourceInterface, *loc, *maybeExpression, apInt));
                }
                value = result->castTo(PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()),
                                       this, m_sourceInterface.getLanguageOptions());
            }
        }
        else
        {
            value = nextValue;
        }
        if (validValue)
        {
            nextValue = value.plus(one, m_sourceInterface.getLanguageOptions());
        }
        auto [prevValue, notRedefined] = getCurrentScope().declarations.insert(
            {loc->getText(),
             DeclarationInScope{
                 loc, std::pair{std::move(value),
                                PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions())}}});
        if (!notRedefined)
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prevValue->second.identifier, m_sourceInterface,
                                                     *prevValue->second.identifier));
        }
    }
    if (enumDef.getName())
    {
        auto name = enumDef.getName()->getText();
        m_enumDefinitions.emplace_back(name,
                                       PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()));
        auto [prev, notRedefined] = getCurrentScope().types.insert(
            {name, TagTypeInScope{enumDef.getName(), EnumDefTag(m_enumDefinitions.size() - 1)}});
        if (!notRedefined)
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*enumDef.getName(), m_sourceInterface, *enumDef.getName()));
            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                     *prev->second.identifier));
        }
        return EnumType::create(isConst, isVolatile, name, m_enumDefinitions.size() - 1);
    }
    return AnonymousEnumType::create(isConst, isVolatile, reinterpret_cast<std::uintptr_t>(enumDecl->begin()),
                                     PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()));
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::primitiveTypeSpecifiersToType(
    bool isConst, bool isVolatile, const std::vector<const Syntax::TypeSpecifier*>& typeSpecs)
{
    using PrimitiveTypeSpecifier = Syntax::TypeSpecifier::PrimitiveTypeSpecifier;
    CLD_ASSERT(std::holds_alternative<PrimitiveTypeSpecifier>(typeSpecs[0]->getVariant()));
    auto excessSpecifiersError = [this](std::string_view type, const Syntax::TypeSpecifier* typeSpec) {
        if (std::holds_alternative<std::string_view>(typeSpec->getVariant()))
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
        return getEnumDefinition(enumType.getName(), enumType.getScopeOrId());
    }
    if (std::holds_alternative<StructType>(type.get()))
    {
        auto& structType = cld::get<StructType>(type.get());
        return getStructDefinition(structType.getName(), structType.getScopeOrId());
    }
    if (std::holds_alternative<UnionType>(type.get()))
    {
        auto& unionType = cld::get<UnionType>(type.get());
        return getUnionDefinition(unionType.getName(), unionType.getScopeOrId());
    }
    return true;
}

bool cld::Semantics::SemanticAnalysis::typesAreCompatible(const cld::Semantics::Type& lhs,
                                                          const cld::Semantics::Type& rhs,
                                                          bool leftIsFuncDefinition) const
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
                return true;
            }
            auto& kandRFunc = lhsFtype.isKandR() ? lhsFtype : rhsFtype;
            auto& paramFunc = lhsFtype.isKandR() ? rhsFtype : lhsFtype;
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
                if (lhsFtype.isKandR() && leftIsFuncDefinition && !paramFunc.getArguments().empty())
                {
                    return false;
                }
                for (auto& iter : paramFunc.getArguments())
                {
                    auto nonQualifiedType = Type(false, false, iter.first.get());
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
                auto nonQualifiedkandR = Type(false, false, kandRType.get());
                auto nonQualifiedParam = Type(false, false, paramType.get());
                if (!typesAreCompatible(defaultArgumentPromotion(nonQualifiedkandR), nonQualifiedParam))
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
            auto nonQualifiedLhs = Type(false, false, lhsType.get());
            auto nonQualifiedRhs = Type(false, false, rhsType.get());
            if (!typesAreCompatible(nonQualifiedLhs, nonQualifiedRhs))
            {
                return false;
            }
        }
        return true;
    }
    if (std::holds_alternative<StructType>(lhs.get()) && std::holds_alternative<StructType>(rhs.get()))
    {
        auto* lhsDef = getStructDefinition(cld::get<StructType>(lhs.get()).getName(),
                                           cld::get<StructType>(lhs.get()).getScopeOrId());
        auto* rhsDef = getStructDefinition(cld::get<StructType>(rhs.get()).getName(),
                                           cld::get<StructType>(rhs.get()).getScopeOrId());
        if (!lhsDef && !rhsDef)
        {
            return cld::get<StructType>(lhs.get()).getName() == cld::get<StructType>(rhs.get()).getName();
        }
        return lhsDef == rhsDef;
    }
    if (std::holds_alternative<UnionType>(lhs.get()) && std::holds_alternative<UnionType>(rhs.get()))
    {
        auto* lhsDef =
            getUnionDefinition(cld::get<UnionType>(lhs.get()).getName(), cld::get<UnionType>(lhs.get()).getScopeOrId());
        auto* rhsDef =
            getUnionDefinition(cld::get<UnionType>(rhs.get()).getName(), cld::get<UnionType>(rhs.get()).getScopeOrId());
        if (!lhsDef && !rhsDef)
        {
            return cld::get<UnionType>(lhs.get()).getName() == cld::get<UnionType>(rhs.get()).getName();
        }
        return lhsDef == rhsDef;
    }
    if (std::holds_alternative<EnumType>(lhs.get()) && std::holds_alternative<EnumType>(rhs.get()))
    {
        auto* lhsDef =
            getEnumDefinition(cld::get<EnumType>(lhs.get()).getName(), cld::get<EnumType>(lhs.get()).getScopeOrId());
        auto* rhsDef =
            getEnumDefinition(cld::get<EnumType>(rhs.get()).getName(), cld::get<EnumType>(rhs.get()).getScopeOrId());
        if (!lhsDef && !rhsDef)
        {
            return cld::get<EnumType>(lhs.get()).getName() == cld::get<EnumType>(rhs.get()).getName();
        }
        return lhsDef == rhsDef;
    }
    return lhs == rhs;
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
        std::vector<std::pair<Type, std::string_view>> parameters;
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

bool cld::Semantics::SemanticAnalysis::isVariablyModified(const cld::Semantics::Type& type) const
{
    auto typeVisitor = RecursiveVisitor(type, ARRAY_TYPE_NEXT_FN);
    return std::any_of(typeVisitor.begin(), typeVisitor.end(),
                       [](const Type& type) { return std::holds_alternative<ValArrayType>(type.get()); });
}

bool cld::Semantics::SemanticAnalysis::hasFlexibleArrayMember(const Type& type) const
{
    if (std::holds_alternative<StructType>(type.get()))
    {
        auto* maybeStructDef = getStructDefinition(cld::get<StructType>(type.get()).getName(),
                                                   cld::get<StructType>(type.get()).getScopeOrId());
        if (maybeStructDef)
        {
            return !maybeStructDef->getFields().empty()
                   && std::holds_alternative<AbstractArrayType>(maybeStructDef->getFields().back().type->get());
        }
    }
    else if (std::holds_alternative<UnionType>(type.get()))
    {
        auto* maybeUnionDef = getUnionDefinition(cld::get<UnionType>(type.get()).getName(),
                                                 cld::get<UnionType>(type.get()).getScopeOrId());
        if (maybeUnionDef)
        {
            for (auto& iter : maybeUnionDef->getFields())
            {
                if (std::holds_alternative<AbstractArrayType>(iter.type->get()))
                {
                    return true;
                }
                if (hasFlexibleArrayMember(*iter.type))
                {
                    return true;
                }
            }
            return false;
        }
    }
    else if (std::holds_alternative<AnonymousStructType>(type.get()))
    {
        auto& structDef = cld::get<AnonymousStructType>(type.get());
        return !structDef.getFields().empty()
               && std::holds_alternative<AbstractArrayType>(structDef.getFields().back().type->get());
    }
    else if (std::holds_alternative<AnonymousUnionType>(type.get()))
    {
        auto& unionDef = cld::get<AnonymousUnionType>(type.get());
        for (auto& iter : unionDef.getFields())
        {
            if (std::holds_alternative<AbstractArrayType>(iter.type->get()))
            {
                return true;
            }
            if (hasFlexibleArrayMember(*iter.type))
            {
                return true;
            }
        }
    }
    return false;
}

template <class T>
void cld::Semantics::SemanticAnalysis::handleArray(cld::Semantics::Type& type,
                                                   const std::vector<Syntax::TypeQualifier>& typeQualifiers,
                                                   const cld::Syntax::AssignmentExpression* assignmentExpression,
                                                   bool isStatic, bool valarray, T&& returnTypeLoc)
{
    if (std::holds_alternative<FunctionType>(type.get()))
    {
        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(returnTypeLoc, m_sourceInterface,
                                                                              returnTypeLoc, type));
        type = Type{};
    }
    else if (!isCompleteType(type))
    {
        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_BE_A_COMPLETE_TYPE.args(returnTypeLoc, m_sourceInterface,
                                                                               returnTypeLoc, type));
        type = Type{};
    }
    else if (hasFlexibleArrayMember(type))
    {
        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_CONTAIN_A_FLEXIBLE_ARRAY_MEMBER.args(
            returnTypeLoc, m_sourceInterface, returnTypeLoc));
        type = Type{};
    }

    auto [isConst, isVolatile, restricted] = getQualifiers(typeQualifiers);
    if (valarray)
    {
        type = ValArrayType::create(isConst, isVolatile, restricted, isStatic, std::move(type));
        return;
    }
    if (!assignmentExpression)
    {
        type = AbstractArrayType::create(isConst, isVolatile, restricted, std::move(type));
        return;
    }
    auto expr = visit(*assignmentExpression);
    auto result = evaluateConstantExpression(expr, Arithmetic);
    if (!result)
    {
        type = ValArrayType::create(isConst, isVolatile, restricted, isStatic, std::move(type));
        return;
    }
    if (result->isUndefined())
    {
        type = Type{};
        return;
    }
    if (!isInteger(expr.getType()))
    {
        log(Errors::Semantics::ARRAY_SIZE_MUST_BE_AN_INTEGER_TYPE.args(expr, m_sourceInterface, expr));
        type = Type{};
        return;
    }
    if (cld::get<PrimitiveType>(expr.getType().get()).isSigned())
    {
        if (result->toInt() <= 0)
        {
            log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(*assignmentExpression, m_sourceInterface,
                                                                             *assignmentExpression, *result));
            type = Type{};
            return;
        }
    }
    else if (result->toUInt() == 0)
    {
        log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(
            *assignmentExpression, m_sourceInterface, *assignmentExpression,
            cld::get<llvm::APSInt>(result->getValue()).toString(10)));
        type = Type{};
        return;
    }
    auto size = result->toUInt();
    type = ArrayType::create(isConst, isVolatile, restricted, isStatic, std::move(type), size);
}

cld::Semantics::StructDefinition* cld::Semantics::SemanticAnalysis::getStructDefinition(std::string_view name,
                                                                                        std::uint64_t scopeOrId,
                                                                                        std::uint64_t* idOut)
{
    if (!(scopeOrId & IS_SCOPE))
    {
        if (idOut)
        {
            *idOut = scopeOrId;
        }
        return &m_structDefinitions[scopeOrId & SCOPE_OR_ID_MASK];
    }
    auto* type = lookupType<StructDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
    if (!type)
    {
        return nullptr;
    }
    if (idOut)
    {
        *idOut = static_cast<std::uint64_t>(*type);
    }
    return &m_structDefinitions[static_cast<std::uint64_t>(*type)];
}

const cld::Semantics::StructDefinition*
    cld::Semantics::SemanticAnalysis::getStructDefinition(std::string_view name, std::uint64_t scopeOrId,
                                                          std::uint64_t* idOut) const
{
    if (!(scopeOrId & IS_SCOPE))
    {
        if (idOut)
        {
            *idOut = scopeOrId;
        }
        return &m_structDefinitions[scopeOrId];
    }
    auto* type = lookupType<StructDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
    if (!type)
    {
        return nullptr;
    }
    if (idOut)
    {
        *idOut = static_cast<std::uint64_t>(*type);
    }
    return &m_structDefinitions[static_cast<std::uint64_t>(*type)];
}

cld::Semantics::EnumDefinition* cld::Semantics::SemanticAnalysis::getEnumDefinition(std::string_view name,
                                                                                    std::uint64_t scopeOrId,
                                                                                    std::uint64_t* idOut)
{
    if (!(scopeOrId & IS_SCOPE))
    {
        if (idOut)
        {
            *idOut = scopeOrId;
        }
        return &m_enumDefinitions[scopeOrId];
    }
    auto* type = lookupType<EnumDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
    if (!type)
    {
        return nullptr;
    }
    if (idOut)
    {
        *idOut = static_cast<std::uint64_t>(*type);
    }
    return &m_enumDefinitions[static_cast<std::uint64_t>(*type)];
}

const cld::Semantics::EnumDefinition* cld::Semantics::SemanticAnalysis::getEnumDefinition(std::string_view name,
                                                                                          std::uint64_t scopeOrId,
                                                                                          std::uint64_t* idOut) const
{
    if (!(scopeOrId & IS_SCOPE))
    {
        if (idOut)
        {
            *idOut = scopeOrId;
        }
        return &m_enumDefinitions[scopeOrId];
    }
    auto* type = lookupType<EnumDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
    if (!type)
    {
        return nullptr;
    }
    if (idOut)
    {
        *idOut = static_cast<std::uint64_t>(*type);
    }
    return &m_enumDefinitions[static_cast<std::uint64_t>(*type)];
}

cld::Semantics::UnionDefinition* cld::Semantics::SemanticAnalysis::getUnionDefinition(std::string_view name,
                                                                                      std::uint64_t scopeOrId,
                                                                                      std::uint64_t* idOut)
{
    if (!(scopeOrId & IS_SCOPE))
    {
        if (idOut)
        {
            *idOut = scopeOrId;
        }
        return &m_unionDefinitions[scopeOrId];
    }
    auto* type = lookupType<UnionDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
    if (!type)
    {
        return nullptr;
    }
    if (idOut)
    {
        *idOut = static_cast<std::uint64_t>(*type);
    }
    return &m_unionDefinitions[static_cast<std::uint64_t>(*type)];
}

const cld::Semantics::UnionDefinition* cld::Semantics::SemanticAnalysis::getUnionDefinition(std::string_view name,
                                                                                            std::uint64_t scopeOrId,
                                                                                            std::uint64_t* idOut) const
{
    if (!(scopeOrId & IS_SCOPE))
    {
        if (idOut)
        {
            *idOut = scopeOrId;
        }
        return &m_unionDefinitions[scopeOrId];
    }
    auto* type = lookupType<UnionDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
    if (!type)
    {
        return nullptr;
    }
    if (idOut)
    {
        *idOut = static_cast<std::uint64_t>(*type);
    }
    return &m_unionDefinitions[static_cast<std::uint64_t>(*type)];
}

cld::Expected<cld::Semantics::ConstValue, std::vector<cld::Message>>
    cld::Semantics::SemanticAnalysis::evaluateConstantExpression(const Expression& constantExpression, Mode mode)
{
    std::vector<Message> messages;
    bool errors = false;
    auto value = evaluate(constantExpression, mode, [&](const Message& message) {
        if (message.getSeverity() == Severity::Error)
        {
            errors = true;
        }
        messages.push_back(message);
    });
    if (errors)
    {
        return {std::move(messages)};
    }
    for (auto& iter : messages)
    {
        log(iter);
    }
    return value;
}

cld::Semantics::ConstValue
    cld::Semantics::SemanticAnalysis::evaluate(const Expression& expression, Mode mode,
                                               llvm::function_ref<void(const Message&)> logger) const
{
    auto typeCheck = [=](const Expression& exp, const ConstValue& value) {
        if (!value.isUndefined() && !isInteger(exp.getType()) && mode == Integer)
        {
            logger(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(exp, m_sourceInterface,
                                                                                                 exp));
            return false;
        }
        return !value.isUndefined();
    };
    return cld::match(
        expression.get(), [](const std::pair<Lexer::CTokenIterator, Lexer::CTokenIterator>&) { return ConstValue{}; },
        [&](const Constant& constant) -> ConstValue {
            if (std::holds_alternative<std::string>(constant.getValue())
                || std::holds_alternative<Lexer::NonCharString>(constant.getValue()))
            {
                if (mode != Initialization)
                {
                    logger(Errors::Semantics::STRING_LITERALS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                        constant, m_sourceInterface, constant));
                    return {};
                }
                return {AddressConstant{}};
            }
            if (std::holds_alternative<llvm::APSInt>(constant.getValue()))
            {
                return {cld::get<llvm::APSInt>(constant.getValue())};
            }
            if (std::holds_alternative<llvm::APFloat>(constant.getValue()))
            {
                return {cld::get<llvm::APFloat>(constant.getValue())};
            }
            CLD_UNREACHABLE;
        },
        [&](const CommaExpression& commaExpression) -> ConstValue {
            for (auto& [exp, comma] : commaExpression.getCommaExpressions())
            {
                (void)exp;
                logger(Errors::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(*comma, m_sourceInterface, *comma));
            }
            return evaluate(commaExpression.getLastExpression(), mode, logger);
        },
        [&](const DeclarationRead& declRead) -> ConstValue {
            if (mode != Initialization
                || cld::match(
                    declRead.getDeclRead(), [](const FunctionDefinition*) { return false; },
                    [](const Declaration* declaration) { return declaration->getLifetime() != Lifetime::Static; }))
            {
                logger(Errors::Semantics::VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                    declRead, m_sourceInterface, declRead));
                return {};
            }
            return {AddressConstant{}};
        },
        [&](const Conversion& conversion) -> ConstValue {
            auto exp = evaluate(conversion.getExpression(), mode, logger);
            if (exp.isUndefined())
            {
                return exp;
            }
            if (conversion.getKind() == Conversion::LValue)
            {
                if (mode == Initialization
                    && (isArray(conversion.getExpression().getType())
                        || std::holds_alternative<FunctionType>(conversion.getExpression().getType().get())))
                {
                    return {AddressConstant{}};
                }
                logger(Errors::Semantics::VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                    conversion.getExpression(), m_sourceInterface, conversion.getExpression()));
                return {};
            }
            return exp.castTo(expression.getType(), this, m_sourceInterface.getLanguageOptions());
        },
        [&](const BinaryOperator& binaryOperator) -> ConstValue {
            auto lhs = evaluate(binaryOperator.getLeftExpression(), mode, logger);
            bool integer = typeCheck(binaryOperator.getLeftExpression(), lhs);
            auto rhs = evaluate(binaryOperator.getRightExpression(), mode, logger);
            if (!integer || !typeCheck(binaryOperator.getRightExpression(), rhs))
            {
                return {};
            }
            switch (binaryOperator.getKind())
            {
                case BinaryOperator::Addition:
                {
                    ConstValue::Issue issue = ConstValue::NoIssue;
                    auto result = lhs.plus(rhs, m_sourceInterface.getLanguageOptions(), &issue);
                    if (issue == ConstValue::NotRepresentable)
                    {
                        logger(Warnings::Semantics::VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N.args(
                            binaryOperator, m_sourceInterface, result, binaryOperator.getRightExpression().getType(),
                            binaryOperator));
                    }
                    return result;
                }
                case BinaryOperator::Subtraction:
                {
                    ConstValue::Issue issue = ConstValue::NoIssue;
                    auto result = lhs.minus(rhs, m_sourceInterface.getLanguageOptions(), &issue);
                    if (issue == ConstValue::NotRepresentable)
                    {
                        logger(Warnings::Semantics::VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N.args(
                            binaryOperator, m_sourceInterface, result, binaryOperator.getRightExpression().getType(),
                            binaryOperator));
                    }
                    return result;
                }
                case BinaryOperator::Multiply:
                {
                    ConstValue::Issue issue = ConstValue::NoIssue;
                    auto result = lhs.multiply(rhs, m_sourceInterface.getLanguageOptions(), &issue);
                    if (issue == ConstValue::NotRepresentable)
                    {
                        logger(Warnings::Semantics::VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N.args(
                            binaryOperator, m_sourceInterface, result, binaryOperator.getRightExpression().getType(),
                            binaryOperator));
                    }
                    return result;
                }
                case BinaryOperator::Divide:
                {
                    ConstValue::Issue issue = ConstValue::NoIssue;
                    auto result = lhs.divide(rhs, m_sourceInterface.getLanguageOptions(), &issue);
                    if (issue == ConstValue::NotRepresentable)
                    {
                        // TODO:
                    }
                    return result;
                }
                case BinaryOperator::Modulo: return lhs.modulo(rhs, m_sourceInterface.getLanguageOptions());
                case BinaryOperator::LeftShift:
                {
                    ConstValue::Issue issue = ConstValue::NoIssue;
                    auto result = lhs.shiftLeft(rhs, m_sourceInterface.getLanguageOptions(), &issue);
                    if (issue == ConstValue::NotRepresentable)
                    {
                        logger(Warnings::Semantics::VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N.args(
                            binaryOperator, m_sourceInterface, result, binaryOperator.getRightExpression().getType(),
                            binaryOperator));
                    }
                    return result;
                }
                case BinaryOperator::RightShift:
                {
                    ConstValue::Issue issue = ConstValue::NoIssue;
                    auto result = lhs.shiftRight(rhs, m_sourceInterface.getLanguageOptions(), &issue);
                    if (issue == ConstValue::NotRepresentable)
                    {
                        // TODO:
                    }
                    return result;
                }
                case BinaryOperator::LessThan: return lhs.lessThan(rhs, m_sourceInterface.getLanguageOptions());
                case BinaryOperator::GreaterThan: return lhs.greaterThan(rhs, m_sourceInterface.getLanguageOptions());
                case BinaryOperator::LessOrEqual: return lhs.lessOrEqual(rhs, m_sourceInterface.getLanguageOptions());
                case BinaryOperator::GreaterOrEqual:
                    return lhs.greaterOrEqual(rhs, m_sourceInterface.getLanguageOptions());
                case BinaryOperator::Equal: return lhs.equal(rhs, m_sourceInterface.getLanguageOptions());
                case BinaryOperator::NotEqual: return lhs.notEqual(rhs, m_sourceInterface.getLanguageOptions());
                case BinaryOperator::BitOr: return lhs.bitOr(rhs, m_sourceInterface.getLanguageOptions());
                case BinaryOperator::BitAnd: return lhs.bitAnd(rhs, m_sourceInterface.getLanguageOptions());
                case BinaryOperator::BitXor: return lhs.bitXor(rhs, m_sourceInterface.getLanguageOptions());
                case BinaryOperator::LogicAnd:
                    if (lhs && rhs)
                    {
                        return {
                            llvm::APSInt(llvm::APInt(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, 1), false)};
                    }
                    else
                    {
                        return {llvm::APSInt(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, false)};
                    }
                case BinaryOperator::LogicOr:
                    if (lhs || rhs)
                    {
                        return {
                            llvm::APSInt(llvm::APInt(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, 1), false)};
                    }
                    else
                    {
                        return {llvm::APSInt(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, false)};
                    }
            }
            CLD_UNREACHABLE;
        },
        [&](const Cast& cast) -> ConstValue {
            if (mode == Integer && !isInteger(cast.getNewType()))
            {
                logger(Errors::Semantics::CANNOT_CAST_TO_NON_INTEGER_TYPE_IN_INTEGER_CONSTANT_EXPRESSION.args(
                    std::forward_as_tuple(*(cast.getOpenParentheses() + 1), *(cast.getCloseParentheses() - 1)),
                    m_sourceInterface,
                    std::forward_as_tuple(*(cast.getOpenParentheses() + 1), *(cast.getCloseParentheses() - 1))));
                return {};
            }
            if (mode == Arithmetic && !isArithmetic(cast.getNewType()))
            {
                logger(Errors::Semantics::CANNOT_CAST_TO_NON_ARITHMETIC_TYPE_IN_ARITHMETIC_CONSTANT_EXPRESSION.args(
                    std::forward_as_tuple(*(cast.getOpenParentheses() + 1), *(cast.getCloseParentheses() - 1)),
                    m_sourceInterface,
                    std::forward_as_tuple(*(cast.getOpenParentheses() + 1), *(cast.getCloseParentheses() - 1))));
                return {};
            }
            ConstValue::Issue issue;
            auto original = evaluate(cast.getExpression(), mode, logger);
            auto ret = original.castTo(cast.getNewType(), this, m_sourceInterface.getLanguageOptions(), &issue);
            if (issue == ConstValue::NotRepresentable)
            {
                logger(Warnings::Semantics::VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N.args(
                    cast.getExpression(), m_sourceInterface, original, cast.getNewType(), cast.getExpression()));
            }
            return ret;
        },
        [&](const UnaryOperator& unaryOperator) -> ConstValue {
            if (unaryOperator.getKind() == UnaryOperator::AddressOf)
            {
                if (std::holds_alternative<UnaryOperator>(unaryOperator.getOperand().get()))
                {
                    auto& innerUnary = cld::get<UnaryOperator>(unaryOperator.getOperand().get());
                    if (innerUnary.getKind() == UnaryOperator::Dereference)
                    {
                        return evaluate(innerUnary.getOperand(), mode, logger);
                    }
                }
                else if (std::holds_alternative<SubscriptOperator>(unaryOperator.getOperand().get()))
                {
                    auto& subScript = cld::get<SubscriptOperator>(unaryOperator.getOperand().get());
                    auto lhs = evaluate(subScript.getLeftExpression(), mode, logger);
                    auto rhs = evaluate(subScript.getRightExpression(), mode, logger);
                    if (lhs.isUndefined() || rhs.isUndefined())
                    {
                        return {};
                    }
                    return lhs.plus(rhs, m_sourceInterface.getLanguageOptions());
                }
            }
            auto op = evaluate(unaryOperator.getOperand(), mode, logger);
            if (!typeCheck(unaryOperator.getOperand(), op))
            {
                return {};
            }
            switch (unaryOperator.getKind())
            {
                case UnaryOperator::AddressOf:
                    if (mode == Initialization)
                    {
                        return {AddressConstant{}};
                    }
                    [[fallthrough]]; // Although not fully correct it's practically not allowed due to yielding or using
                                     // pointer types. Therefore we fall through to give diagnostic
                case UnaryOperator::Dereference:
                case UnaryOperator::PostDecrement:
                case UnaryOperator::PreIncrement:
                case UnaryOperator::PreDecrement:
                case UnaryOperator::PostIncrement:
                    logger(Errors::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                        *unaryOperator.getOperatorToken(), m_sourceInterface, *unaryOperator.getOperatorToken()));
                    return {};
                case UnaryOperator::Plus: return op;
                case UnaryOperator::Minus: return op.negate(m_sourceInterface.getLanguageOptions());
                case UnaryOperator::BooleanNegate: return op.logicalNegate(m_sourceInterface.getLanguageOptions());
                case UnaryOperator::BitwiseNegate: return op.bitwiseNegate(m_sourceInterface.getLanguageOptions());
            }
            CLD_UNREACHABLE;
        },
        [&](const SizeofOperator& sizeofOperator) -> ConstValue {
            if (sizeofOperator.getSize())
            {
                auto type = PrimitiveType::createSizeT(false, false, m_sourceInterface.getLanguageOptions());
                return {llvm::APSInt(
                    llvm::APInt(cld::get<PrimitiveType>(type.get()).getBitCount(), *sizeofOperator.getSize()))};
            }
            // TODO:
            CLD_UNREACHABLE;
        },
        [&](const SubscriptOperator& subscriptOperator) -> ConstValue {
            auto lhs = evaluate(subscriptOperator.getLeftExpression(), mode, logger);
            if (!typeCheck(subscriptOperator.getLeftExpression(), lhs))
            {
                return {};
            }
            auto rhs = evaluate(subscriptOperator.getRightExpression(), mode, logger);
            if (!typeCheck(subscriptOperator.getRightExpression(), rhs))
            {
                return {};
            }
            CLD_UNREACHABLE;
        },
        [&](const Conditional& conditional) -> ConstValue {
            auto boolean = evaluate(conditional.getBoolExpression(), mode, logger);
            if (!typeCheck(conditional.getBoolExpression(), boolean))
            {
                evaluate(conditional.getTrueExpression(), mode, logger);
                evaluate(conditional.getFalseExpression(), mode, logger);
                return {};
            }
            if (boolean)
            {
                auto result = evaluate(conditional.getTrueExpression(), mode, logger);
                if (!typeCheck(conditional.getTrueExpression(), result))
                {
                    return {};
                }
                return result.castTo(expression.getType(), this, m_sourceInterface.getLanguageOptions());
            }
            else
            {
                auto result = evaluate(conditional.getFalseExpression(), mode, logger);
                if (!typeCheck(conditional.getFalseExpression(), result))
                {
                    return {};
                }
                return result.castTo(expression.getType(), this, m_sourceInterface.getLanguageOptions());
            }
        },
        [&](const Assignment& assignment) -> ConstValue {
            logger(Errors::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                *assignment.getOperatorToken(), m_sourceInterface, *assignment.getOperatorToken()));
            return {};
        },
        [&](const CallExpression& call) -> ConstValue {
            logger(Errors::Semantics::FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(call, m_sourceInterface,
                                                                                            call));
            return {};
        },
        [&](const MemberAccess& memberAccess) -> ConstValue {
            auto exp = evaluate(memberAccess.getRecordExpression(), mode, logger);
            return {};
        });
}
