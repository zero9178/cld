#include "SemanticAnalysis.hpp"

#include <cld/Common/Text.hpp>

#include <algorithm>
#include <array>
#include <numeric>
#include <optional>
#include <unordered_map>
#include <utility>

#include "ConstValue.hpp"
#include "ErrorMessages.hpp"
#include "SemanticUtil.hpp"
#include "SourceObject.hpp"

bool cld::Semantics::SemanticAnalysis::log(const Message& message)
{
    if (m_reporter)
    {
        *m_reporter << message;
    }
    if (message.getSeverity() == Severity::Error && m_errors)
    {
        *m_errors = true;
    }
    return message.getSeverity() != Severity::None;
}

cld::Semantics::TranslationUnit cld::Semantics::SemanticAnalysis::visit(const Syntax::TranslationUnit& node)
{
    std::vector<TranslationUnit::Variant> globals;
    for (auto& iter : node.getGlobals())
    {
        auto result = cld::match(
            iter,
            [&](const Syntax::FunctionDefinition& value) -> std::vector<TranslationUnit::Variant> {
                return visit(value);
            },
            [&](const Syntax::Declaration& declaration) -> std::vector<TranslationUnit::Variant> {
                auto value = visit(declaration);
                std::vector<TranslationUnit::Variant> ret(value.size());
                std::transform(
                    std::move_iterator(value.begin()), std::move_iterator(value.end()), ret.begin(),
                    [](DeclRetVariant&& variant) -> TranslationUnit::Variant {
                        return cld::match(
                            std::move(variant),
                            [](std::shared_ptr<const Expression>&&) -> TranslationUnit::Variant { CLD_UNREACHABLE; },
                            [](auto&& value) -> TranslationUnit::Variant { return {std::move(value)}; });
                    });
                return ret;
            });
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
        return {};
    }

    auto& ft = cld::get<FunctionType>(type.get());
    if (!(isVoid(ft.getReturnType()) && !ft.getReturnType().isConst() && !ft.getReturnType().isVolatile())
        && !isCompleteType(ft.getReturnType()))
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
            if (loc->getText() == "__func__")
            {
                log(Errors::Semantics::DECLARING_PARAMETERS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
                    *loc, m_sourceInterface, *loc));
                continue;
            }
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
            auto& ptr = parameterDeclarations.emplace_back(std::make_unique<Declaration>(
                std::move(paramType), Linkage::None, lifetime, loc, Declaration::Kind::Definition));
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
                auto& ptr = parameterDeclarations.emplace_back(std::make_unique<Declaration>(
                    result->second, Linkage::None, lifetime, loc, Declaration::Kind::Definition));
                getCurrentScope().declarations.insert({loc->getText(), DeclarationInScope{loc, ptr.get()}});
                // Don't check for duplicates again. We already did that when processing the identifier list
            }
        }
    }

    const auto* loc = declaratorToLoc(node.getDeclarator());
    if (loc->getText() == "__func__")
    {
        log(Errors::Semantics::DEFINING_FUNCTIONS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
            *loc, m_sourceInterface, *loc));
        return {};
    }
    std::vector<TranslationUnit::Variant> result;
    auto& ptr = cld::get<std::unique_ptr<FunctionDefinition>>(result.emplace_back(std::make_unique<FunctionDefinition>(
        std::move(type), loc, std::move(parameterDeclarations),
        storageClassSpecifier ?
            (storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static ? Linkage::Internal :
                                                                                              Linkage::External) :
            Linkage::External,
        CompoundStatement(m_currentScope, {}))));
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
            prev.value() = DeclarationInScope{loc, ptr.get()};
        }
    }

    auto functionScope = pushFunctionScope(*ptr);

    auto comp = visit(node.getCompoundStatement(), false);
    ptr->setCompoundStatement(std::move(comp));
    return result;
}

std::vector<cld::Semantics::SemanticAnalysis::DeclRetVariant>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::Declaration& node)
{
    std::vector<DeclRetVariant> decls;
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
            log(Notes::Semantics::PREVIOUS_STORAGE_SPECIFIER_HERE.args(*storageClassSpecifier, m_sourceInterface,
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
    // C99 6.7§2:
    // A declaration shall declare at least a declarator (other than the parameters of a function or
    // the members of a structure or union), a tag, or the members of an enumeration
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
            if (storageClassSpecifier
                && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Typedef)
            {
                log(Errors::Semantics::TYPEDEF_DECLARATION_DOES_NOT_HAVE_A_NAME.args(
                    *storageClassSpecifier, m_sourceInterface, *storageClassSpecifier));
            }
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
            if (loc->getText() == "__func__")
            {
                log(Errors::Semantics::DECLARING_FUNCTIONS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
                    *loc, m_sourceInterface, *loc));
                continue;
            }
            auto declaration = std::make_unique<Declaration>(std::move(result), linkage, lifetime, loc,
                                                             Declaration::Kind::DeclarationOnly);
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
                    *declaration =
                        Declaration(std::move(composite), linkage, lifetime, loc, Declaration::Kind::DeclarationOnly);
                    prev.value().declared = declaration.get();
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
            // C99 6.2.2§5:
            // If the declaration of an identifier for an object has file scope and no storage-class specifier,
            // its linkage is external.
            Linkage linkage = m_currentScope == 0 ? Linkage::External : Linkage::None;
            Lifetime lifetime = m_currentScope == 0 ? Lifetime::Static : Lifetime::Automatic;
            if (storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static)
            {
                // C99 6.2.2§3:
                // If the declaration of a file scope identifier for an object or a function contains the storage-
                // class specifier static, the identifier has internal linkage
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

            if (isVariablyModified(result))
            {
                if (m_currentScope == 0)
                {
                    if (storageClassSpecifier
                        && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Typedef)
                    {
                        log(Errors::Semantics::VARIABLY_MODIFIED_TYPEDEF_NOT_ALLOWED_AT_FILE_SCOPE.args(
                            *loc, m_sourceInterface, *loc));
                    }
                    else
                    {
                        log(Errors::Semantics::VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_AT_FILE_SCOPE.args(
                            *loc, m_sourceInterface, *loc));
                    }
                    errors = true;
                }
                else if (linkage != Linkage::None)
                {
                    if (storageClassSpecifier)
                    {
                        log(Errors::Semantics::VARIABLY_MODIFIED_TYPE_MUST_NOT_HAVE_ANY_LINKAGE.args(
                            *loc, m_sourceInterface, *storageClassSpecifier));
                    }
                    else
                    {
                        log(Errors::Semantics::VARIABLY_MODIFIED_TYPE_MUST_NOT_HAVE_ANY_LINKAGE.args(
                            *loc, m_sourceInterface, *loc));
                    }
                    errors = true;
                }
            }
            if (isVariableLengthArray(result))
            {
                if (lifetime == Lifetime::Static)
                {
                    if (storageClassSpecifier)
                    {
                        log(Errors::Semantics::VARIABLE_LENGTH_ARRAY_MUST_NOT_HAVE_STATIC_LIFETIME.args(
                            *loc, m_sourceInterface, *storageClassSpecifier));
                    }
                    else
                    {
                        log(Errors::Semantics::VARIABLE_LENGTH_ARRAY_MUST_NOT_HAVE_STATIC_LIFETIME.args(
                            *loc, m_sourceInterface, *loc));
                    }
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
                if (loc->getText() == "__func__")
                {
                    log(Errors::Semantics::DECLARING_TYPEDEFS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
                        *loc, m_sourceInterface, *loc));
                    continue;
                }
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
                for (auto& iter : typeVisitor)
                {
                    if (std::holds_alternative<ValArrayType>(iter.get()))
                    {
                        decls.push_back(cld::get<ValArrayType>(iter.get()).getExpression());
                    }
                }
                continue;
            }
            if (loc->getText() == "__func__")
            {
                log(Errors::Semantics::DECLARING_VARIABLES_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
                    *loc, m_sourceInterface, *loc));
                continue;
            }
            Declaration::Kind kind;
            // C99 6.8.2§1:
            // If the declaration of an identifier for an object has file scope and an initializer, the
            // declaration is an external definition for the identifier.
            if (m_currentScope == 0 && initializer
                && (!storageClassSpecifier || *storageClassSpecifier != Syntax::StorageClassSpecifier::Extern))
            {
                kind = Declaration::Definition;
            }
            else if (m_currentScope == 0
                     && (!storageClassSpecifier || *storageClassSpecifier == Syntax::StorageClassSpecifier::Static))
            {
                // C99 6.9.2§2:
                // A declaration of an identifier for an object that has file scope without an initializer, and
                // without a storage-class specifier or with the storage-class specifier static, constitutes a
                // tentative definition
                kind = Declaration::TentativeDefinition;
            }
            else if (storageClassSpecifier && *storageClassSpecifier == Syntax::StorageClassSpecifier::Extern)
            {
                kind = Declaration::DeclarationOnly;
            }
            else
            {
                kind = Declaration::Definition;
            }

            auto declaration = std::make_unique<Declaration>(std::move(result), linkage, lifetime, loc, kind);
            auto [prev, notRedefinition] =
                getCurrentScope().declarations.insert({loc->getText(), DeclarationInScope{loc, declaration.get()}});
            if (!notRedefinition)
            {
                if (!std::holds_alternative<const Declaration*>(prev->second.declared)
                    // C99 6.7§3:
                    // If an identifier has no linkage, there shall be no more than one declaration of the identifier
                    // (in a declarator or type specifier) with the same scope and in the same name space, except
                    // for tags as specified in 6.7.2.3.
                    || cld::get<const Declaration*>(prev->second.declared)->getLinkage() == Linkage::None
                    || declaration->getLinkage() == Linkage::None
                    // C99 6.7§2:
                    // All declarations in the same scope that refer to the same object or function shall specify
                    // compatible types.
                    || !typesAreCompatible(declaration->getType(),
                                           cld::get<const Declaration*>(prev->second.declared)->getType())
                    // C99 6.9§3:
                    // There shall be no more than one external definition for each identifier declared with
                    // internal linkage in a translation unit.
                    || (m_currentScope == 0 && (kind == Declaration::Definition && linkage == Linkage::Internal)
                        && (cld::get<const Declaration*>(prev->second.declared)->getKind() == Declaration::Definition
                            && cld::get<const Declaration*>(prev->second.declared)->getLinkage() == Linkage::Internal)))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
                else
                {
                    auto& prevDecl = cld::get<const Declaration*>(prev->second.declared);
                    auto composite = compositeType(prevDecl->getType(), declaration->getType());
                    // C99 6.2.2§4:
                    // For an identifier declared with the storage-class specifier extern in a scope in which a
                    // prior declaration of that identifier is visible, if the prior declaration specifies internal or
                    // external linkage, the linkage of the identifier at the later declaration is the same as the
                    // linkage specified at the prior declaration. If no prior declaration is visible, or if the prior
                    // declaration specifies no linkage, then the identifier has external linkage.
                    if (storageClassSpecifier && *storageClassSpecifier == Syntax::StorageClassSpecifier::Extern
                        && prevDecl->getLinkage() != Linkage::None)
                    {
                        linkage = prevDecl->getLinkage();
                    }
                    else if (linkage != Linkage::Internal && prevDecl->getLinkage() == Linkage::Internal)
                    {
                        // C99 6.2.2§7:
                        // If, within a translation unit, the same identifier appears with both internal and external
                        // linkage, the behavior is undefined.

                        // We error like clang does, will allow the composite type to be formed with internal linkage
                        // tho
                        log(Errors::Semantics::STATIC_VARIABLE_N_REDEFINED_WITHOUT_STATIC.args(*loc, m_sourceInterface,
                                                                                               *loc));
                        log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                                 *prev->second.identifier));
                        linkage = Linkage::Internal;
                    }
                    *declaration =
                        Declaration(std::move(composite), linkage, lifetime, loc, std::max(prevDecl->getKind(), kind));
                    prev.value().declared = declaration.get();
                }
            }

            if (initializer)
            {
                // C99 6.7.5$5:
                // If the declaration of an identifier has block scope, and the identifier has external or
                // internal linkage, the declaration shall have no initializer for the identifier.
                if (m_currentScope != 0 && linkage != Linkage::None)
                {
                    log(Errors::Semantics::CANNOT_INITIALIZE_STATIC_OR_EXTERN_VARIABLE_AT_BLOCK_SCOPE.args(
                        *loc, m_sourceInterface, *loc));
                }
                if (isVariableLengthArray(declaration->getType()))
                {
                    log(Errors::Semantics::CANNOT_INITIALIZE_VARIABLE_LENGTH_ARRAY_TYPE.args(
                        *loc, m_sourceInterface, *loc, declaration->getType()));
                    visit(*initializer, Type{}, declaration->getLifetime() == Lifetime::Static);
                }
                else
                {
                    m_inStaticInitializer = lifetime == Lifetime::Static;
                    auto prevType = declaration->getType();
                    std::size_t size = 0;
                    auto expr = visit(*initializer, declaration->getType(),
                                      declaration->getLifetime() == Lifetime::Static, &size);
                    if (std::holds_alternative<AbstractArrayType>(declaration->getType().get()))
                    {
                        prevType = ArrayType::create(prevType.isConst(), prevType.isVolatile(),
                                                     cld::get<AbstractArrayType>(prevType.get()).isRestricted(), false,
                                                     cld::get<AbstractArrayType>(prevType.get()).getType(), size);
                    }
                    *declaration = Declaration(std::move(prevType), linkage, lifetime, loc, kind, std::move(expr));
                    m_inStaticInitializer = false;
                }
            }

            // C99 6.7§7:
            // If an identifier for an object is declared with no linkage, the type for the object shall be
            // complete by the end of its declarator, or by the end of its init-declarator if it has an
            // initializer;
            //
            // C99 6.9.2§3:
            // If the declaration of an identifier for an object is a tentative definition and has internal
            // linkage, the declared type shall not be an incomplete type.
            if (linkage == Linkage::None
                || (kind == Declaration::Kind::TentativeDefinition && linkage == Linkage::Internal))
            {
                if (isVoid(declaration->getType()))
                {
                    log(Errors::Semantics::DECLARATION_MUST_NOT_BE_VOID.args(*loc, m_sourceInterface, *loc));
                }
                else if (!isCompleteType(declaration->getType()))
                {
                    log(Errors::Semantics::DECLARATION_MUST_HAVE_A_COMPLETE_TYPE.args(*loc, m_sourceInterface, *loc,
                                                                                      declaration->getType()));
                }
            }

            decls.push_back(std::move(declaration));
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
    return CompoundStatement(m_currentScope, std::move(result));
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
                           [](DeclRetVariant&& variant) {
                               return cld::match(std::move(variant), [](auto&& value) -> CompoundStatement::Variant {
                                   return {std::move(value)};
                               });
                           });
        },
        [&](const Syntax::Statement& statement) { result.emplace_back(visit(statement)); });
    return result;
}

cld::Semantics::Statement cld::Semantics::SemanticAnalysis::visit(const Syntax::Statement& node)
{
    return cld::match(
        node, [&](const auto& node) -> Statement { return visit(node); },
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
                                        compositeType(valArray->getType(), getElementType(rhs)),
                                        valArray->getExpression());
        }
        if (auto* valArray = std::get_if<ValArrayType>(&rhs.get()))
        {
            return ValArrayType::create(rhs.isConst(), rhs.isVolatile(), valArray->isRestricted(), valArray->isStatic(),
                                        compositeType(valArray->getType(), getElementType(lhs)),
                                        valArray->getExpression());
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
                                               cld::function_ref<void(const Message&)> logger) const
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
        [&](const CompoundLiteral& compoundLiteral) -> ConstValue {
            if (mode != Initialization)
            {
                logger(Errors::Semantics::COMPOUND_LITERAL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                    compoundLiteral, m_sourceInterface, compoundLiteral));
                return {};
            }
            return {AddressConstant{}};
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
                        || std::holds_alternative<CompoundLiteral>(conversion.getExpression().get())
                        || std::holds_alternative<FunctionType>(conversion.getExpression().getType().get())))
                {
                    return {AddressConstant{}};
                }
                logger(Errors::Semantics::VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                    conversion.getExpression(), m_sourceInterface, conversion.getExpression()));
                return {};
            }
            else if (conversion.getKind() == Conversion::Implicit)
            {
                if (!typeCheck(conversion.getExpression(), exp))
                {
                    return {};
                }
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
                case UnaryOperator::Dereference:
                    if (mode == Initialization)
                    {
                        return {AddressConstant{}};
                    }
                    [[fallthrough]]; // Although not fully correct it's practically not allowed due to yielding or using
                                     // pointer types. Therefore we fall through to give diagnostic
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
            logger(Errors::Semantics::SIZEOF_VAL_MODIFIED_TYPE_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION.args(
                sizeofOperator, m_sourceInterface, sizeofOperator));
            return {};
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
            return {AddressConstant{}};
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