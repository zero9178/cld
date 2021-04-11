#include "SemanticAnalysis.hpp"

#include <cld/Support/Constexpr.hpp>
#include <cld/Support/Text.hpp>

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
    std::vector<IntrVarPtr<Useable>> globals;
    for (auto& iter : node.getGlobals())
    {
        auto result = cld::match(
            iter,
            [&](const Syntax::FunctionDefinition& value) -> std::vector<IntrVarPtr<Useable>> { return visit(value); },
            [&](const Syntax::Declaration& declaration) -> std::vector<IntrVarPtr<Useable>>
            {
                auto value = visit(declaration);
                std::vector<IntrVarPtr<Useable>> ret(value.size());
                std::transform(
                    std::move_iterator(value.begin()), std::move_iterator(value.end()), ret.begin(),
                    [](DeclRetVariant&& variant) -> IntrVarPtr<Useable>
                    {
                        return cld::match(
                            std::move(variant),
                            [](std::shared_ptr<const ExpressionBase>&&) -> IntrVarPtr<Useable> { CLD_UNREACHABLE; },
                            [](auto&& value) -> IntrVarPtr<Useable> { return std::move(value); });
                    });
                return ret;
            },
            [&](const Syntax::GNUSimpleASM&) -> std::vector<IntrVarPtr<Useable>>
            {
                // TODO:
                return {};
            });
        globals.insert(globals.end(), std::move_iterator(result.begin()), std::move_iterator(result.end()));
    }
    for (auto& [name, declared] : m_scopes[0].declarations)
    {
        (void)name;
        if (auto* decl = std::get_if<FunctionDeclaration*>(&declared.declared))
        {
            // C99 6.7.4§6:
            // For a function with external linkage, the following restrictions apply:
            // If a function is declared with an inline function specifier, then it shall also be defined in the
            // same translation unitprogram.
            if ((*decl)->isInline() && (*decl)->getLinkage() == Linkage::External)
            {
                log(Errors::Semantics::NO_DEFINITION_FOR_INLINE_FUNCTION_N_FOUND.args(
                    *(*decl)->getNameToken(), m_sourceInterface, *(*decl)->getNameToken()));
            }
        }
        cld::match(
            declared.declared, [](const auto&) {},
            [&declared = declared, this](VariableDeclaration* declaration) {
                if (declaration->getKind() == VariableDeclaration::TentativeDefinition
                    && !isCompleteType(declaration->getType()) && !isAbstractArray(declaration->getType()))
                {
                    log(Errors::Semantics::TYPE_OF_TENTATIVE_DEFINITION_IS_NEVER_COMPLETED.args(
                        *declared.identifier, m_sourceInterface, *declared.identifier, declaration->getType()));
                }
                if (declaration->isUsed() || declaration->getLinkage() == Linkage::External
                    || declaration->hasAttribute<UsedAttribute>())
                {
                    return;
                }
                log(Warnings::Semantics::UNUSED_VARIABLE_N.args(*declared.identifier, m_sourceInterface,
                                                                *declared.identifier));
            },
            [&declared = declared, this](FunctionDefinition* functionDefinition) {
                if (functionDefinition->isUsed() || functionDefinition->getLinkage() == Linkage::External
                    || functionDefinition->hasAttribute<UsedAttribute>())
                {
                    return;
                }
                log(Warnings::Semantics::UNUSED_FUNCTION_N.args(*declared.identifier, m_sourceInterface,
                                                                *declared.identifier));
            });
    }
    return TranslationUnit(std::move(globals));
}

std::vector<cld::IntrVarPtr<cld::Semantics::Useable>>
    cld::Semantics::SemanticAnalysis::visit(const cld::Syntax::FunctionDefinition& node)
{
    bool isInline = false;
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        if (std::holds_alternative<Syntax::FunctionSpecifier>(iter))
        {
            isInline = true;
        }
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
    std::vector<std::unique_ptr<VariableDeclaration>> parameterDeclarations;
    std::vector<GNUAttribute> attributes;
    auto type = declaratorsToType(
        node.getDeclarationSpecifiers(), node.getDeclarator(), node.getDeclarations(),
        [&](IntrVarValue<Type> paramType, Lexer::CTokenIterator loc,
            const std::vector<Syntax::DeclarationSpecifier>& declarationSpecifiers,
            std::vector<GNUAttribute>&& attributes)
        {
            if (loc->getText() == "__func__")
            {
                log(Errors::Semantics::DECLARING_PARAMETERS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
                    *loc, m_sourceInterface, *loc));
                return;
            }
            Lifetime lifetime = Lifetime ::Automatic;
            for (auto& iter : declarationSpecifiers)
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
            paramType = adjustParameterType(paramType);

            auto& ptr = parameterDeclarations.emplace_back(std::make_unique<VariableDeclaration>(
                std::move(paramType), Linkage::None, lifetime, loc, VariableDeclaration::Kind::Definition));
            auto [prev, notRedefined] =
                getCurrentScope().declarations.insert({loc->getText(), DeclarationInScope{loc, ptr.get()}});
            if (!notRedefined)
            {
                log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
            else
            {
                attributes = applyAttributes(ptr.get(), std::move(attributes));
                reportNotApplicableAttributes(attributes);
            }
        },
        &attributes);
    if (!type->is<FunctionType>())
    {
        log(Errors::Semantics::FUNCTION_DEFINITION_MUST_HAVE_FUNCTION_TYPE.args(
            std::forward_as_tuple(node.getDeclarationSpecifiers(), node.getDeclarator()), m_sourceInterface,
            std::forward_as_tuple(node.getDeclarationSpecifiers(), node.getDeclarator()), type));
        return {};
    }

    auto& ft = type->as<FunctionType>();
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
    if (node.getOptionalAttributes())
    {
        auto vector = visit(*node.getOptionalAttributes());
        attributes.insert(attributes.end(), std::move_iterator(vector.begin()), std::move_iterator(vector.end()));
    }

    if (parameters)
    {
        if (!node.getDeclarations().empty())
        {
            log(Errors::Semantics::FUNCTION_DEFINITION_WITH_A_PARAMETER_LIST_MUST_NOT_HAVE_DECLARATIONS_FOLLOWING_IT
                    .args(node.getDeclarations(), m_sourceInterface, node.getDeclarations()));
        }
    }
    else if (identifierList)
    {
        // parameterDeclarations is currently in the order of how the declarations appear but we need it in the order
        // of the identifiers in the identifierList
        auto begin = parameterDeclarations.begin();
        for (auto& iter : identifierList->getIdentifiers())
        {
            auto element = std::find_if(begin, parameterDeclarations.end(), [iter](const auto& ptr) {
                return ptr->getNameToken()->getText() == iter->getText();
            });
            if (element == parameterDeclarations.end())
            {
                // Possible in error cases
                continue;
            }
            std::swap(*begin, *element);
            begin++;
        }
    }

    const auto* loc = declaratorToLoc(node.getDeclarator());
    if (loc->getText() == "__func__")
    {
        log(Errors::Semantics::DEFINING_FUNCTIONS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
            *loc, m_sourceInterface, *loc));
        return {};
    }
    if (loc->getText() == "main" && isInline && !getLanguageOptions().freeStanding)
    {
        log(Errors::Semantics::INLINE_MAIN_IS_NOT_ALLOWED_IN_A_HOSTED_ENVIRONMENT.args(*loc, m_sourceInterface, *loc));
    }
    InlineKind inlineKind = InlineKind::None;
    if (isInline && storageClassSpecifier
        && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Extern)
    {
        inlineKind = InlineKind::Inline;
    }
    else if (isInline)
    {
        inlineKind = InlineKind::InlineDefinition;
    }
    Linkage linkage = Linkage::External;
    if (storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static)
    {
        linkage = Linkage::Internal;
    }
    std::vector<IntrVarPtr<Useable>> result;
    auto& ptr = result
                    .emplace_back(std::make_unique<FunctionDefinition>(
                        std::move(ft), loc, std::move(parameterDeclarations), linkage, inlineKind,
                        CompoundStatement(m_currentScope, loc, {}, loc)))
                    ->as<FunctionDefinition>();
    // We are currently in block scope. Functions are always at file scope though so we can't use getCurrentScope
    auto [prev, notRedefinition] = m_scopes[0].declarations.insert({loc->getText(), DeclarationInScope{loc, &ptr}});
    if (!notRedefinition)
    {
        if (!std::holds_alternative<FunctionDeclaration*>(prev->second.declared)
            || !typesAreCompatible(ptr.getType(), cld::get<FunctionDeclaration*>(prev->second.declared)->getType(),
                                   true))
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                     *prev->second.identifier));
        }
        else
        {
            auto& prevDecl = cld::get<FunctionDeclaration*>(prev->second.declared);
            inlineKind = std::max(ptr.getInlineKind(), prevDecl->getInlineKind());
            if (prevDecl->getLinkage() == Linkage::Internal)
            {
                linkage = Linkage::Internal;
            }
            else if (linkage == Linkage::Internal)
            {
                log(Errors::Semantics::REDEFINITION_OF_FUNCTION_N_WITH_INTERNAL_LINKAGE.args(*loc, m_sourceInterface,
                                                                                             *loc));
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
            if (auto attr = prevDecl->removeAttribute<DllImportAttribute>())
            {
                log(Warnings::Semantics::ATTRIBUTE_DLLIMPORT_IGNORED_AFTER_DEFINITION_OF_FUNCTION_N.args(
                    *attr->identifier, m_sourceInterface, *loc, *attr->identifier));
            }
            ptr = FunctionDefinition(ptr.getType(), loc, std::move(ptr).getParameterDeclarations(), linkage, inlineKind,
                                     std::move(ptr).getCompoundStatement());
            ptr.setUses(prevDecl->getUses());
            ptr.tryAddFromOther(*prevDecl);
            prev.value() = DeclarationInScope{loc, &ptr};
        }
    }
    attributes = applyAttributes(&ptr, std::move(attributes), FunctionContext{isInline});
    reportNotApplicableAttributes(attributes);

    auto funcType = ArrayType(typeAlloc<PrimitiveType>(PrimitiveType::Char, getLanguageOptions(), flag::isConst = true),
                              loc->getText().size() + 1);
    auto funcName = std::make_unique<VariableDeclaration>(
        std::move(funcType), Linkage::Internal, Lifetime::Static, loc, VariableDeclaration::Definition,
        std::make_unique<Constant>(
            ArrayType(typeAlloc<PrimitiveType>(PrimitiveType::Char, getLanguageOptions()), loc->getText().size() + 1),
            cld::to_string(loc->getText()), loc, loc + 1));
    getCurrentScope().declarations.insert({"__func__", DeclarationInScope{nullptr, funcName.get()}});
    // GCC Extensions
    getCurrentScope().declarations.insert({"__FUNCTION__", DeclarationInScope{nullptr, funcName.get()}});
    getCurrentScope().declarations.insert({"__PRETTY_FUNCTION__", DeclarationInScope{nullptr, funcName.get()}});

    auto functionScope = pushFunctionScope(ptr);

    auto comp = visit(node.getCompoundStatement(), false);
    comp->prependItem(std::move(funcName));
    ptr.setCompoundStatement(std::move(*comp));
    return result;
}

std::vector<cld::Semantics::SemanticAnalysis::DeclRetVariant>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::Declaration& node)
{
    std::vector<DeclRetVariant> decls;
    bool isInline = false;
    const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
    for (auto& iter : node.getDeclarationSpecifiers())
    {
        if (std::holds_alternative<Syntax::FunctionSpecifier>(iter))
        {
            isInline = true;
        }
        if (!std::holds_alternative<Syntax::StorageClassSpecifier>(iter))
        {
            continue;
        }
        auto& storage = cld::get<Syntax::StorageClassSpecifier>(iter);
        if (storageClassSpecifier)
        {
            log(Errors::Semantics::ONLY_ONE_STORAGE_SPECIFIER.args(storage, m_sourceInterface, storage));
            log(Notes::Semantics::PREVIOUS_STORAGE_SPECIFIER_HERE.args(*storageClassSpecifier, m_sourceInterface,
                                                                       *storageClassSpecifier));
            continue;
        }
        storageClassSpecifier = &storage;
        if (m_currentScope == 0 && storage.getSpecifier() == Syntax::StorageClassSpecifier::Auto)
        {
            log(Errors::Semantics::DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO.args(storage, m_sourceInterface, storage));
        }
        else if (m_currentScope == 0 && storage.getSpecifier() == Syntax::StorageClassSpecifier::Register)
        {
            log(Errors::Semantics::DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_REGISTER.args(storage, m_sourceInterface,
                                                                                      storage));
        }
    }
    bool isTypedef =
        storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Typedef;

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
            if (isTypedef)
            {
                log(Errors::Semantics::TYPEDEF_DECLARATION_DOES_NOT_HAVE_A_NAME.args(
                    *storageClassSpecifier, m_sourceInterface, *storageClassSpecifier));
            }
            declaratorsToType(node.getDeclarationSpecifiers());
        }
        return decls;
    }

    std::vector<GNUAttribute> attributes;
    auto baseType = qualifiersToType(node.getDeclarationSpecifiers(), &attributes);
    for (auto& iter : node.getInitDeclarators())
    {
        auto thisAttributes = attributes;
        const auto* loc = declaratorToLoc(*iter.declarator);
        auto result = applyDeclarator(baseType, *iter.declarator, {}, {}, &thisAttributes);
        if (iter.optionalBeforeAttributes)
        {
            auto vector = visit(*iter.optionalBeforeAttributes);
            thisAttributes.insert(thisAttributes.end(), std::move_iterator(vector.begin()),
                                  std::move_iterator(vector.end()));
        }
        if (iter.optionalAfterAttributes)
        {
            auto vector = visit(*iter.optionalAfterAttributes);
            thisAttributes.insert(thisAttributes.end(), std::move_iterator(vector.begin()),
                                  std::move_iterator(vector.end()));
        }

        if (auto* functionType = result->tryAs<FunctionType>(); functionType && !isTypedef)
        {
            if (iter.optionalInitializer)
            {
                log(Errors::Semantics::FUNCTION_PROTOTYPE_MUST_NOT_HAVE_AN_INITIALIZER.args(
                    *iter.optionalInitializer, m_sourceInterface, *iter.optionalInitializer));
            }
            if (auto declaration = visitFunctionDeclaration(loc, std::move(*functionType), storageClassSpecifier,
                                                            isInline, std::move(thisAttributes)))
            {
                decls.push_back(std::move(declaration));
            }
            continue;
        }

        for (auto& type : node.getDeclarationSpecifiers())
        {
            if (auto* funcSpec = std::get_if<Syntax::FunctionSpecifier>(&type))
            {
                log(Errors::Semantics::INLINE_ONLY_ALLOWED_FOR_FUNCTIONS.args(*funcSpec, m_sourceInterface, *funcSpec));
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
                if (isTypedef)
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
        if (errors)
        {
            result.emplace<ErrorType>();
        }
        if (isTypedef)
        {
            if (loc->getText() == "__func__")
            {
                log(Errors::Semantics::DECLARING_TYPEDEFS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
                    *loc, m_sourceInterface, *loc));
                continue;
            }
            thisAttributes = applyAttributes(std::pair{&result, diag::getPointRange(*loc)}, std::move(thisAttributes));
            auto [prev, noRedefinition] = insertTypedef({loc->getText(), result, m_currentScope, loc});
            if (!noRedefinition
                && (!std::holds_alternative<TypedefInfo*>(prev->second.declared)
                    || !typesAreCompatible(result, cld::get<TypedefInfo*>(prev->second.declared)->type)))
            {
                log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
            else
            {
                thisAttributes =
                    applyAttributes(cld::get<TypedefInfo*>(prev->second.declared), std::move(thisAttributes));
            }
            reportNotApplicableAttributes(thisAttributes);
            for (auto& type : RecursiveVisitor(result, TYPE_NEXT_FN))
            {
                if (auto* valArrayType = type.tryAs<ValArrayType>())
                {
                    decls.push_back(valArrayType->getExpression());
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
        VariableDeclaration::Kind kind;
        // C99 6.8.2§1:
        // If the declaration of an identifier for an object has file scope and an initializer, the
        // declaration is an external definition for the identifier.
        if (m_currentScope == 0 && iter.optionalInitializer
            && (!storageClassSpecifier || *storageClassSpecifier != Syntax::StorageClassSpecifier::Extern))
        {
            kind = VariableDeclaration::Definition;
        }
        else if (m_currentScope == 0
                 && (!storageClassSpecifier || *storageClassSpecifier == Syntax::StorageClassSpecifier::Static))
        {
            // C99 6.9.2§2:
            // A declaration of an identifier for an object that has file scope without an initializer, and
            // without a storage-class specifier or with the storage-class specifier static, constitutes a
            // tentative definition
            kind = VariableDeclaration::TentativeDefinition;
        }
        else if (storageClassSpecifier && *storageClassSpecifier == Syntax::StorageClassSpecifier::Extern)
        {
            kind = VariableDeclaration::DeclarationOnly;
        }
        else
        {
            kind = VariableDeclaration::Definition;
        }

        auto declaration = std::make_unique<VariableDeclaration>(std::move(result), linkage, lifetime, loc, kind);
        auto [prev, notRedefinition] =
            getCurrentScope().declarations.insert({loc->getText(), DeclarationInScope{loc, declaration.get()}});
        if (!notRedefinition)
        {
            if (!std::holds_alternative<VariableDeclaration*>(prev->second.declared)
                // C99 6.7§3:
                // If an identifier has no linkage, there shall be no more than one declaration of the identifier
                // (in a declarator or type specifier) with the same scope and in the same name space, except
                // for tags as specified in 6.7.2.3.
                || cld::get<VariableDeclaration*>(prev->second.declared)->getLinkage() == Linkage::None
                || declaration->getLinkage() == Linkage::None
                // C99 6.7§2:
                // All declarations in the same scope that refer to the same object or function shall specify
                // compatible types.
                || !typesAreCompatible(declaration->getType(),
                                       cld::get<VariableDeclaration*>(prev->second.declared)->getType())
                // C99 6.9§3:
                // There shall be no more than one external definition for each identifier declared with
                // internal linkage in a translation unit.
                || (m_currentScope == 0 && (kind == VariableDeclaration::Definition && linkage == Linkage::Internal)
                    && (cld::get<VariableDeclaration*>(prev->second.declared)->getKind()
                            == VariableDeclaration::Definition
                        && cld::get<VariableDeclaration*>(prev->second.declared)->getLinkage() == Linkage::Internal)))
            {
                log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
            else
            {
                auto& prevDecl = cld::get<VariableDeclaration*>(prev->second.declared);
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
                *declaration = VariableDeclaration(std::move(composite), linkage, lifetime, loc,
                                                   std::max(prevDecl->getKind(), kind));
                declaration->tryAddFromOther(*prevDecl);
                prev.value().declared = declaration.get();
            }
        }
        thisAttributes = applyAttributes(declaration.get(), std::move(thisAttributes));
        reportNotApplicableAttributes(thisAttributes);

        if (iter.optionalInitializer)
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
                log(Errors::Semantics::CANNOT_INITIALIZE_VARIABLE_LENGTH_ARRAY_TYPE.args(*loc, m_sourceInterface, *loc,
                                                                                         declaration->getType()));
                visit(*iter.optionalInitializer, ErrorType{}, declaration->getLifetime() == Lifetime::Static);
            }
            else if (!isCompleteType(declaration->getType()) && !isAbstractArray(declaration->getType()))
            {
                log(Errors::Semantics::CANNOT_INITIALIZE_VARIABLE_OF_INCOMPLETE_TYPE.args(*loc, m_sourceInterface,
                                                                                          *loc));
                visit(*iter.optionalInitializer, ErrorType{}, declaration->getLifetime() == Lifetime::Static);
            }
            else
            {
                m_inStaticInitializer = lifetime == Lifetime::Static;
                IntrVarValue prevType = declaration->getType();
                std::size_t size = 0;
                auto expr = visit(*iter.optionalInitializer, declaration->getType(),
                                  declaration->getLifetime() == Lifetime::Static, &size);
                if (auto* abstractArray = prevType->tryAs<AbstractArrayType>())
                {
                    prevType.emplace<ArrayType>(&abstractArray->getType(), size, flag::useFlags = prevType->getFlags());
                }
                auto temp = std::move(*declaration).getAttributes();
                *declaration = VariableDeclaration(std::move(prevType), linkage, lifetime, loc, kind, std::move(expr));
                std::for_each(std::move_iterator(temp.begin()), std::move_iterator(temp.end()),
                              cld::bind_front(&VariableDeclaration::addAttribute, declaration.get()));
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
            || (kind == VariableDeclaration::TentativeDefinition && linkage == Linkage::Internal))
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

        // C99 6.7.4§3:
        // An inline definition of a function with external linkage shall not contain a definition of a
        // modifiable object with static storage duration, and shall not contain a reference to an
        // identifier with internal linkage.
        if (getCurrentFunctionScope() && getCurrentFunctionScope()->currentFunction->isInline()
            && getCurrentFunctionScope()->currentFunction->getLinkage() == Linkage::External
            && declaration->getKind() == VariableDeclaration::Definition
            && declaration->getLifetime() == Lifetime::Static && !declaration->getType().isConst())
        {
            log(Errors::Semantics::
                    INLINE_FUNCTION_N_WITH_EXTERNAL_LINKAGE_IS_NOT_ALLOWED_TO_CONTAIN_OR_ACCESS_THE_INTERNAL_IDENTIFIER_N
                        .args(*declaration->getNameToken(), m_sourceInterface,
                              *getCurrentFunctionScope()->currentFunction->getNameToken(),
                              *declaration->getNameToken()));
        }

        decls.push_back(std::move(declaration));
    }
    return decls;
}

std::unique_ptr<cld::Semantics::FunctionDeclaration> cld::Semantics::SemanticAnalysis::visitFunctionDeclaration(
    Lexer::CTokenIterator loc, FunctionType&& type, const Syntax::StorageClassSpecifier* storageClassSpecifier,
    bool isInline, std::vector<GNUAttribute>&& attributes)
{
    Linkage linkage = Linkage::External;
    // C99 6.7.1§5:
    // The declaration of an identifier for a function that has block scope shall have no explicit
    // storage-class specifier other than extern.
    if (m_currentScope != 0 && storageClassSpecifier
        && storageClassSpecifier->getSpecifier() != Syntax::StorageClassSpecifier::Extern)
    {
        log(Errors::Semantics::FUNCTION_PROTOTYPE_AT_BLOCK_SCOPE_MAY_ONLY_BE_EXTERN.args(
            *storageClassSpecifier, m_sourceInterface, *storageClassSpecifier));
    }
    else if (storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Static)
    {
        linkage = Linkage::Internal;
    }
    if (loc->getText() == "__func__")
    {
        log(Errors::Semantics::DECLARING_FUNCTIONS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
            *loc, m_sourceInterface, *loc));
        return {};
    }

    // C99 6.7.4§4:
    // In a hosted environment, the inline function specifier shall not appear in a declaration
    // of main
    if (loc->getText() == "main" && isInline && !getLanguageOptions().freeStanding)
    {
        log(Errors::Semantics::INLINE_MAIN_IS_NOT_ALLOWED_IN_A_HOSTED_ENVIRONMENT.args(*loc, m_sourceInterface, *loc));
    }

    InlineKind inlineKind = InlineKind::None;
    // C99 6.7.4§6:
    // If all of the file scope declarations for a function in a translation unit include the inline function
    // specifier without extern, then the definition in that translation unit is an inline definition.
    if (isInline && storageClassSpecifier
        && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Extern)
    {
        inlineKind = InlineKind::Inline;
    }
    else if (isInline)
    {
        inlineKind = InlineKind::InlineDefinition;
    }

    auto declaration = std::make_unique<FunctionDeclaration>(std::move(type), linkage, loc, inlineKind);
    auto [prev, notRedefinition] =
        getCurrentScope().declarations.insert({loc->getText(), DeclarationInScope{loc, declaration.get()}});
    if (!notRedefinition)
    {
        if ((!std::holds_alternative<FunctionDeclaration*>(prev->second.declared)
             || !typesAreCompatible(declaration->getType(),
                                    cld::get<FunctionDeclaration*>(prev->second.declared)->getType()))
            && (!std::holds_alternative<FunctionDefinition*>(prev->second.declared)
                || !typesAreCompatible(declaration->getType(),
                                       cld::get<FunctionDefinition*>(prev->second.declared)->getType())))
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                     *prev->second.identifier));
        }
        else if (std::holds_alternative<FunctionDeclaration*>(prev->second.declared))
        {
            auto& prevDecl = *cld::get<FunctionDeclaration*>(prev->second.declared);
            auto& otherType = prevDecl.getType();
            auto composite = compositeType(otherType, declaration->getType());
            // C99 6.2.2§4:
            // For an identifier declared with the storage-class specifier extern in a scope in which a
            // prior declaration of that identifier is visible, if the prior declaration specifies internal or
            // external linkage, the linkage of the identifier at the later declaration is the same as the
            // linkage specified at the prior declaration. If no prior declaration is visible, or if the prior
            // declaration specifies no linkage, then the identifier has external linkage.
            if (prevDecl.getLinkage() == Linkage::Internal)
            {
                linkage = Linkage::Internal;
            }
            else if (linkage == Linkage::Internal)
            {
                // C99 6.2.2§7:
                // If, within a translation unit, the same identifier appears with both internal and external
                // linkage, the behavior is undefined.
                log(Errors::Semantics::REDECLARATION_OF_FUNCTION_N_WITH_INTERNAL_LINKAGE.args(*loc, m_sourceInterface,
                                                                                              *loc));
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
            *declaration = FunctionDeclaration(std::move(composite->as<FunctionType>()), linkage, loc,
                                               std::max(inlineKind, prevDecl.getInlineKind()));
            declaration->setUses(prevDecl.getUses());
            declaration->tryAddFromOther(prevDecl);
            attributes = applyAttributes(declaration.get(), std::move(attributes));
            reportNotApplicableAttributes(attributes);
            prev.value().declared = declaration.get();
            return declaration;
        }
        else if (std::holds_alternative<FunctionDefinition*>(prev->second.declared))
        {
            auto& fd = *cld::get<FunctionDefinition*>(prev->second.declared);

            // C99 6.2.2§7:
            // If, within a translation unit, the same identifier appears with both internal and external
            // linkage, the behavior is undefined.
            if (linkage == Linkage::Internal && fd.getLinkage() == Linkage::External)
            {
                log(Errors::Semantics::REDECLARATION_OF_FUNCTION_N_WITH_INTERNAL_LINKAGE.args(*loc, m_sourceInterface,
                                                                                              *loc));
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
            inlineKind = std::max(fd.getInlineKind(), inlineKind);
            fd = FunctionDefinition(fd.getType(), fd.getNameToken(), std::move(fd).getParameterDeclarations(),
                                    fd.getLinkage(), inlineKind, std::move(fd).getCompoundStatement());
        }
        return {};
    }
    attributes = applyAttributes(declaration.get(), std::move(attributes), FunctionContext{isInline});
    reportNotApplicableAttributes(attributes);
    return declaration;
}

std::unique_ptr<cld::Semantics::CompoundStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::CompoundStatement& node, bool pushScope)
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
    return std::make_unique<CompoundStatement>(m_currentScope, node.begin(), std::move(result), node.end() - 1);
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

cld::IntrVarPtr<cld::Semantics::Statement> cld::Semantics::SemanticAnalysis::visit(const Syntax::Statement& node)
{
    return cld::match(
        node, [&](const auto& node) -> cld::IntrVarPtr<Statement> { return visit(node); },
        [&](const Syntax::ExpressionStatement& node) -> cld::IntrVarPtr<Statement> {
            if (!node.getOptionalExpression())
            {
                return std::make_unique<ExpressionStatement>(m_currentScope, nullptr);
            }
            return std::make_unique<ExpressionStatement>(m_currentScope, visit(*node.getOptionalExpression()));
        });
}

bool cld::Semantics::SemanticAnalysis::isTypedef(std::string_view name) const
{
    auto range = scopeIterator(m_currentScope);
    return std::any_of(range.begin(), range.end(),
                       [name](const Scope& scope)
                       {
                           auto result = scope.declarations.find(name);
                           if (result != scope.declarations.end()
                               && std::holds_alternative<TypedefInfo*>(result->second.declared))
                           {
                               return true;
                           }
                           return false;
                       });
}

bool cld::Semantics::SemanticAnalysis::isTypedefInScope(std::string_view name) const
{
    for (auto& scope : scopeIterator(m_currentScope))
    {
        auto result = scope.declarations.find(name);
        if (result != scope.declarations.end())
        {
            return std::holds_alternative<TypedefInfo*>(result->second.declared);
        }
        return false;
    }
    return false;
}

const cld::Semantics::SemanticAnalysis::DeclarationInScope::Variant*
    cld::Semantics::SemanticAnalysis::lookupDecl(std::string_view name, std::int64_t scope)
{
    for (auto& iter : scopeIterator(scope))
    {
        auto result = iter.declarations.find(name);
        if (result != iter.declarations.end())
        {
            return &result->second.declared;
        }
    }
    return getBuiltinFuncDecl(name);
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

std::tuple<bool, bool, bool> cld::Semantics::SemanticAnalysis::getQualifiers(
    const std::vector<std::variant<Syntax::TypeQualifier, Syntax::GNUAttributes>>& typeQualifiers)
{
    bool isConst = false;
    bool isVolatile = false;
    bool isRestricted = false;
    for (auto& typeQual : typeQualifiers)
    {
        if (!std::holds_alternative<Syntax::TypeQualifier>(typeQual))
        {
            continue;
        }
        switch (cld::get<Syntax::TypeQualifier>(typeQual).getQualifier())
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
                                                          const cld::Semantics::Type& rhs, bool leftIsFuncDefinition)
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
        const auto& lhsType = lhs.match(
            [](auto&& value) -> const Type&
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<
                                  ArrayType,
                                  T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
                {
                    return value.getType();
                }
                CLD_UNREACHABLE;
            });
        const auto& rhsType = rhs.match(
            [](auto&& value) -> const Type&
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<
                                  ArrayType,
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
        if (!lhs.is<ArrayType>() || !rhs.is<ArrayType>())
        {
            return true;
        }
        return lhs.as<ArrayType>().getSize() == rhs.as<ArrayType>().getSize();
    }
    if (lhs.index() != rhs.index())
    {
        return false;
    }
    if (lhs.is<PointerType>())
    {
        auto& lhsType = lhs.as<PointerType>();
        auto& rhsType = rhs.as<PointerType>();
        if (lhsType.isRestricted() != rhsType.isRestricted())
        {
            return false;
        }
        return typesAreCompatible(lhsType.getElementType(), rhsType.getElementType());
    }
    if (lhs.is<FunctionType>())
    {
        // C99 6.7.5.3§15:
        // (In the determination of type
        // compatibility and of a composite type, each parameter declared with function or array
        // type is taken as having the adjusted type and each parameter declared with qualified type
        // is taken as having the unqualified version of its declared type.)
        auto& lhsFtype = lhs.as<FunctionType>();
        auto& rhsFtype = rhs.as<FunctionType>();
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
            if (kandRFunc.getParameters().empty())
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
                if (lhsFtype.isKandR() && leftIsFuncDefinition && !paramFunc.getParameters().empty())
                {
                    return false;
                }
                for (auto& iter : paramFunc.getParameters())
                {
                    auto nonQualifiedType = removeQualifiers(*iter.type);
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
            if (kandRFunc.getParameters().size() != paramFunc.getParameters().size())
            {
                return false;
            }
            for (std::size_t i = 0; i < kandRFunc.getParameters().size(); i++)
            {
                auto kandRType = adjustParameterType(*kandRFunc.getParameters()[i].type);
                auto paramType = adjustParameterType(*paramFunc.getParameters()[i].type);
                auto nonQualifiedParam = removeQualifiers(paramType);
                if (!typesAreCompatible(defaultArgumentPromotion(removeQualifiers(kandRType)), nonQualifiedParam))
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
        if (lhsFtype.getParameters().size() != rhsFtype.getParameters().size())
        {
            return false;
        }
        if (lhsFtype.isLastVararg() != rhsFtype.isLastVararg())
        {
            return false;
        }
        for (std::size_t i = 0; i < lhsFtype.getParameters().size(); i++)
        {
            auto lhsType = adjustParameterType(*lhsFtype.getParameters()[i].type);
            auto rhsType = adjustParameterType(*rhsFtype.getParameters()[i].type);
            auto nonQualifiedLhs = removeQualifiers(lhsType);
            auto nonQualifiedRhs = removeQualifiers(rhsType);
            if (!typesAreCompatible(nonQualifiedLhs, nonQualifiedRhs))
            {
                return false;
            }
        }
        return true;
    }
    return lhs == rhs;
}

cld::IntrVarValue<cld::Semantics::Type>
    cld::Semantics::SemanticAnalysis::defaultArgumentPromotion(cld::IntrVarValue<Type> type)
{
    auto* prim = type->tryAs<PrimitiveType>();
    if (!prim)
    {
        return type;
    }
    if (prim->isFloatingPoint())
    {
        if (prim->getKind() == PrimitiveType::Float)
        {
            return PrimitiveType(PrimitiveType::Double, getLanguageOptions(), flag::useFlags = type->getFlags());
        }
        return type;
    }
    return integerPromotion(std::move(type));
}

cld::IntrVarValue<cld::Semantics::Type> cld::Semantics::SemanticAnalysis::integerPromotion(cld::IntrVarValue<Type> type)
{
    if (auto* enumType = type->tryAs<EnumType>())
    {
        return enumType->getInfo().type.getType();
    }
    auto* prim = type->tryAs<PrimitiveType>();
    if (!prim)
    {
        return lvalueConversion(std::move(type));
    }
    if (prim->isFloatingPoint())
    {
        return lvalueConversion(std::move(type));
    }
    if (prim->getBitCount() == 0)
    {
        return lvalueConversion(std::move(type));
    }
    if (prim->getBitCount() < getLanguageOptions().sizeOfInt * 8)
    {
        return PrimitiveType(PrimitiveType::Int, getLanguageOptions(), flag::useFlags = type->getFlags());
    }
    return lvalueConversion(std::move(type));
}

cld::IntrVarValue<cld::Semantics::Type> cld::Semantics::SemanticAnalysis::compositeType(const cld::Semantics::Type& lhs,
                                                                                        const cld::Semantics::Type& rhs)
{
    if (isArray(lhs) || isArray(rhs))
    {
        auto getElementType = [](const Type& type) -> const Type&
        {
            return type.match(
                [](auto&& value) -> const Type&
                {
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
        if (auto* array = lhs.tryAs<ArrayType>())
        {
            return ArrayType(typeAlloc(*compositeType(array->getType(), getElementType(rhs))), array->getSize(),
                             flag::useFlags = lhs.getFlags());
        }
        if (auto* array = rhs.tryAs<ArrayType>())
        {
            return ArrayType(typeAlloc(*compositeType(array->getType(), getElementType(lhs))), array->getSize(),
                             flag::useFlags = rhs.getFlags());
        }
        if (auto* valArray = lhs.tryAs<ValArrayType>())
        {
            return ValArrayType(typeAlloc(*compositeType(valArray->getType(), getElementType(rhs))),
                                valArray->getExpression(), flag::useFlags = lhs.getFlags());
        }
        if (auto* valArray = rhs.tryAs<ValArrayType>())
        {
            return ValArrayType(typeAlloc(*compositeType(valArray->getType(), getElementType(lhs))),
                                valArray->getExpression(), flag::useFlags = rhs.getFlags());
        }
        return AbstractArrayType(typeAlloc(*compositeType(getElementType(lhs), getElementType(rhs))),
                                 flag::useFlags = rhs.getFlags() | lhs.getFlags());
    }
    if (lhs.is<FunctionType>())
    {
        auto& lhsFtype = lhs.as<FunctionType>();
        auto& rhsFtype = rhs.as<FunctionType>();
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
        std::vector<FunctionType::Parameter> parameters;
        for (std::size_t i = 0; i < rhsFtype.getParameters().size(); i++)
        {
            parameters.push_back(
                {typeAlloc(*compositeType(*lhsFtype.getParameters()[i].type, *rhsFtype.getParameters()[i].type)),
                 rhsFtype.getParameters()[i].name});
        }
        return FunctionType(typeAlloc(*compositeType(lhsFtype.getReturnType(), rhsFtype.getReturnType())),
                            std::move(parameters), flag::isVARArg = rhsFtype.isLastVararg());
    }
    if (lhs.is<PointerType>())
    {
        return PointerType(typeAlloc(*compositeType(getPointerElementType(lhs), getPointerElementType(rhs))),
                           flag::useFlags = rhs.getFlags() | lhs.getFlags());
    }
    return *typeAlloc(rhs);
}

bool cld::Semantics::SemanticAnalysis::hasFlexibleArrayMember(const Type& type) const
{
    if (auto* structType = type.tryAs<StructType>())
    {
        auto* maybeStructDef = std::get_if<StructDefinition>(&structType->getInfo().type);
        if (maybeStructDef)
        {
            return !maybeStructDef->getFields().empty()
                   && isAbstractArray(*maybeStructDef->getFields().back().second.type);
        }
    }
    else if (auto* unionType = type.tryAs<UnionType>())
    {
        auto* maybeUnionDef = std::get_if<UnionDefinition>(&unionType->getInfo().type);
        if (maybeUnionDef)
        {
            for (auto& [name, field] : maybeUnionDef->getFields())
            {
                if (isAbstractArray(*field.type))
                {
                    return true;
                }
                if (hasFlexibleArrayMember(*field.type))
                {
                    return true;
                }
            }
            return false;
        }
    }
    return false;
}

cld::Expected<cld::Semantics::ConstValue, std::vector<cld::Message>>
    cld::Semantics::SemanticAnalysis::evaluateConstantExpression(const ExpressionBase& constantExpression, Mode mode)
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
    std::for_each(messages.begin(), messages.end(), cld::bind_front(&SemanticAnalysis::log, this));
    return value;
}

cld::Semantics::ConstValue
    cld::Semantics::SemanticAnalysis::evaluate(const ExpressionBase& expression, Mode mode,
                                               cld::function_ref<void(const Message&)> logger) const
{
    auto typeCheck = [=](const ExpressionBase& exp, const ConstValue& value) {
        if (!value.isUndefined() && !isInteger(exp.getType()) && mode == Integer)
        {
            logger(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(exp, m_sourceInterface,
                                                                                                 exp));
            return false;
        }
        return !value.isUndefined();
    };
    return expression.match(
        [](const ErrorExpression&) { return ConstValue{}; },
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
                || declRead.getDeclRead().match([](const FunctionDefinition&) { return false; },
                                                [](const FunctionDeclaration&) { return false; },
                                                [](const VariableDeclaration& declaration)
                                                { return declaration.getLifetime() != Lifetime::Static; },
                                                [](const BuiltinFunction&)
                                                {
                                                    // TODO:?
                                                    return false;
                                                }))
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
                        || conversion.getExpression().is<CompoundLiteral>()
                        || conversion.getExpression().getType().is<FunctionType>()))
                {
                    return {AddressConstant{}};
                }
                logger(Errors::Semantics::VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                    conversion.getExpression(), m_sourceInterface, conversion.getExpression()));
                return {};
            }
            if (conversion.getKind() == Conversion::Implicit && !typeCheck(conversion.getExpression(), exp))
            {
                return {};
            }
            return exp.castTo(conversion.getType(), this, getLanguageOptions());
        },
        [&](const BinaryOperator& binaryOperator) -> ConstValue {
            auto lhs = evaluate(binaryOperator.getLeftExpression(), mode, logger);
            bool integer = typeCheck(binaryOperator.getLeftExpression(), lhs);
            if (binaryOperator.getKind() == BinaryOperator::LogicAnd
                || binaryOperator.getKind() == BinaryOperator::LogicOr)
            {
                if (!integer)
                {
                    return {};
                }
                switch (binaryOperator.getKind())
                {
                    default: CLD_UNREACHABLE;
                    case BinaryOperator::LogicAnd:
                    {
                        if (!lhs)
                        {
                            // Check if the right operand is an integer so that we do check if it'd be a valid integer
                            // constant expression if this wasn't short circuiting. This actually isn't sufficient
                            // though. TODO: Implement a tree walk to check for non integer constant expressions
                            if (mode == Integer && binaryOperator.getRightExpression().is<Conversion>()
                                && binaryOperator.getRightExpression().as<Conversion>().getKind()
                                       == Conversion::Implicit
                                && isBool(binaryOperator.getRightExpression().getType()))
                            {
                                auto& rExpr = binaryOperator.getRightExpression().as<Conversion>().getExpression();
                                if (!rExpr.getType().isUndefined() && !isInteger(rExpr.getType()))
                                {
                                    logger(
                                        Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                                            rExpr, m_sourceInterface, rExpr));
                                }
                            }
                            return {llvm::APSInt(llvm::APInt(getLanguageOptions().sizeOfInt * 8, 0), false)};
                        }

                        auto rhs = evaluate(binaryOperator.getRightExpression(), mode, logger);
                        if (!typeCheck(binaryOperator.getRightExpression(), rhs))
                        {
                            return {};
                        }
                        return {llvm::APSInt(llvm::APInt(getLanguageOptions().sizeOfInt * 8, static_cast<bool>(rhs)),
                                             false)};
                    }
                    case BinaryOperator::LogicOr:
                    {
                        if (lhs)
                        {
                            // TODO: Implement a tree walk to check for non integer constant expressions
                            if (mode == Integer && binaryOperator.getRightExpression().is<Conversion>()
                                && binaryOperator.getRightExpression().as<Conversion>().getKind()
                                       == Conversion::Implicit
                                && isBool(binaryOperator.getRightExpression().getType()))
                            {
                                auto& rExpr = binaryOperator.getRightExpression().as<Conversion>().getExpression();
                                if (!rExpr.getType().isUndefined() && !isInteger(rExpr.getType()))
                                {
                                    logger(
                                        Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                                            rExpr, m_sourceInterface, rExpr));
                                }
                            }
                            return {llvm::APSInt(llvm::APInt(getLanguageOptions().sizeOfInt * 8, 1), false)};
                        }

                        auto rhs = evaluate(binaryOperator.getRightExpression(), mode, logger);
                        if (!typeCheck(binaryOperator.getRightExpression(), rhs))
                        {
                            return {};
                        }
                        return {llvm::APSInt(llvm::APInt(getLanguageOptions().sizeOfInt * 8, static_cast<bool>(rhs)),
                                             false)};
                    }
                }
            }
            auto rhs = evaluate(binaryOperator.getRightExpression(), mode, logger);
            if (!integer || !typeCheck(binaryOperator.getRightExpression(), rhs))
            {
                return {};
            }
            switch (binaryOperator.getKind())
            {
                case BinaryOperator::LogicOr:
                case BinaryOperator::LogicAnd: CLD_UNREACHABLE;
                case BinaryOperator::Addition:
                {
                    ConstValue::Issue issue = ConstValue::NoIssue;
                    auto result = lhs.plus(rhs, getLanguageOptions(), &issue);
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
                    auto result = lhs.minus(rhs, getLanguageOptions(), &issue);
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
                    auto result = lhs.multiply(rhs, getLanguageOptions(), &issue);
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
                    auto result = lhs.divide(rhs, getLanguageOptions(), &issue);
                    if (issue == ConstValue::NotRepresentable)
                    {
                        // TODO:
                    }
                    if (issue == ConstValue::IntDivByZero)
                    {
                        logger(Errors::Semantics::INTEGER_DIVISION_BY_ZERO_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                            binaryOperator, m_sourceInterface, binaryOperator, binaryOperator.getRightExpression(),
                            rhs));
                    }
                    return result;
                }
                case BinaryOperator::Modulo: return lhs.modulo(rhs, getLanguageOptions());
                case BinaryOperator::LeftShift:
                {
                    ConstValue::Issue issue = ConstValue::NoIssue;
                    auto result = lhs.shiftLeft(rhs, getLanguageOptions(), &issue);
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
                    auto result = lhs.shiftRight(rhs, getLanguageOptions(), &issue);
                    if (issue == ConstValue::NotRepresentable)
                    {
                        // TODO:
                    }
                    return result;
                }
                case BinaryOperator::LessThan: return lhs.lessThan(rhs, getLanguageOptions());
                case BinaryOperator::GreaterThan: return lhs.greaterThan(rhs, getLanguageOptions());
                case BinaryOperator::LessOrEqual: return lhs.lessOrEqual(rhs, getLanguageOptions());
                case BinaryOperator::GreaterOrEqual: return lhs.greaterOrEqual(rhs, getLanguageOptions());
                case BinaryOperator::Equal: return lhs.equal(rhs, getLanguageOptions());
                case BinaryOperator::NotEqual: return lhs.notEqual(rhs, getLanguageOptions());
                case BinaryOperator::BitOr: return lhs.bitOr(rhs, getLanguageOptions());
                case BinaryOperator::BitAnd: return lhs.bitAnd(rhs, getLanguageOptions());
                case BinaryOperator::BitXor: return lhs.bitXor(rhs, getLanguageOptions());
            }
            CLD_UNREACHABLE;
        },
        [&](const Cast& cast) -> ConstValue {
            if (mode == Integer && !isInteger(expression.getType()))
            {
                logger(Errors::Semantics::CANNOT_CAST_TO_NON_INTEGER_TYPE_IN_INTEGER_CONSTANT_EXPRESSION.args(
                    std::forward_as_tuple(*(cast.getOpenParentheses() + 1), *(cast.getCloseParentheses() - 1)),
                    m_sourceInterface,
                    std::forward_as_tuple(*(cast.getOpenParentheses() + 1), *(cast.getCloseParentheses() - 1))));
                return {};
            }
            if (mode == Arithmetic && !isArithmetic(expression.getType()))
            {
                logger(Errors::Semantics::CANNOT_CAST_TO_NON_ARITHMETIC_TYPE_IN_ARITHMETIC_CONSTANT_EXPRESSION.args(
                    std::forward_as_tuple(*(cast.getOpenParentheses() + 1), *(cast.getCloseParentheses() - 1)),
                    m_sourceInterface,
                    std::forward_as_tuple(*(cast.getOpenParentheses() + 1), *(cast.getCloseParentheses() - 1))));
                return {};
            }
            ConstValue::Issue issue;
            auto original = evaluate(cast.getExpression(), mode, logger);
            auto ret = original.castTo(expression.getType(), this, getLanguageOptions(), &issue);
            if (issue == ConstValue::NotRepresentable)
            {
                logger(Warnings::Semantics::VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N.args(
                    cast.getExpression(), m_sourceInterface, original, expression.getType(), cast.getExpression()));
            }
            return ret;
        },
        [&](const UnaryOperator& unaryOperator) -> ConstValue {
            if (unaryOperator.getKind() == UnaryOperator::AddressOf)
            {
                if (unaryOperator.getOperand().is<UnaryOperator>())
                {
                    auto& innerUnary = unaryOperator.getOperand().as<UnaryOperator>();
                    if (innerUnary.getKind() == UnaryOperator::Dereference)
                    {
                        return evaluate(innerUnary.getOperand(), mode, logger);
                    }
                }
                else if (unaryOperator.getOperand().is<SubscriptOperator>())
                {
                    auto& subScript = unaryOperator.getOperand().as<SubscriptOperator>();
                    auto lhs = evaluate(subScript.getLeftExpression(), mode, logger);
                    auto rhs = evaluate(subScript.getRightExpression(), mode, logger);
                    if (lhs.isUndefined() || rhs.isUndefined())
                    {
                        return {};
                    }
                    return lhs.plus(rhs, getLanguageOptions());
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
                case UnaryOperator::Minus: return op.negate(getLanguageOptions());
                case UnaryOperator::BooleanNegate: return op.logicalNegate(getLanguageOptions());
                case UnaryOperator::BitwiseNegate: return op.bitwiseNegate(getLanguageOptions());
            }
            CLD_UNREACHABLE;
        },
        [&](const SizeofOperator& sizeofOperator) -> ConstValue {
            if (sizeofOperator.getSize())
            {
                auto type = PrimitiveType(getLanguageOptions().sizeTType, getLanguageOptions());
                return {llvm::APSInt(llvm::APInt(type.as<PrimitiveType>().getBitCount(), *sizeofOperator.getSize()))};
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
                return result.castTo(expression.getType(), this, getLanguageOptions());
            }

            auto result = evaluate(conditional.getFalseExpression(), mode, logger);
            if (!typeCheck(conditional.getFalseExpression(), result))
            {
                return {};
            }
            return result.castTo(expression.getType(), this, getLanguageOptions());
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
        },
        [&](const BuiltinVAArg& builtinVaArg) -> ConstValue {
            logger(Errors::Semantics::FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                builtinVaArg, m_sourceInterface, builtinVaArg));
            return {};
        },
        [&](const BuiltinOffsetOf& builtinOffsetOf) -> ConstValue {
            auto type = PrimitiveType(getLanguageOptions().sizeTType, getLanguageOptions());
            return {llvm::APSInt(llvm::APInt(type.as<PrimitiveType>().getBitCount(), builtinOffsetOf.getOffset()))};
        });
}

cld::ValueReset<bool> cld::Semantics::SemanticAnalysis::enableExtensions(bool extensions)
{
    auto prev = m_extensionsEnabled;
    m_extensionsEnabled = prev || extensions;
    return cld::ValueReset<bool>(m_extensionsEnabled, prev);
}

bool cld::Semantics::SemanticAnalysis::extensionsEnabled(const cld::Lexer::CToken* token)
{
    return m_extensionsEnabled || getLanguageOptions().extension == LanguageOptions::Extension::GNU
           || (token && m_sourceInterface.getFiles()[token->getFileId()].systemHeader);
}

auto cld::Semantics::SemanticAnalysis::insertTypedef(TypedefInfo typedefInfo)
    -> std::pair<tsl::ordered_map<std::string_view, DeclarationInScope>::iterator, bool>
{
    auto& back = m_typedefDefinitions.emplace_back(std::move(typedefInfo));
    back.type->setTypedef(&back);
    return getCurrentScope().declarations.emplace(typedefInfo.name,
                                                  DeclarationInScope{typedefInfo.identifierToken, &back});
}

void cld::Semantics::SemanticAnalysis::diagnoseUnusedLocals()
{
    for (auto& [name, declInScope] : getCurrentScope().declarations)
    {
        cld::match(
            declInScope.declared, [](FunctionDeclaration*) {},
            [this, &declInScope = declInScope](VariableDeclaration* declaration)
            {
                if (declaration->isUsed() || declaration->getLinkage() == Linkage::External || !declInScope.identifier)
                {
                    return;
                }
                log(Warnings::Semantics::UNUSED_VARIABLE_N.args(*declInScope.identifier, m_sourceInterface,
                                                                *declInScope.identifier));
            },
            [](FunctionDefinition*) {}, [](BuiltinFunction*) {}, [](const TypedefInfo*) {},
            [](const std::pair<ConstValue, IntrVarValue<Type>>&) {});
    }
}

std::vector<cld::Semantics::SemanticAnalysis::GNUAttribute>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::GNUAttributes& node)
{
    std::vector<GNUAttribute> result;
    for (auto& iter : node.getAttributes())
    {
        std::vector<std::shared_ptr<ExpressionBase>> parameters;
        std::transform(iter.arguments.begin(), iter.arguments.end(), std::back_inserter(parameters),
                       [this](auto&& expr) -> std::shared_ptr<ExpressionBase> { return visit(expr); });
        result.push_back(GNUAttribute{GNUAttribute::Nothing, iter.nameToken, iter.optionalFirstIdentifierArgument,
                                      std::move(parameters)});
    }
    return result;
}
