#include "SemanticAnalysis.hpp"

#include "ErrorMessages.hpp"

cld::Semantics::TranslationUnit cld::Semantics::SemanticAnalysis::visit(const Syntax::TranslationUnit& node)
{
    std::vector<IntrVarPtr<Useable>> globals;
    for (auto& iter : node.getGlobals())
    {
        auto result = cld::match(
            iter,
            [&](const Syntax::FunctionDefinition& value) -> std::vector<IntrVarPtr<Useable>>
            {
                std::vector<IntrVarPtr<Useable>> result;
                result.emplace_back(visit(value));
                return result;
            },
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
            if ((*decl)->getLinkage() == Linkage::External && (*decl)->getFunctionGroup().isInline()
                && !(*decl)->getFunctionGroup().isDefined())
            {
                log(Errors::Semantics::NO_DEFINITION_FOR_INLINE_FUNCTION_N_FOUND.args(
                    *(*decl)->getNameToken(), m_sourceInterface, *(*decl)->getNameToken()));
            }
        }
        cld::match(
            declared.declared, [](const auto&) {},
            [&declared = declared, this](VariableDeclaration* declaration)
            {
                if (declaration->getKind() == VariableDeclaration::TentativeDefinition
                    && !isCompleteType(declaration->getType()) && !declaration->getType().is<AbstractArrayType>())
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
            [&declared = declared, this](FunctionDefinition* functionDefinition)
            {
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

std::unique_ptr<cld::Semantics::FunctionDefinition>
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
    std::vector<ParsedAttribute<>> attributes;
    auto type = declaratorsToType(
        node.getDeclarationSpecifiers(), node.getDeclarator(), node.getDeclarations(),
        [&](IntrVarValue<Type> paramType, Lexer::CTokenIterator loc,
            const std::vector<Syntax::DeclarationSpecifier>& declarationSpecifiers,
            std::vector<ParsedAttribute<>>&& attributes)
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

            auto& ptr = parameterDeclarations.emplace_back(
                std::make_unique<VariableDeclaration>(std::move(paramType), Linkage::None, lifetime, loc,
                                                      VariableDeclaration::Kind::Definition, m_currentScope));
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
                if (!ptr->hasAttribute<DeprecatedAttribute>())
                {
                    checkForDeprecatedType(ptr->getType());
                }
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
            [&](const Syntax::DirectDeclaratorParenthesesParameters& dd)
            {
                parameters = &dd;
                identifierList = nullptr;
            },
            [&](const Syntax::DirectDeclaratorParenthesesIdentifiers& dd)
            {
                parameters = nullptr;
                identifierList = &dd;
            },
            [](const Syntax::DirectDeclaratorIdentifier&) {},
            [&](const Syntax::DirectDeclaratorParentheses& parentheses)
            {
                if (!parentheses.getDeclarator().getPointers().empty())
                {
                    parameters = nullptr;
                    identifierList = nullptr;
                }
            },
            [&](const auto&)
            {
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
            auto element =
                std::find_if(begin, parameterDeclarations.end(),
                             [iter](const auto& ptr) { return ptr->getNameToken()->getText() == iter->getText(); });
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

    FunctionDeclaration* prevDecl = nullptr;
    if (auto result = m_scopes[GLOBAL_SCOPE].declarations.find(loc->getText());
        result != m_scopes[GLOBAL_SCOPE].declarations.end())
    {
        if (!std::holds_alternative<FunctionDeclaration*>(result->second.declared)
            || !typesAreCompatible(ft, cld::get<FunctionDeclaration*>(result->second.declared)->getType(), true))
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*result->second.identifier, m_sourceInterface,
                                                     *result->second.identifier));
        }
        else
        {
            prevDecl = cld::get<FunctionDeclaration*>(result->second.declared);
            if (prevDecl->getLinkage() == Linkage::Internal)
            {
                linkage = Linkage::Internal;
            }
            else if (linkage == Linkage::Internal)
            {
                log(Errors::Semantics::REDEFINITION_OF_FUNCTION_N_WITH_INTERNAL_LINKAGE.args(*loc, m_sourceInterface,
                                                                                             *loc));
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*result->second.identifier, m_sourceInterface,
                                                         *result->second.identifier));
            }
            if (prevDecl->removeAttribute<DllImportAttribute>())
            {
                log(Warnings::Semantics::ATTRIBUTE_DLLIMPORT_IGNORED_AFTER_DEFINITION_OF_FUNCTION_N.args(
                    *loc, m_sourceInterface, *loc));
            }
        }
    }

    auto ptr =
        std::make_unique<FunctionDefinition>(std::move(ft), loc, std::move(parameterDeclarations), linkage, inlineKind,
                                             CompoundStatement(m_currentScope, loc, {}, loc), prevDecl);
    attributes = applyAttributes(ptr.get(), std::move(attributes), FunctionContext{isInline});
    reportNotApplicableAttributes(attributes);
    if (prevDecl)
    {
        ptr->tryAddFromOther(*prevDecl);
    }
    m_scopes[GLOBAL_SCOPE].declarations.insert_or_assign(loc->getText(), DeclarationInScope{loc, ptr.get()});

    if (!ptr->hasAttribute<DeprecatedAttribute>())
    {
        checkForDeprecatedType(ptr->getType().getReturnType());
    }

    auto funcType = ArrayType(typeAlloc<PrimitiveType>(PrimitiveType::Char, getLanguageOptions(), flag::isConst = true),
                              loc->getText().size() + 1);
    auto funcName = std::make_unique<VariableDeclaration>(
        std::move(funcType), Linkage::Internal, Lifetime::Static, loc, VariableDeclaration::Definition, m_currentScope,
        std::make_unique<Constant>(
            ArrayType(typeAlloc<PrimitiveType>(PrimitiveType::Char, getLanguageOptions()), loc->getText().size() + 1),
            cld::to_string(loc->getText()), loc, loc + 1));
    getCurrentScope().declarations.insert({"__func__", DeclarationInScope{nullptr, funcName.get()}});
    // GCC Extensions
    getCurrentScope().declarations.insert({"__FUNCTION__", DeclarationInScope{nullptr, funcName.get()}});
    getCurrentScope().declarations.insert({"__PRETTY_FUNCTION__", DeclarationInScope{nullptr, funcName.get()}});

    auto functionScope = pushFunctionScope(*ptr);

    auto comp = visit(node.getCompoundStatement(), false);
    comp->prependItem(std::move(funcName));
    ptr->setCompoundStatement(std::move(*comp));
    return ptr;
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
            [](const Syntax::DeclarationSpecifier& declarationSpecifier)
            {
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

    std::vector<ParsedAttribute<>> attributes;
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

        bool isExtern =
            (storageClassSpecifier && storageClassSpecifier->getSpecifier() == Syntax::StorageClassSpecifier::Extern)
            || (!isTypedef
                && std::any_of(thisAttributes.begin(), thisAttributes.end(),
                               [](const ParsedAttribute<>& attribute)
                               { return std::holds_alternative<DllImportAttribute>(attribute.attribute); }));

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
        else if (isExtern)
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
            if (!cld::get<TypedefInfo*>(prev->second.declared)->hasAttribute<DeprecatedAttribute>())
            {
                checkForDeprecatedType(result);
            }
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
        if (m_currentScope == 0 && iter.optionalInitializer && !isExtern)
        {
            kind = VariableDeclaration::Definition;
        }
        else if (m_currentScope == 0 && !isExtern
                 && (!storageClassSpecifier || *storageClassSpecifier == Syntax::StorageClassSpecifier::Static))
        {
            // C99 6.9.2§2:
            // A declaration of an identifier for an object that has file scope without an initializer, and
            // without a storage-class specifier or with the storage-class specifier static, constitutes a
            // tentative definition
            kind = VariableDeclaration::TentativeDefinition;
        }
        else if (isExtern)
        {
            kind = VariableDeclaration::DeclarationOnly;
        }
        else
        {
            kind = VariableDeclaration::Definition;
        }

        VariableDeclaration* prevDecl = nullptr;
        if (auto prev = getCurrentScope().declarations.find(loc->getText());
            prev != getCurrentScope().declarations.end())
        {
            if (!std::holds_alternative<VariableDeclaration*>(prev->second.declared)
                // C99 6.7§3:
                // If an identifier has no linkage, there shall be no more than one declaration of the identifier
                // (in a declarator or type specifier) with the same scope and in the same name space, except
                // for tags as specified in 6.7.2.3.
                || cld::get<VariableDeclaration*>(prev->second.declared)->getLinkage() == Linkage::None
                || linkage == Linkage::None
                // C99 6.7§2:
                // All declarations in the same scope that refer to the same object or function shall specify
                // compatible types.
                || !typesAreCompatible(result, cld::get<VariableDeclaration*>(prev->second.declared)->getType())
                // C99 6.9§5:
                // If an identifier declared with external
                // linkage is used in an expression (other than as part of the operand of a sizeof operator
                // whose result is an integer constant), somewhere in the entire program there shall be
                // exactly one external definition for the identifier; otherwise, there shall be no more than
                // one.
                || (kind == VariableDeclaration::Definition
                    && cld::get<VariableDeclaration*>(prev->second.declared)->getKind()
                           == VariableDeclaration::Definition))
            {
                log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
            else
            {
                prevDecl = cld::get<VariableDeclaration*>(prev->second.declared);
                result = compositeType(prevDecl->getType(), result);
                // C99 6.2.2§4:
                // For an identifier declared with the storage-class specifier extern in a scope in which a
                // prior declaration of that identifier is visible, if the prior declaration specifies internal or
                // external linkage, the linkage of the identifier at the later declaration is the same as the
                // linkage specified at the prior declaration. If no prior declaration is visible, or if the prior
                // declaration specifies no linkage, then the identifier has external linkage.
                if (isExtern && prevDecl->getLinkage() != Linkage::None)
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
                if (kind == VariableDeclaration::Definition && prevDecl->removeAttribute<DllImportAttribute>())
                {
                    log(Warnings::Semantics::ATTRIBUTE_DLLIMPORT_IGNORED_AFTER_DEFINITION_OF_VARIABLE_N.args(
                        *loc, m_sourceInterface, *loc));
                }
                kind = std::max(kind, prevDecl->getKind());
            }
        }

        auto declaration = std::make_unique<VariableDeclaration>(std::move(result), linkage, lifetime, loc, kind,
                                                                 m_currentScope, std::nullopt, prevDecl);
        if (prevDecl && prevDecl->getKind() == VariableDeclaration::Definition)
        {
            for (auto& iter : thisAttributes)
            {
                if (log(Warnings::Semantics::ATTRIBUTE_N_ON_DECLARATION_OF_VARIABLE_N_MUST_PRECEDE_ITS_DEFINITION.args(
                        *iter.name, m_sourceInterface, *iter.name, *loc)))
                {
                    log(Notes::Semantics::VARIABLE_DEFINITION_HERE.args(*prevDecl->getNameToken(), m_sourceInterface,
                                                                        *prevDecl->getNameToken()));
                }
            }
        }
        thisAttributes = applyAttributes(declaration.get(), std::move(thisAttributes));
        reportNotApplicableAttributes(thisAttributes);
        if (prevDecl && prevDecl->getKind() != VariableDeclaration::Definition)
        {
            declaration->tryAddFromOther(*prevDecl);
        }
        else if (!prevDecl || prevDecl->getKind() != VariableDeclaration::Definition)
        {
            getCurrentScope().declarations.insert_or_assign(loc->getText(), DeclarationInScope{loc, declaration.get()});
        }

        if (!prevDecl && !declaration->hasAttribute<DeprecatedAttribute>())
        {
            checkForDeprecatedType(declaration->getType());
        }

        if (iter.optionalInitializer)
        {
            if (declaration->hasAttribute<DllImportAttribute>())
            {
                log(Errors::Semantics::DLLIMPORT_VARIABLE_N_CANNOT_BE_INITIALIZED.args(*loc, m_sourceInterface, *loc));
            }

            // C99 6.7.5§5:
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
            else if (!isCompleteType(declaration->getType()) && !declaration->getType().is<AbstractArrayType>())
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
                declaration->setType(std::move(prevType));
                declaration->setInitializer(std::move(expr));
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
        if (getCurrentFunctionScope()
            && getCurrentFunctionScope()->currentFunction->getInlineKind() == InlineKind::InlineDefinition
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
    bool isInline, std::vector<ParsedAttribute<>>&& attributes)
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

    Useable* previous = nullptr;
    if (auto prev = getCurrentScope().declarations.find(loc->getText()); prev != getCurrentScope().declarations.end())
    {
        if ((!std::holds_alternative<FunctionDeclaration*>(prev->second.declared)
             || !typesAreCompatible(type, cld::get<FunctionDeclaration*>(prev->second.declared)->getType()))
            && (!std::holds_alternative<FunctionDefinition*>(prev->second.declared)
                || !typesAreCompatible(type, cld::get<FunctionDefinition*>(prev->second.declared)->getType())))
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                     *prev->second.identifier));
            return {};
        }
        if (auto* prevDecl = std::get_if<FunctionDeclaration*>(&prev->second.declared))
        {
            previous = *prevDecl;
            type = compositeType(type, (*prevDecl)->getType())->as<FunctionType>();
            // C99 6.2.2§4:
            // For an identifier declared with the storage-class specifier extern in a scope in which a
            // prior declaration of that identifier is visible, if the prior declaration specifies internal or
            // external linkage, the linkage of the identifier at the later declaration is the same as the
            // linkage specified at the prior declaration. If no prior declaration is visible, or if the prior
            // declaration specifies no linkage, then the identifier has external linkage.
            if ((*prevDecl)->getLinkage() == Linkage::Internal)
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
        }
        if (auto* fd = std::get_if<FunctionDefinition*>(&prev->second.declared))
        {
            previous = *fd;
            // C99 6.2.2§7:
            // If, within a translation unit, the same identifier appears with both internal and external
            // linkage, the behavior is undefined.
            if (linkage == Linkage::Internal && (*fd)->getLinkage() == Linkage::External)
            {
                log(Errors::Semantics::REDECLARATION_OF_FUNCTION_N_WITH_INTERNAL_LINKAGE.args(*loc, m_sourceInterface,
                                                                                              *loc));
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
        }
    }

    auto declaration =
        std::make_unique<FunctionDeclaration>(std::move(type), linkage, loc, inlineKind, m_currentScope, previous);
    if (previous && previous->is<FunctionDefinition>())
    {
        for (auto& iter : attributes)
        {
            if (log(Warnings::Semantics::ATTRIBUTE_N_ON_DECLARATION_OF_FUNCTION_N_MUST_PRECEDE_ITS_DEFINITION.args(
                    *iter.name, m_sourceInterface, *iter.name, *loc)))
            {
                auto& fd = previous->as<FunctionDefinition>();
                log(Notes::Semantics::FUNCTION_DEFINITION_HERE.args(*fd.getNameToken(), m_sourceInterface,
                                                                    *fd.getNameToken()));
            }
        }
    }
    attributes = applyAttributes(declaration.get(), std::move(attributes));
    reportNotApplicableAttributes(attributes);

    if (previous)
    {
        declaration->tryAddFromOther(previous->match(
            [](auto&& value) -> AttributeHolder<FunctionAttribute>&
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_base_of_v<AttributeHolder<FunctionAttribute>, T>)
                {
                    return value;
                }
                else
                {
                    CLD_UNREACHABLE;
                }
            }));
    }
    getCurrentScope().declarations.insert_or_assign(loc->getText(), DeclarationInScope{loc, declaration.get()});
    return declaration;
}
