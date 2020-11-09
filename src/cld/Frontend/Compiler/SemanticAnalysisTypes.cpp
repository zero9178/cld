#include "SemanticAnalysis.hpp"

#include <bitset2.hpp>

#include "ErrorMessages.hpp"
#include "SemanticUtil.hpp"

template <class T>
void cld::Semantics::SemanticAnalysis::handleParameterList(
    Type& type, const Syntax::ParameterTypeList* parameterTypeList, T&& returnTypeLoc,
    cld::function_ref<void(const Type&, Lexer::CTokenIterator, const std::vector<Syntax::DeclarationSpecifier>&, bool)>
        paramCallback)
{
    if (std::holds_alternative<FunctionType>(type.getVariant()))
    {
        log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_A_FUNCTION.args(returnTypeLoc, m_sourceInterface,
                                                                                returnTypeLoc, type));
        type = Type{};
        return;
    }
    if (isArray(type))
    {
        log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_AN_ARRAY.args(returnTypeLoc, m_sourceInterface,
                                                                              returnTypeLoc, type));
        type = Type{};
        return;
    }
    if (std::holds_alternative<FunctionType>(type.getVariant()))
    {
        log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_A_FUNCTION.args(returnTypeLoc, m_sourceInterface,
                                                                                returnTypeLoc, type));
        type = Type{};
        return;
    }
    if (isArray(type))
    {
        log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_AN_ARRAY.args(returnTypeLoc, m_sourceInterface,
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
                log(Errors::Semantics::NO_STORAGE_CLASS_SPECIFIER_ALLOWED_IN_PARAMETER_BESIDES_REGISTER.args(
                    *storageSpec, m_sourceInterface, *storageSpec));
            }
        }
        ValueReset<bool> reset(m_inParameter, m_inParameter);
        m_inParameter = true;
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
            && !cld::get<std::unique_ptr<cld::Syntax::AbstractDeclarator>>(iter.declarator)
            && parameterTypeList->getParameters().size() == 1 && !parameterTypeList->hasEllipse())
        {
            type = FunctionType::create(std::move(type), {}, false, false);
            return;
        }
        if (isVoid(paramType))
        {
            log(Errors::Semantics::VOID_TYPE_NOT_ALLOWED_AS_FUNCTION_PARAMETER.args(
                iter.declarationSpecifiers, m_sourceInterface, iter.declarationSpecifiers));
            paramType = Type{};
        }
        // C99 6.7.5.2§1:
        // The optional type qualifiers and the keyword static shall appear only in a
        // declaration of a function parameter with an array type, and then only in the outermost
        // array type derivation.
        if (isArray(paramType))
        {
            auto visitor = RecursiveVisitor(paramType, ARRAY_TYPE_NEXT_FN);
            auto begin = visitor.begin();
            begin++;
            auto hasStatic = std::any_of(begin, visitor.end(), [](const Type& type) {
                if (std::holds_alternative<ValArrayType>(type.getVariant()))
                {
                    return cld::get<ValArrayType>(type.getVariant()).isStatic();
                }
                if (std::holds_alternative<ArrayType>(type.getVariant()))
                {
                    return cld::get<ArrayType>(type.getVariant()).isStatic();
                }
                return false;
            });
            if (hasStatic)
            {
                log(Errors::Semantics::STATIC_ONLY_ALLOWED_IN_OUTERMOST_ARRAY.args(iter.declarator, m_sourceInterface,
                                                                                   iter.declarator));
            }
            auto hasQualifiers = std::any_of(begin, visitor.end(), [](const Type& type) {
                if (std::holds_alternative<ValArrayType>(type.getVariant()))
                {
                    return cld::get<ValArrayType>(type.getVariant()).isRestricted() || type.isConst()
                           || type.isVolatile();
                }
                if (std::holds_alternative<ArrayType>(type.getVariant()))
                {
                    return cld::get<ArrayType>(type.getVariant()).isRestricted() || type.isConst() || type.isVolatile();
                }
                return false;
            });
            if (hasQualifiers)
            {
                log(Errors::Semantics::ARRAY_QUALIFIERS_ONLY_ALLOWED_IN_OUTERMOST_ARRAY.args(
                    iter.declarator, m_sourceInterface, iter.declarator));
            }
        }
        else
        {
            auto visitor = RecursiveVisitor(paramType, TYPE_NEXT_FN);
            auto hasStatic = std::any_of(visitor.begin(), visitor.end(), [](const Type& type) {
                if (std::holds_alternative<ValArrayType>(type.getVariant()))
                {
                    return cld::get<ValArrayType>(type.getVariant()).isStatic();
                }
                if (std::holds_alternative<ArrayType>(type.getVariant()))
                {
                    return cld::get<ArrayType>(type.getVariant()).isStatic();
                }
                return false;
            });
            if (hasStatic)
            {
                log(Errors::Semantics::ONLY_PARAMETER_OF_ARRAY_TYPE_MAY_BE_STATIC.args(
                    iter.declarator, m_sourceInterface, iter.declarator, paramType));
            }
            auto hasQualifiers = std::any_of(visitor.begin(), visitor.end(), [](const Type& type) {
                if (std::holds_alternative<ValArrayType>(type.getVariant()))
                {
                    return cld::get<ValArrayType>(type.getVariant()).isRestricted() || type.isConst()
                           || type.isVolatile();
                }
                if (std::holds_alternative<ArrayType>(type.getVariant()))
                {
                    return cld::get<ArrayType>(type.getVariant()).isRestricted() || type.isConst() || type.isVolatile();
                }
                return false;
            });
            if (hasQualifiers)
            {
                log(Errors::Semantics::ONLY_PARAMETER_OF_ARRAY_TYPE_MAY_BE_QUALIFIED.args(
                    iter.declarator, m_sourceInterface, iter.declarator, paramType));
            }
        }
        std::string_view name;
        const Lexer::CToken* loc = nullptr;
        if (std::holds_alternative<std::unique_ptr<Syntax::Declarator>>(iter.declarator))
        {
            loc = declaratorToLoc(*cld::get<std::unique_ptr<Syntax::Declarator>>(iter.declarator));
            name = loc->getText();
        }
        // Not transforming array types to pointers here as we might still want to use that information
        // to warn callers.
        if (std::holds_alternative<FunctionType>(paramType.getVariant()))
        {
            paramType = PointerType::create(false, false, false, std::move(paramType));
        }
        auto& ret = parameters.emplace_back(std::move(paramType), name);
        if (paramCallback && loc)
        {
            paramCallback(ret.first, loc, iter.declarationSpecifiers, true);
        }
    }
    type = FunctionType::create(std::move(type), std::move(parameters), parameterTypeList->hasEllipse(), false);
}

cld::Semantics::Type cld::Semantics::SemanticAnalysis::declaratorsToTypeImpl(
    const std::vector<DeclarationOrSpecifierQualifier>& declarationOrSpecifierQualifiers,
    const PossiblyAbstractQualifierRef& declarator, const std::vector<Syntax::Declaration>& declarations,
    cld::function_ref<void(const Type&, Lexer::CTokenIterator, const std::vector<Syntax::DeclarationSpecifier>&, bool)>
        paramCallback)
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
        const Syntax::Node* declarationsOwner{};
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
                [&](const Syntax::DirectDeclaratorParenthesesParameters& dd) {
                    declarationsOwner = &dd;
                    isFunctionPrototype = true;
                },
                [&](const Syntax::DirectDeclaratorParentheses& parentheses) {
                    if (!parentheses.getDeclarator().getPointers().empty())
                    {
                        isFunctionPrototype = false;
                    }
                },
                [](const Syntax::DirectDeclaratorIdentifier&) {}, [&](const auto&) { isFunctionPrototype = false; });
        }
        isFunctionPrototype = isFunctionPrototype && !paramCallback;
        auto changedValue = changeFunctionPrototypeScope(m_inFunctionPrototype || isFunctionPrototype);
        cld::matchWithSelf<void>(
            realDecl.getDirectDeclarator(), [](auto&&, const Syntax::DirectDeclaratorIdentifier&) {},
            [&](auto&& self, const Syntax::DirectDeclaratorParentheses& parentheses) {
                for (auto& iter : parentheses.getDeclarator().getPointers())
                {
                    auto [isConst, isVolatile, restricted] = getQualifiers(iter.getTypeQualifiers());
                    if (restricted && std::holds_alternative<FunctionType>(type.getVariant()))
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
                auto scope = cld::ScopeExit([&] { cld::match(noStaticOrAsterisk.getDirectDeclarator(), self); });
                handleArray(type, noStaticOrAsterisk.getTypeQualifiers(),
                            noStaticOrAsterisk.getAssignmentExpression().get(), nullptr, false,
                            noStaticOrAsterisk.getDirectDeclarator());
            },
            [&](auto&& self, const Syntax::DirectDeclaratorStatic& declaratorStatic) {
                auto scope = cld::ScopeExit([&] { cld::match(declaratorStatic.getDirectDeclarator(), self); });
                handleArray(type, declaratorStatic.getTypeQualifiers(), &declaratorStatic.getAssignmentExpression(),
                            declaratorStatic.getStaticLoc(), false, declaratorStatic.getDirectDeclarator());
            },
            [&](auto&& self, const Syntax::DirectDeclaratorAsterisk& asterisk) {
                auto scope = cld::ScopeExit([&] { cld::match(asterisk.getDirectDeclarator(), self); });
                if (!m_inFunctionPrototype)
                {
                    type = Type{};
                    log(Errors::Semantics::STAR_IN_ARRAY_DECLARATOR_ONLY_ALLOWED_IN_FUNCTION_PROTOTYPES.args(
                        *asterisk.getAsterisk(), m_sourceInterface, *asterisk.getAsterisk()));
                }
                handleArray(type, asterisk.getTypeQualifiers(), nullptr, nullptr, true, asterisk.getDirectDeclarator());
            },
            [&](auto&& self, const Syntax::DirectDeclaratorParenthesesIdentifiers& identifiers) {
                auto scope = cld::ScopeExit([&] { cld::match(identifiers.getDirectDeclarator(), self); });
                std::optional<decltype(pushScope())> scope2;
                if (isFunctionPrototype)
                {
                    scope2.emplace(pushScope());
                }
                if (std::holds_alternative<FunctionType>(type.getVariant()))
                {
                    log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                        std::forward_as_tuple(declarationOrSpecifierQualifiers[0], identifiers.getDirectDeclarator()),
                        m_sourceInterface,
                        std::forward_as_tuple(declarationOrSpecifierQualifiers[0], identifiers.getDirectDeclarator()),
                        type));
                    type = Type{};
                    return;
                }
                if (isArray(type))
                {
                    log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_AN_ARRAY.args(
                        std::forward_as_tuple(declarationOrSpecifierQualifiers[0], identifiers.getDirectDeclarator()),
                        m_sourceInterface,
                        std::forward_as_tuple(declarationOrSpecifierQualifiers[0], identifiers.getDirectDeclarator()),
                        type));
                    type = Type{};
                    return;
                }
                if (identifiers.getIdentifiers().empty() && !paramCallback)
                {
                    type = FunctionType::create(std::move(type), {}, false, true);
                    return;
                }
                if (!paramCallback || declarationsOwner != &identifiers)
                {
                    log(Errors::Semantics::IDENTIFIER_LIST_ONLY_ALLOWED_AS_PART_OF_A_FUNCTION_DEFINITION.args(
                        identifiers.getIdentifiers(), m_sourceInterface, identifiers.getIdentifiers()));
                    type = FunctionType::create(std::move(type), {}, false, true);
                    return;
                }
                std::unordered_map<std::string_view, std::size_t> paramNames;
                std::vector<std::pair<Type, std::string_view>> parameters;
                std::unordered_map<std::string_view, Lexer::CTokenIterator> seenParameters;
                for (auto& iter : identifiers.getIdentifiers())
                {
                    if (iter->getText() == "__func__")
                    {
                        log(Errors::Semantics::DECLARING_PARAMETERS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
                            *iter, m_sourceInterface, *iter));
                    }
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
                            // In case the parameter name __func__ appears in the declarations but not in the identifier
                            // list
                            if (loc->getText() == "__func__")
                            {
                                log(Errors::Semantics::DECLARING_PARAMETERS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR
                                        .args(*loc, m_sourceInterface, *loc));
                            }
                            else
                            {
                                log(Errors::Semantics::DECLARATION_OF_IDENTIFIER_LIST_NOT_BELONGING_TO_ANY_PARAMETER
                                        .args(*loc, m_sourceInterface, *loc, identifiers.getIdentifiers()));
                            }
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
                            parameters[result->second].first = std::move(paramType);
                            element = loc;
                            if (paramCallback)
                            {
                                paramCallback(parameters[result->second].first, loc, iter.getDeclarationSpecifiers(),
                                              false);
                            }
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
                auto scope = cld::ScopeExit([&] { cld::match(parameterList.getDirectDeclarator(), self); });
                std::optional<decltype(pushScope())> scope2;
                if (isFunctionPrototype)
                {
                    scope2.emplace(pushScope());
                }
                if (&parameterList == declarationsOwner)
                {
                    handleParameterList(
                        type, &parameterList.getParameterTypeList(),
                        std::forward_as_tuple(declarationOrSpecifierQualifiers, parameterList.getDirectDeclarator()),
                        paramCallback);
                }
                else
                {
                    handleParameterList(
                        type, &parameterList.getParameterTypeList(),
                        std::forward_as_tuple(declarationOrSpecifierQualifiers, parameterList.getDirectDeclarator()));
                }
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
                    if (restricted && std::holds_alternative<FunctionType>(type.getVariant()))
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
                auto scope = cld::ScopeExit([&] {
                    if (asterisk.getDirectAbstractDeclarator())
                    {
                        cld::match(*asterisk.getDirectAbstractDeclarator(), self);
                    }
                });
                if (!m_inFunctionPrototype)
                {
                    type = Type{};
                    log(Errors::Semantics::STAR_IN_ARRAY_DECLARATOR_ONLY_ALLOWED_IN_FUNCTION_PROTOTYPES.args(
                        *asterisk.getAsterisk(), m_sourceInterface, *asterisk.getAsterisk()));
                }
                if (asterisk.getDirectAbstractDeclarator())
                {
                    handleArray(type, {}, nullptr, nullptr, true, *asterisk.getDirectAbstractDeclarator());
                }
                else
                {
                    handleArray(type, {}, nullptr, nullptr, true, declarationOrSpecifierQualifiers);
                }
            },
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorAssignmentExpression& expression) {
                auto scope = cld::ScopeExit([&] {
                    if (expression.getDirectAbstractDeclarator())
                    {
                        cld::match(*expression.getDirectAbstractDeclarator(), self);
                    }
                });
                if (expression.getDirectAbstractDeclarator())
                {
                    handleArray(type, expression.getTypeQualifiers(), expression.getAssignmentExpression(), nullptr,
                                false, *expression.getDirectAbstractDeclarator());
                }
                else
                {
                    handleArray(type, expression.getTypeQualifiers(), expression.getAssignmentExpression(), nullptr,
                                false, declarationOrSpecifierQualifiers);
                }
            },
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorParameterTypeList& parameterTypeList) {
                auto scope = cld::ScopeExit([&] {
                    if (parameterTypeList.getDirectAbstractDeclarator())
                    {
                        cld::match(*parameterTypeList.getDirectAbstractDeclarator(), self);
                    }
                });
                handleParameterList(type, parameterTypeList.getParameterTypeList(), declarationOrSpecifierQualifiers,
                                    {});
            },
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorStatic& declaratorStatic) {
                auto scope = cld::ScopeExit([&] {
                    if (declaratorStatic.getDirectAbstractDeclarator())
                    {
                        cld::match(*declaratorStatic.getDirectAbstractDeclarator(), self);
                    }
                });
                handleArray(type, declaratorStatic.getTypeQualifiers(), &declaratorStatic.getAssignmentExpression(),
                            declaratorStatic.getStaticLoc(), false, declaratorStatic.getAssignmentExpression());
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
        // C99 6.7.3§8:
        // If the specification of an array type includes any type qualifiers, the element type is soqualified, not the
        // array type. If the specification of a function type includes any type qualifiers, the behavior is undefined
        if (isArray(*type))
        {
            if (!isConst && !isVolatile)
            {
                return *type;
            }
            auto& elementType = getArrayElementType(*type);
            if ((!isConst || elementType.isConst()) && (!isVolatile || elementType.isVolatile()))
            {
                return *type;
            }
            auto newElementType = Type(elementType.isConst() || isConst, elementType.isVolatile() || isVolatile,
                                       elementType.getVariant());
            return cld::match(
                type->getVariant(), [](const auto&) -> Type { CLD_UNREACHABLE; },
                [&](const ArrayType& arrayType) -> Type {
                    return ArrayType::create(type->isConst(), type->isVolatile(), arrayType.isRestricted(),
                                             arrayType.isStatic(), std::move(newElementType), arrayType.getSize());
                },
                [&](const AbstractArrayType& arrayType) -> Type {
                    return AbstractArrayType::create(type->isConst(), type->isVolatile(), arrayType.isRestricted(),
                                                     std::move(newElementType));
                },
                [&](const ValArrayType& arrayType) -> Type {
                    return ValArrayType::create(type->isConst(), type->isVolatile(), arrayType.isRestricted(),
                                                arrayType.isStatic(), std::move(newElementType),
                                                arrayType.getExpression());
                });
        }
        if ((isConst && !type->isConst()) || (isVolatile && !type->isVolatile()))
        {
            return Type(isConst || type->isConst(), isVolatile || type->isVolatile(), type->getVariant());
        }
        return *type;
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
                if (auto* unionTag = lookupType<UnionTag>(name))
                {
                    return UnionType::create(isConst, isVolatile, name, static_cast<std::size_t>(*unionTag));
                }
                m_unionDefinitions.push_back({UnionDecl{}, m_currentScope, structOrUnion->begin()});
                auto [prev, notRedefined] = getCurrentScope().types.insert(
                    {name, TagTypeInScope{structOrUnion->getIdentifierLoc(), UnionTag{m_unionDefinitions.size() - 1}}});
                if (!notRedefined)
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion->getIdentifierLoc(), m_sourceInterface,
                                                              *structOrUnion->getIdentifierLoc()));
                    if (prev->second.identifier)
                    {
                        log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                                 *prev->second.identifier));
                    }
                }
                return UnionType::create(isConst, isVolatile, name, m_unionDefinitions.size() - 1);
            }

            if (auto* structTag = lookupType<StructTag>(name))
            {
                return StructType::create(isConst, isVolatile, name, static_cast<std::size_t>(*structTag));
            }
            m_structDefinitions.push_back({StructDecl{}, m_currentScope, structOrUnion->begin()});
            auto [prev, notRedefined] = getCurrentScope().types.insert(
                {name, TagTypeInScope{structOrUnion->getIdentifierLoc(), StructTag{m_structDefinitions.size() - 1}}});
            if (!notRedefined)
            {
                log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion->getIdentifierLoc(), m_sourceInterface,
                                                          *structOrUnion->getIdentifierLoc()));
                if (prev->second.identifier)
                {
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
            }
            return StructType::create(isConst, isVolatile, name, m_structDefinitions.size() - 1);
        }

        std::optional<std::size_t> structOrUnionID;
        if (!structOrUnion->getIdentifierLoc())
        {
            if (structOrUnion->isUnion())
            {
                if (auto result = m_anonymousTagToID.find(structOrUnion->begin()); result != m_anonymousTagToID.end())
                {
                    return UnionType::create(isConst, isVolatile, "", result->second);
                }
                structOrUnionID = m_unionDefinitions.size();
                m_unionDefinitions.push_back({UnionDecl{}, m_currentScope, structOrUnion->begin()});
            }
            else
            {
                if (auto result = m_anonymousTagToID.find(structOrUnion->begin()); result != m_anonymousTagToID.end())
                {
                    return StructType::create(isConst, isVolatile, "", result->second);
                }
                structOrUnionID = m_structDefinitions.size();
                m_structDefinitions.push_back({StructDecl{}, m_currentScope, structOrUnion->begin()});
            }
        }
        else
        {
            auto name = structOrUnion->getIdentifierLoc()->getText();
            if (structOrUnion->isUnion())
            {
                auto [prev, notRedefined] = getCurrentScope().types.insert(
                    {name, TagTypeInScope{structOrUnion->getIdentifierLoc(), UnionTag{m_unionDefinitions.size()}}});
                if (!notRedefined)
                {
                    if (!std::holds_alternative<UnionTag>(prev->second.tagType)
                        || getUnionDefinition(static_cast<std::size_t>(cld::get<UnionTag>(prev->second.tagType))))
                    {
                        if (structOrUnion->getIdentifierLoc() == prev->second.identifier)
                        {
                            return UnionType::create(
                                isConst, isVolatile, name,
                                static_cast<std::size_t>(cld::get<UnionTag>(prev->second.tagType)));
                        }
                        log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion->getIdentifierLoc(), m_sourceInterface,
                                                                  *structOrUnion->getIdentifierLoc()));
                        if (prev->second.identifier)
                        {
                            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                                     *prev->second.identifier));
                        }
                    }
                    else
                    {
                        structOrUnionID = static_cast<std::size_t>(cld::get<UnionTag>(prev->second.tagType));
                    }
                }
                else
                {
                    structOrUnionID = m_unionDefinitions.size();
                    m_unionDefinitions.push_back({UnionDecl{}, m_currentScope, structOrUnion->begin()});
                }
            }
            else
            {
                auto [prev, notRedefined] = getCurrentScope().types.insert(
                    {name, TagTypeInScope{structOrUnion->getIdentifierLoc(), StructTag{m_structDefinitions.size()}}});
                if (!notRedefined)
                {
                    if (!std::holds_alternative<StructTag>(prev->second.tagType)
                        || getStructDefinition(static_cast<std::size_t>(cld::get<StructTag>(prev->second.tagType))))
                    {
                        if (structOrUnion->getIdentifierLoc() == prev->second.identifier)
                        {
                            return StructType::create(
                                isConst, isVolatile, name,
                                static_cast<std::size_t>(cld::get<StructTag>(prev->second.tagType)));
                        }
                        log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion->getIdentifierLoc(), m_sourceInterface,
                                                                  *structOrUnion->getIdentifierLoc()));
                        if (prev->second.identifier)
                        {
                            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                                     *prev->second.identifier));
                        }
                    }
                    else
                    {
                        structOrUnionID = static_cast<std::size_t>(cld::get<StructTag>(prev->second.tagType));
                    }
                }
                else
                {
                    structOrUnionID = m_structDefinitions.size();
                    m_structDefinitions.push_back({StructDecl{}, m_currentScope, structOrUnion->begin()});
                }
            }
        }

        FieldMap fields;
        std::vector<FieldInLayout> fieldLayout;
        std::unordered_set<std::size_t> zeroBitFields;
        for (auto iter = structOrUnion->getStructDeclarations().begin();
             iter != structOrUnion->getStructDeclarations().end(); iter++)
        {
            auto& [specifiers, declarators] = *iter;
            auto fieldStructOrUnion = std::find_if(
                specifiers.begin(), specifiers.end(), [](const Syntax::SpecifierQualifier& specifierQualifier) {
                    if (!std::holds_alternative<Syntax::TypeSpecifier>(specifierQualifier))
                    {
                        return false;
                    }
                    auto& specifier = cld::get<Syntax::TypeSpecifier>(specifierQualifier);
                    if (!std::holds_alternative<std::unique_ptr<Syntax::StructOrUnionSpecifier>>(
                            specifier.getVariant()))
                    {
                        return false;
                    }
                    return true;
                });
            bool hadExtension = (fieldStructOrUnion != specifiers.end()
                                 && cld::get<std::unique_ptr<Syntax::StructOrUnionSpecifier>>(
                                        cld::get<Syntax::TypeSpecifier>(*fieldStructOrUnion).getVariant())
                                        ->extensionsEnabled());
            auto enableReset = enableExtensions(hadExtension);
            if (declarators.empty())
            {
                auto type = declaratorsToType(specifiers);
                auto parentType = std::make_shared<const Type>(std::move(type));
                fieldLayout.push_back({parentType, static_cast<std::size_t>(-1), {}});
                if (!extensionsEnabled(structOrUnion->begin()) || !isAnonymous(*parentType))
                {
                    log(Errors::Semantics::FIELD_WITHOUT_A_NAME_IS_NOT_ALLOWED.args(specifiers, m_sourceInterface,
                                                                                    specifiers));
                    continue;
                }
                auto& subFields = getFields(*parentType);
                for (auto [name, field] : subFields)
                {
                    field.indices.insert(field.indices.begin(), static_cast<std::size_t>(-1));
                    field.parentTypes.insert(field.parentTypes.begin(), parentType);
                    if (std::pair(parentType->isConst(), parentType->isVolatile())
                        > std::pair(field.type->isConst(), field.type->isVolatile()))
                    {
                        field.type = std::make_shared<const Type>(parentType->isConst() || field.type->isConst(),
                                                                  parentType->isVolatile() || field.type->isVolatile(),
                                                                  field.type->getVariant());
                    }
                    const auto* token = field.nameToken;
                    auto [prev, notRedefined] = fields.insert({name, std::move(field)});
                    if (!notRedefined)
                    {
                        log(Errors::Semantics::REDEFINITION_OF_FIELD_N.args(*token, m_sourceInterface, *token));
                        if (prev->second.nameToken)
                        {
                            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.nameToken, m_sourceInterface,
                                                                     *prev->second.nameToken));
                        }
                    }
                }
                continue;
            }
            for (auto iter2 = declarators.begin(); iter2 != declarators.end(); iter2++)
            {
                bool last = iter2 + 1 == declarators.end() && iter + 1 == structOrUnion->getStructDeclarations().end();
                bool first = iter2 == declarators.begin() && iter == structOrUnion->getStructDeclarations().begin();

                auto& [declarator, size] = *iter2;
                auto type = declarator ? declaratorsToType(specifiers, *declarator) : declaratorsToType(specifiers);
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
                              && std::holds_alternative<AbstractArrayType>(type.getVariant())))
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
                else if (std::holds_alternative<FunctionType>(type.getVariant()))
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
                    if (std::holds_alternative<StructType>(type.getVariant()))
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
                std::optional<std::pair<std::uint32_t, std::uint32_t>> value;
                if (size)
                {
                    bool hadValidType = true;
                    if (!type.isUndefined() && !std::holds_alternative<PrimitiveType>(type.getVariant()))
                    {
                        log(Errors::Semantics::BITFIELD_MAY_ONLY_BE_OF_TYPE_INT_OR_BOOL.args(
                            specifiers, m_sourceInterface, specifiers));
                        hadValidType = false;
                    }
                    else if (!type.isUndefined())
                    {
                        auto& primitive = cld::get<PrimitiveType>(type.getVariant());
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
                    if (expr->isUndefined())
                    {
                        continue;
                    }
                    auto result = evaluateConstantExpression(*expr);
                    if (!result)
                    {
                        for (auto& message : result.error())
                        {
                            log(message);
                        }
                        continue;
                    }
                    CLD_ASSERT(std::holds_alternative<PrimitiveType>(expr->getType().getVariant()));
                    if (cld::get<PrimitiveType>(expr->getType().getVariant()).isSigned() && result->toInt() < 0)
                    {
                        log(Errors::Semantics::BITFIELD_MUST_BE_OF_SIZE_ZERO_OR_GREATER.args(*size, m_sourceInterface,
                                                                                             *size, result->toInt()));
                        continue;
                    }
                    if (!hadValidType)
                    {
                        continue;
                    }
                    auto objectWidth = cld::get<PrimitiveType>(type.getVariant()).getBitCount();
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
                    if (result->toUInt() == 0)
                    {
                        zeroBitFields.emplace(fields.size());
                        continue;
                    }
                    value.emplace(0, result->toUInt());
                }
                const auto* token = declarator ? declaratorToLoc(*declarator) : nullptr;
                if (token)
                {
                    auto sharedType = std::make_shared<Type>(std::move(type));
                    auto [prev, notRedefinition] = fields.insert(
                        {token->getText(),
                         {sharedType, token->getText(), token, {static_cast<std::size_t>(-1)}, value, {}}});
                    fieldLayout.push_back({sharedType, static_cast<std::size_t>(-1), value});
                    if (!notRedefinition)
                    {
                        log(Errors::Semantics::REDEFINITION_OF_FIELD_N.args(*token, m_sourceInterface, *token));
                        if (prev->second.nameToken)
                        {
                            log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.nameToken, m_sourceInterface,
                                                                     *prev->second.nameToken));
                        }
                    }
                }
            }
        }
        std::size_t currentSize = 0, currentAlignment = 0;
        std::vector<MemoryLayout> memoryLayout;
        std::size_t fieldLayoutCounter = 0;
        if (!structOrUnion->isUnion())
        {
            for (auto iter = fields.begin(); iter != fields.end();)
            {
                if (iter->second.type->isUndefined())
                {
                    iter.value().indices[0] = memoryLayout.size();
                    memoryLayout.push_back({*iter->second.type, currentSize});
                    iter++;
                    continue;
                }
                if (iter->second.indices.size() > 1)
                {
                    auto& parentType = iter->second.parentTypes.front();
                    auto end = std::find_if_not(iter, fields.end(), [&](const auto& pair) {
                        if (pair.second.parentTypes.empty())
                        {
                            return false;
                        }
                        return pair.second.parentTypes.front().get() == parentType.get();
                    });
                    fieldLayout[fieldLayoutCounter++].layoutIndex = memoryLayout.size();
                    for (; iter != end; iter++)
                    {
                        iter.value().indices[0] = memoryLayout.size();
                    }
                    auto alignment = parentType->getAlignOf(*this);
                    currentAlignment = std::max(currentAlignment, alignment);
                    currentSize = roundUpTo(currentSize, alignment);
                    memoryLayout.push_back({*parentType, currentSize});
                    auto subSize = parentType->getSizeOf(*this);
                    currentSize += subSize;
                    continue;
                }
                if (!iter->second.bitFieldBounds)
                {
                    iter.value().indices[0] = memoryLayout.size();
                    fieldLayout[fieldLayoutCounter++].layoutIndex = memoryLayout.size();
                    if (!isCompleteType(*iter->second.type)
                        && !std::holds_alternative<AbstractArrayType>(iter->second.type->getVariant()))
                    {
                        iter++;
                        continue;
                    }
                    auto alignment = iter->second.type->getAlignOf(*this);
                    currentAlignment = std::max(currentAlignment, alignment);
                    currentSize = roundUpTo(currentSize, alignment);
                    memoryLayout.push_back({*iter->second.type, currentSize});
                    if (!isCompleteType(*iter->second.type))
                    {
                        iter++;
                        continue;
                    }
                    auto subSize = iter->second.type->getSizeOf(*this);
                    currentSize += subSize;
                    iter++;
                    continue;
                }
                bool lastWasZero = false;
                std::uint64_t storageLeft = 0;
                std::uint64_t prevSize = 0;
                std::uint64_t used = 0;
                for (; iter != fields.end() && iter->second.bitFieldBounds;)
                {
                    if (zeroBitFields.count(iter - fields.begin()))
                    {
                        lastWasZero = true;
                    }
                    std::uint64_t size = iter->second.type->getSizeOf(*this);
                    if (!lastWasZero && storageLeft > iter->second.bitFieldBounds->second
                        && (!m_sourceInterface.getLanguageOptions().discreteBitfields || prevSize == size))
                    {
                        iter.value().indices[0] = memoryLayout.size() - 1;
                        storageLeft -= iter->second.bitFieldBounds->second;
                        fieldLayout[fieldLayoutCounter].bitFieldBounds.emplace(
                            used, used + iter->second.bitFieldBounds->second);
                        fieldLayout[fieldLayoutCounter++].layoutIndex = memoryLayout.size() - 1;
                        iter.value().bitFieldBounds.emplace(used, used + iter->second.bitFieldBounds->second);
                        used = iter->second.bitFieldBounds->second;
                        iter++;
                        continue;
                    }
                    lastWasZero = false;
                    currentSize += prevSize;
                    auto alignment = iter->second.type->getAlignOf(*this);
                    currentAlignment = std::max(currentAlignment, alignment);
                    currentSize = roundUpTo(currentSize, alignment);
                    prevSize = size;
                    storageLeft = cld::get<PrimitiveType>(iter->second.type->getVariant()).getBitCount()
                                  - iter->second.bitFieldBounds->second;
                    used = iter->second.bitFieldBounds->second;
                    iter.value().bitFieldBounds.emplace(0, used);
                    iter.value().indices[0] = memoryLayout.size();
                    fieldLayout[fieldLayoutCounter].bitFieldBounds.emplace(0, used);
                    fieldLayout[fieldLayoutCounter++].layoutIndex = memoryLayout.size();
                    memoryLayout.push_back({*iter->second.type, currentSize});
                    iter++;
                }
                currentSize += prevSize;
            }
        }
        else
        {
            for (auto iter = fields.begin(); iter != fields.end();)
            {
                auto& field = iter.value();
                if (field.type->isUndefined())
                {
                    iter++;
                    continue;
                }
                field.indices[0] = iter - fields.begin();
                fieldLayout[fieldLayoutCounter++].layoutIndex = iter - fields.begin();
                if (!fieldLayout[fieldLayoutCounter - 1].type->isUndefined())
                {
                    auto size = fieldLayout[fieldLayoutCounter - 1].type->getSizeOf(*this);
                    if (size > currentSize)
                    {
                        currentSize = size;
                    }
                    currentAlignment =
                        std::max(currentAlignment, fieldLayout[fieldLayoutCounter - 1].type->getAlignOf(*this));
                }
                if (!field.parentTypes.empty())
                {
                    auto end = std::find_if_not(iter + 1, fields.end(), [&](const auto& pair) {
                        if (pair.second.parentTypes.empty())
                        {
                            return false;
                        }
                        return pair.second.parentTypes.front().get() == field.parentTypes.front().get();
                    });
                    for (iter++; iter != end; iter++)
                    {
                        iter.value().indices[0] = fieldLayout[fieldLayoutCounter - 1].layoutIndex;
                    }
                }
                else
                {
                    iter++;
                }
            }
        }
        currentSize = roundUpTo(currentSize, currentAlignment);

        std::string_view name = structOrUnion->getIdentifierLoc() ? structOrUnion->getIdentifierLoc()->getText() : "";
        if (name.empty() && structOrUnionID)
        {
            m_anonymousTagToID.emplace(structOrUnion->begin(), *structOrUnionID);
        }
        if (structOrUnion->isUnion())
        {
            if (structOrUnionID)
            {
                m_unionDefinitions[*structOrUnionID].type.emplace<UnionDefinition>(
                    name, std::move(fields), std::move(fieldLayout), currentSize, currentAlignment);
                return UnionType::create(isConst, isVolatile, name, *structOrUnionID);
            }
            return Type{};
        }
        if (structOrUnionID)
        {
            m_structDefinitions[*structOrUnionID].type.emplace<StructDefinition>(
                name, std::move(fields), std::move(fieldLayout), std::move(memoryLayout), currentSize,
                currentAlignment);
            return StructType::create(isConst, isVolatile, name, *structOrUnionID);
        }
        return Type{};
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
        const auto* lookup = lookupType<EnumTag>((*loc)->getText());
        if (!lookup)
        {
            // C99 6.7.2.3:
            // A type specifier of the form
            //  enum identifier
            // without an enumerator list shall only appear after the type it specifies is complete
            log(Errors::Semantics::FORWARD_DECLARING_AN_ENUM_IS_NOT_ALLOWED.args(*typeSpec[0], m_sourceInterface,
                                                                                 *typeSpec[0]));
            return Type{};
        }
        return EnumType::create(isConst, isVolatile, (*loc)->getText(), static_cast<std::size_t>(*lookup));
    }
    auto& enumDef = cld::get<Syntax::EnumDeclaration>(enumDecl->getVariant());
    // TODO: Type depending on values as an extension
    const ConstValue one = {llvm::APSInt(llvm::APInt(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, 1), false)};
    ConstValue nextValue = {llvm::APSInt(m_sourceInterface.getLanguageOptions().sizeOfInt * 8, false)};
    std::vector<std::pair<std::string_view, llvm::APSInt>> values;
    for (auto& [loc, maybeExpression] : enumDef.getValues())
    {
        ConstValue value;
        bool validValue = true;
        if (maybeExpression)
        {
            auto expr = visit(*maybeExpression);
            auto result = evaluateConstantExpression(*expr);
            if (!result || !isInteger(expr->getType()))
            {
                if (result && !isInteger(expr->getType()))
                {
                    log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(
                        *expr, m_sourceInterface, *expr));
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
            values.emplace_back(loc->getText(), cld::get<llvm::APSInt>(value.getValue()));
        }
        auto [prev, notRedefined] = getCurrentScope().declarations.insert(
            {loc->getText(),
             DeclarationInScope{
                 loc, std::pair{std::move(value),
                                PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions())}}});
        if (!notRedefined)
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*loc, m_sourceInterface, *loc));
            if (prev->second.identifier)
            {
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
        }
    }
    std::string_view name = enumDef.getName() ? enumDef.getName()->getText() : "";
    if (enumDef.getName())
    {
        auto [prev, notRedefined] = getCurrentScope().types.insert(
            {name, TagTypeInScope{enumDef.getName(), EnumTag{m_enumDefinitions.size()}}});
        if (!notRedefined)
        {
            if (enumDef.getName() == prev->second.identifier)
            {
                return EnumType::create(isConst, isVolatile, name,
                                        static_cast<std::size_t>(cld::get<EnumTag>(prev->second.tagType)));
            }
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*enumDef.getName(), m_sourceInterface, *enumDef.getName()));
            if (prev->second.identifier)
            {
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
        }
    }
    else
    {
        m_anonymousTagToID.emplace(enumDecl->begin(), m_enumDefinitions.size());
    }
    m_enumDefinitions.push_back(
        {EnumDefinition(name, PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions()),
                        std::move(values)),
         m_currentScope, enumDef.begin()});
    return EnumType::create(isConst, isVolatile, name, m_enumDefinitions.size() - 1);
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

template <class T>
void cld::Semantics::SemanticAnalysis::handleArray(cld::Semantics::Type& type,
                                                   const std::vector<Syntax::TypeQualifier>& typeQualifiers,
                                                   const cld::Syntax::AssignmentExpression* assignmentExpression,
                                                   const Lexer::CToken* isStatic, bool valArray, T&& returnTypeLoc)
{
    if (std::holds_alternative<FunctionType>(type.getVariant()))
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
    if ((isConst || isVolatile || restricted) && !m_inParameter)
    {
        log(Errors::Semantics::ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_QUALIFIED.args(
            typeQualifiers, m_sourceInterface, typeQualifiers));
        isConst = isVolatile = restricted = false;
    }
    if (isStatic && !m_inParameter)
    {
        log(Errors::Semantics::ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_STATIC.args(*isStatic, m_sourceInterface,
                                                                                          *isStatic));
        isStatic = nullptr;
    }
    if (valArray)
    {
        type = ValArrayType::create(isConst, isVolatile, restricted, isStatic, std::move(type), {});
        return;
    }
    if (!assignmentExpression)
    {
        type = AbstractArrayType::create(isConst, isVolatile, restricted, std::move(type));
        return;
    }
    auto expr = visit(*assignmentExpression);
    if (!expr->getType().isUndefined() && !isInteger(expr->getType()))
    {
        log(Errors::Semantics::ARRAY_SIZE_MUST_BE_AN_INTEGER_TYPE.args(*expr, m_sourceInterface, *expr));
        type = Type{};
        return;
    }
    if (expr->getType().isUndefined())
    {
        type = Type{};
        return;
    }
    auto result = evaluateConstantExpression(*expr, Arithmetic);
    if (!result)
    {
        expr = lvalueConversion(std::move(expr));
        type = ValArrayType::create(isConst, isVolatile, restricted, isStatic, std::move(type), std::move(expr));
        return;
    }
    if (result->isUndefined())
    {
        type = Type{};
        return;
    }
    if (cld::get<PrimitiveType>(expr->getType().getVariant()).isSigned())
    {
        if (extensionsEnabled(expr->begin()) ? result->toInt() < 0 : result->toInt() <= 0)
        {
            log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(*assignmentExpression, m_sourceInterface,
                                                                             *assignmentExpression, *result));
            type = Type{};
            return;
        }
    }
    else if (result->toUInt() == 0 && !extensionsEnabled(expr->begin()))
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
