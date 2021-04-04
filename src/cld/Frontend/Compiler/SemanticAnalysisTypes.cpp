#include "SemanticAnalysis.hpp"

#include <bitset2.hpp>

#include "ErrorMessages.hpp"
#include "SemanticUtil.hpp"

void cld::Semantics::SemanticAnalysis::handleParameterList(
    IntrVarValue<Type>& type, const Syntax::ParameterTypeList* parameterTypeList, const diag::PointRange& returnTypeLoc,
    cld::function_ref<void(const Type&, Lexer::CTokenIterator, const std::vector<Syntax::DeclarationSpecifier>&,
                           std::vector<GNUAttribute>&&)>
        paramCallback)
{
    if (isFunctionType(type))
    {
        log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_A_FUNCTION.args(returnTypeLoc, m_sourceInterface,
                                                                                returnTypeLoc, type));
        type.emplace<ErrorType>();
        return;
    }
    if (isArray(type))
    {
        log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_AN_ARRAY.args(returnTypeLoc, m_sourceInterface,
                                                                              returnTypeLoc, type));
        type.emplace<ErrorType>();
        return;
    }
    if (!parameterTypeList)
    {
        type = FunctionType(typeAlloc(std::move(*type)), {}, flag::isKAndR = true);
        return;
    }
    std::vector<FunctionType::Parameter> parameters;
    for (auto& iter : parameterTypeList->getParameters())
    {
        std::vector<GNUAttribute> attributes;
        for (auto& specs : iter.declarationSpecifiers)
        {
            auto* storageSpec = std::get_if<Syntax::StorageClassSpecifier>(&specs);
            if (!storageSpec)
            {
                continue;
            }
            if (storageSpec->getSpecifier() != Syntax::StorageClassSpecifier::Register)
            {
                log(Errors::Semantics::NO_STORAGE_CLASS_SPECIFIER_ALLOWED_IN_PARAMETER_BESIDES_REGISTER.args(
                    *storageSpec, m_sourceInterface, *storageSpec));
            }
        }
        ValueReset<bool> reset(m_inParameter, m_inParameter);
        m_inParameter = true;
        auto paramType = cld::match(
            iter.declarator,
            [&](const std::unique_ptr<Syntax::Declarator>& ptr)
            { return declaratorsToType(iter.declarationSpecifiers, *ptr, {}, {}, &attributes); },
            [&](const std::unique_ptr<Syntax::AbstractDeclarator>& ptr)
            { return declaratorsToType(iter.declarationSpecifiers, ptr.get(), &attributes); });
        if (isVoid(paramType) && !paramType->isConst() && !paramType->isVolatile()
            && !std::holds_alternative<std::unique_ptr<Syntax::Declarator>>(iter.declarator)
            && !cld::get<std::unique_ptr<Syntax::AbstractDeclarator>>(iter.declarator)
            && parameterTypeList->getParameters().size() == 1 && !parameterTypeList->hasEllipse())
        {
            type = FunctionType(typeAlloc(std::move(*type)), {});
            reportNotApplicableAttributes(attributes);
            return;
        }
        if (isVoid(paramType))
        {
            log(Errors::Semantics::VOID_TYPE_NOT_ALLOWED_AS_FUNCTION_PARAMETER.args(
                iter.declarationSpecifiers, m_sourceInterface, iter.declarationSpecifiers));
            paramType = ErrorType();
        }
        if (iter.optionalAttributes)
        {
            auto result = visit(*iter.optionalAttributes);
            attributes.insert(attributes.end(), std::move_iterator(result.begin()), std::move_iterator(result.end()));
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
            auto hasStatic = std::any_of(begin, visitor.end(),
                                         [](const Type& type)
                                         {
                                             if (auto* valArray = type.tryAs<ValArrayType>())
                                             {
                                                 return valArray->isStatic();
                                             }
                                             if (isArrayType(type))
                                             {
                                                 return type.as<ArrayType>().isStatic();
                                             }
                                             return false;
                                         });
            if (hasStatic)
            {
                log(Errors::Semantics::STATIC_ONLY_ALLOWED_IN_OUTERMOST_ARRAY.args(iter.declarator, m_sourceInterface,
                                                                                   iter.declarator));
            }
            auto hasQualifiers = std::any_of(
                begin, visitor.end(),
                [](const Type& type)
                {
                    if (type.is<ValArrayType>())
                    {
                        return type.as<ValArrayType>().isRestricted() || type.isConst() || type.isVolatile();
                    }
                    if (type.is<ArrayType>())
                    {
                        return type.as<ArrayType>().isRestricted() || type.isConst() || type.isVolatile();
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
            auto hasStatic = std::any_of(visitor.begin(), visitor.end(),
                                         [](const Type& type)
                                         {
                                             if (type.is<ValArrayType>())
                                             {
                                                 return type.as<ValArrayType>().isStatic();
                                             }
                                             if (type.is<ArrayType>())
                                             {
                                                 return type.as<ArrayType>().isStatic();
                                             }
                                             return false;
                                         });
            if (hasStatic)
            {
                log(Errors::Semantics::ONLY_PARAMETER_OF_ARRAY_TYPE_MAY_BE_STATIC.args(
                    iter.declarator, m_sourceInterface, iter.declarator, paramType));
            }
            auto hasQualifiers = std::any_of(
                visitor.begin(), visitor.end(),
                [](const Type& type)
                {
                    if (type.is<ValArrayType>())
                    {
                        return type.as<ValArrayType>().isRestricted() || type.isConst() || type.isVolatile();
                    }
                    if (isArrayType(type))
                    {
                        return type.as<ArrayType>().isRestricted() || type.isConst() || type.isVolatile();
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
        if (isFunctionType(paramType))
        {
            paramType.emplace<PointerType>(typeAlloc(std::move(*paramType)));
        }
        attributes = applyAttributes(std::pair{&paramType, diag::getPointRange(iter.declarationSpecifiers)},
                                     std::move(attributes));
        auto& ret = parameters.emplace_back(FunctionType::Parameter{typeAlloc(std::move(*paramType)), name});
        if (paramCallback && loc)
        {
            paramCallback(*ret.type, loc, iter.declarationSpecifiers, std::move(attributes));
        }
    }
    type.emplace<FunctionType>(typeAlloc(std::move(*type)), std::move(parameters),
                               flag::isVARArg = parameterTypeList->hasEllipse());
}

cld::IntrVarValue<cld::Semantics::Type> cld::Semantics::SemanticAnalysis::qualifiersToTypeImpl(
    const std::vector<DeclarationOrSpecifierQualifier>& directAbstractDeclaratorParentheses,
    std::vector<GNUAttribute>* attributesOut)
{
    bool isConst = false;
    bool isVolatile = false;
    const Syntax::TypeQualifier* restrictQual = nullptr;
    std::vector<const Syntax::TypeSpecifier*> typeSpecs;
    std::vector<GNUAttribute> attributes;
    for (auto& iter : directAbstractDeclaratorParentheses)
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
                case Syntax::TypeQualifier::Restrict: restrictQual = *typeQual; break;
            }
        }
        else if (auto* gnuAttributes = std::get_if<const Syntax::GNUAttributes*>(&iter))
        {
            auto result = visit(**gnuAttributes);
            attributes.insert(attributes.end(), std::move_iterator(result.begin()), std::move_iterator(result.end()));
        }
    }
    if (typeSpecs.empty())
    {
        log(Errors::Semantics::AT_LEAST_ONE_TYPE_SPECIFIER_REQUIRED.args(
            directAbstractDeclaratorParentheses, m_sourceInterface, directAbstractDeclaratorParentheses));
        return ErrorType{};
    }
    auto result = typeSpecifiersToType(isConst, isVolatile, std::move(typeSpecs));
    if (attributesOut)
    {
        auto temp = applyAttributes(std::pair{&result, diag::getPointRange(directAbstractDeclaratorParentheses)},
                                    std::move(attributes));
        attributesOut->insert(attributesOut->end(), std::move_iterator(temp.begin()), std::move_iterator(temp.end()));
    }
    else
    {
        (void)applyAttributes(std::pair{&result, diag::getPointRange(directAbstractDeclaratorParentheses)},
                              std::move(attributes));
    }
    if (restrictQual)
    {
        if (!result->is<PointerType>())
        {
            log(Errors::Semantics::RESTRICT_CAN_ONLY_BE_APPLIED_TO_POINTERS.args(*restrictQual, m_sourceInterface,
                                                                                 *restrictQual));
        }
        else
        {
            result->setRestricted(true);
        }
    }
    return result;
}

cld::IntrVarValue<cld::Semantics::Type> cld::Semantics::SemanticAnalysis::applyDeclaratorsImpl(
    cld::IntrVarValue<Type>&& type, const PossiblyAbstractQualifierRef& declarator,
    const std::vector<Syntax::Declaration>& declarations,
    cld::function_ref<void(const Type&, Lexer::CTokenIterator, const std::vector<Syntax::DeclarationSpecifier>&,
                           std::vector<GNUAttribute>&&)>
        paramCallback,
    std::vector<GNUAttribute>* attributesOut)
{
    if (!cld::match(declarator, [](auto&& value) -> bool { return value; }))
    {
        return std::move(type);
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
            type.emplace<PointerType>(typeAlloc(std::move(*type)), flag::isConst = isConst,
                                      flag::isVolatile = isVolatile, flag::isRestricted = restricted);
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
            [&](auto&& self, const Syntax::DirectDeclaratorParentheses& parentheses)
            {
                if (parentheses.getOptionalAttributes())
                {
                    auto result = visit(*parentheses.getOptionalAttributes());
                    result = applyAttributes(
                        std::pair{&type, diag::getPointRange(parentheses.getDeclarator().getDirectDeclarator())},
                        std::move(result));
                    if (attributesOut)
                    {
                        attributesOut->insert(attributesOut->end(), std::move_iterator(result.begin()),
                                              std::move_iterator(result.end()));
                    }
                }
                for (auto& iter : parentheses.getDeclarator().getPointers())
                {
                    auto [isConst, isVolatile, restricted] = getQualifiers(iter.getTypeQualifiers());
                    if (restricted && isFunctionType(type))
                    {
                        auto restrictQual = std::find_if(
                            iter.getTypeQualifiers().begin(), iter.getTypeQualifiers().end(),
                            [](const std::variant<Syntax::TypeQualifier, Syntax::GNUAttributes>& typeQualifier)
                            {
                                if (!std::holds_alternative<Syntax::TypeQualifier>(typeQualifier))
                                {
                                    return false;
                                }
                                return cld::get<Syntax::TypeQualifier>(typeQualifier).getQualifier()
                                       == Syntax::TypeQualifier::Restrict;
                            });
                        CLD_ASSERT(restrictQual != iter.getTypeQualifiers().end());
                        log(Errors::Semantics::
                                ELEMENT_TYPE_OF_POINTER_WITH_RESTRICT_QUALIFIER_MUST_NOT_BE_A_FUNCTION_TYPE.args(
                                    parentheses.getDeclarator().getDirectDeclarator(), m_sourceInterface,
                                    parentheses.getDeclarator().getDirectDeclarator(), *restrictQual));
                        restricted = false;
                    }
                    type.emplace<PointerType>(typeAlloc(std::move(*type)), flag::isConst = isConst,
                                              flag::isVolatile = isVolatile, flag::isRestricted = restricted);
                    for (auto& iter2 : iter.getTypeQualifiers())
                    {
                        if (auto* attribute = std::get_if<Syntax::GNUAttributes>(&iter2))
                        {
                            auto result = visit(*attribute);
                            result = applyAttributes(
                                std::pair{&type,
                                          diag::getPointRange(parentheses.getDeclarator().getDirectDeclarator())},
                                std::move(result));
                            if (attributesOut)
                            {
                                attributesOut->insert(attributesOut->end(), std::move_iterator(result.begin()),
                                                      std::move_iterator(result.end()));
                            }
                        }
                    }
                }
                cld::match(parentheses.getDeclarator().getDirectDeclarator(), self);
            },
            [&](auto&& self, const Syntax::DirectDeclaratorNoStaticOrAsterisk& noStaticOrAsterisk) {
                auto scope = cld::ScopeExit([&] { cld::match(noStaticOrAsterisk.getDirectDeclarator(), self); });
                handleArray(type, noStaticOrAsterisk.getTypeQualifiers(),
                            noStaticOrAsterisk.getAssignmentExpression().get(), nullptr, false,
                            diag::getPointRange(noStaticOrAsterisk.getDirectDeclarator()));
            },
            [&](auto&& self, const Syntax::DirectDeclaratorStatic& declaratorStatic) {
                auto scope = cld::ScopeExit([&] { cld::match(declaratorStatic.getDirectDeclarator(), self); });
                handleArray(type, declaratorStatic.getTypeQualifiers(), &declaratorStatic.getAssignmentExpression(),
                            declaratorStatic.getStaticLoc(), false,
                            diag::getPointRange(declaratorStatic.getDirectDeclarator()));
            },
            [&](auto&& self, const Syntax::DirectDeclaratorAsterisk& asterisk) {
                auto scope = cld::ScopeExit([&] { cld::match(asterisk.getDirectDeclarator(), self); });
                if (!m_inFunctionPrototype)
                {
                    type.emplace<ErrorType>();
                    log(Errors::Semantics::STAR_IN_ARRAY_DECLARATOR_ONLY_ALLOWED_IN_FUNCTION_PROTOTYPES.args(
                        *asterisk.getAsterisk(), m_sourceInterface, *asterisk.getAsterisk()));
                }
                handleArray(type, asterisk.getTypeQualifiers(), nullptr, nullptr, true,
                            diag::getPointRange(asterisk.getDirectDeclarator()));
            },
            [&](auto&& self, const Syntax::DirectDeclaratorParenthesesIdentifiers& identifiers) {
                auto scope = cld::ScopeExit([&] { cld::match(identifiers.getDirectDeclarator(), self); });
                std::optional<decltype(pushScope())> scope2;
                if (isFunctionPrototype)
                {
                    scope2.emplace(pushScope());
                }
                if (isFunctionType(type))
                {
                    log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_A_FUNCTION.args(
                        /*TODO: Better source location*/ identifiers.getDirectDeclarator(), m_sourceInterface,
                        /*TODO: Better source location*/ identifiers.getDirectDeclarator(), type));
                    type.emplace<ErrorType>();
                    return;
                }
                if (isArray(type))
                {
                    log(Errors::Semantics::FUNCTION_RETURN_TYPE_MUST_NOT_BE_AN_ARRAY.args(
                        /*TODO: Better source location*/ identifiers.getDirectDeclarator(), m_sourceInterface,
                        /*TODO: Better source location*/ identifiers.getDirectDeclarator(), type));
                    type.emplace<ErrorType>();
                    return;
                }
                if (identifiers.getIdentifiers().empty() && !paramCallback)
                {
                    type = FunctionType(typeAlloc(std::move(type)), {}, flag::isKAndR = true);
                    return;
                }
                if (!paramCallback || declarationsOwner != &identifiers)
                {
                    log(Errors::Semantics::IDENTIFIER_LIST_ONLY_ALLOWED_AS_PART_OF_A_FUNCTION_DEFINITION.args(
                        identifiers.getIdentifiers(), m_sourceInterface, identifiers.getIdentifiers()));
                    type = FunctionType(typeAlloc(std::move(type)), {}, flag::isKAndR = true);
                    return;
                }
                std::unordered_map<std::string_view, std::size_t> paramNames;
                std::vector<FunctionType::Parameter> parameters;
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
                    parameters.push_back({typeAlloc<ErrorType>(), iter->getText()});
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
                    std::vector<GNUAttribute> attributes;
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
                    auto baseType = qualifiersToType(iter.getDeclarationSpecifiers(), &attributes);
                    if (iter.getInitDeclarators().empty())
                    {
                        log(Errors::Semantics::DECLARATION_OF_IDENTIFIER_LIST_MUST_DECLARE_AT_LEAST_ONE_IDENTIFIER.args(
                            iter, m_sourceInterface, iter));
                        continue;
                    }
                    for (auto& iter2 : iter.getInitDeclarators())
                    {
                        auto thisAttributes = attributes;
                        if (iter2.optionalInitializer)
                        {
                            log(Errors::Semantics::DECLARATION_OF_IDENTIFIER_LIST_NOT_ALLOWED_TO_HAVE_AN_INITIALIZER
                                    .args(*iter2.optionalInitializer, m_sourceInterface, *iter2.optionalInitializer));
                        }
                        auto paramType = applyDeclarator(baseType, *iter2.declarator, {}, {}, &thisAttributes);
                        if (iter2.optionalBeforeAttributes)
                        {
                            auto vector = visit(*iter2.optionalBeforeAttributes);
                            thisAttributes.insert(thisAttributes.end(), std::move_iterator(vector.begin()),
                                                  std::move_iterator(vector.end()));
                        }
                        if (iter2.optionalAfterAttributes)
                        {
                            auto vector = visit(*iter2.optionalAfterAttributes);
                            thisAttributes.insert(thisAttributes.end(), std::move_iterator(vector.begin()),
                                                  std::move_iterator(vector.end()));
                        }
                        const auto* loc = declaratorToLoc(*iter2.declarator);
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
                            parameters[result->second].type = typeAlloc(std::move(*paramType));
                            element = loc;
                            if (paramCallback)
                            {
                                paramCallback(*parameters[result->second].type, loc, iter.getDeclarationSpecifiers(),
                                              std::move(attributes));
                            }
                            else
                            {
                                reportNotApplicableAttributes(thisAttributes);
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
                type.emplace<FunctionType>(typeAlloc(std::move(*type)), std::move(parameters), flag::isKAndR = true);
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
                        /*TODO: better source location*/ diag::getPointRange(parameterList.getDirectDeclarator()),
                        paramCallback);
                }
                else
                {
                    handleParameterList(
                        type, &parameterList.getParameterTypeList(),
                        /*TODO: better source location*/ diag::getPointRange(parameterList.getDirectDeclarator()));
                }
            });
    }
    else
    {
        auto& realDecl = *cld::get<const Syntax::AbstractDeclarator*>(declarator);
        for (auto& iter : realDecl.getPointers())
        {
            auto [isConst, isVolatile, restricted] = getQualifiers(iter.getTypeQualifiers());
            type.emplace<PointerType>(typeAlloc(std::move(*type)), flag::isConst = isConst,
                                      flag::isVolatile = isVolatile, flag::isRestricted = restricted);
        }
        if (!realDecl.getDirectAbstractDeclarator())
        {
            return std::move(type);
        }
        cld::matchWithSelf<void>(
            *realDecl.getDirectAbstractDeclarator(),
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorParentheses& parentheses)
            {
                if (parentheses.getOptionalAttributes())
                {
                    auto result = visit(*parentheses.getOptionalAttributes());
                    result = applyAttributes(std::pair{&type, diag::getPointRange(parentheses)}, std::move(result));
                    if (attributesOut)
                    {
                        attributesOut->insert(attributesOut->end(), std::move_iterator(result.begin()),
                                              std::move_iterator(result.end()));
                    }
                }
                for (auto& iter : parentheses.getAbstractDeclarator().getPointers())
                {
                    auto [isConst, isVolatile, restricted] = getQualifiers(iter.getTypeQualifiers());
                    if (restricted && type->is<FunctionType>())
                    {
                        auto restrictQual = std::find_if(
                            iter.getTypeQualifiers().begin(), iter.getTypeQualifiers().end(),
                            [](const std::variant<Syntax::TypeQualifier, Syntax::GNUAttributes>& typeQualifier)
                            {
                                if (!std::holds_alternative<Syntax::TypeQualifier>(typeQualifier))
                                {
                                    return false;
                                }
                                return cld::get<Syntax::TypeQualifier>(typeQualifier).getQualifier()
                                       == Syntax::TypeQualifier::Restrict;
                            });
                        CLD_ASSERT(restrictQual != iter.getTypeQualifiers().end());
                        log(Errors::Semantics::
                                ELEMENT_TYPE_OF_POINTER_WITH_RESTRICT_QUALIFIER_MUST_NOT_BE_A_FUNCTION_TYPE.args(
                                    parentheses, m_sourceInterface, parentheses, *restrictQual));
                        type.emplace<ErrorType>();
                        continue;
                    }
                    type.emplace<PointerType>(typeAlloc(std::move(*type)), flag::isConst = isConst,
                                              flag::isVolatile = isVolatile, flag::isRestricted = restricted);
                    for (auto& iter2 : iter.getTypeQualifiers())
                    {
                        if (auto* attribute = std::get_if<Syntax::GNUAttributes>(&iter2))
                        {
                            auto result = visit(*attribute);
                            result =
                                applyAttributes(std::pair{&type, diag::getPointRange(parentheses)}, std::move(result));
                            if (attributesOut)
                            {
                                attributesOut->insert(attributesOut->end(), std::move_iterator(result.begin()),
                                                      std::move_iterator(result.end()));
                            }
                        }
                    }
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
                    type.emplace<ErrorType>();
                    log(Errors::Semantics::STAR_IN_ARRAY_DECLARATOR_ONLY_ALLOWED_IN_FUNCTION_PROTOTYPES.args(
                        *asterisk.getAsterisk(), m_sourceInterface, *asterisk.getAsterisk()));
                }
                if (asterisk.getDirectAbstractDeclarator())
                {
                    handleArray(type, {}, nullptr, nullptr, true,
                                diag::getPointRange(*asterisk.getDirectAbstractDeclarator()));
                }
                else
                {
                    handleArray(type, {}, nullptr, nullptr, true, /*TODO:*/ diag::getPointRange(asterisk));
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
                                false, diag::getPointRange(*expression.getDirectAbstractDeclarator()));
                }
                else
                {
                    handleArray(type, expression.getTypeQualifiers(), expression.getAssignmentExpression(), nullptr,
                                false, /*TODO:*/ diag::getPointRange(expression));
                }
            },
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorParameterTypeList& parameterTypeList) {
                auto scope = cld::ScopeExit([&] {
                    if (parameterTypeList.getDirectAbstractDeclarator())
                    {
                        cld::match(*parameterTypeList.getDirectAbstractDeclarator(), self);
                    }
                });
                handleParameterList(type, parameterTypeList.getParameterTypeList(),
                                    /*TODO:*/ diag::getPointRange(parameterTypeList), {});
            },
            [&](auto&& self, const Syntax::DirectAbstractDeclaratorStatic& declaratorStatic) {
                auto scope = cld::ScopeExit([&] {
                    if (declaratorStatic.getDirectAbstractDeclarator())
                    {
                        cld::match(*declaratorStatic.getDirectAbstractDeclarator(), self);
                    }
                });
                handleArray(type, declaratorStatic.getTypeQualifiers(), &declaratorStatic.getAssignmentExpression(),
                            declaratorStatic.getStaticLoc(), false,
                            diag::getPointRange(declaratorStatic.getAssignmentExpression()));
            });
    }
    return std::move(type);
}

cld::IntrVarValue<cld::Semantics::Type>
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
        const auto* typeDef = getTypedef(*name);
        CLD_ASSERT(typeDef);
        // C99 6.7.3§8:
        // If the specification of an array type includes any type qualifiers, the element type is soqualified, not the
        // array type. If the specification of a function type includes any type qualifiers, the behavior is undefined
        if (isArray(typeDef->type))
        {
            if (!isConst && !isVolatile)
            {
                return typeDef->type;
            }
            auto& elementType = getArrayElementType(typeDef->type);
            if ((!isConst || elementType.isConst()) && (!isVolatile || elementType.isVolatile()))
            {
                return typeDef->type;
            }
            auto newElementType = typeAlloc(elementType);
            newElementType->setConst(elementType.isConst() || isConst);
            newElementType->setVolatile(elementType.isVolatile() || isVolatile);
            return typeDef->type->match(
                [](const auto&) -> IntrVarValue<Type> { CLD_UNREACHABLE; },
                [&](const ArrayType& arrayType) -> IntrVarValue<Type> {
                    return ArrayType(newElementType, arrayType.getSize(), flag::useFlags = arrayType.getFlags());
                },
                [&](const AbstractArrayType& arrayType) -> IntrVarValue<Type> {
                    return AbstractArrayType(newElementType, flag::useFlags = arrayType.getFlags());
                },
                [&](const ValArrayType& arrayType) -> IntrVarValue<Type> {
                    return ValArrayType(newElementType, arrayType.getExpression(),
                                        flag::useFlags = arrayType.getFlags());
                });
        }
        if ((isConst && !typeDef->isConst) || (isVolatile && !typeDef->isVolatile))
        {
            IntrVarValue copy = typeDef->type;
            copy->setConst(isConst || copy->isConst());
            copy->setVolatile(isVolatile || copy->isVolatile());
            return copy;
        }
        return typeDef->type;
    }
    if (auto* structOrUnionPtr =
            std::get_if<std::unique_ptr<Syntax::StructOrUnionSpecifier>>(&typeSpec[0]->getVariant()))
    {
        if (typeSpec.size() != 1)
        {
            this->log(Errors::Semantics::EXPECTED_NO_FURTHER_TYPE_SPECIFIERS_AFTER_N.args(
                *typeSpec[1], this->m_sourceInterface,
                (*structOrUnionPtr)->isUnion() ? cld::Lexer::TokenType::UnionKeyword :
                                                 cld::Lexer::TokenType::StructKeyword,
                llvm::ArrayRef(typeSpec).drop_front()));
        }
        return structOrUnionSpecifierToType(isConst, isVolatile, **structOrUnionPtr);
    }
    CLD_ASSERT(std::holds_alternative<std::unique_ptr<Syntax::EnumSpecifier>>(typeSpec[0]->getVariant()));
    if (typeSpec.size() != 1)
    {
        log(Errors::Semantics::EXPECTED_NO_FURTHER_TYPE_SPECIFIERS_AFTER_N.args(
            *typeSpec[1], m_sourceInterface, Lexer::TokenType::EnumKeyword, llvm::ArrayRef(typeSpec).drop_front()));
    }
    auto& enumDecl = cld::get<std::unique_ptr<Syntax::EnumSpecifier>>(typeSpec[0]->getVariant());
    if (auto* loc = std::get_if<Syntax::EnumSpecifier::EnumTag>(&enumDecl->getVariant()))
    {
        const auto* lookup = lookupType<EnumInfo>(loc->identifier->getText());
        if (!lookup)
        {
            // C99 6.7.2.3:
            // A type specifier of the form
            //  enum identifier
            // without an enumerator list shall only appear after the type it specifies is complete
            log(Errors::Semantics::FORWARD_DECLARING_AN_ENUM_IS_NOT_ALLOWED.args(*typeSpec[0], m_sourceInterface,
                                                                                 *typeSpec[0]));
            return ErrorType{};
        }
        return EnumType(*lookup, flag::isConst = isConst, flag::isVolatile = isVolatile);
    }
    auto& enumDef = cld::get<Syntax::EnumDeclaration>(enumDecl->getVariant());
    // TODO: Type depending on values as an extension
    const ConstValue one = {llvm::APSInt(llvm::APInt(getLanguageOptions().sizeOfInt * 8, 1), false)};
    ConstValue nextValue = {llvm::APSInt(getLanguageOptions().sizeOfInt * 8, false)};
    std::vector<std::pair<std::string_view, llvm::APSInt>> values;
    for (auto& iter : enumDef.getValues())
    {
        const auto* loc = iter.name;
        auto& maybeExpression = iter.value;
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
                std::for_each(result.error().begin(), result.error().end(),
                              cld::bind_front(&SemanticAnalysis::log, this));
            }
            else
            {
                CLD_ASSERT(std::holds_alternative<llvm::APSInt>(result->getValue()));
                auto& apInt = cld::get<llvm::APSInt>(result->getValue());
                if (apInt.ugt(llvm::APSInt::getMaxValue(getLanguageOptions().sizeOfInt * 8, true)
                                  .extOrTrunc(apInt.getBitWidth())))
                {
                    log(Errors::Semantics::VALUE_OF_ENUMERATION_CONSTANT_MUST_FIT_IN_TYPE_INT.args(
                        *loc, m_sourceInterface, *loc, *maybeExpression, apInt));
                }
                value =
                    result->castTo(PrimitiveType(PrimitiveType::Int, getLanguageOptions()), this, getLanguageOptions());
            }
        }
        else
        {
            value = nextValue;
        }
        if (validValue)
        {
            nextValue = value.plus(one, getLanguageOptions());
            values.emplace_back(loc->getText(), cld::get<llvm::APSInt>(value.getValue()));
        }
        auto [prev, notRedefined] = getCurrentScope().declarations.insert(
            {loc->getText(),
             DeclarationInScope{loc,
                                std::pair{std::move(value), PrimitiveType(PrimitiveType::Int, getLanguageOptions())}}});
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
    m_enumDefinitions.push_back(
        {m_enumDefinitions.size(),
         EnumDefinition(name, PrimitiveType(PrimitiveType::Int, getLanguageOptions()), std::move(values)),
         m_currentScope, enumDef.begin(), name});
    if (enumDef.getName())
    {
        auto [prev, notRedefined] =
            getCurrentScope().types.insert({name, TagTypeInScope{enumDef.getName(), &m_enumDefinitions.back()}});
        if (!notRedefined)
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*enumDef.getName(), m_sourceInterface, *enumDef.getName()));
            if (prev->second.identifier)
            {
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
        }
    }
    return EnumType(m_enumDefinitions.back(), flag::isConst = isConst, flag::isVolatile = isVolatile);
}

cld::IntrVarValue<cld::Semantics::Type>
    cld::Semantics::SemanticAnalysis::structOrUnionSpecifierToType(bool isConst, bool isVolatile,
                                                                   const Syntax::StructOrUnionSpecifier& structOrUnion)
{
    std::vector<SemanticAnalysis::GNUAttribute> pureTypeAttributes;
    if (structOrUnion.getOptionalBeforeAttributes())
    {
        pureTypeAttributes = visit(*structOrUnion.getOptionalBeforeAttributes());
    }
    if (structOrUnion.getStructDeclarations().empty())
    {
        CLD_ASSERT(structOrUnion.getIdentifierLoc());
        auto name = structOrUnion.getIdentifierLoc()->getText();
        if (structOrUnion.isUnion())
        {
            if (auto* unionInfo = lookupType<UnionInfo>(name))
            {
                pureTypeAttributes = applyAttributes(unionInfo, std::move(pureTypeAttributes));
                reportNotApplicableAttributes(pureTypeAttributes);
                return UnionType(*unionInfo, flag::isConst = isConst, flag::isVolatile = isVolatile);
            }
            m_unionDefinitions.push_back(
                {m_unionDefinitions.size(), UnionDecl{}, m_currentScope, structOrUnion.begin(), name});
            auto [prev, notRedefined] = getCurrentScope().types.insert(
                {name, TagTypeInScope{structOrUnion.getIdentifierLoc(), &m_unionDefinitions.back()}});
            if (!notRedefined)
            {
                log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion.getIdentifierLoc(), m_sourceInterface,
                                                          *structOrUnion.getIdentifierLoc()));
                if (prev->second.identifier)
                {
                    log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                             *prev->second.identifier));
                }
            }
            pureTypeAttributes = applyAttributes(&m_unionDefinitions.back(), std::move(pureTypeAttributes));
            reportNotApplicableAttributes(pureTypeAttributes);
            return UnionType(m_unionDefinitions.back(), flag::isConst = isConst, flag::isVolatile = isVolatile);
        }

        if (auto* structInfo = lookupType<StructInfo>(name))
        {
            pureTypeAttributes = applyAttributes(structInfo, std::move(pureTypeAttributes));
            reportNotApplicableAttributes(pureTypeAttributes);
            return StructType(*structInfo, flag::isConst = isConst, flag::isVolatile = isVolatile);
        }
        m_structDefinitions.push_back(
            {m_structDefinitions.size(), StructDecl{}, m_currentScope, structOrUnion.begin(), name});
        auto [prev, notRedefined] = getCurrentScope().types.insert(
            {name, TagTypeInScope{structOrUnion.getIdentifierLoc(), &m_structDefinitions.back()}});
        if (!notRedefined)
        {
            log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion.getIdentifierLoc(), m_sourceInterface,
                                                      *structOrUnion.getIdentifierLoc()));
            if (prev->second.identifier)
            {
                log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                         *prev->second.identifier));
            }
        }
        pureTypeAttributes = applyAttributes(&m_structDefinitions.back(), std::move(pureTypeAttributes));
        reportNotApplicableAttributes(pureTypeAttributes);
        return StructType(m_structDefinitions.back(), flag::isConst = isConst, flag::isVolatile = isVolatile);
    }
    if (structOrUnion.getOptionalAfterAttributes())
    {
        auto result = visit(*structOrUnion.getOptionalAfterAttributes());
        pureTypeAttributes.insert(pureTypeAttributes.end(), std::move_iterator(result.begin()),
                                  std::move_iterator(result.end()));
    }

    std::variant<std::monostate, UnionInfo * CLD_NON_NULL, StructInfo * CLD_NON_NULL> structOrUnionInfo;
    if (!structOrUnion.getIdentifierLoc())
    {
        if (structOrUnion.isUnion())
        {
            m_unionDefinitions.push_back(
                {m_unionDefinitions.size(), UnionDecl{}, m_currentScope, structOrUnion.begin(), ""});
            structOrUnionInfo = &m_unionDefinitions.back();
        }
        else
        {
            m_structDefinitions.push_back(
                {m_structDefinitions.size(), StructDecl{}, m_currentScope, structOrUnion.begin(), ""});
            structOrUnionInfo = &m_structDefinitions.back();
        }
    }
    else
    {
        auto name = structOrUnion.getIdentifierLoc()->getText();
        if (structOrUnion.isUnion())
        {
            auto prev = getCurrentScope().types.find(name);
            if (prev != getCurrentScope().types.end())
            {
                if (auto* unionInfo = std::get_if<UnionInfo*>(&prev->second.tagType);
                    !unionInfo || std::holds_alternative<UnionDefinition>((*unionInfo)->type))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion.getIdentifierLoc(), m_sourceInterface,
                                                              *structOrUnion.getIdentifierLoc()));
                    if (prev->second.identifier)
                    {
                        log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second.identifier, m_sourceInterface,
                                                                 *prev->second.identifier));
                    }
                }
                else
                {
                    structOrUnionInfo = *unionInfo;
                }
            }
            else
            {
                m_unionDefinitions.push_back(
                    {m_unionDefinitions.size(), UnionDecl{}, m_currentScope, structOrUnion.begin(), name});
                structOrUnionInfo = &m_unionDefinitions.back();
                getCurrentScope().types.insert(
                    {name, TagTypeInScope{structOrUnion.getIdentifierLoc(), &m_unionDefinitions.back()}});
            }
        }
        else
        {
            auto result = getCurrentScope().types.find(name);
            if (result != getCurrentScope().types.end())
            {
                if (auto* structInfo = std::get_if<StructInfo*>(&result->second.tagType);
                    !structInfo || std::holds_alternative<StructDefinition>((*structInfo)->type))
                {
                    log(Errors::REDEFINITION_OF_SYMBOL_N.args(*structOrUnion.getIdentifierLoc(), m_sourceInterface,
                                                              *structOrUnion.getIdentifierLoc()));
                    if (result->second.identifier)
                    {
                        log(Notes::PREVIOUSLY_DECLARED_HERE.args(*result->second.identifier, m_sourceInterface,
                                                                 *result->second.identifier));
                    }
                }
                else
                {
                    structOrUnionInfo = *structInfo;
                }
            }
            else
            {
                m_structDefinitions.push_back(
                    {m_structDefinitions.size(), StructDecl{}, m_currentScope, structOrUnion.begin(), name});
                structOrUnionInfo = &m_structDefinitions.back();
                getCurrentScope().types.insert(
                    {name, TagTypeInScope{structOrUnion.getIdentifierLoc(), &m_structDefinitions.back()}});
            }
        }
    }

    FieldMap fields;
    std::vector<FieldInLayout> fieldLayout;
    std::unordered_set<std::size_t> zeroBitFields;
    for (auto iter = structOrUnion.getStructDeclarations().begin(); iter != structOrUnion.getStructDeclarations().end();
         iter++)
    {
        auto& [specifiers, declarators] = *iter;
        auto fieldStructOrUnion = std::find_if(
            specifiers.begin(), specifiers.end(), [](const Syntax::SpecifierQualifier& specifierQualifier) {
                if (!std::holds_alternative<Syntax::TypeSpecifier>(specifierQualifier))
                {
                    return false;
                }
                auto& specifier = cld::get<Syntax::TypeSpecifier>(specifierQualifier);
                if (!std::holds_alternative<std::unique_ptr<Syntax::StructOrUnionSpecifier>>(specifier.getVariant()))
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
            auto parentType = typeAlloc(std::move(*type));
            fieldLayout.push_back({parentType, static_cast<std::size_t>(-1), {}});
            if (!extensionsEnabled(structOrUnion.begin()) || !isAnonymous(*parentType))
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
                if ((parentType->isConst() && !field.type->isConst())
                    || (parentType->isVolatile() && !field.type->isVolatile()))
                {
                    auto copy = typeAlloc(*field.type);
                    copy->setConst(parentType->isConst() || copy->isConst());
                    copy->setConst(parentType->isVolatile() || copy->isVolatile());
                    field.type = copy;
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
        std::vector<SemanticAnalysis::GNUAttribute> attributes;
        auto baseType = qualifiersToType(specifiers, &attributes);
        for (auto iter2 = declarators.begin(); iter2 != declarators.end(); iter2++)
        {
            auto thisAttributes = attributes;
            bool last = iter2 + 1 == declarators.end() && iter + 1 == structOrUnion.getStructDeclarations().end();
            bool first = iter2 == declarators.begin() && iter == structOrUnion.getStructDeclarations().begin();

            auto& declarator = iter2->optionalDeclarator;
            auto& size = iter2->optionalBitfield;
            auto type = declarator ? applyDeclarator(baseType, *declarator, {}, {}, &thisAttributes) : baseType;
            reportNotApplicableAttributes(thisAttributes);
            auto errorLoc = declarator ? diag::getPointRange(*declarator) : diag::getPointRange(specifiers);
            if (isVoid(type))
            {
                if (structOrUnion.isUnion())
                {
                    log(Errors::Semantics::VOID_TYPE_NOT_ALLOWED_IN_UNION.args(errorLoc, m_sourceInterface, specifiers,
                                                                               errorLoc));
                }
                else
                {
                    log(Errors::Semantics::VOID_TYPE_NOT_ALLOWED_IN_STRUCT.args(errorLoc, m_sourceInterface, specifiers,
                                                                                errorLoc));
                }
                type.emplace<ErrorType>();
            }
            else if (isVariablyModified(type))
            {
                if (structOrUnion.isUnion())
                {
                    log(Errors::Semantics::VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_IN_UNION.args(errorLoc, m_sourceInterface,
                                                                                            specifiers, errorLoc));
                }
                else
                {
                    log(Errors::Semantics::VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_IN_STRUCT.args(
                        errorLoc, m_sourceInterface, specifiers, errorLoc));
                }
                type.emplace<ErrorType>();
            }
            else if (!isCompleteType(type) && !(!structOrUnion.isUnion() && last && !first && isAbstractArray(type)))
            {
                if (structOrUnion.isUnion())
                {
                    log(Errors::Semantics::INCOMPLETE_TYPE_NOT_ALLOWED_IN_UNION.args(errorLoc, m_sourceInterface, type,
                                                                                     specifiers, errorLoc));
                }
                else
                {
                    log(Errors::Semantics::INCOMPLETE_TYPE_NOT_ALLOWED_IN_STRUCT.args(errorLoc, m_sourceInterface, type,
                                                                                      specifiers, errorLoc));
                }
                type.emplace<ErrorType>();
            }
            else if (isFunctionType(type))
            {
                if (structOrUnion.isUnion())
                {
                    log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_UNION.args(errorLoc, m_sourceInterface,
                                                                                   specifiers, errorLoc, type));
                }
                else
                {
                    log(Errors::Semantics::FUNCTION_TYPE_NOT_ALLOWED_IN_STRUCT.args(errorLoc, m_sourceInterface,
                                                                                    specifiers, errorLoc, type));
                }
                type.emplace<ErrorType>();
            }
            else if (!structOrUnion.isUnion() && hasFlexibleArrayMember(type))
            {
                if (isStruct(type))
                {
                    log(Errors::Semantics::STRUCT_WITH_FLEXIBLE_ARRAY_MEMBER_NOT_ALLOWED_IN_STRUCT.args(
                        specifiers, m_sourceInterface, specifiers));
                }
                else
                {
                    log(Errors::Semantics::
                            UNION_WITH_STRUCT_OR_UNION_CONTAINING_A_FLEXIBLE_ARRAY_MEMBER_IS_NOT_ALLOWED_IN_STRUCT.args(
                                specifiers, m_sourceInterface, specifiers));
                }
                type.emplace<ErrorType>();
            }
            std::optional<std::pair<uint32_t, uint32_t>> value;
            if (size)
            {
                bool hadValidType = true;
                if (!type->isUndefined() && !isInteger(type))
                {
                    log(Errors::Semantics::BITFIELD_MAY_ONLY_BE_OF_TYPE_INT_OR_BOOL.args(specifiers, m_sourceInterface,
                                                                                         specifiers));
                    hadValidType = false;
                }
                else if (!type->isUndefined())
                {
                    auto& primitive = type->as<PrimitiveType>();
                    switch (primitive.getKind())
                    {
                        case PrimitiveType::Bool:
                        case PrimitiveType::Int:
                        case PrimitiveType::UnsignedInt: break;
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
                    std::for_each(result.error().begin(), result.error().end(),
                                  cld::bind_front(&SemanticAnalysis::log, this));
                    continue;
                }
                CLD_ASSERT(expr->getType().is<PrimitiveType>());
                if (result->getInteger().isNegative())
                {
                    log(Errors::Semantics::BITFIELD_MUST_BE_OF_SIZE_ZERO_OR_GREATER.args(*size, m_sourceInterface,
                                                                                         *size, *result));
                    continue;
                }
                if (!hadValidType)
                {
                    continue;
                }
                auto objectWidth = type->as<PrimitiveType>().getBitCount();
                if (result->getInteger() > objectWidth)
                {
                    log(Errors::Semantics::BITFIELD_MUST_NOT_HAVE_A_GREATER_WIDTH_THAN_THE_TYPE.args(
                        *size, m_sourceInterface, specifiers, objectWidth, *size, *result));
                }
                if (result->getInteger() == 0 && declarator)
                {
                    log(Errors::Semantics::BITFIELD_WITH_SIZE_ZERO_MAY_NOT_HAVE_A_NAME.args(
                        *declarator, m_sourceInterface, *declarator));
                }
                if (result->getInteger() == 0)
                {
                    zeroBitFields.emplace(fields.size());
                    continue;
                }
                value.emplace(0, result->getInteger().getZExtValue());
            }
            const auto* token = declarator ? declaratorToLoc(*declarator) : nullptr;
            if (token)
            {
                auto sharedType = typeAlloc(std::move(*type));
                auto [prev, notRedefinition] =
                    fields.insert({token->getText(),
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
    if (!structOrUnion.isUnion())
    {
        for (auto iter = fields.begin(); iter != fields.end();)
        {
            if (iter->second.type->isUndefined())
            {
                iter.value().indices[0] = memoryLayout.size();
                memoryLayout.push_back({iter->second.type, currentSize});
                iter++;
                continue;
            }
            if (iter->second.indices.size() > 1)
            {
                auto& parentType = iter->second.parentTypes.front();
                auto end = std::find_if_not(iter, fields.end(),
                                            [&](const auto& pair)
                                            {
                                                if (pair.second.parentTypes.empty())
                                                {
                                                    return false;
                                                }
                                                return pair.second.parentTypes.front() == parentType;
                                            });
                fieldLayout[fieldLayoutCounter++].layoutIndex = memoryLayout.size();
                for (; iter != end; iter++)
                {
                    iter.value().indices[0] = memoryLayout.size();
                }
                auto alignment = parentType->getAlignOf(*this);
                currentAlignment = std::max(currentAlignment, alignment);
                currentSize = cld::roundUpTo(currentSize, alignment);
                memoryLayout.push_back({parentType, currentSize});
                auto subSize = parentType->getSizeOf(*this);
                currentSize += subSize;
                continue;
            }
            if (!iter->second.bitFieldBounds)
            {
                iter.value().indices[0] = memoryLayout.size();
                fieldLayout[fieldLayoutCounter++].layoutIndex = memoryLayout.size();
                if (!isCompleteType(*iter->second.type) && !isAbstractArray(*iter->second.type))
                {
                    iter++;
                    continue;
                }
                auto alignment = iter->second.type->getAlignOf(*this);
                currentAlignment = std::max(currentAlignment, alignment);
                currentSize = cld::roundUpTo(currentSize, alignment);
                memoryLayout.push_back({iter->second.type, currentSize});
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
                    && (!getLanguageOptions().discreteBitfields || prevSize == size))
                {
                    iter.value().indices[0] = memoryLayout.size() - 1;
                    storageLeft -= iter->second.bitFieldBounds->second;
                    fieldLayout[fieldLayoutCounter].bitFieldBounds.emplace(used,
                                                                           used + iter->second.bitFieldBounds->second);
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
                currentSize = cld::roundUpTo(currentSize, alignment);
                prevSize = size;
                storageLeft =
                    iter->second.type->as<PrimitiveType>().getBitCount() - iter->second.bitFieldBounds->second;
                used = iter->second.bitFieldBounds->second;
                iter.value().bitFieldBounds.emplace(0, used);
                iter.value().indices[0] = memoryLayout.size();
                fieldLayout[fieldLayoutCounter].bitFieldBounds.emplace(0, used);
                fieldLayout[fieldLayoutCounter++].layoutIndex = memoryLayout.size();
                memoryLayout.push_back({iter->second.type, currentSize});
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
                auto end = std::find_if_not(iter + 1, fields.end(),
                                            [&](const auto& pair)
                                            {
                                                if (pair.second.parentTypes.empty())
                                                {
                                                    return false;
                                                }
                                                return pair.second.parentTypes.front() == field.parentTypes.front();
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
    currentSize = cld::roundUpTo(currentSize, currentAlignment);
    if (std::holds_alternative<std::monostate>(structOrUnionInfo))
    {
        return ErrorType{};
    }
    cld::match(
        structOrUnionInfo, [](std::monostate) { CLD_UNREACHABLE; },
        [&](auto* info)
        {
            pureTypeAttributes = applyAttributes(info, std::move(pureTypeAttributes));
            reportNotApplicableAttributes(pureTypeAttributes);
        });

    std::string_view name = structOrUnion.getIdentifierLoc() ? structOrUnion.getIdentifierLoc()->getText() : "";
    if (structOrUnion.isUnion())
    {
        cld::get<UnionInfo*>(structOrUnionInfo)
            ->type.emplace<UnionDefinition>(name, std::move(fields), std::move(fieldLayout), currentSize,
                                            currentAlignment);
        return UnionType(*cld::get<UnionInfo*>(structOrUnionInfo), flag::isConst = isConst,
                         flag::isVolatile = isVolatile);
    }
    cld::get<StructInfo*>(structOrUnionInfo)
        ->type.emplace<StructDefinition>(name, std::move(fields), std::move(fieldLayout), std::move(memoryLayout),
                                         currentSize, currentAlignment);
    return StructType(*cld::get<StructInfo*>(structOrUnionInfo), flag::isConst = isConst,
                      flag::isVolatile = isVolatile);
}

cld::Semantics::PrimitiveType cld::Semantics::SemanticAnalysis::primitiveTypeSpecifiersToType(
    bool isConst, bool isVolatile, const std::vector<const Syntax::TypeSpecifier*>& typeSpecs)
{
    using PrimitiveTypeSpecifier = Syntax::TypeSpecifier::PrimitiveTypeSpecifier;
    CLD_ASSERT(std::holds_alternative<PrimitiveTypeSpecifier>(typeSpecs[0]->getVariant()));
    auto excessSpecifiersError = [this](std::string_view type, const Syntax::TypeSpecifier* typeSpec)
    {
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
                case PrimitiveTypeSpecifier::Int128: tokenType = Lexer::TokenType::Int128Keyword; break;
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

    constexpr auto clz = [](std::size_t value) {
        std::size_t i = 0;
        for (; value != 0; value >>= 1, i++)
            ;
        return i;
    };

    using BitSet = Bitset2::bitset2<clz(PrimitiveTypeSpecifier::MAX_VALUE)>;
    static const auto table = []() -> std::unordered_map<BitSet, std::pair<BitSet, PrimitiveType::Kind>> {
        using Tuple = std::tuple<std::underlying_type_t<PrimitiveTypeSpecifier>,
                                 std::underlying_type_t<PrimitiveTypeSpecifier>, PrimitiveType::Kind>;
        std::vector<Tuple> temp = {
            {PrimitiveTypeSpecifier::Void, 0, PrimitiveType::Void},
            {PrimitiveTypeSpecifier::Char, PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::Char},
            {PrimitiveTypeSpecifier::Char + PrimitiveTypeSpecifier::Signed, 0, PrimitiveType::SignedChar},
            {PrimitiveTypeSpecifier::Char + PrimitiveTypeSpecifier::Unsigned, 0, PrimitiveType::UnsignedChar},
            {PrimitiveTypeSpecifier::Float, 0, PrimitiveType::Float},
            {PrimitiveTypeSpecifier::Bool, 0, PrimitiveType::Bool},
            {PrimitiveTypeSpecifier::Double, PrimitiveTypeSpecifier::Long, PrimitiveType::Double},
            {PrimitiveTypeSpecifier::Double + PrimitiveTypeSpecifier::Long, 0, PrimitiveType::LongDouble},
            {PrimitiveTypeSpecifier::Int,
             PrimitiveTypeSpecifier::Long | PrimitiveTypeSpecifier::Short | PrimitiveTypeSpecifier::Signed
                 | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::Int},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long,
             PrimitiveTypeSpecifier::Long | PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::Long},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long,
             PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned, PrimitiveType::LongLong},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long
                 + PrimitiveTypeSpecifier::Signed,
             0, PrimitiveType::LongLong},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long
                 + PrimitiveTypeSpecifier::Unsigned,
             0, PrimitiveType::UnsignedLongLong},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Signed,
             PrimitiveTypeSpecifier::Long, PrimitiveType::Long},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Unsigned,
             PrimitiveTypeSpecifier::Long, PrimitiveType::UnsignedLong},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Short,
             PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned, PrimitiveType::Short},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Short + PrimitiveTypeSpecifier::Signed, 0,
             PrimitiveType::Short},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Short + PrimitiveTypeSpecifier::Unsigned, 0,
             PrimitiveType::UnsignedShort},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Signed,
             PrimitiveTypeSpecifier ::Long | PrimitiveTypeSpecifier::Short, PrimitiveType::Int},
            {PrimitiveTypeSpecifier::Int + PrimitiveTypeSpecifier::Unsigned,
             PrimitiveTypeSpecifier ::Long | PrimitiveTypeSpecifier::Short, PrimitiveType::UnsignedInt},
            {PrimitiveTypeSpecifier::Short,
             PrimitiveTypeSpecifier ::Int | PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::Short},
            {PrimitiveTypeSpecifier::Short + PrimitiveTypeSpecifier::Signed, PrimitiveTypeSpecifier ::Int,
             PrimitiveType::Short},
            {PrimitiveTypeSpecifier::Short + PrimitiveTypeSpecifier::Unsigned, PrimitiveTypeSpecifier ::Int,
             PrimitiveType::UnsignedShort},
            {PrimitiveTypeSpecifier::Long,
             PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Long | PrimitiveTypeSpecifier::Signed
                 | PrimitiveTypeSpecifier::Unsigned | PrimitiveTypeSpecifier::Double,
             PrimitiveType::Long},
            {PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Signed,
             PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Long, PrimitiveType::Long},
            {PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Unsigned,
             PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Long, PrimitiveType::UnsignedLong},
            {PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long,
             PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::LongLong},
            {PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Signed,
             PrimitiveTypeSpecifier::Int, PrimitiveType::LongLong},
            {PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Long + PrimitiveTypeSpecifier::Unsigned,
             PrimitiveTypeSpecifier::Int, PrimitiveType::UnsignedLongLong},
            {PrimitiveTypeSpecifier::Signed,
             PrimitiveTypeSpecifier::Short | PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Char
                 | PrimitiveTypeSpecifier::Long | PrimitiveTypeSpecifier::Int128,
             PrimitiveType::Int},
            {PrimitiveTypeSpecifier::Unsigned,
             PrimitiveTypeSpecifier::Short | PrimitiveTypeSpecifier::Int | PrimitiveTypeSpecifier::Char
                 | PrimitiveTypeSpecifier::Long | PrimitiveTypeSpecifier::Int128,
             PrimitiveType::UnsignedInt},
            {PrimitiveTypeSpecifier::Int128, PrimitiveTypeSpecifier::Signed | PrimitiveTypeSpecifier::Unsigned,
             PrimitiveType::Int128},
            {PrimitiveTypeSpecifier::Int128 + PrimitiveTypeSpecifier::Signed, 0, PrimitiveType::Int128},
            {PrimitiveTypeSpecifier::Int128 + PrimitiveTypeSpecifier::Unsigned, 0, PrimitiveType::UnsignedInt128},
        };
        std::unordered_map<BitSet, std::pair<BitSet, PrimitiveType::Kind>> result;
        for (auto& [state, next, type] : temp)
        {
            auto [prev, inserted] = result.insert({BitSet(state), {BitSet(next), std::move(type)}});
            (void)prev;
            CLD_ASSERT(inserted);
        }
        return result;
    }();

    auto primKindToType = [isConst, isVolatile, this](PrimitiveType::Kind kind)
    { return PrimitiveType(kind, getLanguageOptions(), flag::isConst = isConst, flag::isVolatile = isVolatile); };

    auto primTypeSpecToString = [](PrimitiveTypeSpecifier spec) -> std::string_view {
        switch (spec)
        {
            case Syntax::TypeSpecifier::Void: return "void";
            case Syntax::TypeSpecifier::Char: return "char";
            case Syntax::TypeSpecifier::Short: return "short";
            case Syntax::TypeSpecifier::Int: return "int";
            case Syntax::TypeSpecifier::Int128: return "__int128";
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
            return primKindToType(result->second.second);
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
        return primKindToType(result->second.second);
    }
    auto result = table.find(type);
    CLD_ASSERT(result != table.end());
    return primKindToType(result->second.second);
}

void cld::Semantics::SemanticAnalysis::handleArray(IntrVarValue<Type>& type,
                                                   const std::vector<Syntax::TypeQualifier>& typeQualifiers,
                                                   const Syntax::AssignmentExpression* assignmentExpression,
                                                   const Lexer::CToken* isStatic, bool valArray,
                                                   const diag::PointRange& returnTypeLoc)
{
    if (isFunctionType(type))
    {
        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION.args(returnTypeLoc, m_sourceInterface,
                                                                              returnTypeLoc, type));
        type.emplace<ErrorType>();
    }
    else if (!isCompleteType(type))
    {
        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_BE_A_COMPLETE_TYPE.args(returnTypeLoc, m_sourceInterface,
                                                                               returnTypeLoc, type));
        type.emplace<ErrorType>();
    }
    else if (hasFlexibleArrayMember(type))
    {
        log(Errors::Semantics::ARRAY_ELEMENT_TYPE_MUST_NOT_CONTAIN_A_FLEXIBLE_ARRAY_MEMBER.args(
            returnTypeLoc, m_sourceInterface, returnTypeLoc));
        type.emplace<ErrorType>();
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
        type = ValArrayType(typeAlloc(std::move(*type)), {}, flag::isConst = isConst, flag::isVolatile = isVolatile,
                            flag::isRestricted = restricted, flag::isStatic = isStatic);
        return;
    }
    if (!assignmentExpression)
    {
        type = AbstractArrayType(typeAlloc(std::move(*type)), flag::isConst = isConst, flag::isVolatile = isVolatile,
                                 flag::isRestricted = restricted);
        return;
    }
    auto expr = visit(*assignmentExpression);
    if (!expr->getType().isUndefined() && !isInteger(expr->getType()))
    {
        log(Errors::Semantics::ARRAY_SIZE_MUST_BE_AN_INTEGER_TYPE.args(*expr, m_sourceInterface, *expr));
        type.emplace<ErrorType>();
        return;
    }
    if (expr->getType().isUndefined())
    {
        type.emplace<ErrorType>();
        return;
    }
    auto result = evaluateConstantExpression(*expr, Arithmetic);
    if (!result)
    {
        expr = lvalueConversion(std::move(expr));
        type = ValArrayType(typeAlloc(std::move(*type)), std::move(expr), flag::isConst = isConst,
                            flag::isVolatile = isVolatile, flag::isRestricted = restricted, flag::isStatic = isStatic);
        return;
    }
    if (result->isUndefined())
    {
        type.emplace<ErrorType>();
        return;
    }
    if (expr->getType().as<PrimitiveType>().isSigned())
    {
        if (extensionsEnabled(expr->begin()) ? result->getInteger() < 0 : result->getInteger() <= 0)
        {
            log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(*assignmentExpression, m_sourceInterface,
                                                                             *assignmentExpression, *result));
            type.emplace<ErrorType>();
            return;
        }
    }
    else if (result->getInteger() == 0 && !extensionsEnabled(expr->begin()))
    {
        log(Errors::Semantics::ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO.args(*assignmentExpression, m_sourceInterface,
                                                                         *assignmentExpression, *result));
        type.emplace<ErrorType>();
        return;
    }
    auto size = result->getInteger().getZExtValue();
    type = ArrayType(typeAlloc(std::move(*type)), size, flag::isConst = isConst, flag::isVolatile = isVolatile,
                     flag::isRestricted = restricted, flag::isStatic = isStatic);
}
