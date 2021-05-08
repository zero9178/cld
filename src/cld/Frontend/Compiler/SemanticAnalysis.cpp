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
                   && maybeStructDef->getFields().back().second.type->is<AbstractArrayType>();
        }
    }
    else if (auto* unionType = type.tryAs<UnionType>())
    {
        auto* maybeUnionDef = std::get_if<UnionDefinition>(&unionType->getInfo().type);
        if (maybeUnionDef)
        {
            for (auto& [name, field] : maybeUnionDef->getFields())
            {
                if (field.type->is<AbstractArrayType>())
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
    auto value = evaluate(constantExpression, mode,
                          [&](const Message& message)
                          {
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
    auto typeCheck = [=](const ExpressionBase& exp, const ConstValue& value)
    {
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
        [&](const Constant& constant) -> ConstValue
        {
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
        [&](const CommaExpression& commaExpression) -> ConstValue
        {
            for (auto& [exp, comma] : commaExpression.getCommaExpressions())
            {
                (void)exp;
                logger(Errors::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(*comma, m_sourceInterface, *comma));
            }
            return evaluate(commaExpression.getLastExpression(), mode, logger);
        },
        [&](const CompoundLiteral& compoundLiteral) -> ConstValue
        {
            if (mode != Initialization)
            {
                logger(Errors::Semantics::COMPOUND_LITERAL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                    compoundLiteral, m_sourceInterface, compoundLiteral));
                return {};
            }
            return {AddressConstant{}};
        },
        [&](const DeclarationRead& declRead) -> ConstValue
        {
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
        [&](const Conversion& conversion) -> ConstValue
        {
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
        [&](const BinaryOperator& binaryOperator) -> ConstValue
        {
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
        [&](const Cast& cast) -> ConstValue
        {
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
        [&](const UnaryOperator& unaryOperator) -> ConstValue
        {
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
        [&](const SizeofOperator& sizeofOperator) -> ConstValue
        {
            if (sizeofOperator.getSize())
            {
                auto type = PrimitiveType(getLanguageOptions().sizeTType, getLanguageOptions());
                return {llvm::APSInt(llvm::APInt(type.getBitCount(), *sizeofOperator.getSize()))};
            }
            logger(Errors::Semantics::SIZEOF_VAL_MODIFIED_TYPE_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION.args(
                sizeofOperator, m_sourceInterface, sizeofOperator));
            return {};
        },
        [&](const SubscriptOperator& subscriptOperator) -> ConstValue
        {
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
        [&](const Conditional& conditional) -> ConstValue
        {
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
        [&](const Assignment& assignment) -> ConstValue
        {
            logger(Errors::Semantics::N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                *assignment.getOperatorToken(), m_sourceInterface, *assignment.getOperatorToken()));
            return {};
        },
        [&](const CallExpression& call) -> ConstValue
        {
            logger(Errors::Semantics::FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(call, m_sourceInterface,
                                                                                            call));
            return {};
        },
        [&](const MemberAccess& memberAccess) -> ConstValue
        {
            auto exp = evaluate(memberAccess.getRecordExpression(), mode, logger);
            return {};
        },
        [&](const BuiltinVAArg& builtinVaArg) -> ConstValue
        {
            logger(Errors::Semantics::FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION.args(
                builtinVaArg, m_sourceInterface, builtinVaArg));
            return {};
        },
        [&](const BuiltinOffsetOf& builtinOffsetOf) -> ConstValue
        {
            auto type = PrimitiveType(getLanguageOptions().sizeTType, getLanguageOptions());
            if (auto* value = std::get_if<std::uint64_t>(&builtinOffsetOf.getOffset()))
            {
                return {llvm::APSInt(llvm::APInt(type.getBitCount(), *value))};
            }
            auto& loc = *cld::get<BuiltinOffsetOf::RuntimeEval>(builtinOffsetOf.getOffset()).failedConstExpr;
            logger(Errors::Semantics::EXPRESSION_IN_OFFSETOF_IS_NOT_A_VALID_CONSTANT_EXPRESSION.args(
                loc, m_sourceInterface, loc));
            return {};
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
                if (declaration->isUsed() || declaration->getLinkage() == Linkage::External || !declInScope.identifier
                    || declaration->hasAttribute<CleanupAttribute>() || declaration->hasAttribute<UsedAttribute>())
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
