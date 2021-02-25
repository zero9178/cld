#include "SemanticAnalysis.hpp"

#include <optional>
#include <vector>

#include "ErrorMessages.hpp"

namespace
{
template <class Applicant>
std::optional<Applicant> tryConvertApplicant(cld::Semantics::SemanticAnalysis::AffectsAll applicant)
{
    if constexpr (cld::IsVariant<Applicant>{})
    {
        return cld::match(applicant, [](auto&& value) -> std::optional<Applicant> {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_constructible_v<Applicant, T>)
            {
                return std::optional<Applicant>{std::in_place, value};
            }
            else
            {
                return {};
            }
        });
    }
    else
    {
        if (std::holds_alternative<Applicant>(applicant))
        {
            return std::get<Applicant>(applicant);
        }
        return {};
    }
}

template <class Applicant>
bool tryMatch(cld::Semantics::SemanticAnalysis& analysis, cld::Semantics::SemanticAnalysis::AffectsAll applicant,
              void (cld::Semantics::SemanticAnalysis::*method)(Applicant,
                                                               const cld::Semantics::SemanticAnalysis::GNUAttribute&),
              const cld::Semantics::SemanticAnalysis::GNUAttribute& attribute)
{
    auto castedApplicant = tryConvertApplicant<std::decay_t<Applicant>>(applicant);
    if (!castedApplicant)
    {
        return false;
    }
    auto result = *castedApplicant;
    (analysis.*method)(result, attribute);
    return true;
}
} // namespace

void cld::Semantics::SemanticAnalysis::applyAttributes(AffectsAll applicant,
                                                       const std::vector<GNUAttribute>& attributes)
{
    using UnorderedMap =
        std::unordered_map<std::string, std::function<bool(const GNUAttribute&, SemanticAnalysis&, AffectsAll)>>;
    static UnorderedMap handlers = [&] {
        UnorderedMap result;
        auto lambda = [](auto memberFunction, const GNUAttribute& iter, SemanticAnalysis& analysis,
                         AffectsAll applicant) -> bool { return tryMatch(analysis, applicant, memberFunction, iter); };
        auto unixSpelling = [&](std::string name, auto function) {
            result.emplace(name, function);
            result.emplace("__" + name + "__", function);
        };
        unixSpelling("align", cld::bind_front(lambda, &SemanticAnalysis::applyAlignAttribute));
        unixSpelling("used", cld::bind_front(lambda, &SemanticAnalysis::applyUsedAttribute));
        unixSpelling("vector_size", cld::bind_front(lambda, &SemanticAnalysis::applyVectorSizeAttribute));
        return result;
    }();
    for (auto& iter : attributes)
    {
        auto name = Lexer::normalizeSpelling(iter.name->getRepresentation(m_sourceInterface));
        if (auto result = handlers.find(name); result != handlers.end())
        {
            auto applied = (result->second)(iter, *this, applicant);
            if (!applied)
            {
                cld::match(
                    applicant,
                    [&](VariableDeclaration* decl) {
                        log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_VARIABLES.args(
                            *iter.name, m_sourceInterface, *iter.name, *decl->getNameToken()));
                    },
                    [&](FunctionDeclaration* decl) {
                        log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_FUNCTIONS.args(
                            *iter.name, m_sourceInterface, *iter.name, *decl->getNameToken()));
                    },
                    [&](FunctionDefinition* def) {
                        log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_VARIABLES.args(
                            *iter.name, m_sourceInterface, *iter.name, *def->getNameToken()));
                    },
                    [&](const std::pair<Type*, diag::PointRange>& pair) {
                        log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_TYPES.args(*iter.name, m_sourceInterface,
                                                                                          *iter.name, pair.second));
                    });
            }
        }
        else
        {
            log(Warnings::Semantics::UNKNOWN_ATTRIBUTE_N_IGNORED.args(*iter.name, m_sourceInterface, *iter.name));
        }
    }
}

void cld::Semantics::SemanticAnalysis::applyAlignAttribute(AffectsAll applicant, const GNUAttribute& attribute) {}

void cld::Semantics::SemanticAnalysis::applyVectorSizeAttribute(AffectsTypeVariable applicant,
                                                                const GNUAttribute& attribute)
{
    auto argCount = (attribute.firstParamName ? 1 : 0) + attribute.paramExpressions.size();
    if (argCount != 1)
    {
        log(Errors::Semantics::INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_N_GOT_N.args(
            *attribute.name, m_sourceInterface, *attribute.name, 1, argCount));
        return;
    }
    if (attribute.firstParamName)
    {
        log(Errors::Semantics::EXPECTED_INTEGER_CONSTANT_EXPRESSION_AS_ARGUMENT_TO_VECTOR_SIZE.args(
            *attribute.firstParamName, m_sourceInterface, *attribute.firstParamName));
        return;
    }
    if (!isInteger(attribute.paramExpressions[0]->getType()))
    {
        log(Errors::Semantics::EXPECTED_INTEGER_CONSTANT_EXPRESSION_AS_ARGUMENT_TO_VECTOR_SIZE.args(
            *attribute.paramExpressions[0], m_sourceInterface, *attribute.paramExpressions[0]));
        return;
    }
    auto result = evaluateConstantExpression(*attribute.paramExpressions[0]);
    if (!result)
    {
        std::for_each(result.error().begin(), result.error().end(), cld::bind_front(&SemanticAnalysis::log, this));
        return;
    }
    if (result->getInteger() < 0)
    {
        log(Errors::Semantics::ARGUMENT_TO_VECTOR_SIZE_MUST_BE_A_POSITIVE_NUMBER.args(
            *attribute.paramExpressions[0], m_sourceInterface, *attribute.paramExpressions[0], *result));
        return;
    }
    Type baseType = cld::match(
        applicant, [](VariableDeclaration* variableDeclaration) { return variableDeclaration->getType(); },
        [](auto pair) { return *pair.first; });
    if (!isArithmetic(baseType) || isEnum(baseType))
    {
        cld::match(
            applicant,
            [&](VariableDeclaration* variableDeclaration) {
                log(Errors::Semantics::VECTOR_SIZE_CAN_ONLY_BE_APPLIED_TO_VARIABLES_OF_ARITHMETIC_TYPES.args(
                    *variableDeclaration->getNameToken(), m_sourceInterface, *variableDeclaration->getNameToken(),
                    baseType));
            },
            [&](const std::pair<Type*, diag::PointRange>& pair) {
                log(Errors::Semantics::VECTOR_SIZE_CAN_ONLY_BE_APPLIED_TO_ARITHMETIC_TYPES.args(
                    pair.second, m_sourceInterface, pair.second, baseType));
            });
        return;
    }
    if (cld::get<PrimitiveType>(baseType.getVariant()).getKind() == PrimitiveType::LongDouble)
    {
        cld::match(
            applicant,
            [&](VariableDeclaration* variableDeclaration) {
                log(Errors::Semantics::VECTOR_SIZE_CAN_NOT_BE_APPLIED_TO_LONG_DOUBLE.args(
                    *variableDeclaration->getNameToken(), m_sourceInterface, *variableDeclaration->getNameToken(),
                    baseType));
            },
            [&](const std::pair<Type*, diag::PointRange>& pair) {
                log(Errors::Semantics::VECTOR_SIZE_CAN_NOT_BE_APPLIED_TO_LONG_DOUBLE.args(
                    pair.second, m_sourceInterface, pair.second, baseType));
            });
        return;
    }
    auto size = baseType.getSizeOf(*this);
    auto sizeConstValue = ConstValue(llvm::APSInt(llvm::APInt(result->getInteger().getBitWidth(), size)));
    auto mod = result->modulo(sizeConstValue, getLanguageOptions());
    if (mod.getInteger() != 0)
    {
        log(Errors::Semantics::ARGUMENT_OF_VECTOR_SIZE_MUST_BE_A_MULTIPLE_OF_THE_SIZE_OF_THE_BASE_TYPE.args(
            *attribute.paramExpressions[0], m_sourceInterface, *attribute.paramExpressions[0],
            result->toString() + " % sizeof(" + diag::StringConverter<Type>::inArg(baseType, &m_sourceInterface)
                + ") /*" + std::to_string(size) + "*/ = " + mod.toString()));
        return;
    }
    auto multiple = result->divide(sizeConstValue, getLanguageOptions());
    if (!multiple.getInteger().isPowerOf2())
    {
        log(Errors::Semantics::ARGUMENT_OF_VECTOR_SIZE_SHOULD_BE_A_POWER_OF_2_MULTIPLE_OF_THE_SIZE_OF_THE_BASE_TYPE
                .args(*attribute.paramExpressions[0], m_sourceInterface, *attribute.paramExpressions[0],
                      result->toString() + " / sizeof("
                          + diag::StringConverter<Type>::inArg(baseType, &m_sourceInterface) + ") /*"
                          + std::to_string(size) + "*/ = " + multiple.toString()));
        return;
    }
    cld::match(
        applicant,
        [&](const std::pair<Type*, diag::PointRange>& pair) {
            *pair.first =
                VectorType::create(baseType.isConst(), baseType.isVolatile(), removeQualifiers(std::move(baseType)),
                                   multiple.getInteger().getZExtValue());
        },
        [&](VariableDeclaration* variableDeclaration) {
            auto newType =
                VectorType::create(baseType.isConst(), baseType.isVolatile(), removeQualifiers(std::move(baseType)),
                                   multiple.getInteger().getZExtValue());
            *variableDeclaration =
                VariableDeclaration(std::move(newType), variableDeclaration->getLinkage(),
                                    variableDeclaration->getLifetime(), variableDeclaration->getNameToken(),
                                    variableDeclaration->getKind(), std::move(*variableDeclaration).getInitializer());
        });
}

void cld::Semantics::SemanticAnalysis::applyUsedAttribute(AffectsVariableFunction applicant,
                                                          const GNUAttribute& attribute)
{
    if (attribute.firstParamName || !attribute.paramExpressions.empty())
    {
        log(Errors::Semantics::INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_N_GOT_N.args(
            *attribute.name, m_sourceInterface, *attribute.name, 0,
            (attribute.firstParamName ? 1 : 0) + attribute.paramExpressions.size()));
    }
    if (cld::match(
            applicant,
            [&](FunctionDefinition* def) {
                if (def->getLinkage() != Linkage::Internal)
                {
                    log(Warnings::Semantics::ATTRIBUTE_USED_ONLY_APPLIES_TO_FUNCTIONS_WITH_INTERNAL_LINKAGE.args(
                        *attribute.name, m_sourceInterface, *attribute.name, *def->getNameToken()));
                    return true;
                }
                return false;
            },
            [&](VariableDeclaration* declaration) {
                if (declaration->getLinkage() != Linkage::Internal || m_currentScope != 0)
                {
                    log(Warnings::Semantics::ATTRIBUTE_USED_ONLY_APPLIES_TO_GLOBAL_VARIABLES_WITH_INTERNAL_LINKAGE.args(
                        *attribute.name, m_sourceInterface, *attribute.name, *declaration->getNameToken()));
                    return true;
                }
                return false;
            },
            [&](FunctionDeclaration* declaration) {
                if (declaration->getLinkage() != Linkage::Internal)
                {
                    log(Warnings::Semantics::ATTRIBUTE_USED_ONLY_APPLIES_TO_FUNCTIONS_WITH_INTERNAL_LINKAGE.args(
                        *attribute.name, m_sourceInterface, *attribute.name, *declaration->getNameToken()));
                    return true;
                }
                return false;
            }))
    {
        return;
    }
    cld::match(applicant, [](auto* holder) { holder->addAttribute(UsedAttribute{}); });
}
