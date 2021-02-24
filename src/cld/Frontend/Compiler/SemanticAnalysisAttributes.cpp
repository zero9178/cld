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
                    [&](Type*) {
                        log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_TYPES.args(*iter.name, m_sourceInterface,
                                                                                          *iter.name));
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
