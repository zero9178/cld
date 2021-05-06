#include "SemanticAnalysis.hpp"

#include <numeric>
#include <optional>
#include <vector>

#include "ErrorMessages.hpp"

std::vector<cld::Semantics::ParsedAttribute<>>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::GNUAttributes& node)
{
    std::vector<ParsedAttribute<>> result;
    for (auto& iter : node.getAttributes())
    {
        auto parser =
            m_attributesParser.find(Lexer::normalizeSpelling(iter.nameToken->getRepresentation(m_sourceInterface)));
        if (parser == m_attributesParser.end())
        {
            log(Warnings::Semantics::UNKNOWN_ATTRIBUTE_N_IGNORED.args(*iter.nameToken, m_sourceInterface,
                                                                      *iter.nameToken));
            continue;
        }
        auto parsedAttribute = std::invoke(parser->second, this, iter);
        if (!parsedAttribute)
        {
            continue;
        }
        std::vector<diag::PointRange> ranges;
        std::transform(iter.arguments.begin(), iter.arguments.end(), std::back_inserter(ranges),
                       diag::getPointRange<Syntax::AssignmentExpression>);
        result.push_back(ParsedAttribute<>{
            ParsedAttribute<>::Nothing,
            iter.nameToken,
            std::move(ranges),
            std::move(*parsedAttribute),
        });
    }
    return result;
}

namespace
{
template <class Applicant>
std::optional<Applicant> tryConvertApplicant(cld::Semantics::SemanticAnalysis::AffectsAll applicant)
{
    if constexpr (cld::IsVariant<Applicant>{})
    {
        return cld::match(applicant,
                          [](auto&& value) -> std::optional<Applicant>
                          {
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

template <class Applicant, class Attribute>
bool tryMatch(cld::Semantics::SemanticAnalysis& analysis,
              void (cld::Semantics::SemanticAnalysis::*method)(Applicant,
                                                               const cld::Semantics::ParsedAttribute<Attribute>&),
              cld::Semantics::SemanticAnalysis::AffectsAll applicant,
              const cld::Semantics::ParsedAttribute<Attribute>& attribute,
              const cld::Semantics::SemanticAnalysis::CallingContext&)
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

template <class Applicant, class Attribute>
bool tryMatch(
    cld::Semantics::SemanticAnalysis& analysis,
    void (cld::Semantics::SemanticAnalysis::*method)(Applicant, const cld::Semantics::ParsedAttribute<Attribute>&,
                                                     const cld::Semantics::SemanticAnalysis::CallingContext& context),
    cld::Semantics::SemanticAnalysis::AffectsAll applicant, const cld::Semantics::ParsedAttribute<Attribute>& attribute,
    const cld::Semantics::SemanticAnalysis::CallingContext& context)
{
    auto castedApplicant = tryConvertApplicant<std::decay_t<Applicant>>(applicant);
    if (!castedApplicant)
    {
        return false;
    }
    auto result = *castedApplicant;
    (analysis.*method)(result, attribute, context);
    return true;
}

template <class T>
auto to_tuple(T& object) noexcept
{
    using type = std::decay_t<T>;
    if constexpr (type::count == 4)
    {
        auto& [p1, p2, p3, p4] = object;
        return std::make_tuple(std::ref(p1), std::ref(p2), std::ref(p3), std::ref(p4));
    }
    else if constexpr (type::count == 3)
    {
        auto& [p1, p2, p3] = object;
        return std::make_tuple(std::ref(p1), std::ref(p2), std::ref(p3));
    }
    else if constexpr (type::count == 2)
    {
        auto& [p1, p2] = object;
        return std::make_tuple(std::ref(p1), std::ref(p2));
    }
    else if constexpr (type::count == 1)
    {
        auto& [p1] = object;
        return std::make_tuple(std::ref(p1));
    }
    else if constexpr (type::count == 0)
    {
        return std::make_tuple();
    }
    else
    {
        static_assert(cld::always_false<T>, "Too many members in Type T. Case not handled");
    }
}

template <class... Args>
auto tupleWithoutRef(std::tuple<Args...>) -> std::tuple<std::decay_t<Args>...>;

template <class... Args>
auto toTypePtrTuple(std::tuple<Args...>) -> std::tuple<std::decay_t<Args>*...>;

template <bool found = false, class First, class... Args>
constexpr bool checkValidStruct(std::tuple<First, Args...>*)
{
    if constexpr (cld::IsOptional<First>{})
    {
        if constexpr (sizeof...(Args) == 0)
        {
            return true;
        }
        else
        {
            return checkValidStruct<true>(static_cast<std::tuple<Args...>*>(nullptr));
        }
    }
    else if constexpr (cld::IsVector<First>{})
    {
        return sizeof...(Args) == 0;
    }
    else if constexpr (found)
    {
        return false;
    }
    else if constexpr (sizeof...(Args) > 0)
    {
        return checkValidStruct<false>(static_cast<std::tuple<Args...>*>(nullptr));
    }
    else
    {
        return true;
    }
}

template <class T, class Attribute, class = void>
struct HasOverload : std::false_type
{
};

template <class T, class Attribute>
struct HasOverload<T, Attribute,
                   std::void_t<decltype(std::declval<cld::Semantics::SemanticAnalysis>().apply(
                       std::declval<T>(), std::declval<cld::Semantics::ParsedAttribute<Attribute>>()))>>
    : std::true_type
{
};

template <class T, class Attribute, class = void>
struct HasOverloadWithC : std::false_type
{
};

template <class T, class Attribute>
struct HasOverloadWithC<T, Attribute,
                        std::void_t<decltype(std::declval<cld::Semantics::SemanticAnalysis>().apply(
                            std::declval<T>(), std::declval<cld::Semantics::ParsedAttribute<Attribute>>(),
                            std::declval<cld::Semantics::SemanticAnalysis::CallingContext>()))>> : std::true_type
{
};

template <class First, class... Args, class Attribute>
bool tryAll(cld::Semantics::SemanticAnalysis& analysis, cld::Semantics::SemanticAnalysis::AffectsAll applicant,
            const cld::Semantics::ParsedAttribute<Attribute>& attribute,
            const cld::Semantics::SemanticAnalysis::CallingContext& context)
{
    if constexpr (HasOverload<First, Attribute>{})
    {
        return tryMatch<First, Attribute>(analysis, &cld::Semantics::SemanticAnalysis::apply, applicant, attribute,
                                          context);
    }
    else if constexpr (HasOverloadWithC<First, Attribute>{})
    {
        return tryMatch<First, Attribute>(analysis, &cld::Semantics::SemanticAnalysis::apply, applicant, attribute,
                                          context);
    }
    else if constexpr (sizeof...(Args) == 0)
    {
        return tryMatch<cld::Semantics::SemanticAnalysis::SuitableApplicant<Attribute>, Attribute>(
            analysis, &cld::Semantics::SemanticAnalysis::applyFallback<Attribute>, applicant, attribute, context);
    }
    else
    {
        return tryAll<Args...>(analysis, applicant, attribute, context);
    }
}

} // namespace

bool cld::Semantics::SemanticAnalysis::parseMember(std::uint64_t& unsignedInteger, Lexer::CTokenIterator attributeName,
                                                   const Syntax::AssignmentExpression* expression)
{
    auto expr = visit(*expression);
    if (!expr->getType().isUndefined() && !isInteger(expr->getType()))
    {
        log(Errors::Semantics::EXPECTED_INTEGER_CONSTANT_EXPRESSION_AS_ARGUMENT_TO_N.args(
            *expression, m_sourceInterface, *attributeName, *expression));
        return false;
    }
    if (expr->getType().isUndefined())
    {
        return false;
    }
    auto result = evaluateConstantExpression(*expr);
    if (!result)
    {
        std::for_each(result.error().begin(), result.error().end(), cld::bind_front(&SemanticAnalysis::log, this));
        return false;
    }
    if (result->getInteger() < 0)
    {
        log(Errors::Semantics::ARGUMENT_TO_N_MUST_BE_A_POSITIVE_NUMBER.args(*expression, m_sourceInterface,
                                                                            *attributeName, *expression, *result));
        return false;
    }
    unsignedInteger = result->getInteger().getZExtValue();
    return true;
}

bool cld::Semantics::SemanticAnalysis::parseMember(Lexer::CTokenIterator& identifier,
                                                   Lexer::CTokenIterator attributeName,
                                                   const Syntax::AssignmentExpression* expression)
{
    return true;
}

bool cld::Semantics::SemanticAnalysis::parseMember(const Useable*& useable, Lexer::CTokenIterator attributeName,
                                                   const Syntax::AssignmentExpression* expression)
{
    return true;
}

bool cld::Semantics::SemanticAnalysis::parseMember(std::string& text, Lexer::CTokenIterator,
                                                   const Syntax::AssignmentExpression* expression)
{
    auto expr = visit(*expression);
    if (expr->isUndefined())
    {
        return false;
    }
    if (!isStringLiteralExpr(*expr) || !std::holds_alternative<std::string>(expr->as<Constant>().getValue()))
    {
        log(Errors::Semantics::ARGUMENT_TO_DEPRECATED_MUST_BE_A_STRING_LITERAL.args(*expression, m_sourceInterface,
                                                                                    *expression));
        return false;
    }

    text = cld::get<std::string>(expr->as<Constant>().getValue());
    return true;
}

template <class T>
bool cld::Semantics::SemanticAnalysis::parseMember(std::optional<T>& optional, Lexer::CTokenIterator attributeName,
                                                   const Syntax::AssignmentExpression* expression)
{
    if (!expression)
    {
        optional = {};
        return true;
    }
    optional.emplace();
    return parseMember(*optional, attributeName, expression);
}

template <class T>
std::optional<cld::Semantics::AllAttributes>
    cld::Semantics::SemanticAnalysis::parseAttribute(const Syntax::GNUAttributes::GNUAttribute& attribute)
{
    T result;
    auto tuple = to_tuple(result);
    constexpr std::size_t minExpressions = std::apply(
        [&](auto... refs) -> std::size_t
        {
            std::size_t count = 0;
            (void)(
                [&](auto&& value) -> bool
                                         {
                                             using U = std::decay_t<decltype(*value)>;
                                             if constexpr (cld::IsOptional<U>{} || cld::IsVector<U>{})
                                             {
                                                 return true;
                                             }
                                             else
                                             {
                                                 count++;
                                                 return false;
                                             }
                                         }(static_cast<typename std::decay_t<decltype(refs)>>(nullptr))
                                         || ...);
            return count;
        },
        decltype(toTypePtrTuple(std::declval<decltype(tuple)>())){});
    constexpr bool lastIsVector = []
    {
        if constexpr (std::tuple_size_v<decltype(tuple)> != 0)
        {
            return cld::IsVector<
                std::decay_t<std::tuple_element_t<std::tuple_size_v<decltype(tuple)> - 1, decltype(tuple)>>>{};
        }
        else
        {
            return false;
        }
    }();
    constexpr std::size_t maxExpressions =
        lastIsVector ? static_cast<std::size_t>(-1) : std::tuple_size_v<decltype(tuple)>;
    if constexpr (maxExpressions > 0)
    {
        static_assert(
            checkValidStruct(static_cast<decltype(tupleWithoutRef(std::declval<decltype(tuple)>()))*>(nullptr)),
            "struct T is malformed. Optionals can only be followed by optionals");
    }

    if (attribute.arguments.size() < minExpressions)
    {
        if constexpr (minExpressions == maxExpressions)
        {
            log(Errors::Semantics::INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_N_GOT_N.args(
                *attribute.nameToken, m_sourceInterface, *attribute.nameToken, minExpressions,
                attribute.arguments.size()));
        }
        else
        {
            log(Errors::Semantics::INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_AT_LEAST_N_GOT_N.args(
                *attribute.nameToken, m_sourceInterface, *attribute.nameToken, minExpressions,
                attribute.arguments.size()));
        }
        return {};
    }
    if (attribute.arguments.size() > maxExpressions)
    {
        if constexpr (maxExpressions == 0)
        {
            log(Errors::Semantics::INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_NONE_GOT_N.args(
                *attribute.nameToken, m_sourceInterface, *attribute.nameToken, attribute.arguments.size()));
        }
        else if constexpr (minExpressions == maxExpressions)
        {
            log(Errors::Semantics::INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_N_GOT_N.args(
                *attribute.nameToken, m_sourceInterface, *attribute.nameToken, minExpressions,
                attribute.arguments.size()));
        }
        else
        {
            log(Errors::Semantics::INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_AT_MOST_N_GOT_N.args(
                *attribute.nameToken, m_sourceInterface, *attribute.nameToken, maxExpressions,
                attribute.arguments.size()));
        }
        return {};
    }

    if constexpr (lastIsVector)
    {
        auto begin = attribute.arguments.begin();
        if (!std::apply(
                [&](auto&... refs, auto& last) -> bool
                {
                    auto result = (true & ...
                                   & parseMember(refs, attribute.nameToken,
                                                 begin == attribute.arguments.end() ? nullptr : &*(begin++)));
                    return std::accumulate(
                        begin, attribute.arguments.end(), result,
                        [&](bool result, const auto& value)
                        { return result & parseMember(last.emplace_back(), attribute.nameToken, &value); });
                },
                tuple))
        {
            return {};
        }
    }
    else
    {
        auto begin = attribute.arguments.begin();
        if (!std::apply(
                [&](auto&... refs) -> bool
                {
                    return (true & ...
                            & parseMember(refs, attribute.nameToken,
                                          begin == attribute.arguments.end() ? nullptr : &*(begin++)));
                },
                tuple))
        {
            return {};
        }
    }
    return result;
}

void cld::Semantics::SemanticAnalysis::createAttributes()
{
    auto gnuSpelling = [&](std::string name, auto parser)
    {
        m_attributesParser.emplace(name, parser);
        m_attributesParser.emplace("__" + name + "__", parser);
    };
    gnuSpelling("aligned", &SemanticAnalysis::parseAttribute<AlignedAttribute>);
    gnuSpelling("vector_size", &SemanticAnalysis::parseAttribute<VectorSizeAttribute>);
    gnuSpelling("noinline", &SemanticAnalysis::parseAttribute<NoinlineAttribute>);
    gnuSpelling("used", &SemanticAnalysis::parseAttribute<UsedAttribute>);
    gnuSpelling("always_inline", &SemanticAnalysis::parseAttribute<AlwaysInlineAttribute>);
    gnuSpelling("gnu_inline", &SemanticAnalysis::parseAttribute<GnuInlineAttribute>);
    gnuSpelling("artificial", &SemanticAnalysis::parseAttribute<ArtificialAttribute>);
    gnuSpelling("nothrow", &SemanticAnalysis::parseAttribute<NothrowAttribute>);
    gnuSpelling("const", &SemanticAnalysis::parseAttribute<ConstAttribute>);
    gnuSpelling("nonnull", &SemanticAnalysis::parseAttribute<NonnullAttribute>);
    gnuSpelling("noreturn", &SemanticAnalysis::parseAttribute<NoreturnAttribute>);
    gnuSpelling("deprecated", &SemanticAnalysis::parseAttribute<DeprecatedAttribute>);
    gnuSpelling("weak", &SemanticAnalysis::parseAttribute<WeakAttribute>);
    gnuSpelling("leaf", &SemanticAnalysis::parseAttribute<LeafAttribute>);
    gnuSpelling("pure", &SemanticAnalysis::parseAttribute<PureAttribute>);
    gnuSpelling("warn_unused_result", &SemanticAnalysis::parseAttribute<WarnUnusedResultAttribute>);
    if (getLanguageOptions().triple.getPlatform() == Platform::Windows)
    {
        gnuSpelling("dllimport", &SemanticAnalysis::parseAttribute<DllImportAttribute>);
    }
}

std::vector<cld::Semantics::ParsedAttribute<>>
    cld::Semantics::SemanticAnalysis::applyAttributes(AffectsAll applicant, std::vector<ParsedAttribute<>>&& attributes,
                                                      const CallingContext& context)
{
    std::vector<ParsedAttribute<>> results;
    for (auto& iter : attributes)
    {
        if (!cld::match(iter.attribute,
                        [&](auto&& attribute)
                        {
                            using T = std::decay_t<decltype(attribute)>;
                            return tryAll<AffectsTag, AffectsType, AffectsFunction, AffectsTagType, AffectsVariable,
                                          AffectsTypeVariable, AffectsTagTypeVariable, AffectsVariableFunction,
                                          AffectsTagVariableFunction, AffectsAll>(
                                *this, applicant, ParsedAttribute<T>{iter.name, iter.expressionRanges, attribute},
                                context);
                        }))
        {
            cld::match(
                applicant, [&](AffectsVariable) { iter.attempts |= ParsedAttribute<>::Variable; },
                [&](AffectsFunction) { iter.attempts |= ParsedAttribute<>::Function; },
                [&](AffectsType) { iter.attempts |= ParsedAttribute<>::Type; },
                [&](AffectsTag) { iter.attempts |= ParsedAttribute<>::Tag; });
            results.push_back(std::move(iter));
        }
    }
    return results;
}

void cld::Semantics::SemanticAnalysis::reportNotApplicableAttributes(const std::vector<ParsedAttribute<>>& attributes)
{
    for (auto& iter : attributes)
    {
        switch (iter.attempts)
        {
            case ParsedAttribute<>::Nothing:
                log(Warnings::Semantics::UNKNOWN_ATTRIBUTE_N_IGNORED.args(*iter.name, m_sourceInterface, *iter.name));
                break;
            case ParsedAttribute<>::Tag:
            case ParsedAttribute<>::Tag | ParsedAttribute<>::Type:
                log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_TYPES.args(*iter.name, m_sourceInterface,
                                                                                  *iter.name));
                break;
            case ParsedAttribute<>::Type:
                log(Warnings::Semantics::ATTRIBUTE_N_IGNORED_WHILE_PARSING_TYPE.args(*iter.name, m_sourceInterface,
                                                                                     *iter.name));
                break;
            case ParsedAttribute<>::Variable:
                log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_VARIABLES.args(*iter.name, m_sourceInterface,
                                                                                      *iter.name));
                break;
            case ParsedAttribute<>::Function:
                log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_FUNCTIONS.args(*iter.name, m_sourceInterface,
                                                                                      *iter.name));
                break;
            case ParsedAttribute<>::Variable | ParsedAttribute<>::Type:
            case ParsedAttribute<>::Variable | ParsedAttribute<>::Tag:
            case ParsedAttribute<>::Variable | ParsedAttribute<>::Tag | ParsedAttribute<>::Type:
                log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_TYPES_OR_VARIABLES.args(
                    *iter.name, m_sourceInterface, *iter.name));
                break;
            case ParsedAttribute<>::Function | ParsedAttribute<>::Type:
            case ParsedAttribute<>::Function | ParsedAttribute<>::Tag:
            case ParsedAttribute<>::Function | ParsedAttribute<>::Tag | ParsedAttribute<>::Type:
                log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_TYPES_OR_FUNCTIONS.args(
                    *iter.name, m_sourceInterface, *iter.name));
                break;
            case ParsedAttribute<>::Variable | ParsedAttribute<>::Function:
                log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_VARIABLES_OR_FUNCTIONS.args(
                    *iter.name, m_sourceInterface, *iter.name));
                break;
            case ParsedAttribute<>::Variable | ParsedAttribute<>::Function | ParsedAttribute<>::Type:
            case ParsedAttribute<>::Variable | ParsedAttribute<>::Function | ParsedAttribute<>::Tag:
            case ParsedAttribute<>::Variable | ParsedAttribute<>::Function | ParsedAttribute<>::Type
                | ParsedAttribute<>::Tag:
                log(Warnings::Semantics::ATTRIBUTE_N_DOES_NOT_APPLY_TO_TYPES_VARIABLES_OR_FUNCTIONS.args(
                    *iter.name, m_sourceInterface, *iter.name));
                break;
            default: CLD_UNREACHABLE;
        }
    }
}

void cld::Semantics::SemanticAnalysis::apply(AffectsTagVariableFunction applicant,
                                             const ParsedAttribute<AlignedAttribute>& attribute)
{
    if (attribute.attribute.alignment)
    {
        auto value = *attribute.attribute.alignment;
        if ((value & (value - 1)) != 0)
        {
            log(Errors::Semantics::ARGUMENT_TO_ALIGNED_MUST_BE_A_POWER_OF_2.args(
                attribute.expressionRanges[0], m_sourceInterface, attribute.expressionRanges[0], value));
            return;
        }
    }
    auto value = attribute.attribute.alignment.value_or(getLanguageOptions().alignOfLongDouble);
    cld::match(applicant,
               [&](auto holder)
               {
                   if (auto* existent = holder->template getAttributeIf<AlignedAttribute>())
                   {
                       *existent = AlignedAttribute{std::max(value, *existent->alignment)};
                   }
                   else
                   {
                       holder->addAttribute(AlignedAttribute{value});
                   }
               });
}

void cld::Semantics::SemanticAnalysis::apply(AffectsTypeVariable applicant,
                                             const ParsedAttribute<VectorSizeAttribute>& attribute)
{
    const Type& baseType = cld::match(
        applicant,
        [](not_null<VariableDeclaration> variableDeclaration) -> const Type& { return variableDeclaration->getType(); },
        [](auto pair) -> const Type& { return *pair.first; });
    if (!isArithmetic(baseType) || baseType.is<EnumType>())
    {
        cld::match(
            applicant,
            [&](VariableDeclaration* variableDeclaration)
            {
                log(Errors::Semantics::VECTOR_SIZE_CAN_ONLY_BE_APPLIED_TO_VARIABLES_OF_ARITHMETIC_TYPES.args(
                    *variableDeclaration->getNameToken(), m_sourceInterface, *variableDeclaration->getNameToken(),
                    baseType));
            },
            [&](const std::pair<IntrVarValue<Type>*, diag::PointRange>& pair)
            {
                log(Errors::Semantics::VECTOR_SIZE_CAN_ONLY_BE_APPLIED_TO_ARITHMETIC_TYPES.args(
                    pair.second, m_sourceInterface, pair.second, baseType));
            });
        return;
    }
    if (baseType.as<PrimitiveType>().getKind() == PrimitiveType::LongDouble)
    {
        cld::match(
            applicant,
            [&](VariableDeclaration* variableDeclaration)
            {
                log(Errors::Semantics::VECTOR_SIZE_CAN_NOT_BE_APPLIED_TO_LONG_DOUBLE.args(
                    *variableDeclaration->getNameToken(), m_sourceInterface, *variableDeclaration->getNameToken(),
                    baseType));
            },
            [&](const std::pair<IntrVarValue<Type>*, diag::PointRange>& pair)
            {
                log(Errors::Semantics::VECTOR_SIZE_CAN_NOT_BE_APPLIED_TO_LONG_DOUBLE.args(
                    pair.second, m_sourceInterface, pair.second, baseType));
            });
        return;
    }
    auto size = baseType.getSizeOf(*this);
    auto mod = attribute.attribute.size % size;
    if (mod != 0)
    {
        log(Errors::Semantics::ARGUMENT_OF_VECTOR_SIZE_MUST_BE_A_MULTIPLE_OF_THE_SIZE_OF_THE_BASE_TYPE.args(
            attribute.expressionRanges[0], m_sourceInterface, attribute.expressionRanges[0],
            cld::to_string(attribute.attribute.size) + " % sizeof("
                + diag::StringConverter<Type>::inArg(baseType, &m_sourceInterface) + ") /*" + cld::to_string(size)
                + "*/ = " + cld::to_string(mod)));
        return;
    }
    auto multiple = attribute.attribute.size / size;
    if ((multiple & (multiple - 1)) != 0)
    {
        log(Errors::Semantics::ARGUMENT_OF_VECTOR_SIZE_SHOULD_BE_A_POWER_OF_2_MULTIPLE_OF_THE_SIZE_OF_THE_BASE_TYPE
                .args(attribute.expressionRanges[0], m_sourceInterface, attribute.expressionRanges[0],
                      cld::to_string(attribute.attribute.size) + " / sizeof("
                          + diag::StringConverter<Type>::inArg(baseType, &m_sourceInterface) + ") /*"
                          + cld::to_string(size) + "*/ = " + cld::to_string(multiple)));
        return;
    }
    cld::match(
        applicant,
        [&](const std::pair<IntrVarValue<Type>*, diag::PointRange>& pair)
        {
            pair.first->emplace<VectorType>(typeAlloc(*removeQualifiers(std::move(baseType))), multiple,
                                            flag::useFlags = baseType.getFlags());
        },
        [&](VariableDeclaration* variableDeclaration)
        {
            auto newType = VectorType(typeAlloc(*removeQualifiers(std::move(baseType))), multiple,
                                      flag::useFlags = baseType.getFlags());
            variableDeclaration->setType(std::move(newType));
        });
}

void cld::Semantics::SemanticAnalysis::apply(AffectsVariableFunction applicant,
                                             const ParsedAttribute<UsedAttribute>& attribute)
{
    if (cld::match(
            applicant,
            [&](FunctionDefinition* def)
            {
                if (def->getLinkage() != Linkage::Internal)
                {
                    log(Warnings::Semantics::ATTRIBUTE_USED_ONLY_APPLIES_TO_FUNCTIONS_WITH_INTERNAL_LINKAGE.args(
                        *attribute.name, m_sourceInterface, *attribute.name, *def->getNameToken()));
                    return true;
                }
                return false;
            },
            [&](VariableDeclaration* declaration)
            {
                if (declaration->getLinkage() != Linkage::Internal || m_currentScope != 0)
                {
                    log(Warnings::Semantics::ATTRIBUTE_USED_ONLY_APPLIES_TO_GLOBAL_VARIABLES_WITH_INTERNAL_LINKAGE.args(
                        *attribute.name, m_sourceInterface, *attribute.name, *declaration->getNameToken()));
                    return true;
                }
                return false;
            },
            [&](FunctionDeclaration* declaration)
            {
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
    cld::match(applicant, [&](auto holder) { holder->addAttribute(attribute.attribute); });
}

void cld::Semantics::SemanticAnalysis::apply(AffectsTagVariableFunction applicant,
                                             const ParsedAttribute<DeprecatedAttribute>& attribute)
{
    cld::match(applicant, [&](auto holder) { holder->addAttribute(attribute.attribute); });
}

void cld::Semantics::SemanticAnalysis::apply(AffectsFunction applicant,
                                             const ParsedAttribute<GnuInlineAttribute>& attribute,
                                             const CallingContext& context)
{
    cld::match(
        applicant,
        [&](auto holder)
        {
            if (auto* funCon = std::get_if<FunctionContext>(&context); funCon && funCon->hasInlineSpecifier)
            {
                return;
            }
            log(Warnings::Semantics::GNU_INLINE_CAN_NOT_BE_APPLIED_TO_FUNCTION_N_BECAUSE_IT_IS_NOT_DECLARED_INLINE.args(
                *attribute.name, m_sourceInterface, *holder->getNameToken(), *attribute.name));
        });
    cld::match(applicant, [](auto holder) { holder->addAttribute(GnuInlineAttribute{}); });
}

void cld::Semantics::SemanticAnalysis::apply(AffectsVariableFunction applicant,
                                             const ParsedAttribute<DllImportAttribute>& attribute)
{
    cld::match(
        applicant,
        [&](not_null<VariableDeclaration> var)
        {
            if (var->getLinkage() == Linkage::Internal)
            {
                log(Errors::Semantics::DLLIMPORT_CANNOT_BE_APPLIED_TO_VARIABLE_N_WITH_INTERNAL_LINKAGE.args(
                    *attribute.name, m_sourceInterface, *var->getNameToken(), *attribute.name));
                return;
            }
            var->addAttribute(DllImportAttribute{});
        },
        [&](not_null<FunctionDeclaration> decl)
        {
            if (!decl->getFunctionGroup().isInline())
            {
                decl->addAttribute(DllImportAttribute{});
                return;
            }
            log(Warnings::Semantics::ATTRIBUTE_DLLIMPORT_IGNORED_ON_INLINE_FUNCTION_N.args(
                *attribute.name, m_sourceInterface, *decl->getNameToken(), *attribute.name));
        },
        [&](not_null<FunctionDefinition> def)
        {
            log(Errors::Semantics::DLLIMPORT_CANNOT_BE_APPLIED_TO_DEFINITION_OF_FUNCTION_N.args(
                *attribute.name, m_sourceInterface, *def->getNameToken(), *attribute.name));
        });
}

void cld::Semantics::SemanticAnalysis::apply(AffectsFunction applicant,
                                             const ParsedAttribute<NonnullAttribute>& attribute)
{
    auto& ft = cld::match(applicant, [](auto holder) -> const FunctionType& { return holder->getType(); });
    for (auto iter = attribute.attribute.indices.begin(); iter != attribute.attribute.indices.end(); iter++)
    {
        if (*iter == 0 || (*iter - 1) >= ft.getParameters().size())
        {
            log(Errors::Semantics::NONNULL_INDEX_N_OUT_OF_BOUNDS.args(
                attribute.expressionRanges[iter - attribute.attribute.indices.begin()], m_sourceInterface, *iter,
                attribute.expressionRanges[iter - attribute.attribute.indices.begin()]));
            continue;
        }
        if (!ft.getParameters()[*iter - 1].type->is<PointerType>())
        {
            // TODO: Better source locations

            log(Warnings::Semantics::ARGUMENT_N_OF_NONNULL_PARAMETER_IS_OF_NON_POINTER_TYPE_N.args(
                attribute.expressionRanges[iter - attribute.attribute.indices.begin()], m_sourceInterface, *iter,
                *ft.getParameters()[*iter - 1].type,
                attribute.expressionRanges[iter - attribute.attribute.indices.begin()]));
        }
    }
    if (attribute.attribute.indices.empty()
        && std::none_of(ft.getParameters().begin(), ft.getParameters().end(),
                        cld::compose(&Type::is<PointerType>, &FunctionType::Parameter::type)))
    {
        log(Warnings::Semantics::FUNCTION_N_WITH_NONNULL_ATTRIBUTE_DOES_NOT_HAVE_ANY_POINTER_PARAMETERS.args(
            *attribute.name, m_sourceInterface,
            *cld::match(applicant, [](auto holder) { return holder->getNameToken(); }), *attribute.name));
    }
    cld::match(applicant, [&](auto holder) { holder->addAttribute(attribute.attribute); });
}

void cld::Semantics::SemanticAnalysis::apply(AffectsFunction applicant,
                                             const ParsedAttribute<WarnUnusedResultAttribute>& attribute)
{
    auto& retType =
        cld::match(applicant, [](auto&& value) -> decltype(auto) { return value->getType().getReturnType(); });
    if (isVoid(retType))
    {
        auto* token = cld::match(applicant, [](auto&& value) { return value->getNameToken(); });
        log(Warnings::Semantics::FUNCTION_N_WITH_WARN_UNUSED_RESULT_ATTRIBUTE_RETURNS_NOTHING.args(
            *token, m_sourceInterface, *token, *attribute.name));
        return;
    }
    cld::match(applicant, [](auto holder) { holder->addAttribute(WarnUnusedResultAttribute{}); });
}

void cld::Semantics::SemanticAnalysis::apply(AffectsVariableFunction applicant,
                                             const ParsedAttribute<WeakAttribute>& attribute)
{
    if (cld::match(applicant, [](auto&& value) { return value->getLinkage(); }) == Linkage::Internal)
    {
        cld::match(
            applicant,
            [&](cld::not_null<VariableDeclaration> variable)
            {
                log(Errors::Semantics::WEAK_ATTRIBUTE_CANNOT_BE_APPLIED_TO_VARIABLE_N_WITH_INTERNAL_LINKAGE.args(
                    *variable->getNameToken(), m_sourceInterface, *variable->getNameToken(), *attribute.name));
            },
            [&](auto function)
            {
                log(Errors::Semantics::WEAK_ATTRIBUTE_CANNOT_BE_APPLIED_TO_FUNCTION_N_WITH_INTERNAL_LINKAGE.args(
                    *function->getNameToken(), m_sourceInterface, *function->getNameToken(), *attribute.name));
            });
        return;
    }
    cld::match(applicant, [](auto holder) { holder->addAttribute(WeakAttribute{}); });
}
