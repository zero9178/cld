#include "ConstantEvaluator.hpp"

#include <llvm/ADT/StringExtras.h>

#include <algorithm>
#include <utility>

cld::Semantics::ConstRetType::ConstRetType(const cld::Semantics::ConstRetType::ValueType& value,
                                           const cld::Semantics::Type& type)
    : m_value(value), m_type(type)
{
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::integerPromotion(const LanguageOptions& options) const
{
    return match(
        m_value, [this](VoidStar) -> ConstRetType { return *this; },
        [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [this](const llvm::APFloat&) -> ConstRetType { return *this; },
        [this, &options](const llvm::APSInt& integer) -> ConstRetType {
            if (integer.getBitWidth() < options.sizeOfInt * 8u)
            {
                auto apsInt = integer.extend(options.sizeOfInt * 8);
                apsInt.setIsSigned(true);
                return {std::move(apsInt), PrimitiveType::createInt(false, false, options)};
            }
            return {std::move(integer), m_type};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::unaryPlus(const LanguageOptions& options) const
{
    return integerPromotion(options);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::negate(const LanguageOptions& options) const
{
    auto temp = integerPromotion(options);
    return match(
        temp.getValue(), [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&temp](llvm::APFloat floating) -> ConstRetType {
            floating.changeSign();
            return {floating, temp.getType()};
        },
        [&temp](llvm::APSInt integer) -> ConstRetType {
            integer.negate();
            return {integer, temp.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::logicalNegate(const LanguageOptions& options) const
{
    return match(
        m_value,
        [&options](VoidStar address) -> ConstRetType {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, address.address == 0), false),
                    Semantics::PrimitiveType::createInt(false, false, options)};
        },
        [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&options](const llvm::APFloat& floating) -> ConstRetType {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, floating.isZero()), false),
                    Semantics::PrimitiveType::createInt(false, false, options)};
        },
        [&options](const llvm::APSInt& integer) -> ConstRetType {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, integer == 0), false),
                    Semantics::PrimitiveType::createInt(false, false, options)};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::bitwiseNegate(const LanguageOptions& options) const
{
    auto temp = integerPromotion(options);
    return match(
        temp.getValue(), [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&temp](llvm::APSInt integer) -> ConstRetType {
            integer.flipAllBits();
            return {integer, temp.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::castTo(const cld::Semantics::Type& type,
                                                                  const LanguageOptions& options, Issue* issues) const
{
    if (issues)
    {
        *issues = NoIssue;
    }
    auto copy = type.get();
    auto nonLvalue = Type(false, false, std::move(copy));
    return match(
        m_value, [](std::monostate) -> ConstRetType { return {}; },
        [&](VoidStar address) -> ConstRetType {
            return match(
                nonLvalue.get(), [](const auto&) -> ConstRetType { CLD_UNREACHABLE; },
                [&](const PrimitiveType& primitiveType) -> ConstRetType {
                    if (primitiveType.isFloatingPoint())
                    {
                        CLD_UNREACHABLE;
                    }
                    if (primitiveType.getBitCount() == 1)
                    {
                        auto result = toBool(options);
                        if (!std::holds_alternative<llvm::APSInt>(result.getValue()))
                        {
                            CLD_UNREACHABLE;
                        }
                        return {llvm::APSInt(cld::get<llvm::APSInt>(result.getValue()).zextOrTrunc(8)),
                                PrimitiveType::createUnderlineBool(false, false)};
                    }
                    if (issues
                        && (primitiveType.isSigned() ? llvm::APInt::getSignedMaxValue(primitiveType.getBitCount()) :
                                                       llvm::APInt::getMaxValue(primitiveType.getBitCount()))
                               .ugt(address.address))
                    {
                        *issues = Issue::NotRepresentable;
                    }
                    return {llvm::APSInt(llvm::APInt(primitiveType.getBitCount(), address.address),
                                         !primitiveType.isSigned()),
                            nonLvalue};
                },
                [&](const PointerType&) -> ConstRetType {
                    return {address, nonLvalue};
                });
        },
        [&](llvm::APFloat floating) -> ConstRetType {
            return match(
                nonLvalue.get(), [](const auto&) -> ConstRetType { CLD_UNREACHABLE; },
                [&](const PrimitiveType& primitiveType) mutable -> ConstRetType {
                    bool response;
                    llvm::APFloat::opStatus op;
                    if (primitiveType.isFloatingPoint())
                    {
                        switch (primitiveType.getBitCount())
                        {
                            case 32:
                                op = floating.convert(llvm::APFloat::IEEEsingle(), llvm::APFloat::rmNearestTiesToEven,
                                                      &response);
                                break;
                            case 64:
                                op = floating.convert(llvm::APFloat::IEEEdouble(), llvm::APFloat::rmNearestTiesToEven,
                                                      &response);
                                break;
                            case 80:
                                op = floating.convert(llvm::APFloat::x87DoubleExtended(),
                                                      llvm::APFloat::rmNearestTiesToEven, &response);
                                break;
                            case 128:
                                op = floating.convert(llvm::APFloat::IEEEquad(), llvm::APFloat::rmNearestTiesToEven,
                                                      &response);
                                break;
                            default: CLD_UNREACHABLE;
                        }
                        return {floating, nonLvalue};
                    }
                    if (primitiveType.getBitCount() == 1)
                    {
                        auto result = toBool(options);
                        if (!std::holds_alternative<llvm::APSInt>(result.getValue()))
                        {
                            CLD_UNREACHABLE;
                        }
                        return {llvm::APSInt(cld::get<llvm::APSInt>(result.getValue()).zextOrTrunc(8)),
                                PrimitiveType::createUnderlineBool(false, false)};
                    }

                    llvm::APSInt result(primitiveType.getBitCount(), !primitiveType.isSigned());
                    op = floating.convertToInteger(result, llvm::APFloat::rmNearestTiesToEven, &response);
                    if (issues && op == llvm::APFloat::opInvalidOp)
                    {
                        *issues = Issue::NotRepresentable;
                    }
                    return {result, nonLvalue};
                });
        },
        [&](const llvm::APSInt& integer) -> ConstRetType {
            return match(
                nonLvalue.get(), [](const auto&) -> ConstRetType { CLD_UNREACHABLE; },
                [&](const PrimitiveType& primitiveType) -> ConstRetType {
                    if (primitiveType.isFloatingPoint())
                    {
                        decltype(auto) semantics = [&primitiveType]() -> decltype(auto) {
                            switch (primitiveType.getBitCount())
                            {
                                case 32: return llvm::APFloat::IEEEsingle();
                                case 64: return llvm::APFloat::IEEEdouble();
                                case 80: return llvm::APFloat::x87DoubleExtended();
                                case 128: return llvm::APFloat::IEEEquad();
                                default: CLD_UNREACHABLE;
                            }
                        }();
                        // A 64 bit integer can always be correctly represented in 32 bit float
                        // Unless we add 16 bit floats we don't need to check for conversion errors
                        llvm::APFloat result(semantics);
                        result.convertFromAPInt(integer, integer.isSigned(), llvm::APFloat::rmNearestTiesToEven);
                        return {result, nonLvalue};
                    }
                    if (primitiveType.getBitCount() == 1)
                    {
                        auto result = toBool(options);
                        if (!std::holds_alternative<llvm::APSInt>(result.getValue()))
                        {
                            CLD_UNREACHABLE;
                        }
                        return {llvm::APSInt(cld::get<llvm::APSInt>(result.getValue()).zextOrTrunc(8)),
                                PrimitiveType::createUnderlineBool(false, false)};
                    }

                    if (issues)
                    {
                        auto apInt = primitiveType.isSigned() ?
                                         llvm::APInt::getSignedMaxValue(primitiveType.getBitCount()) :
                                         llvm::APInt::getMaxValue(primitiveType.getBitCount());
                        auto other = integer.extOrTrunc(
                            std::max<std::size_t>(primitiveType.getBitCount(), integer.getBitWidth()));
                        apInt = apInt.zextOrTrunc(
                            std::max<std::size_t>(primitiveType.getBitCount(), integer.getBitWidth()));
                        if (apInt.ult(other))
                        {
                            *issues = Issue::NotRepresentable;
                        }
                    }

                    auto apsInt = integer.extOrTrunc(primitiveType.getBitCount());
                    apsInt.setIsSigned(primitiveType.isSigned());
                    return {apsInt, nonLvalue};
                },
                [&](const PointerType&) -> ConstRetType {
                    return {VoidStar{integer.getZExtValue()}, nonLvalue};
                });
        });
}

std::pair<cld::Semantics::ConstRetType, cld::Semantics::ConstRetType>
    cld::Semantics::ConstRetType::arithmeticConversions(ConstRetType lhs, ConstRetType rhs,
                                                        const LanguageOptions& options)
{
    if (std::holds_alternative<std::monostate>(lhs.getValue()) || std::holds_alternative<VoidStar>(lhs.getValue())
        || std::holds_alternative<std::monostate>(rhs.getValue()) || std::holds_alternative<VoidStar>(rhs.getValue()))
    {
        return {std::move(lhs), std::move(rhs)};
    }
    lhs = lhs.integerPromotion(options);
    rhs = rhs.integerPromotion(options);
    if (rhs.getType() == lhs.getType())
    {
        return {std::move(lhs), std::move(rhs)};
    }
    if (std::holds_alternative<llvm::APFloat>(lhs.getValue()) || std::holds_alternative<llvm::APFloat>(rhs.getValue()))
    {
        if (std::holds_alternative<llvm::APFloat>(lhs.getValue())
            && std::holds_alternative<llvm::APFloat>(rhs.getValue()))
        {
            bool useless;
            auto lhsFloat = cld::get<llvm::APFloat>(lhs.getValue());
            auto rhsFloat = cld::get<llvm::APFloat>(rhs.getValue());

            auto leftBigger = llvm::APFloat::getSizeInBits(lhsFloat.getSemantics())
                              > llvm::APFloat::getSizeInBits(rhsFloat.getSemantics());
            const auto& biggerSemantics = leftBigger ? lhsFloat.getSemantics() : rhsFloat.getSemantics();
            lhsFloat.convert(biggerSemantics, llvm::APFloat::rmNearestTiesToEven, &useless);
            rhsFloat.convert(biggerSemantics, llvm::APFloat::rmNearestTiesToEven, &useless);
            auto biggerType = leftBigger ? lhs.getType() : rhs.getType();
            return {{lhsFloat, biggerType}, {rhsFloat, biggerType}};
        }
        auto& floating = std::holds_alternative<llvm::APFloat>(lhs.getValue()) ? lhs : rhs;
        const auto& semantics = cld::get<llvm::APFloat>(floating.getValue()).getSemantics();
        if (std::holds_alternative<llvm::APSInt>(lhs.getValue()))
        {
            auto& integer = cld::get<llvm::APSInt>(lhs.getValue());
            auto result = llvm::APFloat(semantics);
            result.convertFromAPInt(integer, integer.isSigned(), llvm::APFloat::rmNearestTiesToEven);
            lhs = {std::move(result), floating.getType()};
        }
        if (std::holds_alternative<llvm::APSInt>(rhs.getValue()))
        {
            auto& integer = cld::get<llvm::APSInt>(rhs.getValue());
            auto result = llvm::APFloat(semantics);
            result.convertFromAPInt(integer, integer.isSigned(), llvm::APFloat::rmNearestTiesToEven);
            rhs = {std::move(result), floating.getType()};
        }
        return {std::move(lhs), std::move(rhs)};
    }
    auto lhsInteger = cld::get<llvm::APSInt>(lhs.getValue());
    auto rhsInteger = cld::get<llvm::APSInt>(rhs.getValue());
    if (lhsInteger.isSigned() == rhsInteger.isSigned() || lhsInteger.getBitWidth() != rhsInteger.getBitWidth())
    {
        auto lhsBigger = lhsInteger.getBitWidth() > rhsInteger.getBitWidth();
        auto biggerBits = lhsBigger ? lhsInteger.getBitWidth() : rhsInteger.getBitWidth();
        auto sign = lhsBigger ? lhsInteger.isSigned() : rhsInteger.isSigned();
        auto& type = lhsBigger ? lhs.getType() : rhs.getType();
        lhsInteger = lhsInteger.extOrTrunc(biggerBits);
        rhsInteger = rhsInteger.extOrTrunc(biggerBits);
        lhsInteger.setIsSigned(sign);
        rhsInteger.setIsSigned(sign);
        return {{std::move(lhsInteger), type}, {std::move(rhsInteger), type}};
    }
    auto unsignedType = lhsInteger.isUnsigned() ? lhs.getType() : rhs.getType();
    lhsInteger.setIsSigned(false);
    rhsInteger.setIsSigned(false);
    return {{std::move(lhsInteger), unsignedType}, {std::move(rhsInteger), unsignedType}};
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::multiply(const cld::Semantics::ConstRetType& rhs,
                                                                    const LanguageOptions& options, Issue* issues) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APFloat& floating) -> ConstRetType {
            return {floating * cld::get<llvm::APFloat>(op2.getValue()), op2.getType()};
        },
        [&op2 = op2, issues](const llvm::APSInt& integer) -> ConstRetType {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.smul_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow) :
                                               integer.umul_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssue;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::divide(const cld::Semantics::ConstRetType& rhs,
                                                                  const LanguageOptions& options, Issue* issues) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APFloat& floating) -> ConstRetType {
            return {floating / cld::get<llvm::APFloat>(op2.getValue()), op2.getType()};
        },
        [&op2 = op2, issues](const llvm::APSInt& integer) -> ConstRetType {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.sdiv_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow) :
                                               integer.udiv(cld::get<llvm::APSInt>(op2.getValue()));
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssue;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::modulo(const cld::Semantics::ConstRetType& rhs,
                                                                  const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APFloat& floating) -> ConstRetType {
            return {floating * cld::get<llvm::APFloat>(op2.getValue()), op2.getType()};
        },
        [&op2 = op2](const llvm::APSInt& integer) -> ConstRetType {
            auto apsInt = integer.isSigned() ? integer.srem(cld::get<llvm::APSInt>(op2.getValue())) :
                                               integer.urem(cld::get<llvm::APSInt>(op2.getValue()));
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::plus(const cld::Semantics::ConstRetType& rhs,
                                                                const LanguageOptions& options,
                                                                const SemanticAnalysis* analysis, Issue* issues) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2, &op1 = op1, analysis](VoidStar address) -> ConstRetType {
            if (!std::holds_alternative<llvm::APSInt>(op2.getValue()))
            {
                CLD_UNREACHABLE;
            }
            auto& integer = cld::get<llvm::APSInt>(op2.getValue());
            CLD_ASSERT(analysis);
            auto size = cld::get<PointerType>(op1.getType().get()).getElementType().getSizeOf(*analysis);
            if (integer.isUnsigned())
            {
                address.address += size * integer.getZExtValue();
            }
            else
            {
                address.address += static_cast<std::int64_t>(size) * integer.getSExtValue();
            }
            return {address, op1.getType()};
        },
        [&op2 = op2](const llvm::APFloat& floating) -> ConstRetType {
            return {floating + cld::get<llvm::APFloat>(op2.getValue()), op2.getType()};
        },
        [&op2 = op2, issues, analysis](const llvm::APSInt& integer) -> ConstRetType {
            if (std::holds_alternative<VoidStar>(op2.getValue()))
            {
                auto address = cld::get<VoidStar>(op2.getValue());
                CLD_ASSERT(analysis);
                auto size = cld::get<PointerType>(op2.getType().get()).getElementType().getSizeOf(*analysis);
                if (integer.isUnsigned())
                {
                    address.address += size * integer.getZExtValue();
                }
                else
                {
                    address.address += static_cast<std::int64_t>(size) * integer.getSExtValue();
                }
                return {address, op2.getType()};
            }
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.sadd_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow) :
                                               integer.uadd_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssue;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::minus(const cld::Semantics::ConstRetType& rhs,
                                                                 const LanguageOptions& options,
                                                                 const SemanticAnalysis* analysis, Issue* issues) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2, &op1 = op1, analysis, &options](VoidStar address) -> ConstRetType {
            CLD_ASSERT(analysis);
            auto size = cld::get<PointerType>(op1.getType().get()).getElementType().getSizeOf(*analysis);
            if (std::holds_alternative<VoidStar>(op2.getValue()))
            {
                return {llvm::APSInt(llvm::APInt(options.sizeOfVoidStar * 8,
                                                 (address.address - cld::get<VoidStar>(op2.getValue()).address) / size,
                                                 true),
                                     false),
                        getPtrdiffT(options)};
            }
            if (!std::holds_alternative<llvm::APSInt>(op2.getValue()))
            {
                CLD_UNREACHABLE;
            }
            auto& integer = cld::get<llvm::APSInt>(op2.getValue());
            if (integer.isUnsigned())
            {
                address.address -= size * integer.getZExtValue();
            }
            else
            {
                address.address -= static_cast<std::int64_t>(size) * integer.getSExtValue();
            }
            return {address, op1.getType()};
        },
        [&op2 = op2](const llvm::APFloat& floating) -> ConstRetType {
            return {floating - cld::get<llvm::APFloat>(op2.getValue()), op2.getType()};
        },
        [&op2 = op2, issues](const llvm::APSInt& integer) -> ConstRetType {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.ssub_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow) :
                                               integer.usub_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssue;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op2.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::shiftLeft(const cld::Semantics::ConstRetType& rhs,
                                                                     const LanguageOptions& options,
                                                                     Issue* issues) const
{
    auto op1 = integerPromotion(options);
    auto op2 = rhs.integerPromotion(options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&op1, &op2, issues](const llvm::APSInt& integer) -> ConstRetType {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.sshl_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow) :
                                               integer.ushl_ov(cld::get<llvm::APSInt>(op2.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssue;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned()), op1.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::shiftRight(const cld::Semantics::ConstRetType& rhs,
                                                                      const LanguageOptions& options,
                                                                      Issue* issues) const
{
    auto op1 = integerPromotion(options);
    auto op2 = rhs.integerPromotion(options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&op1, &op2, issues](const llvm::APSInt& integer) -> ConstRetType {
            auto op2Integer = cld::get<llvm::APSInt>(op2.getValue());
            if (issues && (op2Integer.isSignBitSet() || op2Integer.getZExtValue() >= integer.getBitWidth()))
            {
                *issues = NotRepresentable;
            }
            return {integer >> static_cast<unsigned>(op2Integer.getZExtValue()), op1.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::bitAnd(const cld::Semantics::ConstRetType& rhs,
                                                                  const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APSInt& integer) -> ConstRetType {
            return {integer & cld::get<llvm::APSInt>(op2.getValue()), op2.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::bitXor(const cld::Semantics::ConstRetType& rhs,
                                                                  const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APSInt& integer) -> ConstRetType {
            return {integer ^ cld::get<llvm::APSInt>(op2.getValue()), op2.getType()};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::bitOr(const cld::Semantics::ConstRetType& rhs,
                                                                 const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [](VoidStar) -> ConstRetType { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2](const llvm::APSInt& integer) -> ConstRetType {
            return {integer | cld::get<llvm::APSInt>(op2.getValue()), op2.getType()};
        });
}

cld::Semantics::ConstRetType::operator bool() const
{
    return match(
        m_value, [](VoidStar address) -> bool { return address.address != 0; },
        [](const llvm::APFloat& floating) -> bool { return floating.isNonZero(); },
        [](const llvm::APSInt& integer) -> bool { return !integer.isNullValue(); },
        [](std::monostate) -> bool { CLD_UNREACHABLE; });
}

bool cld::Semantics::ConstRetType::isUndefined() const
{
    return std::holds_alternative<std::monostate>(m_value);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::toBool(const LanguageOptions& options) const
{
    return notEqual(
        {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, 0), false), PrimitiveType::createInt(false, false, options)},
        options);
}

std::int64_t cld::Semantics::ConstRetType::toInt() const
{
    return match(
        m_value, [](VoidStar pointer) -> std::int64_t { return pointer.address; },
        [](const llvm::APFloat& floating) -> std::int64_t {
            return static_cast<std::int64_t>(floating.convertToDouble());
        },
        [](const llvm::APSInt& integer) -> std::int64_t { return integer.getSExtValue(); },
        [](std::monostate) -> std::int64_t { CLD_UNREACHABLE; });
}

std::uint64_t cld::Semantics::ConstRetType::toUInt() const
{
    return match(
        m_value, [](VoidStar pointer) -> std::uint64_t { return pointer.address; },
        [](const llvm::APFloat& floating) -> std::uint64_t {
            return static_cast<std::uint64_t>(floating.convertToDouble());
        },
        [](const llvm::APSInt& integer) -> std::uint64_t { return integer.getZExtValue(); },
        [](std::monostate) -> std::uint64_t { CLD_UNREACHABLE; });
}

std::string cld::Semantics::ConstRetType::toString() const
{
    return match(
        m_value, [](VoidStar pointer) -> std::string { return "0x" + llvm::utohexstr(pointer.address); },
        [](const llvm::APFloat& floating) -> std::string {
            llvm::SmallString<20> result;
            floating.toString(result);
            return result.str();
        },
        [](const llvm::APSInt& integer) -> std::string {
            llvm::SmallString<20> result;
            integer.toString(result);
            return result.str();
        },
        [](std::monostate) -> std::string { CLD_UNREACHABLE; });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::lessThan(const cld::Semantics::ConstRetType& rhs,
                                                                    const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2, &options](VoidStar address) -> ConstRetType {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8,
                                             address.address < cld::get<VoidStar>(op2.getValue()).address),
                                 false),
                    PrimitiveType::createInt(false, false, options)};
        },
        [&op2 = op2, &options](const llvm::APFloat& floating) -> ConstRetType {
            auto cmp = floating.compare(cld::get<llvm::APFloat>(op2.getValue()));
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, cmp == llvm::APFloat::cmpLessThan), false),
                    PrimitiveType::createInt(false, false, options)};
        },
        [&op2 = op2, &options](const llvm::APSInt& integer) -> ConstRetType {
            auto apsInt = integer < cld::get<llvm::APSInt>(op2.getValue());
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, apsInt), false),
                    PrimitiveType::createInt(false, false, options)};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::greaterThan(const cld::Semantics::ConstRetType& rhs,
                                                                       const LanguageOptions& options) const
{
    return rhs.lessThan(*this, options);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::lessOrEqual(const cld::Semantics::ConstRetType& rhs,
                                                                       const LanguageOptions& options) const
{
    return rhs.lessThan(*this, options).logicalNegate(options);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::greaterOrEqual(const cld::Semantics::ConstRetType& rhs,
                                                                          const LanguageOptions& options) const
{
    return lessThan(rhs, options).logicalNegate(options);
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::equal(const cld::Semantics::ConstRetType& rhs,
                                                                 const LanguageOptions& options) const
{
    auto [op1, op2] = arithmeticConversions(*this, rhs, options);
    return match(
        op1.getValue(), [](std::monostate) -> ConstRetType { CLD_UNREACHABLE; },
        [&op2 = op2, &options](VoidStar address) -> ConstRetType {
            if (std::holds_alternative<llvm::APSInt>(op2.getValue()))
            {
                CLD_ASSERT(cld::get<llvm::APSInt>(op2.getValue()) == 0);
                return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, address.address == 0), false),
                        PrimitiveType::createInt(false, false, options)};
            }
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8,
                                             address.address == cld::get<VoidStar>(op2.getValue()).address, true),
                                 false),
                    PrimitiveType::createInt(false, false, options)};
        },
        [&op2 = op2, &options](const llvm::APFloat& floating) -> ConstRetType {
            return {llvm::APSInt(
                        llvm::APInt(options.sizeOfInt * 8, floating.compare(cld::get<llvm::APFloat>(op2.getValue()))
                                                               == llvm::APFloat::cmpEqual),
                        false),
                    PrimitiveType::createInt(false, false, options)};
        },
        [&op2 = op2, &options](const llvm::APSInt& integer) -> ConstRetType {
            if (auto* address = std::get_if<VoidStar>(&op2.getValue()))
            {
                CLD_ASSERT(integer == 0);
                return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, address->address == 0), false),
                        PrimitiveType::createInt(false, false, options)};
            }
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, integer == cld::get<llvm::APSInt>(op2.getValue())),
                                 false),
                    PrimitiveType::createInt(false, false, options)};
        });
}

cld::Semantics::ConstRetType cld::Semantics::ConstRetType::notEqual(const cld::Semantics::ConstRetType& rhs,
                                                                    const LanguageOptions& options) const
{
    return equal(rhs, options).logicalNegate(options);
}

std::string cld::diag::StringConverter<cld::Semantics::ConstRetType>::inFormat(const Semantics::ConstRetType& arg,
                                                                               const SourceInterface&)
{
    return '\'' + arg.toString() + '\'';
}

std::string cld::diag::StringConverter<cld::Semantics::ConstRetType>::inArg(const Semantics::ConstRetType& arg,
                                                                            const SourceInterface&)
{
    return arg.toString();
}
