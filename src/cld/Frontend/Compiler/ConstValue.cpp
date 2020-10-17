#include "ConstValue.hpp"

#include <llvm/ADT/StringExtras.h>

#include <algorithm>
#include <utility>

#include "ProgramInterface.hpp"
#include "Semantics.hpp"

cld::Semantics::ConstValue::ConstValue(const cld::Semantics::ConstValue::ValueType& value) : m_value(value) {}

cld::Semantics::ConstValue cld::Semantics::ConstValue::negate(const LanguageOptions&) const
{
    return match(
        getValue(), [](VoidStar) -> ConstValue { CLD_UNREACHABLE; },
        [](AddressConstant) -> ConstValue { CLD_UNREACHABLE; }, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; },
        [](llvm::APFloat floating) -> ConstValue {
            floating.changeSign();
            return {floating};
        },
        [](llvm::APSInt integer) -> ConstValue {
            integer.negate();
            return {integer};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::logicalNegate(const LanguageOptions& options) const
{
    return match(
        m_value,
        [&options](VoidStar address) -> ConstValue {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, address.address == 0), false)};
        },
        [this](AddressConstant) -> ConstValue { return *this; }, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; },
        [&options](const llvm::APFloat& floating) -> ConstValue {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, floating.isZero()), false)};
        },
        [&options](const llvm::APSInt& integer) -> ConstValue {
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, integer == 0), false)};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::bitwiseNegate(const LanguageOptions&) const
{
    return match(
        m_value, [](VoidStar) -> ConstValue { CLD_UNREACHABLE; }, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstValue { CLD_UNREACHABLE; },
        [](AddressConstant) -> ConstValue { CLD_UNREACHABLE; },
        [](llvm::APSInt integer) -> ConstValue {
            integer.flipAllBits();
            return {integer};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::castTo(const cld::Semantics::Type& type,
                                                              const ProgramInterface* program,
                                                              const LanguageOptions& options, Issue* issues) const
{
    if (issues)
    {
        *issues = NoIssue;
    }
    return match(
        m_value, [](std::monostate) -> ConstValue { return {}; }, [&](AddressConstant) -> ConstValue { return *this; },
        [&](VoidStar address) -> ConstValue {
            return match(
                type.getVariant(), [](const auto&) -> ConstValue { CLD_UNREACHABLE; },
                [&](const PrimitiveType& primitiveType) -> ConstValue {
                    if (primitiveType.isFloatingPoint())
                    {
                        CLD_UNREACHABLE;
                    }
                    if (primitiveType.getKind() == PrimitiveType::Bool)
                    {
                        return toBool(options);
                    }
                    if (issues
                        && (primitiveType.isSigned() ? llvm::APInt::getSignedMaxValue(primitiveType.getBitCount()) :
                                                       llvm::APInt::getMaxValue(primitiveType.getBitCount()))
                               .ugt(address.address))
                    {
                        *issues = Issue::NotRepresentable;
                    }
                    return {llvm::APSInt(llvm::APInt(primitiveType.getBitCount(), address.address),
                                         !primitiveType.isSigned())};
                },
                [&](const PointerType&) -> ConstValue { return {address}; });
        },
        [&](llvm::APFloat floating) -> ConstValue {
            return match(
                type.getVariant(), [](const auto&) -> ConstValue { CLD_UNREACHABLE; },
                [&](const PrimitiveType& primitiveType) mutable -> ConstValue {
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
                        return {floating};
                    }
                    if (primitiveType.getKind() == PrimitiveType::Bool)
                    {
                        return toBool(options);
                    }

                    llvm::APSInt result(primitiveType.getBitCount(), !primitiveType.isSigned());
                    op = floating.convertToInteger(result, llvm::APFloat::rmNearestTiesToEven, &response);
                    if (issues && op == llvm::APFloat::opInvalidOp)
                    {
                        *issues = Issue::NotRepresentable;
                    }
                    return {result};
                });
        },
        [&](const llvm::APSInt& integer) -> ConstValue {
            return match(
                type.getVariant(), [](const auto&) -> ConstValue { CLD_UNREACHABLE; },
                [&](const PrimitiveType& primitiveType) -> ConstValue {
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
                        return {result};
                    }
                    if (primitiveType.getKind() == PrimitiveType::Bool)
                    {
                        return toBool(options);
                    }

                    if (issues)
                    {
                        auto apInt = primitiveType.isSigned() ?
                                         llvm::APInt::getSignedMaxValue(primitiveType.getBitCount()) :
                                         llvm::APInt::getMaxValue(primitiveType.getBitCount());
                        // if the cast is to an unsigned type then its maximum value has the highest bit set.
                        // Since we are doing a signed comparison we need to increase the bitwidth by at least one
                        // so that it is not thought off as a signed integer
                        if (apInt.isNegative() && !primitiveType.isSigned())
                        {
                            apInt = apInt.zext(apInt.getBitWidth() + 1);
                        }
                        auto other =
                            integer.extOrTrunc(std::max<std::size_t>(apInt.getBitWidth(), integer.getBitWidth()));
                        apInt = apInt.zextOrTrunc(std::max<std::size_t>(apInt.getBitWidth(), integer.getBitWidth()));
                        if (apInt.slt(other))
                        {
                            *issues = Issue::NotRepresentable;
                        }
                    }

                    auto apsInt = integer.extOrTrunc(primitiveType.getBitCount());
                    apsInt.setIsSigned(primitiveType.isSigned());
                    return {apsInt};
                },
                [&](const PointerType& pointerType) -> ConstValue {
                    CLD_ASSERT(program);
                    if (program->isCompleteType(pointerType.getElementType())
                        && !std::holds_alternative<FunctionType>(pointerType.getElementType().getVariant()))
                    {
                        return {VoidStar{integer.getZExtValue(),
                                         static_cast<std::uint32_t>(pointerType.getElementType().getSizeOf(*program))}};
                    }
                    return {VoidStar{integer.getZExtValue(), 0}};
                });
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::multiply(const cld::Semantics::ConstValue& rhs,
                                                                const LanguageOptions&, Issue* issues) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; }, [](VoidStar) -> ConstValue { CLD_UNREACHABLE; },
        [&rhs](const llvm::APFloat& floating) -> ConstValue {
            return {floating * cld::get<llvm::APFloat>(rhs.getValue())};
        },
        [](AddressConstant) -> ConstValue { CLD_UNREACHABLE; },
        [&rhs, issues](const llvm::APSInt& integer) -> ConstValue {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.smul_ov(cld::get<llvm::APSInt>(rhs.getValue()), overflow) :
                                               integer.umul_ov(cld::get<llvm::APSInt>(rhs.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssue;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned())};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::divide(const cld::Semantics::ConstValue& rhs,
                                                              const LanguageOptions&, Issue* issues) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; }, [](VoidStar) -> ConstValue { CLD_UNREACHABLE; },
        [](AddressConstant) -> ConstValue { CLD_UNREACHABLE; },
        [&rhs](const llvm::APFloat& floating) -> ConstValue {
            return {floating / cld::get<llvm::APFloat>(rhs.getValue())};
        },
        [&rhs, issues](const llvm::APSInt& integer) -> ConstValue {
            if (cld::get<llvm::APSInt>(rhs.getValue()) == 0)
            {
                if (issues)
                {
                    *issues = IntDivByZero;
                }
                return {};
            }
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.sdiv_ov(cld::get<llvm::APSInt>(rhs.getValue()), overflow) :
                                               integer.udiv(cld::get<llvm::APSInt>(rhs.getValue()));
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssue;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned())};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::modulo(const cld::Semantics::ConstValue& rhs,
                                                              const LanguageOptions&) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; }, [](VoidStar) -> ConstValue { CLD_UNREACHABLE; },
        [](AddressConstant) -> ConstValue { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstValue { CLD_UNREACHABLE; },
        [&rhs](const llvm::APSInt& integer) -> ConstValue {
            auto apsInt = integer.isSigned() ? integer.srem(cld::get<llvm::APSInt>(rhs.getValue())) :
                                               integer.urem(cld::get<llvm::APSInt>(rhs.getValue()));
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned())};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::plus(const cld::Semantics::ConstValue& rhs,
                                                            const LanguageOptions&, Issue* issues) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; },
        [this](AddressConstant) -> ConstValue { return *this; },
        [&rhs](VoidStar address) -> ConstValue {
            CLD_ASSERT(address.elementSize > 0);
            auto& integer = cld::get<llvm::APSInt>(rhs.getValue());
            if (integer.isUnsigned())
            {
                address.address += address.elementSize * integer.getZExtValue();
            }
            else
            {
                address.address += address.elementSize * integer.getSExtValue();
            }
            return {address};
        },
        [&rhs](const llvm::APFloat& floating) -> ConstValue {
            return {floating + cld::get<llvm::APFloat>(rhs.getValue())};
        },
        [&rhs, issues](const llvm::APSInt& integer) -> ConstValue {
            if (std::holds_alternative<VoidStar>(rhs.getValue()))
            {
                auto address = cld::get<VoidStar>(rhs.getValue());
                CLD_ASSERT(address.elementSize > 0);
                if (integer.isUnsigned())
                {
                    address.address += address.elementSize * integer.getZExtValue();
                }
                else
                {
                    address.address += address.elementSize * integer.getSExtValue();
                }
                return {address};
            }
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.sadd_ov(cld::get<llvm::APSInt>(rhs.getValue()), overflow) :
                                               integer.uadd_ov(cld::get<llvm::APSInt>(rhs.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssue;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned())};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::minus(const cld::Semantics::ConstValue& rhs,
                                                             const LanguageOptions& options, Issue* issues) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; },
        [this](AddressConstant) -> ConstValue { return *this; },
        [&rhs, &options](VoidStar address) -> ConstValue {
            CLD_ASSERT(address.elementSize > 0);
            if (std::holds_alternative<VoidStar>(rhs.getValue()))
            {
                return {llvm::APSInt(
                    llvm::APInt(
                        cld::get<PrimitiveType>(PrimitiveType::createPtrdiffT(false, false, options).getVariant())
                            .getBitCount(),
                        (address.address - cld::get<VoidStar>(rhs.getValue()).address) / address.elementSize, true),
                    false)};
            }
            if (!std::holds_alternative<llvm::APSInt>(rhs.getValue()))
            {
                CLD_UNREACHABLE;
            }
            auto& integer = cld::get<llvm::APSInt>(rhs.getValue());
            if (integer.isUnsigned())
            {
                address.address -= address.elementSize * integer.getZExtValue();
            }
            else
            {
                address.address -= address.elementSize * integer.getSExtValue();
            }
            return {address};
        },
        [&rhs](const llvm::APFloat& floating) -> ConstValue {
            return {floating - cld::get<llvm::APFloat>(rhs.getValue())};
        },
        [&rhs, issues](const llvm::APSInt& integer) -> ConstValue {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.ssub_ov(cld::get<llvm::APSInt>(rhs.getValue()), overflow) :
                                               integer.usub_ov(cld::get<llvm::APSInt>(rhs.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssue;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned())};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::shiftLeft(const cld::Semantics::ConstValue& rhs,
                                                                 const LanguageOptions&, Issue* issues) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; }, [](VoidStar) -> ConstValue { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstValue { CLD_UNREACHABLE; },
        [](AddressConstant) -> ConstValue { CLD_UNREACHABLE; },
        [&rhs, issues](const llvm::APSInt& integer) -> ConstValue {
            bool overflow = false;
            auto apsInt = integer.isSigned() ? integer.sshl_ov(cld::get<llvm::APSInt>(rhs.getValue()), overflow) :
                                               integer.ushl_ov(cld::get<llvm::APSInt>(rhs.getValue()), overflow);
            if (issues && integer.isSigned())
            {
                *issues = overflow ? NotRepresentable : NoIssue;
            }
            return {llvm::APSInt(std::move(apsInt), integer.isUnsigned())};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::shiftRight(const cld::Semantics::ConstValue& rhs,
                                                                  const LanguageOptions&, Issue* issues) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; }, [](VoidStar) -> ConstValue { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstValue { CLD_UNREACHABLE; },
        [](AddressConstant) -> ConstValue { CLD_UNREACHABLE; },
        [&rhs, issues](const llvm::APSInt& integer) -> ConstValue {
            auto rhsInteger = cld::get<llvm::APSInt>(rhs.getValue());
            if (issues && (rhsInteger.isSignBitSet() || rhsInteger.getZExtValue() >= integer.getBitWidth()))
            {
                *issues = NotRepresentable;
            }
            return {integer >> static_cast<unsigned>(rhsInteger.getZExtValue())};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::bitAnd(const cld::Semantics::ConstValue& rhs,
                                                              const LanguageOptions&) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; }, [](VoidStar) -> ConstValue { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstValue { CLD_UNREACHABLE; },
        [](AddressConstant) -> ConstValue { CLD_UNREACHABLE; },
        [&rhs](const llvm::APSInt& integer) -> ConstValue {
            return {integer & cld::get<llvm::APSInt>(rhs.getValue())};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::bitXor(const cld::Semantics::ConstValue& rhs,
                                                              const LanguageOptions&) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; }, [](VoidStar) -> ConstValue { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstValue { CLD_UNREACHABLE; },
        [](AddressConstant) -> ConstValue { CLD_UNREACHABLE; },
        [&rhs](const llvm::APSInt& integer) -> ConstValue {
            return {integer ^ cld::get<llvm::APSInt>(rhs.getValue())};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::bitOr(const cld::Semantics::ConstValue& rhs,
                                                             const LanguageOptions&) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; }, [](VoidStar) -> ConstValue { CLD_UNREACHABLE; },
        [](const llvm::APFloat&) -> ConstValue { CLD_UNREACHABLE; },
        [](AddressConstant) -> ConstValue { CLD_UNREACHABLE; },
        [&rhs](const llvm::APSInt& integer) -> ConstValue {
            return {integer | cld::get<llvm::APSInt>(rhs.getValue())};
        });
}

cld::Semantics::ConstValue::operator bool() const
{
    return match(
        m_value, [](VoidStar address) -> bool { return address.address != 0; },
        [](const llvm::APFloat& floating) -> bool { return floating.isNonZero(); },
        [](const llvm::APSInt& integer) -> bool { return !integer.isNullValue(); },
        [](std::monostate) -> bool { CLD_UNREACHABLE; }, [](AddressConstant) -> bool { return true; });
}

bool cld::Semantics::ConstValue::isUndefined() const
{
    return std::holds_alternative<std::monostate>(m_value);
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::toBool(const LanguageOptions&) const
{
    return {llvm::APSInt(llvm::APInt(1, static_cast<bool>(*this)), false)};
}

std::int64_t cld::Semantics::ConstValue::toInt() const
{
    return match(
        m_value, [](VoidStar pointer) -> std::int64_t { return pointer.address; },
        [](const llvm::APFloat& floating) -> std::int64_t {
            return static_cast<std::int64_t>(floating.convertToDouble());
        },
        [](const llvm::APSInt& integer) -> std::int64_t { return integer.getSExtValue(); },
        [](std::monostate) -> std::int64_t { CLD_UNREACHABLE; },
        [](AddressConstant) -> std::int64_t { CLD_UNREACHABLE; });
}

std::uint64_t cld::Semantics::ConstValue::toUInt() const
{
    return match(
        m_value, [](VoidStar pointer) -> std::uint64_t { return pointer.address; },
        [](const llvm::APFloat& floating) -> std::uint64_t {
            return static_cast<std::uint64_t>(floating.convertToDouble());
        },
        [](const llvm::APSInt& integer) -> std::uint64_t { return integer.getZExtValue(); },
        [](std::monostate) -> std::uint64_t { CLD_UNREACHABLE; },
        [](AddressConstant) -> std::uint64_t { CLD_UNREACHABLE; });
}

std::string cld::Semantics::ConstValue::toString() const
{
    return match(
        m_value, [](VoidStar pointer) -> std::string { return "0x" + llvm::utohexstr(pointer.address); },
        [](const llvm::APFloat& floating) -> std::string {
            llvm::SmallString<20> result;
            floating.toString(result);
            return std::string(result.begin(), result.end());
        },
        [](const llvm::APSInt& integer) -> std::string {
            llvm::SmallString<20> result;
            integer.toString(result);
            return std::string(result.begin(), result.end());
        },
        [](std::monostate) -> std::string { return "<undefined>"; },
        [](AddressConstant) -> std::string { return "<address>"; });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::lessThan(const cld::Semantics::ConstValue& rhs,
                                                                const LanguageOptions& options) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; },
        [this](AddressConstant) -> ConstValue { return *this; },
        [&rhs, &options](VoidStar address) -> ConstValue {
            return {llvm::APSInt(
                llvm::APInt(options.sizeOfInt * 8, address.address < cld::get<VoidStar>(rhs.getValue()).address),
                false)};
        },
        [&rhs, &options](const llvm::APFloat& floating) -> ConstValue {
            auto cmp = floating.compare(cld::get<llvm::APFloat>(rhs.getValue()));
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, cmp == llvm::APFloat::cmpLessThan), false)};
        },
        [&rhs, &options](const llvm::APSInt& integer) -> ConstValue {
            auto apsInt = integer < cld::get<llvm::APSInt>(rhs.getValue());
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, apsInt), false)};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::greaterThan(const cld::Semantics::ConstValue& rhs,
                                                                   const LanguageOptions& options) const
{
    return rhs.lessThan(*this, options);
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::lessOrEqual(const cld::Semantics::ConstValue& rhs,
                                                                   const LanguageOptions& options) const
{
    return rhs.lessThan(*this, options).logicalNegate(options);
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::greaterOrEqual(const cld::Semantics::ConstValue& rhs,
                                                                      const LanguageOptions& options) const
{
    return lessThan(rhs, options).logicalNegate(options);
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::equal(const cld::Semantics::ConstValue& rhs,
                                                             const LanguageOptions& options) const
{
    return match(
        m_value, [](std::monostate) -> ConstValue { CLD_UNREACHABLE; },
        [this](AddressConstant) -> ConstValue { return *this; },
        [&rhs, &options](VoidStar address) -> ConstValue {
            if (std::holds_alternative<llvm::APSInt>(rhs.getValue()))
            {
                CLD_ASSERT(cld::get<llvm::APSInt>(rhs.getValue()) == 0);
                return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, address.address == 0), false)};
            }
            return {llvm::APSInt(
                llvm::APInt(options.sizeOfInt * 8, address.address == cld::get<VoidStar>(rhs.getValue()).address, true),
                false)};
        },
        [&rhs, &options](const llvm::APFloat& floating) -> ConstValue {
            return {llvm::APSInt(
                llvm::APInt(options.sizeOfInt * 8,
                            floating.compare(cld::get<llvm::APFloat>(rhs.getValue())) == llvm::APFloat::cmpEqual),
                false)};
        },
        [&rhs, &options](const llvm::APSInt& integer) -> ConstValue {
            if (auto* address = std::get_if<VoidStar>(&rhs.getValue()))
            {
                CLD_ASSERT(integer == 0);
                return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, address->address == 0), false)};
            }
            return {llvm::APSInt(llvm::APInt(options.sizeOfInt * 8, integer == cld::get<llvm::APSInt>(rhs.getValue())),
                                 false)};
        });
}

cld::Semantics::ConstValue cld::Semantics::ConstValue::notEqual(const cld::Semantics::ConstValue& rhs,
                                                                const LanguageOptions& options) const
{
    return equal(rhs, options).logicalNegate(options);
}

std::string cld::diag::StringConverter<cld::Semantics::ConstValue>::inFormat(const Semantics::ConstValue& arg,
                                                                             const SourceInterface*)
{
    return '\'' + arg.toString() + '\'';
}

std::string cld::diag::StringConverter<cld::Semantics::ConstValue>::inArg(const Semantics::ConstValue& arg,
                                                                          const SourceInterface*)
{
    return arg.toString();
}
