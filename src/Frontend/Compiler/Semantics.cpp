#include "Semantics.hpp"

#include <llvm/ADT/StringExtras.h>

#include <algorithm>
#include <optional>
#include <utility>

#include "ErrorMessages.hpp"
#include "SemanticAnalysis.hpp"
#include "SemanticUtil.hpp"
#include "Syntax.hpp"

cld::Semantics::Type cld::Semantics::PrimitiveType::createPtrdiffT(bool isConst, bool isVolatile,
                                                                   const LanguageOptions& options)
{
    switch (options.ptrdiffType)
    {
        case LanguageOptions::PtrdiffType ::Int: return PrimitiveType::createInt(isConst, isVolatile, options);
        case LanguageOptions::PtrdiffType ::Long: return PrimitiveType::createLong(isConst, isVolatile, options);
        case LanguageOptions::PtrdiffType ::LongLong: return PrimitiveType::createLongLong(isConst, isVolatile);
    }
    CLD_UNREACHABLE;
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createWcharT(bool isConst, bool isVolatile,
                                                                 const LanguageOptions& options)
{
    switch (options.wcharUnderlyingType)
    {
        case LanguageOptions::WideCharType ::Int: return PrimitiveType::createInt(isConst, isVolatile, options);
        case LanguageOptions::WideCharType ::UnsignedShort:
            return PrimitiveType::createUnsignedShort(isConst, isVolatile, options);
    }
    CLD_UNREACHABLE;
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createSizeT(bool isConst, bool isVolatile,
                                                                const LanguageOptions& options)
{
    switch (options.sizeTType)
    {
        case LanguageOptions::SizeTType ::UnsignedInt:
            return PrimitiveType::createUnsignedInt(isConst, isVolatile, options);
        case LanguageOptions::SizeTType ::UnsignedLong:
            return PrimitiveType::createUnsignedLong(isConst, isVolatile, options);
        case LanguageOptions::SizeTType ::UnsignedLongLong:
            return PrimitiveType::createUnsignedLongLong(isConst, isVolatile);
    }
    CLD_UNREACHABLE;
}

cld::Semantics::ArrayType::ArrayType(bool isRestricted, bool isStatic, std::shared_ptr<cld::Semantics::Type>&& type,
                                     std::size_t size)
    : m_type(std::move(type)), m_size(size), m_restricted(isRestricted), m_static(isStatic)
{
}

cld::Semantics::Type cld::Semantics::ArrayType::create(bool isConst, bool isVolatile, bool isRestricted, bool isStatic,
                                                       cld::Semantics::Type type, std::size_t size)
{
    return cld::Semantics::Type(isConst, isVolatile,
                                ArrayType(isRestricted, isStatic, std::make_shared<Type>(std::move(type)), size));
}

bool cld::Semantics::ArrayType::operator==(const cld::Semantics::ArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type, m_size) == std::tie(rhs.m_restricted, *rhs.m_type, rhs.m_size);
}

bool cld::Semantics::ArrayType::operator!=(const cld::Semantics::ArrayType& rhs) const
{
    return !(rhs == *this);
}

std::size_t cld::Semantics::ArrayType::getSizeOf(const SemanticAnalysis& analysis) const
{
    return m_type->getSizeOf(analysis) * m_size;
}

std::size_t cld::Semantics::ArrayType::getAlignOf(const SemanticAnalysis& analysis) const
{
    return m_type->getSizeOf(analysis);
}

std::size_t cld::Semantics::AbstractArrayType::getAlignOf(const SemanticAnalysis& analysis) const
{
    return m_type->getAlignOf(analysis);
}

std::size_t cld::Semantics::ValArrayType::getAlignOf(const SemanticAnalysis& analysis) const
{
    return m_type->getAlignOf(analysis);
}

bool cld::Semantics::Type::operator==(const cld::Semantics::Type& rhs) const
{
    if (std::tie(m_isConst, m_isVolatile) != std::tie(rhs.m_isConst, rhs.m_isVolatile))
    {
        return false;
    }
    if (m_type.index() != rhs.m_type.index())
    {
        return false;
    }
    return cld::match(m_type, [&](auto&& value) -> bool {
        using T = std::decay_t<decltype(value)>;
        return value == cld::get<T>(rhs.m_type);
    });
}

bool cld::Semantics::Type::operator!=(const cld::Semantics::Type& rhs) const
{
    return !(rhs == *this);
}

std::size_t cld::Semantics::Type::getSizeOf(const SemanticAnalysis& analysis) const
{
    return cld::match(m_type, [&](auto&& value) -> std::size_t {
        if constexpr (std::is_same_v<std::monostate, std::decay_t<decltype(value)>>)
        {
            CLD_UNREACHABLE;
        }
        else
        {
            return value.getSizeOf(analysis);
        }
    });
}

std::size_t cld::Semantics::Type::getAlignOf(const SemanticAnalysis& analysis) const
{
    return cld::match(m_type, [&](auto&& value) -> std::size_t {
        if constexpr (std::is_same_v<std::monostate, std::decay_t<decltype(value)>>)
        {
            CLD_UNREACHABLE;
        }
        else
        {
            return value.getAlignOf(analysis);
        }
    });
}

cld::Semantics::PointerType::PointerType(bool isRestricted, std::shared_ptr<cld::Semantics::Type>&& elementType)
    : m_elementType(std::move(elementType)), m_restricted(isRestricted)
{
}

cld::Semantics::Type cld::Semantics::PointerType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                         cld::Semantics::Type elementType)
{
    return cld::Semantics::Type(isConst, isVolatile,
                                PointerType(isRestricted, std::make_shared<Type>(std::move(elementType))));
}

bool cld::Semantics::PointerType::operator==(const cld::Semantics::PointerType& rhs) const
{
    return std::tie(m_restricted, *m_elementType) == std::tie(rhs.m_restricted, *rhs.m_elementType);
}

bool cld::Semantics::PointerType::operator!=(const cld::Semantics::PointerType& rhs) const
{
    return !(rhs == *this);
}

std::size_t cld::Semantics::PointerType::getSizeOf(const SemanticAnalysis& analysis) const
{
    return analysis.getLanguageOptions().sizeOfVoidStar;
}

std::size_t cld::Semantics::PointerType::getAlignOf(const SemanticAnalysis& analysis) const
{
    return analysis.getLanguageOptions().sizeOfVoidStar;
}

cld::Semantics::EnumType::EnumType(std::string_view name, std::uint64_t scopeOrId)
    : m_name(name), m_scopeOrId(scopeOrId)
{
}

cld::Semantics::Type cld::Semantics::EnumType::create(bool isConst, bool isVolatile, std::string_view name,
                                                      std::uint64_t scopeOrId)
{
    return cld::Semantics::Type(isConst, isVolatile, EnumType(name, scopeOrId));
}

std::size_t cld::Semantics::EnumType::getSizeOf(const SemanticAnalysis& analysis) const
{
    auto* def = analysis.getEnumDefinition(m_name, m_scopeOrId);
    CLD_ASSERT(def);
    return def->getType().getSizeOf(analysis);
}

std::size_t cld::Semantics::EnumType::getAlignOf(const SemanticAnalysis& analysis) const
{
    auto* def = analysis.getEnumDefinition(m_name, m_scopeOrId);
    CLD_ASSERT(def);
    return def->getType().getAlignOf(analysis);
}

cld::Semantics::PrimitiveType::PrimitiveType(bool isFloatingPoint, bool isSigned, std::uint8_t bitCount, Kind kind)
    : m_bitCount(bitCount), m_isFloatingPoint(isFloatingPoint), m_isSigned(isSigned), m_kind(kind)
{
}

std::uint8_t cld::Semantics::PrimitiveType::getByteCount() const
{
    auto value = m_bitCount;
    auto remainder = value % 8;
    if (remainder)
    {
        value += 8 - remainder;
    }
    std::uint8_t temp = (value / 8) - 1;
    temp |= temp >> 1;
    temp |= temp >> 2;
    temp |= temp >> 4;
    return (temp + 1);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::create(bool isConst, bool isVolatile, bool isFloatingPoint,
                                                           bool isSigned, std::uint8_t bitCount, Kind kind)
{
    return cld::Semantics::Type(isConst, isVolatile, PrimitiveType(isFloatingPoint, isSigned, bitCount, kind));
}

bool cld::Semantics::PrimitiveType::operator==(const cld::Semantics::PrimitiveType& rhs) const
{
    if (m_bitCount == 0 && rhs.m_bitCount == 0)
    {
        return true;
    }
    return std::tie(m_isFloatingPoint, m_isSigned, m_bitCount, m_kind)
           == std::tie(rhs.m_isFloatingPoint, rhs.m_isSigned, rhs.m_bitCount, rhs.m_kind);
}

bool cld::Semantics::PrimitiveType::operator!=(const cld::Semantics::PrimitiveType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createChar(bool isConst, bool isVolatile,
                                                               const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, options.charIsSigned, 8, Kind::Char);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createSignedChar(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 8, Kind::SignedChar);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedChar(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 8, Kind::UnsignedChar);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnderlineBool(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 1, Kind::Bool);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createShort(bool isConst, bool isVolatile,
                                                                const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.sizeOfShort * 8, Kind::Short);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedShort(bool isConst, bool isVolatile,
                                                                        const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.sizeOfShort * 8, Kind::UnsignedShort);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createInt(bool isConst, bool isVolatile,
                                                              const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.sizeOfInt * 8, Kind::Int);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedInt(bool isConst, bool isVolatile,
                                                                      const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.sizeOfInt * 8, Kind::UnsignedInt);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createLong(bool isConst, bool isVolatile,
                                                               const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, true, options.sizeOfLong * 8, Kind::Long);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedLong(bool isConst, bool isVolatile,
                                                                       const LanguageOptions& options)
{
    return create(isConst, isVolatile, false, false, options.sizeOfLong * 8, Kind::UnsignedLong);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createLongLong(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 64, Kind::LongLong);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createUnsignedLongLong(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, false, 64, Kind::UnsignedLongLong);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createFloat(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, true, true, 32, Kind::Float);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createDouble(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, true, true, 64, Kind::Double);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createLongDouble(bool isConst, bool isVolatile,
                                                                     const LanguageOptions& options)
{
    return create(isConst, isVolatile, true, true, options.sizeOfLongDoubleBits, Kind::LongDouble);
}

cld::Semantics::Type cld::Semantics::PrimitiveType::createVoid(bool isConst, bool isVolatile)
{
    return create(isConst, isVolatile, false, true, 0, Kind::Void);
}

cld::Semantics::ValArrayType::ValArrayType(bool isRestricted, bool isStatic,
                                           std::shared_ptr<cld::Semantics::Type>&& type)
    : m_type(std::move(type)), m_restricted(isRestricted), m_static(isStatic)
{
}

cld::Semantics::Type cld::Semantics::ValArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                          bool isStatic, cld::Semantics::Type type)
{
    return cld::Semantics::Type(isConst, isVolatile,
                                ValArrayType(isRestricted, isStatic, std::make_shared<Type>(std::move(type))));
}

bool cld::Semantics::ValArrayType::operator==(const cld::Semantics::ValArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type) == std::tie(rhs.m_restricted, *rhs.m_type);
}

bool cld::Semantics::ValArrayType::operator!=(const cld::Semantics::ValArrayType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::Type cld::Semantics::FunctionType::create(cld::Semantics::Type returnType,
                                                          std::vector<std::pair<Type, std::string_view>>&& arguments,
                                                          bool lastIsVararg, bool isKandR)
{
    return cld::Semantics::Type(
        false, false,
        FunctionType(std::make_shared<const Type>(std::move(returnType)), std::move(arguments), lastIsVararg, isKandR));
}

bool cld::Semantics::FunctionType::operator==(const cld::Semantics::FunctionType& rhs) const
{
    if (*m_returnType != *rhs.m_returnType)
    {
        return false;
    }
    if (!std::equal(m_arguments.begin(), m_arguments.end(), rhs.m_arguments.begin(), rhs.m_arguments.end(),
                    [](const auto& lhs, const auto& rhs) { return lhs.first == rhs.first; }))
    {
        return false;
    }
    return m_lastIsVararg == rhs.m_lastIsVararg;
}

bool cld::Semantics::FunctionType::operator!=(const cld::Semantics::FunctionType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::AbstractArrayType::AbstractArrayType(bool isRestricted, std::shared_ptr<cld::Semantics::Type>&& type)
    : m_type(std::move(type)), m_restricted(isRestricted)
{
}

cld::Semantics::Type cld::Semantics::AbstractArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                               cld::Semantics::Type type)
{
    return cld::Semantics::Type(isConst, isVolatile,
                                AbstractArrayType(isRestricted, std::make_shared<Type>(std::move(type))));
}

bool cld::Semantics::AbstractArrayType::operator==(const cld::Semantics::AbstractArrayType& rhs) const
{
    return std::tie(m_restricted, *m_type) == std::tie(rhs.m_restricted, *rhs.m_type);
}

bool cld::Semantics::AbstractArrayType::operator!=(const cld::Semantics::AbstractArrayType& rhs) const
{
    return !(rhs == *this);
}

cld::Lexer::CTokenIterator cld::Semantics::declaratorToLoc(const cld::Syntax::Declarator& declarator)
{
    return matchWithSelf(
        declarator.getDirectDeclarator(),
        [](auto&&, const Syntax::DirectDeclaratorIdentifier& name) -> cld::Lexer::CTokenIterator {
            return name.getIdentifierLoc();
        },
        [](auto&& self, const Syntax::DirectDeclaratorParentheses& declarator) -> cld::Lexer::CTokenIterator {
            return cld::match(declarator.getDeclarator().getDirectDeclarator(),
                              [&self](auto&& value) -> cld::Lexer::CTokenIterator { return self(value); });
        },
        [](auto&& self, auto&& value) -> cld::Lexer::CTokenIterator {
            return cld::match(value.getDirectDeclarator(),
                              [&self](auto&& value) -> cld::Lexer::CTokenIterator { return self(value); });
        });
}

cld::Semantics::StructType::StructType(std::string_view name, int64_t scope) : m_name(name), m_scopeOrId(scope) {
}

cld::Semantics::Type cld::Semantics::StructType::create(bool isConst, bool isVolatile, std::string_view name,
                                                        int64_t scope)
{
    return cld::Semantics::Type(isConst, isVolatile, StructType(name, scope));
}

std::size_t cld::Semantics::StructType::getSizeOf(const SemanticAnalysis& analysis) const
{
    auto* def = analysis.getStructDefinition(m_name, m_scopeOrId);
    CLD_ASSERT(def);
    return def->getSizeOf();
}

std::size_t cld::Semantics::StructType::getAlignOf(const SemanticAnalysis& analysis) const
{
    auto* def = analysis.getStructDefinition(m_name, m_scopeOrId);
    CLD_ASSERT(def);
    return def->getAlignOf();
}

cld::Semantics::UnionType::UnionType(std::string_view name, std::uint64_t scopeOrId)
    : m_name(name), m_scopeOrId(scopeOrId)
{
}

cld::Semantics::Type cld::Semantics::UnionType::create(bool isConst, bool isVolatile, std::string_view name,
                                                       std::uint64_t scopeOrId)
{
    return cld::Semantics::Type(isConst, isVolatile, UnionType(name, scopeOrId));
}

std::size_t cld::Semantics::UnionType::getSizeOf(const SemanticAnalysis& analysis) const
{
    auto* def = analysis.getUnionDefinition(m_name, m_scopeOrId);
    CLD_ASSERT(def);
    return def->getSizeOf();
}

std::size_t cld::Semantics::UnionType::getAlignOf(const SemanticAnalysis& analysis) const
{
    auto* def = analysis.getUnionDefinition(m_name, m_scopeOrId);
    CLD_ASSERT(def);
    return def->getAlignOf();
}

bool cld::Semantics::isVoid(const cld::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.get());
    if (!primitive || type.isConst() || type.isVolatile())
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Void;
}

bool cld::Semantics::isArray(const cld::Semantics::Type& type)
{
    return std::holds_alternative<ArrayType>(type.get()) || std::holds_alternative<ValArrayType>(type.get())
           || std::holds_alternative<AbstractArrayType>(type.get());
}

bool cld::Semantics::isInteger(const cld::Semantics::Type& type)
{
    return std::holds_alternative<PrimitiveType>(type.get()) && !cld::get<PrimitiveType>(type.get()).isFloatingPoint()
           && cld::get<PrimitiveType>(type.get()).getBitCount() != 0;
}

bool cld::Semantics::isArithmetic(const cld::Semantics::Type& type)
{
    return std::holds_alternative<PrimitiveType>(type.get()) && cld::get<PrimitiveType>(type.get()).getBitCount() != 0;
}

bool cld::Semantics::isScalar(const cld::Semantics::Type& type)
{
    return isArithmetic(type) || std::holds_alternative<PointerType>(type.get());
}

bool cld::Semantics::isRecord(const cld::Semantics::Type& type)
{
    return std::holds_alternative<StructType>(type.get()) || std::holds_alternative<UnionType>(type.get())
           || std::holds_alternative<AnonymousStructType>(type.get())
           || std::holds_alternative<AnonymousUnionType>(type.get());
}

bool cld::Semantics::isBool(const cld::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.get());
    if (!primitive)
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Bool;
}

bool cld::Semantics::isCharType(const cld::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.get());
    if (!primitive)
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Char
           || primitive->getKind() == PrimitiveType::Kind::UnsignedChar
           || primitive->getKind() == PrimitiveType::Kind::SignedChar;
}

cld::Semantics::TranslationUnit::TranslationUnit(std::vector<TranslationUnit::Variant> globals)
    : m_globals(std::move(globals))
{
}

bool cld::Semantics::StructDefinition::operator==(const cld::Semantics::StructDefinition& rhs) const
{
    return std::tie(m_name, m_fields) == std::tie(rhs.m_name, rhs.m_fields);
}

bool cld::Semantics::StructDefinition::operator!=(const cld::Semantics::StructDefinition& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::UnionDefinition::operator==(const cld::Semantics::UnionDefinition& rhs) const
{
    return std::tie(m_name, m_fields) == std::tie(rhs.m_name, rhs.m_fields);
}

bool cld::Semantics::UnionDefinition::operator!=(const cld::Semantics::UnionDefinition& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::AnonymousStructType::operator==(const cld::Semantics::AnonymousStructType& rhs) const
{
    return m_id == rhs.m_id;
}

bool cld::Semantics::AnonymousStructType::operator!=(const cld::Semantics::AnonymousStructType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::Type cld::Semantics::AnonymousStructType::create(bool isConst, bool isVolatile, std::uint64_t id,
                                                                 std::vector<Field> fields, std::uint32_t sizeOf,
                                                                 std::uint32_t alignOf)
{
    return cld::Semantics::Type(isConst, isVolatile, AnonymousStructType(id, std::move(fields), sizeOf, alignOf));
}

std::size_t cld::Semantics::AnonymousStructType::getSizeOf(const SemanticAnalysis&) const
{
    return m_sizeOf;
}

std::size_t cld::Semantics::AnonymousStructType::getAlignOf(const SemanticAnalysis&) const
{
    return m_alignOf;
}

cld::Semantics::AnonymousUnionType::AnonymousUnionType(std::uint64_t id, std::vector<Field>&& fields,
                                                       std::uint64_t sizeOf, std::uint64_t alignOf)
    : m_id(id), m_fields(std::move(fields)), m_sizeOf(sizeOf), m_alignOf(alignOf)
{
}

bool cld::Semantics::AnonymousUnionType::operator==(const cld::Semantics::AnonymousUnionType& rhs) const
{
    return m_id == rhs.m_id;
}

bool cld::Semantics::AnonymousUnionType::operator!=(const cld::Semantics::AnonymousUnionType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::Type cld::Semantics::AnonymousUnionType::create(bool isConst, bool isVolatile, std::uint64_t id,
                                                                std::vector<Field> fields, std::uint64_t sizeOf,
                                                                std::uint64_t alignOf)
{
    return cld::Semantics::Type(isConst, isVolatile, AnonymousUnionType(id, std::move(fields), sizeOf, alignOf));
}

std::size_t cld::Semantics::AnonymousUnionType::getSizeOf(const SemanticAnalysis&) const
{
    return m_sizeOf;
}

std::size_t cld::Semantics::AnonymousUnionType::getAlignOf(const SemanticAnalysis&) const
{
    return m_alignOf;
}

cld::Semantics::AnonymousEnumType::AnonymousEnumType(std::uint64_t id, std::shared_ptr<const Type> type)
    : m_type(std::move(type)), m_id(id)
{
}

bool cld::Semantics::AnonymousEnumType::operator==(const cld::Semantics::AnonymousEnumType& rhs) const
{
    return m_id == rhs.m_id;
}

bool cld::Semantics::AnonymousEnumType::operator!=(const cld::Semantics::AnonymousEnumType& rhs) const
{
    return !(rhs == *this);
}

cld::Semantics::Type cld::Semantics::AnonymousEnumType::create(bool isConst, bool isVolatile, std::uint64_t id,
                                                               Type type)
{
    return Type(isConst, isVolatile, AnonymousEnumType(id, std::make_shared<Type>(std::move(type))));
}

std::size_t cld::Semantics::AnonymousEnumType::getSizeOf(const SemanticAnalysis& analysis) const
{
    return m_type->getSizeOf(analysis);
}

std::size_t cld::Semantics::AnonymousEnumType::getAlignOf(const SemanticAnalysis& analysis) const
{
    return m_type->getAlignOf(analysis);
}

bool cld::Semantics::Field::operator==(const cld::Semantics::Field& rhs) const
{
    return std::tie(type, name, bitFieldSize) == std::tie(rhs.type, rhs.name, rhs.bitFieldSize);
}

bool cld::Semantics::Field::operator!=(const cld::Semantics::Field& rhs) const
{
    return !(rhs == *this);
}

namespace
{
std::string typeToString(const cld::Semantics::Type& arg)
{
    using namespace cld::Semantics;
    if (arg.isTypedef())
    {
        return cld::to_string(arg.getName());
    }
    std::string qualifiersAndSpecifiers;
    std::string declarators;
    std::optional<Type> maybeCurr = arg;
    while (maybeCurr)
    {
        maybeCurr = cld::match(
            maybeCurr->get(),
            [&](const PrimitiveType& primitiveType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                switch (primitiveType.getKind())
                {
                    case PrimitiveType::Kind::Char: qualifiersAndSpecifiers += "char"; break;
                    case PrimitiveType::Kind::SignedChar: qualifiersAndSpecifiers += "signed char"; break;
                    case PrimitiveType::Kind::UnsignedChar: qualifiersAndSpecifiers += "unsigned char"; break;
                    case PrimitiveType::Kind::Bool: qualifiersAndSpecifiers += "_Bool"; break;
                    case PrimitiveType::Kind::Short: qualifiersAndSpecifiers += "short"; break;
                    case PrimitiveType::Kind::UnsignedShort: qualifiersAndSpecifiers += "unsigned short"; break;
                    case PrimitiveType::Kind::Int: qualifiersAndSpecifiers += "int"; break;
                    case PrimitiveType::Kind::UnsignedInt: qualifiersAndSpecifiers += "unsigned int"; break;
                    case PrimitiveType::Kind::Long: qualifiersAndSpecifiers += "long"; break;
                    case PrimitiveType::Kind::UnsignedLong: qualifiersAndSpecifiers += "unsigned long"; break;
                    case PrimitiveType::Kind::LongLong: qualifiersAndSpecifiers += "long long"; break;
                    case PrimitiveType::Kind::UnsignedLongLong: qualifiersAndSpecifiers += "unsigned long long"; break;
                    case PrimitiveType::Kind::Float: qualifiersAndSpecifiers += "float"; break;
                    case PrimitiveType::Kind::Double: qualifiersAndSpecifiers += "double"; break;
                    case PrimitiveType::Kind::LongDouble: qualifiersAndSpecifiers += "long double"; break;
                    case PrimitiveType::Kind::Void: qualifiersAndSpecifiers += "void"; break;
                }
                return {};
            },
            [&](const PointerType&) -> std::optional<Type> {
                std::string temp;
                auto curr = *maybeCurr;
                while (auto* pointerType = std::get_if<PointerType>(&curr.get()))
                {
                    temp += "*";
                    if (pointerType->isRestricted())
                    {
                        temp += " restrict";
                    }
                    if (curr.isConst())
                    {
                        temp += " const";
                    }
                    if (maybeCurr->isVolatile())
                    {
                        temp += " volatile";
                    }
                    curr = pointerType->getElementType();
                }
                if (std::holds_alternative<AbstractArrayType>(curr.get())
                    || std::holds_alternative<ValArrayType>(curr.get()) || std::holds_alternative<ArrayType>(curr.get())
                    || std::holds_alternative<FunctionType>(curr.get()))
                {
                    declarators = "(" + temp + declarators + ")";
                    return {std::move(curr)};
                }
                else
                {
                    qualifiersAndSpecifiers = typeToString(curr) + temp;
                    return {};
                }
            },
            [&](const ValArrayType& valArrayType) -> std::optional<Type> {
                declarators += "[";
                if (valArrayType.isStatic())
                {
                    declarators += "static ";
                }
                if (valArrayType.isRestricted())
                {
                    declarators += "restrict ";
                }
                if (maybeCurr->isConst())
                {
                    declarators += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    declarators += "volatile ";
                }
                declarators += "*]";
                return valArrayType.getType();
            },
            [&](const ArrayType& arrayType) -> std::optional<Type> {
                declarators += "[";
                if (arrayType.isStatic())
                {
                    declarators += "static ";
                }
                if (arrayType.isRestricted())
                {
                    declarators += "restrict ";
                }
                if (maybeCurr->isConst())
                {
                    declarators += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    declarators += "volatile ";
                }
                declarators += cld::to_string(arrayType.getSize()) + "]";
                return arrayType.getType();
            },
            [&](const AbstractArrayType& arrayType) -> std::optional<Type> {
                declarators += "[";
                if (arrayType.isRestricted())
                {
                    declarators += "restrict ";
                }
                if (maybeCurr->isConst())
                {
                    declarators += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    declarators += "volatile ";
                }
                declarators += "]";
                return arrayType.getType();
            },
            [&](const FunctionType& functionType) -> std::optional<Type> {
                declarators += "(";
                if (functionType.getArguments().empty())
                {
                    if (!functionType.isKandR())
                    {
                        declarators += "void";
                    }
                    declarators += ")";
                    return functionType.getReturnType();
                }
                declarators += typeToString(functionType.getArguments()[0].first);
                for (auto& iter : llvm::ArrayRef(functionType.getArguments()).drop_front())
                {
                    declarators += ", " + typeToString(iter.first);
                }
                if (functionType.isLastVararg())
                {
                    declarators += ",...";
                }
                declarators += ")";
                return functionType.getReturnType();
            },
            [&](const StructType& structType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "struct " + cld::to_string(structType.getName());
                return {};
            },
            [&](const UnionType& unionType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "union " + cld::to_string(unionType.getName());
                return {};
            },
            [&](const EnumType& enumType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "struct " + cld::to_string(enumType.getName());
                return {};
            },
            [&](const AnonymousStructType& structType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "struct <anonymous 0x" + llvm::utohexstr(structType.getId()) + ">";
                return {};
            },
            [&](const AnonymousUnionType& unionType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "union <anonymous 0x" + llvm::utohexstr(unionType.getId()) + ">";
                return {};
            },
            [&](const AnonymousEnumType& enumType) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "enum <anonymous 0x" + llvm::utohexstr(enumType.getId()) + ">";
                return {};
            },
            [&](std::monostate) -> std::optional<Type> {
                if (maybeCurr->isConst())
                {
                    qualifiersAndSpecifiers += "const ";
                }
                if (maybeCurr->isVolatile())
                {
                    qualifiersAndSpecifiers += "volatile ";
                }
                qualifiersAndSpecifiers += "<undefined>";
                return {};
            });
    }
    return qualifiersAndSpecifiers + declarators;
}
} // namespace

std::string cld::diag::StringConverter<cld::Semantics::Type>::inArg(const Semantics::Type& arg, const SourceInterface&)
{
    return typeToString(arg);
}

std::string cld::diag::CustomFormat<U'f', U'u', U'l', U'l'>::operator()(const Semantics::Type& arg)
{
    auto result = typeToString(arg);
    if (arg.isTypedef())
    {
        return "'" + cld::to_string(arg.getName()) + "' (aka '" + result + "')";
    }
    return "'" + result + "'";
}

std::string cld::diag::CustomFormat<U't', U'y', U'p', U'e'>::operator()(const Semantics::Expression& arg)
{
    return typeToString(arg.getType());
}

std::string cld::diag::CustomFormat<U'f', U'u', U'l', U'l', U'T', U'y', U'p', U'e'>::operator()(
    const Semantics::Expression& arg)
{
    return CustomFormat<U'f', U'u', U'l', U'l'>{}(arg.getType());
}

cld::Lexer::CTokenIterator cld::Semantics::Conversion::begin() const
{
    return m_expression->begin();
}

cld::Lexer::CTokenIterator cld::Semantics::Conversion::end() const
{
    return m_expression->end();
}

cld::Lexer::CTokenIterator cld::Semantics::Cast::end() const
{
    return m_expression->end();
}

cld::Lexer::CTokenIterator cld::Semantics::MemberAccess::begin() const
{
    return m_recordExpr->begin();
}

cld::Lexer::CTokenIterator cld::Semantics::BinaryOperator::begin() const
{
    return m_leftOperand->begin();
}

cld::Lexer::CTokenIterator cld::Semantics::BinaryOperator::end() const
{
    return m_rightOperand->end();
}

cld::Lexer::CTokenIterator cld::Semantics::Assignment::begin() const
{
    return m_leftOperand->begin();
}

cld::Lexer::CTokenIterator cld::Semantics::Assignment::end() const
{
    return m_rightOperand->end();
}

cld::Lexer::CTokenIterator cld::Semantics::UnaryOperator::begin() const
{
    switch (m_kind)
    {
        case PostDecrement:
        case PostIncrement: return m_operand->begin();
        default: return m_operatorToken;
    }
}

cld::Lexer::CTokenIterator cld::Semantics::UnaryOperator::end() const
{
    switch (m_kind)
    {
        case PostDecrement:
        case PostIncrement: return m_operatorToken + 1;
        default: return m_operand->end();
    }
}

cld::Lexer::CTokenIterator cld::Semantics::SizeofOperator::end() const
{
    return cld::match(
        m_variant, [](const std::unique_ptr<Expression>& expression) { return expression->end(); },
        [](const TypeVariant& typeVariant) { return typeVariant.closeParentheses + 1; });
}

cld::Lexer::CTokenIterator cld::Semantics::SubscriptOperator::begin() const
{
    return m_leftExpr->begin();
}

cld::Lexer::CTokenIterator cld::Semantics::Conditional::begin() const
{
    return m_boolExpression->begin();
}

cld::Lexer::CTokenIterator cld::Semantics::Conditional::end() const
{
    return m_falseExpression->end();
}

cld::Semantics::Expression::Expression(const cld::Syntax::Node& node) : Expression(node.begin(), node.end()) {}

cld::Lexer::CTokenIterator cld::Semantics::CommaExpression::begin() const
{
    return m_commaExpressions[0].first.begin();
}

cld::Lexer::CTokenIterator cld::Semantics::CommaExpression::end() const
{
    return m_lastExpression->end();
}

cld::Lexer::CTokenIterator cld::Semantics::CallExpression::begin() const
{
    return m_functionExpression->begin();
}

cld::Lexer::CTokenIterator cld::Semantics::CallExpression::end() const
{
    return m_closeParentheses + 1;
}
