#include "Semantics.hpp"

#include <llvm/ADT/StringExtras.h>

#include <algorithm>
#include <optional>
#include <utility>

#include "Program.hpp"
#include "SemanticAnalysis.hpp"
#include "SemanticUtil.hpp"
#include "Syntax.hpp"

cld::Semantics::Type cld::Semantics::PrimitiveType::createPtrdiffT(bool isConst, bool isVolatile,
                                                                   const LanguageOptions& options)
{
    MonadicStorage<Statement> statement(std::in_place_type<ExpressionStatement>, 0, std::nullopt);
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
    return std::tie(m_restricted, m_static, m_size, *m_type)
           == std::tie(rhs.m_restricted, rhs.m_static, rhs.m_size, *rhs.m_type);
}

bool cld::Semantics::ArrayType::operator!=(const cld::Semantics::ArrayType& rhs) const
{
    return !(rhs == *this);
}

std::size_t cld::Semantics::ArrayType::getSizeOf(const ProgramInterface& program) const
{
    return m_type->getSizeOf(program) * m_size;
}

std::size_t cld::Semantics::ArrayType::getAlignOf(const ProgramInterface& program) const
{
    return m_type->getAlignOf(program);
}

std::size_t cld::Semantics::AbstractArrayType::getAlignOf(const ProgramInterface& program) const
{
    return m_type->getAlignOf(program);
}

std::size_t cld::Semantics::ValArrayType::getAlignOf(const ProgramInterface& program) const
{
    return m_type->getAlignOf(program);
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

std::size_t cld::Semantics::Type::getSizeOf(const ProgramInterface& program) const
{
    return cld::match(m_type, [&](auto&& value) -> std::size_t {
        if constexpr (std::is_same_v<std::monostate, std::decay_t<decltype(value)>>)
        {
            CLD_UNREACHABLE;
        }
        else
        {
            return value.getSizeOf(program);
        }
    });
}

std::size_t cld::Semantics::Type::getAlignOf(const ProgramInterface& program) const
{
    return cld::match(m_type, [&](auto&& value) -> std::size_t {
        if constexpr (std::is_same_v<std::monostate, std::decay_t<decltype(value)>>)
        {
            CLD_UNREACHABLE;
        }
        else
        {
            return value.getAlignOf(program);
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

std::size_t cld::Semantics::PointerType::getSizeOf(const ProgramInterface& program) const
{
    return program.getLanguageOptions().sizeOfVoidStar;
}

std::size_t cld::Semantics::PointerType::getAlignOf(const ProgramInterface& program) const
{
    return program.getLanguageOptions().sizeOfVoidStar;
}

cld::Semantics::EnumType::EnumType(std::string_view name, std::uint64_t id) : m_name(name), m_id(id) {}

cld::Semantics::Type cld::Semantics::EnumType::create(bool isConst, bool isVolatile, std::string_view name,
                                                      std::uint64_t id)
{
    return cld::Semantics::Type(isConst, isVolatile, EnumType(name, id));
}

std::size_t cld::Semantics::EnumType::getSizeOf(const ProgramInterface& program) const
{
    auto* def = program.getEnumDefinition(m_id);
    CLD_ASSERT(def);
    return def->getType().getSizeOf(program);
}

std::size_t cld::Semantics::EnumType::getAlignOf(const ProgramInterface& program) const
{
    auto* def = program.getEnumDefinition(m_id);
    CLD_ASSERT(def);
    return def->getType().getAlignOf(program);
}

cld::Semantics::PrimitiveType::PrimitiveType(bool isFloatingPoint, bool isSigned, std::uint8_t bitCount, Kind kind)
    : m_bitCount(bitCount), m_isFloatingPoint(isFloatingPoint), m_isSigned(isSigned), m_kind(kind)
{
}

std::uint8_t cld::Semantics::PrimitiveType::getByteCount() const
{
    auto value = roundUpTo(m_bitCount, 8);
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
                                           std::shared_ptr<cld::Semantics::Type>&& type,
                                           std::shared_ptr<const ExpressionBase>&& expression)
    : m_type(std::move(type)), m_restricted(isRestricted), m_static(isStatic), m_expression(std::move(expression))
{
}

cld::Semantics::Type cld::Semantics::ValArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                                                          bool isStatic, cld::Semantics::Type type,
                                                          std::shared_ptr<const ExpressionBase> expression)
{
    return cld::Semantics::Type(
        isConst, isVolatile,
        ValArrayType(isRestricted, isStatic, std::make_shared<Type>(std::move(type)), std::move(expression)));
}

bool cld::Semantics::ValArrayType::operator==(const cld::Semantics::ValArrayType& rhs) const
{
    return std::tie(m_restricted, m_static, m_expression, *m_type)
           == std::tie(rhs.m_restricted, rhs.m_static, rhs.m_expression, *rhs.m_type);
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
    if (m_lastIsVararg != rhs.m_lastIsVararg || m_isKandR != rhs.m_isKandR)
    {
        return false;
    }
    if (*m_returnType != *rhs.m_returnType)
    {
        return false;
    }
    return std::equal(m_arguments.begin(), m_arguments.end(), rhs.m_arguments.begin(), rhs.m_arguments.end(),
                      [](const auto& lhs, const auto& rhs) { return lhs.first == rhs.first; });
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

cld::Semantics::StructType::StructType(std::string_view name, int64_t id) : m_name(name), m_id(id) {}

cld::Semantics::Type cld::Semantics::StructType::create(bool isConst, bool isVolatile, std::string_view name,
                                                        int64_t id)
{
    return cld::Semantics::Type(isConst, isVolatile, StructType(name, id));
}

std::size_t cld::Semantics::StructType::getSizeOf(const ProgramInterface& program) const
{
    auto* def = program.getStructDefinition(m_id);
    CLD_ASSERT(def);
    return def->getSizeOf();
}

std::size_t cld::Semantics::StructType::getAlignOf(const ProgramInterface& program) const
{
    auto* def = program.getStructDefinition(m_id);
    CLD_ASSERT(def);
    return def->getAlignOf();
}

cld::Semantics::UnionType::UnionType(std::string_view name, std::uint64_t id) : m_name(name), m_id(id) {}

cld::Semantics::Type cld::Semantics::UnionType::create(bool isConst, bool isVolatile, std::string_view name,
                                                       std::uint64_t id)
{
    return cld::Semantics::Type(isConst, isVolatile, UnionType(name, id));
}

std::size_t cld::Semantics::UnionType::getSizeOf(const ProgramInterface& program) const
{
    auto* def = program.getUnionDefinition(m_id);
    CLD_ASSERT(def);
    return def->getSizeOf();
}

std::size_t cld::Semantics::UnionType::getAlignOf(const ProgramInterface& program) const
{
    auto* def = program.getUnionDefinition(m_id);
    CLD_ASSERT(def);
    return def->getAlignOf();
}

bool cld::Semantics::isStringLiteralExpr(const ExpressionBase& expression)
{
    auto* constant = expression.get_if<Constant>();
    if (!constant)
    {
        return false;
    }
    return std::holds_alternative<std::string>(constant->getValue())
           || std::holds_alternative<Lexer::NonCharString>(constant->getValue());
}

bool cld::Semantics::isVoid(const cld::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.getVariant());
    if (!primitive)
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Void;
}

bool cld::Semantics::isArray(const Type& type)
{
    return std::holds_alternative<ArrayType>(type.getVariant())
           || std::holds_alternative<ValArrayType>(type.getVariant())
           || std::holds_alternative<AbstractArrayType>(type.getVariant());
}

bool cld::Semantics::isCharArray(const Type& type, const LanguageOptions& options)
{
    if (!isArray(type))
    {
        return false;
    }
    return isCharacterLikeType(getArrayElementType(type), options);
}

const cld::Semantics::Type& cld::Semantics::getArrayElementType(const Type& type)
{
    return cld::match(
        type.getVariant(), [](const auto&) -> const Type& { CLD_UNREACHABLE; },
        [](const ArrayType& arrayType) -> const Type& { return arrayType.getType(); },
        [](const AbstractArrayType& abstractArrayType) -> const Type& { return abstractArrayType.getType(); },
        [](const ValArrayType& arrayType) -> const Type& { return arrayType.getType(); });
}

bool cld::Semantics::isInteger(const Type& type)
{
    return std::holds_alternative<PrimitiveType>(type.getVariant())
           && !cld::get<PrimitiveType>(type.getVariant()).isFloatingPoint()
           && cld::get<PrimitiveType>(type.getVariant()).getBitCount() != 0;
}

bool cld::Semantics::isArithmetic(const Type& type)
{
    return (std::holds_alternative<PrimitiveType>(type.getVariant())
            && cld::get<PrimitiveType>(type.getVariant()).getBitCount() != 0)
           || std::holds_alternative<EnumType>(type.getVariant());
}

bool cld::Semantics::isScalar(const Type& type)
{
    return isArithmetic(type) || std::holds_alternative<PointerType>(type.getVariant());
}

bool cld::Semantics::isRecord(const cld::Semantics::Type& type)
{
    return isUnion(type) || isStruct(type);
}

bool cld::Semantics::isStruct(const cld::Semantics::Type& type)
{
    return std::holds_alternative<StructType>(type.getVariant());
}

bool cld::Semantics::isUnion(const cld::Semantics::Type& type)
{
    return std::holds_alternative<UnionType>(type.getVariant());
}

bool cld::Semantics::isAnonymous(const Type& type)
{
    return (std::holds_alternative<EnumType>(type.getVariant()) && cld::get<EnumType>(type.getVariant()).isAnonymous())
           || (std::holds_alternative<StructType>(type.getVariant())
               && cld::get<StructType>(type.getVariant()).isAnonymous())
           || (std::holds_alternative<UnionType>(type.getVariant())
               && cld::get<UnionType>(type.getVariant()).isAnonymous());
}

bool cld::Semantics::isEnum(const Type& type)
{
    return std::holds_alternative<EnumType>(type.getVariant());
}

bool cld::Semantics::isBool(const cld::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.getVariant());
    if (!primitive)
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Bool;
}

bool cld::Semantics::isCharType(const cld::Semantics::Type& type)
{
    auto* primitive = std::get_if<PrimitiveType>(&type.getVariant());
    if (!primitive)
    {
        return false;
    }
    return primitive->getKind() == PrimitiveType::Kind::Char
           || primitive->getKind() == PrimitiveType::Kind::UnsignedChar
           || primitive->getKind() == PrimitiveType::Kind::SignedChar;
}

bool cld::Semantics::isCharacterLikeType(const Type& type, const LanguageOptions& options)
{
    if (isCharType(type))
    {
        return true;
    }
    return removeQualifiers(type) == PrimitiveType::createWcharT(false, false, options);
}

cld::Semantics::Type cld::Semantics::removeQualifiers(Type type)
{
    if (type.isConst() || type.isVolatile()
        || (std::holds_alternative<PointerType>(type.getVariant())
            && cld::get<PointerType>(type.getVariant()).isRestricted()))
    {
        if (!std::holds_alternative<PointerType>(type.getVariant())
            || !cld::get<PointerType>(type.getVariant()).isRestricted())
        {
            return Type(false, false, std::move(type).getVariant());
        }
        return PointerType::create(false, false, false,
                                   cld::get<cld::Semantics::PointerType>(type.getVariant()).getElementType());
    }
    return type;
}

bool cld::Semantics::isAggregate(const Type& type)
{
    return isRecord(type) || isArray(type);
}

bool cld::Semantics::isVariablyModified(const Type& type)
{
    auto typeVisitor = RecursiveVisitor(type, TYPE_NEXT_FN);
    return std::any_of(typeVisitor.begin(), typeVisitor.end(),
                       [](const Type& type) { return std::holds_alternative<ValArrayType>(type.getVariant()); });
}

bool cld::Semantics::isVariableLengthArray(const Type& type)
{
    auto typeVisitor = RecursiveVisitor(type, ARRAY_TYPE_NEXT_FN);
    return std::any_of(typeVisitor.begin(), typeVisitor.end(),
                       [](const Type& type) { return std::holds_alternative<ValArrayType>(type.getVariant()); });
}

cld::Semantics::TranslationUnit::TranslationUnit(std::vector<TranslationUnit::Variant> globals)
    : m_globals(std::move(globals))
{
}

namespace
{
std::string typeToString(const cld::Semantics::Type& arg)
{
    using namespace cld::Semantics;
    std::string qualifiersAndSpecifiers;
    std::string declarators;
    std::optional<Type> maybeCurr = arg;
    while (maybeCurr)
    {
        maybeCurr = cld::match(
            maybeCurr->getVariant(),
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
                while (auto* pointerType = std::get_if<PointerType>(&curr.getVariant()))
                {
                    if (pointerType->isRestricted())
                    {
                        if (temp.empty())
                        {
                            temp = "restrict";
                        }
                        else
                        {
                            temp = "restrict " + temp;
                        }
                    }
                    if (curr.isConst())
                    {
                        if (temp.empty())
                        {
                            temp = "const";
                        }
                        else
                        {
                            temp = "const " + temp;
                        }
                    }
                    if (curr.isVolatile())
                    {
                        if (temp.empty())
                        {
                            temp = "volatile";
                        }
                        else
                        {
                            temp = "volatile " + temp;
                        }
                    }
                    temp = "*" + temp;
                    curr = pointerType->getElementType();
                }
                if (std::holds_alternative<AbstractArrayType>(curr.getVariant())
                    || std::holds_alternative<ValArrayType>(curr.getVariant())
                    || std::holds_alternative<ArrayType>(curr.getVariant())
                    || std::holds_alternative<FunctionType>(curr.getVariant()))
                {
                    declarators = "(" + temp + declarators + ")";
                    return {std::move(curr)};
                }

                qualifiersAndSpecifiers = typeToString(curr) + " " + temp;
                return {};
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
                if (structType.isAnonymous())
                {
                    qualifiersAndSpecifiers += "struct <anonymous 0x" + llvm::utohexstr(structType.getId()) + ">";
                }
                else
                {
                    qualifiersAndSpecifiers += "struct " + cld::to_string(structType.getName());
                }
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
                if (unionType.isAnonymous())
                {
                    qualifiersAndSpecifiers += "union <anonymous 0x" + llvm::utohexstr(unionType.getId()) + ">";
                }
                else
                {
                    qualifiersAndSpecifiers += "union " + cld::to_string(unionType.getName());
                }
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
                if (enumType.isAnonymous())
                {
                    qualifiersAndSpecifiers += "enum <anonymous 0x" + llvm::utohexstr(enumType.getId()) + ">";
                }
                else
                {
                    qualifiersAndSpecifiers += "enum " + cld::to_string(enumType.getName());
                }
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

std::string cld::diag::StringConverter<cld::Semantics::Type>::inArg(const Semantics::Type& arg, const SourceInterface*)
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

std::string cld::diag::CustomFormat<U't', U'y', U'p', U'e'>::operator()(const Semantics::ExpressionBase& arg)
{
    return typeToString(arg.getType());
}

std::string cld::diag::CustomFormat<U'f', U'u', U'l', U'l', U'T', U'y', U'p', U'e'>::operator()(
    const Semantics::ExpressionBase& arg)
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
        m_variant, [](const std::unique_ptr<ExpressionBase>& expression) { return expression->end(); },
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

cld::Lexer::CTokenIterator cld::Semantics::CommaExpression::begin() const
{
    return m_commaExpressions[0].first->begin();
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

cld::Semantics::CompoundLiteral::CompoundLiteral(Type type, Lexer::CTokenIterator openParentheses,
                                                 Initializer initializer, Lexer::CTokenIterator closeParentheses,
                                                 Lexer::CTokenIterator initEnd, bool staticLifetime)
    : ExpressionBase(std::in_place_type<CompoundLiteral>, std::move(type), ValueCategory::Lvalue),
      m_openParentheses(openParentheses),
      m_initializer(std::make_unique<Initializer>(std::move(initializer))),
      m_closeParentheses(closeParentheses),
      m_initEnd(initEnd),
      m_staticLifetime(staticLifetime)
{
}

cld::Lexer::CTokenIterator cld::Semantics::CompoundLiteral::getOpenParentheses() const
{
    return m_openParentheses;
}

const cld::Semantics::Initializer& cld::Semantics::CompoundLiteral::getInitializer() const
{
    return *m_initializer;
}

cld::Lexer::CTokenIterator cld::Semantics::CompoundLiteral::getCloseParentheses() const
{
    return m_closeParentheses;
}

cld::Lexer::CTokenIterator cld::Semantics::CompoundLiteral::begin() const
{
    return m_openParentheses;
}

cld::Lexer::CTokenIterator cld::Semantics::CompoundLiteral::end() const
{
    return m_initEnd;
}

cld::Semantics::Statement::~Statement() = default;

const cld::Semantics::Statement& cld::Semantics::IfStatement::getTrueBranch() const
{
    return *m_trueBranch;
}

const cld::Semantics::Statement& cld::Semantics::ForStatement::getStatement() const
{
    return *m_statement;
}

const cld::Semantics::Statement& cld::Semantics::HeadWhileStatement::getStatement() const
{
    return *m_statement;
}

const cld::Semantics::Statement& cld::Semantics::FootWhileStatement::getStatement() const
{
    return *m_statement;
}

const cld::Semantics::Statement& cld::Semantics::LabelStatement::getStatement() const
{
    return *m_statement;
}

const cld::Semantics::Statement& cld::Semantics::SwitchStatement::getStatement() const
{
    return *m_statement;
}

const cld::Semantics::Statement& cld::Semantics::CaseStatement::getStatement() const
{
    return *m_statement;
}

const cld::Semantics::Statement& cld::Semantics::DefaultStatement::getStatement() const
{
    return *m_statement;
}

cld::Semantics::Type cld::Semantics::adjustParameterType(Type type)
{
    if (isArray(type))
    {
        auto elementType = cld::match(type.getVariant(), [](auto&& value) -> Type {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<ArrayType,
                                         T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
            {
                return value.getType();
            }
            CLD_UNREACHABLE;
        });
        bool restrict = cld::match(type.getVariant(), [](auto&& value) -> bool {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<ArrayType,
                                         T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
            {
                return value.isRestricted();
            }
            CLD_UNREACHABLE;
        });
        return PointerType::create(type.isConst(), type.isVolatile(), restrict, std::move(elementType));
    }
    return type;
}

cld::Semantics::Program cld::Semantics::analyse(const Syntax::TranslationUnit& parseTree, CSourceObject&& cTokens,
                                                llvm::raw_ostream* reporter, bool* errors)
{
    SemanticAnalysis analysis(cTokens, reporter, errors);
    auto translationUnit = analysis.visit(parseTree);
    return Program(std::move(translationUnit), std::move(cTokens), std::move(analysis));
}

const cld::Semantics::ExpressionBase& cld::Semantics::BuiltinVAArg::getExpression() const
{
    return *m_expression;
}

std::size_t std::hash<cld::Semantics::Type>::operator()(const cld::Semantics::Type& type) const noexcept
{
    return cld::hashCombine(type.isConst(), type.isVolatile(), type.getVariant());
}

cld::Lexer::CTokenIterator cld::Semantics::ExpressionBase::begin() const
{
    return this->match([](auto&& value) { return value.begin(); });
}

cld::Lexer::CTokenIterator cld::Semantics::ExpressionBase::end() const
{
    return this->match([](auto&& value) { return value.end(); });
}

cld::Semantics::ErrorExpression::ErrorExpression(Type type, const cld::Syntax::Node& node)
    : ErrorExpression(std::move(type), node.begin(), node.end())
{
}
