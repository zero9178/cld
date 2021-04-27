#include "Semantics.hpp"

#include <llvm/ADT/StringExtras.h>

#include <algorithm>
#include <optional>
#include <utility>

#include "Program.hpp"
#include "SemanticAnalysis.hpp"
#include "SemanticUtil.hpp"
#include "Syntax.hpp"

cld::Semantics::PrimitiveType::PrimitiveType(LanguageOptions::UnderlyingType underlyingType,
                                             const cld::LanguageOptions& options, TypeFlags flags)
    : PrimitiveType(
        [&]
        {
            switch (underlyingType)
            {
                case cld::LanguageOptions::UnderlyingType::UnsignedShort: return UnsignedShort;
                case cld::LanguageOptions::UnderlyingType::Int: return Int;
                case cld::LanguageOptions::UnderlyingType::UnsignedInt: return UnsignedInt;
                case cld::LanguageOptions::UnderlyingType::Long: return Long;
                case cld::LanguageOptions::UnderlyingType::UnsignedLong: return UnsignedLong;
                case cld::LanguageOptions::UnderlyingType::LongLong: return LongLong;
                case cld::LanguageOptions::UnderlyingType::UnsignedLongLong: return UnsignedLongLong;
            }
            CLD_UNREACHABLE;
        }(),
        options, flags)
{
}

cld::Semantics::PrimitiveType::PrimitiveType(PrimitiveType::Kind kind, const cld::LanguageOptions& options,
                                             TypeFlags flags)
    : Type(std::in_place_type<PrimitiveType>, flags & ~(Static | Restricted | VARArg | KAndR)), m_kind(kind)
{
    m_isFloatingPoint = false;
    m_isSigned = true;
    switch (kind)
    {
        case Char:
            m_isSigned = options.charIsSigned;
            m_bitCount = 8;
            m_alignOf = 1;
            break;
        case UnsignedChar: m_isSigned = false; [[fallthrough]];
        case SignedChar:
            m_bitCount = 8;
            m_alignOf = 1;
            break;
        case Bool:
            m_isSigned = false;
            m_bitCount = 1;
            m_alignOf = 1;
            break;
        case UnsignedShort: m_isSigned = false; [[fallthrough]];
        case Short:
            m_bitCount = options.sizeOfShort * 8;
            m_alignOf = options.sizeOfShort;
            break;
        case UnsignedInt: m_isSigned = false; [[fallthrough]];
        case Int:
            m_bitCount = options.sizeOfInt * 8;
            m_alignOf = options.sizeOfInt;
            break;
        case UnsignedLong: m_isSigned = false; [[fallthrough]];
        case Long:
            m_bitCount = options.sizeOfLong * 8;
            m_alignOf = options.sizeOfLong;
            break;
        case UnsignedLongLong: m_isSigned = false; [[fallthrough]];
        case LongLong:
            m_bitCount = 64;
            m_alignOf = options.alignOfLongLong;
            break;
        case Float:
            m_isFloatingPoint = true;
            m_bitCount = 32;
            m_alignOf = 4;
            break;
        case Double:
            m_isFloatingPoint = true;
            m_bitCount = 64;
            m_alignOf = options.alignOfDouble;
            break;
        case LongDouble:
            m_isFloatingPoint = true;
            m_bitCount = options.sizeOfLongDoubleBits;
            m_alignOf = options.alignOfLongDouble;
            break;
        case Void:
            m_bitCount = 0;
            m_alignOf = 0;
            break;
        case UnsignedInt128: m_isSigned = false; [[fallthrough]];
        case Int128:
            m_bitCount = 128;
            m_alignOf = 16;
            break;
        default: CLD_UNREACHABLE;
    }
}

bool cld::Semantics::Type::operator==(const cld::Semantics::Type& rhs) const
{
    if (index() != rhs.index() || m_flags != rhs.m_flags)
    {
        return false;
    }
    return match(
        [&](auto&& value) -> bool
        {
            using T = std::decay_t<decltype(value)>;
            return value.equalsImpl(rhs.template as<T>());
        });
}

bool cld::Semantics::Type::operator!=(const cld::Semantics::Type& rhs) const
{
    return !(rhs == *this);
}

std::uint64_t cld::Semantics::Type::getSizeOf(const ProgramInterface& program) const
{
    return match(
        [&](auto&& value) -> std::size_t
        {
            if constexpr (std::is_same_v<std::monostate, std::decay_t<decltype(value)>>)
            {
                CLD_UNREACHABLE;
            }
            else
            {
                return value.getSizeOfImpl(program);
            }
        });
}

std::uint64_t cld::Semantics::Type::getAlignOf(const ProgramInterface& program) const
{
    if (m_info)
    {
        if (auto* align = m_info->getAttributeIf<AlignedAttribute>())
        {
            return *align->alignment;
        }
    }
    return match(
        [&](auto&& value) -> std::size_t
        {
            if constexpr (std::is_same_v<std::monostate, std::decay_t<decltype(value)>>)
            {
                CLD_UNREACHABLE;
            }
            else
            {
                return value.getAlignOfImpl(program);
            }
        });
}

std::string_view cld::Semantics::Type::getTypedefName() const
{
    return m_info ? m_info->name : "";
}

void cld::Semantics::Type::setConst(bool isConst)
{
    if (isConst)
    {
        CLD_ASSERT(!is<FunctionType>());
        m_flags = static_cast<TypeFlags>(m_flags | Const);
    }
    else
    {
        m_flags = static_cast<TypeFlags>(m_flags & ~Const);
    }
    if (m_info && m_info->isConst && !isConst)
    {
        m_info = nullptr;
    }
}

void cld::Semantics::Type::setVolatile(bool isVolatile)
{
    if (isVolatile)
    {
        CLD_ASSERT(!is<FunctionType>());
        m_flags = static_cast<TypeFlags>(m_flags | Volatile);
    }
    else
    {
        m_flags = static_cast<TypeFlags>(m_flags & ~Volatile);
    }
    if (m_info && m_info->isVolatile && !isVolatile)
    {
        m_info = nullptr;
    }
}

void cld::Semantics::Type::setRestricted(bool isRestricted)
{
    if (isRestricted)
    {
        CLD_ASSERT(is<ArrayType>() || is<AbstractArrayType>() || is<ValArrayType>() || is<PointerType>()
                   || is<ErrorType>());
        m_flags = static_cast<TypeFlags>(m_flags | Restricted);
    }
    else
    {
        m_flags = static_cast<TypeFlags>(m_flags & ~Restricted);
    }
    if (m_info && m_info->isRestricted && !isRestricted)
    {
        m_info = nullptr;
    }
}

std::uint64_t cld::Semantics::PointerType::getSizeOfImpl(const ProgramInterface& program) const
{
    return program.getLanguageOptions().sizeOfVoidStar;
}

std::uint64_t cld::Semantics::PointerType::getAlignOfImpl(const ProgramInterface& program) const
{
    return program.getLanguageOptions().sizeOfVoidStar;
}

std::uint64_t cld::Semantics::EnumType::getSizeOfImpl(const ProgramInterface& program) const
{
    return getInfo().type.getType().getSizeOf(program);
}

std::uint64_t cld::Semantics::EnumType::getAlignOfImpl(const ProgramInterface& program) const
{
    if (auto* alignment = getInfo().getAttributeIf<AlignedAttribute>())
    {
        return *alignment->alignment;
    }
    return getInfo().type.getType().getAlignOf(program);
}

std::string_view cld::Semantics::EnumType::getEnumName() const
{
    return m_info->name;
}

bool cld::Semantics::EnumType::isAnonymous() const
{
    return m_info->name.empty();
}

cld::Lexer::CTokenIterator cld::Semantics::declaratorToLoc(const cld::Syntax::Declarator& declarator)
{
    return matchWithSelf(
        declarator.getDirectDeclarator(),
        [](auto&&, const Syntax::DirectDeclaratorIdentifier& name) -> cld::Lexer::CTokenIterator
        { return name.getIdentifierLoc(); },
        [](auto&& self, const Syntax::DirectDeclaratorParentheses& declarator) -> cld::Lexer::CTokenIterator
        {
            return cld::match(declarator.getDeclarator().getDirectDeclarator(),
                              [&self](auto&& value) -> cld::Lexer::CTokenIterator { return self(value); });
        },
        [](auto&& self, auto&& value) -> cld::Lexer::CTokenIterator
        {
            return cld::match(value.getDirectDeclarator(),
                              [&self](auto&& value) -> cld::Lexer::CTokenIterator { return self(value); });
        });
}

std::uint64_t cld::Semantics::StructType::getSizeOfImpl(const ProgramInterface& interface) const
{
    auto size = cld::get<StructDefinition>(getInfo().type).getSizeOf();
    return cld::roundUpTo(size, getAlignOf(interface));
}

std::uint64_t cld::Semantics::StructType::getAlignOfImpl(const ProgramInterface&) const
{
    auto align = cld::get<StructDefinition>(getInfo().type).getAlignOf();
    if (auto* alignment = getInfo().getAttributeIf<AlignedAttribute>())
    {
        return std::max(*alignment->alignment, align);
    }
    return align;
}

std::string_view cld::Semantics::StructType::getStructName() const
{
    return m_info->name;
}

bool cld::Semantics::StructType::isAnonymous() const
{
    return m_info->name.empty();
}

std::uint64_t cld::Semantics::UnionType::getSizeOfImpl(const ProgramInterface& interface) const
{
    auto size = cld::get<UnionDefinition>(getInfo().type).getSizeOf();
    return cld::roundUpTo(size, getAlignOf(interface));
}

std::uint64_t cld::Semantics::UnionType::getAlignOfImpl(const ProgramInterface&) const
{
    auto align = cld::get<UnionDefinition>(getInfo().type).getAlignOf();
    if (auto* alignment = getInfo().getAttributeIf<AlignedAttribute>())
    {
        return std::max(*alignment->alignment, align);
    }
    return align;
}

std::string_view cld::Semantics::UnionType::getUnionName() const
{
    return m_info->name;
}

bool cld::Semantics::UnionType::isAnonymous() const
{
    return m_info->name.empty();
}

bool cld::Semantics::isStringLiteralExpr(const ExpressionBase& expression)
{
    auto* constant = expression.tryAs<Constant>();
    if (!constant)
    {
        return false;
    }
    return std::holds_alternative<std::string>(constant->getValue())
           || std::holds_alternative<Lexer::NonCharString>(constant->getValue());
}

bool cld::Semantics::isBitfieldAccess(const ExpressionBase& expression)
{
    if (!expression.is<MemberAccess>())
    {
        return false;
    }
    auto& mem = expression.as<MemberAccess>();
    return static_cast<bool>(mem.getField().bitFieldBounds);
}

bool cld::Semantics::isVariablyModified(const Type& type)
{
    auto typeVisitor = RecursiveVisitor(type, TYPE_NEXT_FN);
    return std::any_of(typeVisitor.begin(), typeVisitor.end(),
                       [](const Type& type) { return type.is<ValArrayType>(); });
}

bool cld::Semantics::isVariableLengthArray(const Type& type)
{
    auto typeVisitor = RecursiveVisitor(type, ARRAY_TYPE_NEXT_FN);
    return std::any_of(typeVisitor.begin(), typeVisitor.end(),
                       [](const Type& type) { return type.is<ValArrayType>(); });
}

namespace
{
std::string typeToString(const cld::Semantics::Type& arg, bool respectTypedefs)
{
    using namespace cld::Semantics;
    std::string qualifiersAndSpecifiers;
    std::string declarators;
    const Type* maybeCurr = &arg;
    while (maybeCurr)
    {
        if (auto* info = maybeCurr->getTypedefInfo(); info && respectTypedefs)
        {
            if (maybeCurr->isRestricted() && !info->isRestricted)
            {
                qualifiersAndSpecifiers += "restrict ";
            }
            if (maybeCurr->isConst() && !info->isConst)
            {
                qualifiersAndSpecifiers += "const ";
            }
            if (maybeCurr->isVolatile() && !info->isVolatile)
            {
                qualifiersAndSpecifiers += "volatile ";
            }
            qualifiersAndSpecifiers += cld::to_string(info->name);
            break;
        }
        maybeCurr = maybeCurr->match(
            [&](const PrimitiveType& primitiveType) -> const Type*
            {
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
                    case PrimitiveType::Int128: qualifiersAndSpecifiers += "__int128"; break;
                    case PrimitiveType::UnsignedInt128: qualifiersAndSpecifiers += "unsigned __int128"; break;
                }
                return {};
            },
            [&](const PointerType&) -> const Type*
            {
                std::string temp;
                const auto* curr = maybeCurr;
                while (auto* pointerType = curr->tryAs<PointerType>())
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
                    if (pointerType->isConst())
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
                    if (pointerType->isVolatile())
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
                    curr = &pointerType->getElementType();
                }
                if (isArray(*curr) || curr->is<FunctionType>())
                {
                    declarators = "(" + temp + declarators + ")";
                    return curr;
                }

                qualifiersAndSpecifiers = typeToString(*curr, respectTypedefs) + " " + temp;
                return {};
            },
            [&](const ValArrayType& valArrayType) -> const Type*
            {
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
                return &valArrayType.getType();
            },
            [&](const ArrayType& arrayType) -> const Type*
            {
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
                return &arrayType.getType();
            },
            [&](const AbstractArrayType& arrayType) -> const Type*
            {
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
                return &arrayType.getType();
            },
            [&](const VectorType& vectorType) -> const Type*
            {
                qualifiersAndSpecifiers += "__attribute__((vector_size(" + std::to_string(vectorType.getSize())
                                           + " * sizeof(" + typeToString(vectorType.getType(), respectTypedefs)
                                           + ")))) ";
                return &vectorType.getType();
            },
            [&](const FunctionType& functionType) -> const Type*
            {
                declarators += "(";
                if (functionType.getParameters().empty())
                {
                    if (!functionType.isKandR())
                    {
                        declarators += "void";
                    }
                    declarators += ")";
                    return &functionType.getReturnType();
                }
                declarators += typeToString(*functionType.getParameters()[0].type, respectTypedefs);
                for (auto& iter : llvm::ArrayRef(functionType.getParameters()).drop_front())
                {
                    declarators += ", " + typeToString(*iter.type, respectTypedefs);
                }
                if (functionType.isLastVararg())
                {
                    declarators += ",...";
                }
                declarators += ")";
                return &functionType.getReturnType();
            },
            [&](const StructType& structType) -> const Type*
            {
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
                    qualifiersAndSpecifiers += "struct <anonymous 0x" + llvm::utohexstr(structType.getInfo().id) + ">";
                }
                else
                {
                    qualifiersAndSpecifiers += "struct " + cld::to_string(structType.getStructName());
                }
                return {};
            },
            [&](const UnionType& unionType) -> const Type*
            {
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
                    qualifiersAndSpecifiers += "union <anonymous 0x" + llvm::utohexstr(unionType.getInfo().id) + ">";
                }
                else
                {
                    qualifiersAndSpecifiers += "union " + cld::to_string(unionType.getUnionName());
                }
                return {};
            },
            [&](const EnumType& enumType) -> const Type*
            {
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
                    qualifiersAndSpecifiers += "enum <anonymous 0x" + llvm::utohexstr(enumType.getInfo().id) + ">";
                }
                else
                {
                    qualifiersAndSpecifiers += "enum " + cld::to_string(enumType.getEnumName());
                }
                return {};
            },
            [&](const ErrorType&) -> const Type*
            {
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
    return typeToString(arg, true);
}

std::string cld::diag::CustomFormat<U'f', U'u', U'l', U'l'>::operator()(const Semantics::Type& arg)
{
    auto result = typeToString(arg, false);
    if (arg.isTypedef())
    {
        return "'" + typeToString(arg, true) + "' (aka '" + result + "')";
    }
    return "'" + result + "'";
}

std::string cld::diag::CustomFormat<U't', U'y', U'p', U'e'>::operator()(const Semantics::ExpressionBase& arg)
{
    return typeToString(arg.getType(), true);
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
        m_variant, [](const IntrVarPtr<ExpressionBase>& expression) { return expression->end(); },
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

cld::Semantics::CompoundLiteral::CompoundLiteral(IntrVarValue<Type> type, Lexer::CTokenIterator openParentheses,
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

cld::IntrVarValue<cld::Semantics::Type> cld::Semantics::adjustParameterType(const Type& type)
{
    if (isArray(type))
    {
        auto& elementType = getArrayElementType(type);
        return PointerType(&elementType, flag::useFlags = type.getFlags());
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

cld::Lexer::CTokenIterator cld::Semantics::ExpressionBase::begin() const
{
    return this->match([](auto&& value) { return value.begin(); });
}

cld::Lexer::CTokenIterator cld::Semantics::ExpressionBase::end() const
{
    return this->match([](auto&& value) { return value.end(); });
}

cld::Semantics::ErrorExpression::ErrorExpression(IntrVarValue<Type> type, const cld::Syntax::Node& node)
    : ErrorExpression(std::move(type), node.begin(), node.end())
{
}

bool cld::Semantics::isCompleteType(const Type& type)
{
    if (isVoid(type))
    {
        return false;
    }
    if (type.is<AbstractArrayType>())
    {
        return false;
    }
    if (auto* structType = type.tryAs<StructType>())
    {
        return std::holds_alternative<StructDefinition>(structType->getInfo().type);
    }
    if (auto* unionType = type.tryAs<UnionType>())
    {
        return std::holds_alternative<UnionDefinition>(unionType->getInfo().type);
    }
    return true;
}

const cld::Semantics::FieldMap& cld::Semantics::getFields(const cld::Semantics::Type& recordType)
{
    if (auto* structType = recordType.tryAs<StructType>())
    {
        return cld::get<StructDefinition>(structType->getInfo().type).getFields();
    }
    if (auto* unionType = recordType.tryAs<UnionType>())
    {
        return cld::get<UnionDefinition>(unionType->getInfo().type).getFields();
    }
    CLD_UNREACHABLE;
}

llvm::ArrayRef<cld::Semantics::MemoryLayout> cld::Semantics::getMemoryLayout(const cld::Semantics::Type& structType)
{
    if (auto* structTy = structType.tryAs<StructType>())
    {
        return cld::get<StructDefinition>(structTy->getInfo().type).getMemLayout();
    }
    CLD_UNREACHABLE;
}

llvm::ArrayRef<cld::Semantics::FieldInLayout> cld::Semantics::getFieldLayout(const cld::Semantics::Type& recordType)
{
    if (auto* structType = recordType.tryAs<StructType>())
    {
        return cld::get<StructDefinition>(structType->getInfo().type).getFieldLayout();
    }
    if (auto* unionType = recordType.tryAs<UnionType>())
    {
        return cld::get<UnionDefinition>(unionType->getInfo().type).getFieldLayout();
    }
    CLD_UNREACHABLE;
}

cld::Semantics::FunctionDeclaration::FunctionDeclaration(FunctionType type, cld::Semantics::Linkage linkage,
                                                         cld::Lexer::CTokenIterator nameToken,
                                                         cld::Semantics::InlineKind inlineKind, Useable* previous)
    : Declaration(std::in_place_type<FunctionDeclaration>, linkage, nameToken),
      m_type(std::move(type)),
      m_inlineKind(inlineKind),
      m_first(previous ? previous->match([](FunctionDeclaration& decl) -> const Useable* { return &decl.getFirst(); },
                                         [](FunctionDefinition& def) -> const Useable* { return &def.getFirst(); },
                                         [](auto&) -> const Useable* { CLD_UNREACHABLE; }) :
                         nullptr)
{
    if (previous)
    {
        previous->match([this](FunctionDeclaration& decl) { decl.m_next = this; },
                        [this](FunctionDefinition& def) { def.m_next = this; }, [](auto&) { CLD_UNREACHABLE; });
        setUses(previous->getUses());
    }
}
