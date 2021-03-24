#include "Semantics.hpp"

#include <llvm/ADT/StringExtras.h>

#include <algorithm>
#include <optional>
#include <utility>

#include "Program.hpp"
#include "SemanticAnalysis.hpp"
#include "SemanticUtil.hpp"
#include "Syntax.hpp"

cld::Semantics::PrimitiveType
    cld::Semantics::PrimitiveType::fromUnderlyingType(bool isConst, bool isVolatile,
                                                      cld::LanguageOptions::UnderlyingType underlyingType,
                                                      const cld::LanguageOptions& options)
{
    switch (underlyingType)
    {
        case cld::LanguageOptions::UnderlyingType::UnsignedShort:
            return PrimitiveType::createUnsignedShort(isConst, isVolatile, options);
        case cld::LanguageOptions::UnderlyingType::Int: return PrimitiveType::createInt(isConst, isVolatile, options);
        case cld::LanguageOptions::UnderlyingType::UnsignedInt:
            return PrimitiveType::createUnsignedInt(isConst, isVolatile, options);
        case cld::LanguageOptions::UnderlyingType::Long: return PrimitiveType::createLong(isConst, isVolatile, options);
        case cld::LanguageOptions::UnderlyingType::UnsignedLong:
            return PrimitiveType::createUnsignedLong(isConst, isVolatile, options);
        case cld::LanguageOptions::UnderlyingType::LongLong:
            return PrimitiveType::createLongLong(isConst, isVolatile, options);
        case cld::LanguageOptions::UnderlyingType::UnsignedLongLong:
            return PrimitiveType::createUnsignedLongLong(isConst, isVolatile, options);
    }
    CLD_UNREACHABLE;
}

bool cld::Semantics::ArrayType::operator==(const cld::Semantics::ArrayType& rhs) const
{
    if (!equals(rhs))
    {
        return false;
    }
    return std::tie(m_restricted, m_static, m_size, *m_type)
           == std::tie(rhs.m_restricted, rhs.m_static, rhs.m_size, *rhs.m_type);
}

bool cld::Semantics::ArrayType::operator!=(const cld::Semantics::ArrayType& rhs) const
{
    return !(rhs == *this);
}

std::uint64_t cld::Semantics::ArrayType::getSizeOf(const ProgramInterface& program) const
{
    return m_type->getSizeOf(program) * m_size;
}

std::uint64_t cld::Semantics::ArrayType::getAlignOf(const ProgramInterface& program) const
{
    return m_type->getAlignOf(program);
}

std::uint64_t cld::Semantics::AbstractArrayType::getAlignOf(const ProgramInterface& program) const
{
    return m_type->getAlignOf(program);
}

std::uint64_t cld::Semantics::ValArrayType::getAlignOf(const ProgramInterface& program) const
{
    return m_type->getAlignOf(program);
}

bool cld::Semantics::Type::operator==(const cld::Semantics::Type& rhs) const
{
    if (index() != rhs.index())
    {
        return false;
    }
    return match([&](auto&& value) -> bool {
        using T = std::decay_t<decltype(value)>;
        return value == rhs.template cast<T>();
    });
}

bool cld::Semantics::Type::operator!=(const cld::Semantics::Type& rhs) const
{
    return !(rhs == *this);
}

std::uint64_t cld::Semantics::Type::getSizeOf(const ProgramInterface& program) const
{
    return match([&](auto&& value) -> std::size_t {
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

std::uint64_t cld::Semantics::Type::getAlignOf(const ProgramInterface& program) const
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
                return value.getAlignOf(program);
            }
        });
}

bool cld::Semantics::Type::equals(const cld::Semantics::Type& rhs) const
{
    return std::tie(m_isConst, m_isVolatile) == std::tie(rhs.m_isConst, rhs.m_isVolatile);
}

bool cld::Semantics::PointerType::operator==(const cld::Semantics::PointerType& rhs) const
{
    if (!equals(rhs))
    {
        return false;
    }
    return std::tie(m_restricted, *m_elementType) == std::tie(rhs.m_restricted, *rhs.m_elementType);
}

bool cld::Semantics::PointerType::operator!=(const cld::Semantics::PointerType& rhs) const
{
    return !(rhs == *this);
}

std::uint64_t cld::Semantics::PointerType::getSizeOf(const ProgramInterface& program) const
{
    return program.getLanguageOptions().sizeOfVoidStar;
}

std::uint64_t cld::Semantics::PointerType::getAlignOf(const ProgramInterface& program) const
{
    return program.getLanguageOptions().sizeOfVoidStar;
}

std::uint64_t cld::Semantics::EnumType::getSizeOf(const ProgramInterface& program) const
{
    return getInfo().type.getType().getSizeOf(program);
}

std::uint64_t cld::Semantics::EnumType::getAlignOf(const ProgramInterface& program) const
{
    return getInfo().type.getType().getAlignOf(program);
}

bool cld::Semantics::PrimitiveType::operator==(const cld::Semantics::PrimitiveType& rhs) const
{
    if (!equals(rhs))
    {
        return false;
    }
    return m_kind == rhs.m_kind;
}

bool cld::Semantics::PrimitiveType::operator!=(const cld::Semantics::PrimitiveType& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::ValArrayType::operator==(const cld::Semantics::ValArrayType& rhs) const
{
    if (!equals(rhs))
    {
        return false;
    }
    return std::tie(m_restricted, m_static, m_expression, *m_type)
           == std::tie(rhs.m_restricted, rhs.m_static, rhs.m_expression, *rhs.m_type);
}

bool cld::Semantics::ValArrayType::operator!=(const cld::Semantics::ValArrayType& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::FunctionType::operator==(const cld::Semantics::FunctionType& rhs) const
{
    if (!equals(rhs))
    {
        return false;
    }
    if (m_lastIsVararg != rhs.m_lastIsVararg || m_isKandR != rhs.m_isKandR)
    {
        return false;
    }
    if (*m_returnType != *rhs.m_returnType)
    {
        return false;
    }
    return std::equal(m_parameters.begin(), m_parameters.end(), rhs.m_parameters.begin(), rhs.m_parameters.end(),
                      [](const auto& lhs, const auto& rhs) { return *lhs.type == *rhs.type; });
}

bool cld::Semantics::FunctionType::operator!=(const cld::Semantics::FunctionType& rhs) const
{
    return !(rhs == *this);
}

bool cld::Semantics::AbstractArrayType::operator==(const cld::Semantics::AbstractArrayType& rhs) const
{
    if (!equals(rhs))
    {
        return false;
    }
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

std::uint64_t cld::Semantics::StructType::getSizeOf(const ProgramInterface&) const
{
    return cld::get<StructDefinition>(getInfo().type).getSizeOf();
}

std::uint64_t cld::Semantics::StructType::getAlignOf(const ProgramInterface&) const
{
    return cld::get<StructDefinition>(getInfo().type).getAlignOf();
}

std::uint64_t cld::Semantics::UnionType::getSizeOf(const ProgramInterface&) const
{
    return cld::get<UnionDefinition>(getInfo().type).getAlignOf();
}

std::uint64_t cld::Semantics::UnionType::getAlignOf(const ProgramInterface&) const
{
    return cld::get<UnionDefinition>(getInfo().type).getAlignOf();
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

bool cld::Semantics::isBitfieldAccess(const ExpressionBase& expression)
{
    if (!expression.is<MemberAccess>())
    {
        return false;
    }
    auto& mem = expression.cast<MemberAccess>();
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
std::string typeToString(const cld::Semantics::Type& arg)
{
    using namespace cld::Semantics;
    std::string qualifiersAndSpecifiers;
    std::string declarators;
    const Type* maybeCurr = &arg;
    while (maybeCurr)
    {
        maybeCurr = maybeCurr->match(
            [&](const PrimitiveType& primitiveType) -> const Type* {
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
            [&](const PointerType&) -> const Type* {
                std::string temp;
                const auto* curr = maybeCurr;
                while (auto* pointerType = curr->get_if<PointerType>())
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

                qualifiersAndSpecifiers = typeToString(*curr) + " " + temp;
                return {};
            },
            [&](const ValArrayType& valArrayType) -> const Type* {
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
            [&](const ArrayType& arrayType) -> const Type* {
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
            [&](const AbstractArrayType& arrayType) -> const Type* {
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
            [&](const VectorType& vectorType) -> const Type* {
                qualifiersAndSpecifiers += "__attribute__((vector_size(" + std::to_string(vectorType.getSize())
                                           + " * sizeof(" + typeToString(vectorType.getType()) + ")))) ";
                return &vectorType.getType();
            },
            [&](const FunctionType& functionType) -> const Type* {
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
                declarators += typeToString(*functionType.getParameters()[0].type);
                for (auto& iter : llvm::ArrayRef(functionType.getParameters()).drop_front())
                {
                    declarators += ", " + typeToString(*iter.type);
                }
                if (functionType.isLastVararg())
                {
                    declarators += ",...";
                }
                declarators += ")";
                return &functionType.getReturnType();
            },
            [&](const StructType& structType) -> const Type* {
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
                    qualifiersAndSpecifiers += "struct " + cld::to_string(structType.getName());
                }
                return {};
            },
            [&](const UnionType& unionType) -> const Type* {
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
                    qualifiersAndSpecifiers += "union " + cld::to_string(unionType.getName());
                }
                return {};
            },
            [&](const EnumType& enumType) -> const Type* {
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
                    qualifiersAndSpecifiers += "enum " + cld::to_string(enumType.getName());
                }
                return {};
            },
            [&](const ErrorType&) -> const Type* {
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

cld::Semantics::CompoundLiteral::CompoundLiteral(cld::not_null<const Type> type, Lexer::CTokenIterator openParentheses,
                                                 Initializer initializer, Lexer::CTokenIterator closeParentheses,
                                                 Lexer::CTokenIterator initEnd, bool staticLifetime)
    : ExpressionBase(std::in_place_type<CompoundLiteral>, type, ValueCategory::Lvalue),
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
        auto& elementType = type.match(
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
        bool restrict = type.match(
            [](auto&& value) -> bool
            {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<
                                  ArrayType,
                                  T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
                {
                    return value.isRestricted();
                }
                CLD_UNREACHABLE;
            });
        return PointerType(type.isConst(), type.isVolatile(), restrict, &elementType);
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

cld::Semantics::ErrorExpression::ErrorExpression(cld::not_null<const Type> type, const cld::Syntax::Node& node)
    : ErrorExpression(type, node.begin(), node.end())
{
}

bool cld::Semantics::VectorType::operator==(const cld::Semantics::VectorType& rhs) const
{
    if (!equals(rhs))
    {
        return false;
    }
    return std::tie(m_size, *m_elementType) == std::tie(rhs.m_size, *rhs.m_elementType);
}

bool cld::Semantics::VectorType::operator!=(const cld::Semantics::VectorType& rhs) const
{
    return !(*this == rhs);
}

std::uint64_t cld::Semantics::VectorType::getSizeOf(const cld::Semantics::ProgramInterface& program) const
{
    return m_elementType->getSizeOf(program) * m_size;
}

std::uint64_t cld::Semantics::VectorType::getAlignOf(const cld::Semantics::ProgramInterface& program) const
{
    return getSizeOf(program);
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
    if (auto* structType = type.get_if<StructType>())
    {
        return std::holds_alternative<StructDefinition>(structType->getInfo().type);
    }
    if (auto* unionType = type.get_if<UnionType>())
    {
        return std::holds_alternative<UnionDefinition>(unionType->getInfo().type);
    }
    return true;
}

const cld::Semantics::FieldMap& cld::Semantics::getFields(const cld::Semantics::Type& recordType)
{
    if (auto* structType = recordType.get_if<StructType>())
    {
        return cld::get<StructDefinition>(structType->getInfo().type).getFields();
    }
    if (auto* unionType = recordType.get_if<UnionType>())
    {
        return cld::get<UnionDefinition>(unionType->getInfo().type).getFields();
    }
    CLD_UNREACHABLE;
}

llvm::ArrayRef<cld::Semantics::MemoryLayout> cld::Semantics::getMemoryLayout(const cld::Semantics::Type& structType)
{
    if (auto* structTy = structType.get_if<StructType>())
    {
        return cld::get<StructDefinition>(structTy->getInfo().type).getMemLayout();
    }
    CLD_UNREACHABLE;
}

llvm::ArrayRef<cld::Semantics::FieldInLayout> cld::Semantics::getFieldLayout(const cld::Semantics::Type& recordType)
{
    if (auto* structType = recordType.get_if<StructType>())
    {
        return cld::get<StructDefinition>(structType->getInfo().type).getFieldLayout();
    }
    if (auto* unionType = recordType.get_if<UnionType>())
    {
        return cld::get<UnionDefinition>(unionType->getInfo().type).getFieldLayout();
    }
    CLD_UNREACHABLE;
}
