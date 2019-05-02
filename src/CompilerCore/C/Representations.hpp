#ifndef OPENCLPARSER_REPRESENTATIONS_HPP
#define OPENCLPARSER_REPRESENTATIONS_HPP

#include "Expected.hpp"
#include "FailureReason.hpp"

#include <map>
#include <memory>
#include <string>
#include <vector>

namespace OpenCL::Syntax
{
    class TypeSpecifier;

    enum class TypeQualifier;

    class AbstractDeclarator;

    class Declarator;
} // namespace OpenCL::Syntax

namespace OpenCL::Representations
{
    class Type;

    class PrimitiveType final
    {
        bool m_isFloatingPoint;
        bool m_isSigned;
        std::uint8_t m_bitCount;

        PrimitiveType(bool isFloatingPoint, bool isSigned, std::uint8_t bitCount);

    public:
        static Type create(bool isConst, bool isVolatile, bool isFloatingPoint, bool isSigned, std::uint8_t bitCount);

        bool isFloatingPoint() const;

        bool isSigned() const;

        std::uint8_t getBitCount() const;

        bool operator==(const PrimitiveType& rhs) const;

        bool operator!=(const PrimitiveType& rhs) const;
    };

    class ArrayType final
    {
        bool m_restricted;
        std::unique_ptr<Type> m_type;
        std::size_t m_size;

        ArrayType(bool isRestricted, std::unique_ptr<Type>&& type, std::size_t size);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& type, std::size_t size);

        ArrayType(const ArrayType& rhs);

        ArrayType(ArrayType&& rhs) noexcept = default;

        ArrayType& operator=(const ArrayType& rhs);

        ArrayType& operator=(ArrayType&& rhs) noexcept = default;

        const Type& getType() const;

        std::size_t getSize() const;

        bool isRestricted() const;

        bool operator==(const ArrayType& rhs) const;

        bool operator!=(const ArrayType& rhs) const;
    };

    class AbstractArrayType final
    {
        bool m_restricted;
        std::unique_ptr<Type> m_type;

        AbstractArrayType(bool isRestricted, std::unique_ptr<Type>&& type);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& type);

        AbstractArrayType(const AbstractArrayType& rhs);

        AbstractArrayType(AbstractArrayType&& rhs) noexcept = default;

        AbstractArrayType& operator=(const AbstractArrayType& rhs);

        AbstractArrayType& operator=(AbstractArrayType&& rhss) noexcept = default;

        const Type& getType() const;

        bool isRestricted() const;

        bool operator==(const AbstractArrayType& rhs) const;

        bool operator!=(const AbstractArrayType& rhs) const;
    };

    class ValArrayType final
    {
        bool m_restricted;
        std::unique_ptr<Type> m_type;

        ValArrayType(bool isRestricted, std::unique_ptr<OpenCL::Representations::Type>&& type);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& type);

        ValArrayType(const ValArrayType& rhs);

        ValArrayType(ValArrayType&& rhs) noexcept = default;

        ValArrayType& operator=(const ValArrayType& rhs);

        ValArrayType& operator=(ValArrayType&& rhs) noexcept = default;

        const Type& getType() const;

        bool isRestricted() const;

        bool operator==(const ValArrayType& rhs) const;

        bool operator!=(const ValArrayType& rhs) const;
    };

    class FunctionType final
    {
        std::unique_ptr<Type> m_returnType;
        std::vector<Type> m_arguments;
        bool m_lastIsVararg;

        FunctionType(std::unique_ptr<Type>&& returnType, std::vector<Type> arguments, bool lastIsVararg);

    public:
        static OpenCL::Representations::Type create(OpenCL::Representations::Type&& returnType,
                                                    std::vector<OpenCL::Representations::Type>&& arguments,
                                                    bool lastIsVararg);

        FunctionType(const FunctionType& rhs);

        FunctionType(FunctionType&& rhs) noexcept = default;

        FunctionType& operator=(const FunctionType& rhs);

        FunctionType& operator=(FunctionType&& rhs) noexcept = default;

        const Type& getReturnType() const;

        const std::vector<Type>& getArguments() const;

        bool isLastVararg() const;

        bool operator==(const FunctionType& rhs) const;

        bool operator!=(const FunctionType& rhs) const;
    };

    class RecordType final
    {
        std::string m_name;
        bool m_isUnion;
        std::vector<std::tuple<Type, std::string, std::int64_t>> m_members;

        RecordType(std::string name, bool isUnion, std::vector<std::tuple<Type, std::string, std::int64_t>>&& names);

    public:
        static Type create(bool isConst, bool isVolatile, bool isUnion, const std::string& name,
                           std::vector<std::tuple<Type, std::string, std::int64_t>>&& members = {});

        const std::string& getName() const;

        bool isUnion() const;

        const std::vector<std::tuple<Type, std::string, std::int64_t>>& getMembers() const;

        bool isDeclaration() const;

        bool operator==(const RecordType& rhs) const;

        bool operator!=(const RecordType& rhs) const;
    };

    class EnumType final
    {
        bool m_anonymous;
        std::vector<std::pair<std::string, std::int32_t>> m_values;

        EnumType(const std::string& name, std::vector<std::pair<std::string, std::int32_t>> values);

    public:
        static Type create(bool isConst, bool isVolatile, const std::string& name,
                           std::vector<std::pair<std::string, std::int32_t>> values);

        const std::vector<std::pair<std::string, int32_t>>& getValues() const;

        bool isDeclaration() const;

        bool isAnonymous() const;

        bool operator==(const EnumType& rhs) const;

        bool operator!=(const EnumType& rhs) const;
    };

    class PointerType final
    {
        bool m_restricted;
        std::unique_ptr<Type> m_elementType;

        PointerType(bool isRestricted, std::unique_ptr<Type>&& elementType);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& elementType);

        PointerType(const PointerType& rhs);

        PointerType(PointerType&& rhs) noexcept = default;

        PointerType& operator=(const PointerType& rhs);

        PointerType& operator=(PointerType&& rhs) noexcept = default;

        const Type& getElementType() const;

        bool isRestricted() const;

        bool operator==(const PointerType& rhs) const;

        bool operator!=(const PointerType& rhs) const;
    };

    class Type final
    {
        bool m_isConst;
        bool m_isVolatile;
        std::string m_name;
        using variant = std::variant<PrimitiveType, ArrayType, AbstractArrayType, ValArrayType, FunctionType,
                                     RecordType, EnumType, PointerType>;

        variant m_type;

    public:
        Type(bool isConst, bool isVolatile, std::string name, variant&& type);

        virtual ~Type() = default;

        Type(const Type&) = default;

        Type(Type&&) noexcept = default;

        Type& operator=(const Type&) = default;

        Type& operator=(Type&&) noexcept = default;

        const variant& getType() const;

        bool isConst() const;

        bool isVolatile() const;

        const std::string& getName() const;

        void setName(const std::string& name);

        bool operator==(const Type& rhs) const;

        bool operator!=(const Type& rhs) const;

        bool isCompatibleWith(const Type& rhs) const;
    };

    using SpecifierQualifierRef = std::variant<std::reference_wrapper<const Syntax::TypeSpecifier>,
                                               std::reference_wrapper<const Syntax::TypeQualifier>>;

    using PossiblyAbstractQualifierRef =
        std::variant<const Syntax::AbstractDeclarator*, std::reference_wrapper<const Syntax::Declarator>>;

    Expected<Type, FailureReason>
        declaratorsToType(std::vector<SpecifierQualifierRef> specifierQualifiers,
                          PossiblyAbstractQualifierRef declarator,
                          const std::map<std::string, std::reference_wrapper<const Type>>& typedefs);

    std::string declaratorToName(const OpenCL::Syntax::Declarator& declarator);
} // namespace OpenCL::Representations

#endif // OPENCLPARSER_REPRESENTATIONS_HPP
