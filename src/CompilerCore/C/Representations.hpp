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

    class Declaration;
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
        std::shared_ptr<Type> m_type;
        std::size_t m_size;

        ArrayType(bool isRestricted, std::shared_ptr<Type>&& type, std::size_t size);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& type, std::size_t size);

        const Type& getType() const;

        std::size_t getSize() const;

        bool isRestricted() const;

        bool operator==(const ArrayType& rhs) const;

        bool operator!=(const ArrayType& rhs) const;
    };

    class AbstractArrayType final
    {
        bool m_restricted;
        std::shared_ptr<Type> m_type;

        AbstractArrayType(bool isRestricted, std::shared_ptr<Type>&& type);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& type);

        const Type& getType() const;

        bool isRestricted() const;

        bool operator==(const AbstractArrayType& rhs) const;

        bool operator!=(const AbstractArrayType& rhs) const;
    };

    class ValArrayType final
    {
        bool m_restricted;
        std::shared_ptr<Type> m_type;

        ValArrayType(bool isRestricted, std::shared_ptr<OpenCL::Representations::Type>&& type);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& type);

        const Type& getType() const;

        bool isRestricted() const;

        bool operator==(const ValArrayType& rhs) const;

        bool operator!=(const ValArrayType& rhs) const;
    };

    class FunctionType final
    {
        std::shared_ptr<Type> m_returnType;
        std::vector<Type> m_arguments;
        bool m_lastIsVararg;
        bool m_hasPrototype;

        FunctionType(std::shared_ptr<Type>&& returnType, std::vector<Type> arguments, bool lastIsVararg,
                     bool hasPrototype);

    public:
        static Type create(OpenCL::Representations::Type&& returnType,
                           std::vector<OpenCL::Representations::Type>&& arguments, bool lastIsVararg,
                           bool hasPrototype);

        const Type& getReturnType() const;

        const std::vector<Type>& getArguments() const;

        bool isLastVararg() const;

        bool hasPrototype() const;

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

        bool isDefinition() const;

        bool operator==(const RecordType& rhs) const;

        bool operator!=(const RecordType& rhs) const;
    };

    class EnumType final
    {
        std::string m_name;
        std::vector<std::pair<std::string, std::int32_t>> m_values;

        EnumType(const std::string& name, std::vector<std::pair<std::string, std::int32_t>> values);

    public:
        static Type create(bool isConst, bool isVolatile, const std::string& name,
                           std::vector<std::pair<std::string, std::int32_t>> values);

        const std::vector<std::pair<std::string, int32_t>>& getValues() const;

        bool isDefinition() const;

        bool isAnonymous() const;

        const std::string& getName() const;

        bool operator==(const EnumType& rhs) const;

        bool operator!=(const EnumType& rhs) const;
    };

    class PointerType final
    {
        bool m_restricted;
        std::shared_ptr<Type> m_elementType;

        PointerType(bool isRestricted, std::shared_ptr<Type>&& elementType);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& elementType);

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
                          PossiblyAbstractQualifierRef declarator = {},
                          const std::map<std::string, std::reference_wrapper<const Type>>& typedefs = {},
                          const std::vector<Syntax::Declaration>& declarations = {},const std::map<std::string, Representations::RecordType>& structOrUnions = {});

    std::string declaratorToName(const OpenCL::Syntax::Declarator& declarator);

    Expected<std::size_t, FailureReason> sizeOf(const Type& type);

    Expected<std::size_t, FailureReason> alignmentOf(const Type& type);
} // namespace OpenCL::Representations

#endif // OPENCLPARSER_REPRESENTATIONS_HPP
