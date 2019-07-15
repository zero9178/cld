#ifndef OPENCLPARSER_SEMANTICS_HPP
#define OPENCLPARSER_SEMANTICS_HPP

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "CompilerCore/Common/Expected.hpp"
#include "CompilerCore/Common/FailureReason.hpp"

#include "Message.hpp"
#include "Syntax.hpp"

namespace OpenCL::Semantics
{
    class Type;

    class CompoundStatement;

    class LabelStatement;

    class CaseStatement;

    class DefaultStatement;

    class IfStatement;

    class SwitchStatement;

    class ForStatement;

    class HeadWhileStatement;

    class FootWhileStatement;

    class GotoStatement;

    class ContinueStatement;

    class BreakStatement;

    class ReturnStatement;

    class ExpressionStatement;

    using Statement = std::variant<ReturnStatement, ExpressionStatement, IfStatement, CompoundStatement, ForStatement,
                                   HeadWhileStatement, FootWhileStatement, BreakStatement, ContinueStatement,
                                   SwitchStatement, DefaultStatement, CaseStatement, GotoStatement, LabelStatement>;

    class PrimitiveType final
    {
        bool m_isFloatingPoint;
        bool m_isSigned;
        std::uint8_t m_bitCount;

        PrimitiveType(bool isFloatingPoint, bool isSigned, std::uint8_t bitCount);

    public:
        static Type create(bool isConst, bool isVolatile, bool isFloatingPoint, bool isSigned, std::uint8_t bitCount);

        static Type createChar(bool isConst, bool isVolatile);

        static Type createUnsignedChar(bool isConst, bool isVolatile);

        static Type createShort(bool isConst, bool isVolatile);

        static Type createUnsignedShort(bool isConst, bool isVolatile);

        static Type createInt(bool isConst, bool isVolatile);

        static Type createUnsignedInt(bool isConst, bool isVolatile);

        static Type createLongLong(bool isConst, bool isVolatile);

        static Type createUnsignedLongLong(bool isConst, bool isVolatile);

        static Type createFloat(bool isConst, bool isVolatile);

        static Type createDouble(bool isConst, bool isVolatile);

        static Type createVoid(bool isConst, bool isVolatile);

        [[nodiscard]] bool isFloatingPoint() const;

        [[nodiscard]] bool isSigned() const;

        [[nodiscard]] std::uint8_t getBitCount() const;

        bool operator==(const PrimitiveType& rhs) const;

        bool operator!=(const PrimitiveType& rhs) const;
    };

    class ArrayType final
    {
        bool m_restricted;
        std::shared_ptr<const Type> m_type;
        std::size_t m_size;

        ArrayType(bool isRestricted, std::shared_ptr<Type>&& type, std::size_t size);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& type, std::size_t size);

        [[nodiscard]] const Type& getType() const;

        [[nodiscard]] std::size_t getSize() const;

        [[nodiscard]] bool isRestricted() const;

        bool operator==(const ArrayType& rhs) const;

        bool operator!=(const ArrayType& rhs) const;
    };

    class AbstractArrayType final
    {
        bool m_restricted;
        std::shared_ptr<const Type> m_type;

        AbstractArrayType(bool isRestricted, std::shared_ptr<Type>&& type);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& type);

        [[nodiscard]] const Type& getType() const;

        [[nodiscard]] bool isRestricted() const;

        bool operator==(const AbstractArrayType& rhs) const;

        bool operator!=(const AbstractArrayType& rhs) const;
    };

    class ValArrayType final
    {
        bool m_restricted;
        std::shared_ptr<const Type> m_type;

        ValArrayType(bool isRestricted, std::shared_ptr<OpenCL::Semantics::Type>&& type);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& type);

        [[nodiscard]] const Type& getType() const;

        [[nodiscard]] bool isRestricted() const;

        bool operator==(const ValArrayType& rhs) const;

        bool operator!=(const ValArrayType& rhs) const;
    };

    class FunctionType final
    {
        std::shared_ptr<const Type> m_returnType;
        std::vector<std::pair<Type, std::string>> m_arguments;
        bool m_lastIsVararg;
        bool m_hasPrototype;

        FunctionType(std::shared_ptr<Type>&& returnType, std::vector<std::pair<Type, std::string>> arguments,
                     bool lastIsVararg, bool hasPrototype);

    public:
        static Type create(OpenCL::Semantics::Type&& returnType, std::vector<std::pair<Type, std::string>>&& arguments,
                           bool lastIsVararg, bool hasPrototype);

        [[nodiscard]] const Type& getReturnType() const;

        [[nodiscard]] const std::vector<std::pair<Type, std::string>>& getArguments() const;

        [[nodiscard]] bool isLastVararg() const;

        [[nodiscard]] bool hasPrototype() const;

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

        [[nodiscard]] const std::string& getName() const;

        [[nodiscard]] bool isUnion() const;

        [[nodiscard]] const std::vector<std::tuple<Type, std::string, std::int64_t>>& getMembers() const;

        [[nodiscard]] bool isDefinition() const;

        bool operator==(const RecordType& rhs) const;

        bool operator!=(const RecordType& rhs) const;
    };

    class EnumType final
    {
        std::string m_name;
        std::vector<std::pair<std::string, std::int32_t>> m_values;

        EnumType(std::string name, std::vector<std::pair<std::string, std::int32_t>> values);

    public:
        static Type create(bool isConst, bool isVolatile, const std::string& name,
                           std::vector<std::pair<std::string, std::int32_t>> values);

        [[nodiscard]] const std::vector<std::pair<std::string, int32_t>>& getValues() const;

        [[nodiscard]] bool isDefinition() const;

        [[nodiscard]] bool isAnonymous() const;

        [[nodiscard]] const std::string& getName() const;

        bool operator==(const EnumType& rhs) const;

        bool operator!=(const EnumType& rhs) const;
    };

    class PointerType final
    {
        bool m_restricted;
        std::shared_ptr<const Type> m_elementType;

        PointerType(bool isRestricted, std::shared_ptr<Type>&& elementType);

    public:
        static Type create(bool isConst, bool isVolatile, bool isRestricted, Type&& elementType);

        [[nodiscard]] const Type& getElementType() const;

        [[nodiscard]] bool isRestricted() const;

        bool operator==(const PointerType& rhs) const;

        bool operator!=(const PointerType& rhs) const;
    };

    class Type final
    {
        bool m_isConst;
        bool m_isVolatile;
        std::string m_name;
        std::string m_typeName;
        using variant = std::variant<std::monostate, PrimitiveType, ArrayType, AbstractArrayType, ValArrayType,
                                     FunctionType, RecordType, EnumType, PointerType>;

        variant m_type;

    public:
        explicit Type(bool isConst = false, bool isVolatile = false, std::string name = "<undefined>",
                      variant&& type = std::monostate{});

        [[nodiscard]] const variant& get() const;

        [[nodiscard]] bool isConst() const;

        [[nodiscard]] bool isVolatile() const;

        [[nodiscard]] const std::string& getName() const;

        void setName(const std::string& name);

        [[nodiscard]] const std::string& getTypeName() const;

        [[nodiscard]] bool isTypedef() const;

        [[nodiscard]] std::string getFullFormattedTypeName() const;

        bool operator==(const Type& rhs) const;

        bool operator!=(const Type& rhs) const;

        [[nodiscard]] bool isUndefined() const;
    };

    enum class Linkage
    {
        Internal,
        External,
        None
    };

    enum class Lifetime
    {
        Automatic,
        Static,
        Register,
        Temporary
    };

    class Declaration final
    {
        Type m_type;
        Linkage m_linkage;
        Lifetime m_lifetime;
        std::string m_name;
        // initializer

    public:
        Declaration(Type type, Linkage linkage, Lifetime lifetime, std::string name);

        [[nodiscard]] const Type& getType() const;

        [[nodiscard]] Linkage getLinkage() const;

        [[nodiscard]] Lifetime getLifetime() const;

        [[nodiscard]] const std::string& getName() const;
    };

    class ReturnStatement final
    {
    };

    class ExpressionStatement final
    {
    };

    class IfStatement final
    {
    };

    class CompoundStatement final
    {
        std::vector<std::variant<Statement, Declaration>> m_compoundItems;

    public:
        explicit CompoundStatement(std::vector<std::variant<Statement, Declaration>> compoundItems);

        [[nodiscard]] const std::vector<std::variant<Statement, Declaration>>& getCompoundItems() const;
    };

    class ForStatement final
    {
    };

    class HeadWhileStatement final
    {
    };

    class FootWhileStatement final
    {
    };

    class BreakStatement final
    {
    };

    class ContinueStatement final
    {
    };

    class SwitchStatement final
    {
    };

    class DefaultStatement final
    {
    };

    class CaseStatement final
    {
    };

    class GotoStatement final
    {
    };

    class LabelStatement final
    {
    };

    class FunctionDefinition final
    {
        FunctionType m_type;
        std::string m_name;
        std::vector<Declaration> m_parameterDeclarations;
        Linkage m_linkage;
        CompoundStatement m_compoundStatement;

    public:
        FunctionDefinition(FunctionType type, std::string name, std::vector<Declaration> parameterDeclarations,
                           Linkage linkage, CompoundStatement&& compoundStatement);

        [[nodiscard]] const std::string& getName() const;

        [[nodiscard]] const FunctionType& getType() const;

        [[nodiscard]] const std::vector<Declaration>& getParameterDeclarations() const;

        [[nodiscard]] bool hasPrototype() const;

        [[nodiscard]] Linkage getLinkage() const;
    };

    class TranslationUnit final
    {
    public:
        using Variant = std::variant<FunctionDefinition, Declaration>;

    private:
        std::vector<Variant> m_globals;

    public:
        explicit TranslationUnit(std::vector<Variant> globals);

        [[nodiscard]] const std::vector<Variant>& getGlobals() const;
    };

    using DeclarationTypedefEnums = std::variant<Semantics::Declaration, Semantics::Type, std::int32_t>;

    std::string declaratorToName(const OpenCL::Syntax::Declarator& declarator);

    std::vector<Lexer::Token>::const_iterator declaratorToLoc(const OpenCL::Syntax::Declarator& declarator);

    OpenCL::Expected<std::size_t, std::string> sizeOf(const Type& type);

    OpenCL::Expected<std::size_t, std::string> alignmentOf(const Type& type);

    bool isVoid(const Type& type);
} // namespace OpenCL::Semantics

#endif // OPENCLPARSER_SEMANTICS_HPP
