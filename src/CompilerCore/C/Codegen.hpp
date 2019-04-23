#ifndef OPENCLPARSER_CODEGEN_HPP
#define OPENCLPARSER_CODEGEN_HPP

#include "Syntax.hpp"
#include "Expected.hpp"
#include "FailureReason.hpp"
#include <map>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DIBuilder.h>

namespace OpenCL::Codegen
{
    class Type
    {
        bool m_isConst;
        bool m_isVolatile;
        bool m_isRestricted;
        std::string m_name;

    public:

        Type(bool isConst, bool isVolatile, bool isRestricted);

        bool isConst() const;

        bool isVolatile() const;

        bool isRestricted() const;

        const std::string& getName() const;

        void setName(const std::string& name);
    };

    class PrimitiveType final : public Type
    {
    public:

        enum class Primitive
        {
            Void,
            Char,
            Short,
            Int,
            Long,
            Float,
            Double,
            Signed,
            Unsigned,
        };

    private:

        Primitive m_primitive;

    public:

        PrimitiveType(bool isConst, bool isVolatile, bool isRestricted, Primitive primitive);

        Primitive getPrimitive() const;
    };

    class ArrayType final : public Type
    {
        std::shared_ptr<Type> m_type;
        std::size_t m_size;

    public:

        ArrayType(bool isConst,
                  bool isVolatile,
                  bool isRestricted,
                  std::shared_ptr<Type>  type,
                  std::size_t size);

        const Type& getType() const;

        std::size_t getSize() const;
    };

    class StructType final : public Type
    {
    public:

        StructType(bool isConst, bool isVolatile, bool isRestricted, const std::string& name);
    };

    class UnionType final : public Type
    {
    public:

        UnionType(bool isConst, bool isVolatile, bool isRestricted, const std::string& name);
    };

    class EnumType final : public Type
    {
    public:

        EnumType(bool isConst, bool isVolatile, bool isRestricted, const std::string& name);
    };

    class PointerType final : public Type
    {
        std::shared_ptr<Type> m_elementType;

    public:

        PointerType(bool isConst, bool isVolatile, bool isRestricted, std::shared_ptr<Type>  elementType);

        const Type& getElementType() const;
    };

    OpenCL::Expected<std::shared_ptr<Type>, OpenCL::FailureReason> declaratorsToType(std::vector<Syntax::SpecifierQualifier> specifierQualifiers,
                                                                                     std::variant<const Syntax::AbstractDeclarator*,
                                                                                                  const Syntax::Declarator*> declarator);

    using NodeRetType = std::pair<llvm::Value*, std::shared_ptr<Type>>;

    using TypeRetType = llvm::Type*;

    class Context final : public OpenCL::Syntax::NodeVisitor<NodeRetType, TypeRetType>
    {
        using tuple = std::pair<llvm::Value*, std::shared_ptr<Type>>;

        struct Function
        {
            std::shared_ptr<Type> retType;
            std::vector<std::shared_ptr<Type>> arguments;
        };

        std::map<std::string, Function> m_functions;
        std::map<std::string, tuple> m_globalValues;
        std::vector<std::map<std::string, tuple>> m_namedValues;

        void doForLoop(const OpenCL::Syntax::Expression* controlling,
                       const OpenCL::Syntax::Expression* post,
                       const OpenCL::Syntax::Statement& statement);

    public:

        llvm::LLVMContext context;
        llvm::IRBuilder<> builder{context};
        llvm::DIBuilder* debugBuilder;
        llvm::DIFile* debugUnit = nullptr;
        std::unique_ptr<llvm::Module> module;
        std::vector<llvm::BasicBlock*> continueBlocks;
        std::vector<llvm::BasicBlock*> breakBlocks;
        std::vector<std::pair<llvm::SwitchInst*, bool>> switchStack;
        std::vector<llvm::DIScope*> debugScope;

        struct StructOrUnion
        {
            std::map<std::string, std::uint64_t> order;
            std::vector<Syntax::TypeName> types;
            bool isUnion = false;
        };

        std::map<std::string, StructOrUnion> structs;

        bool hasFunction(const std::string& name) const
        {
            return m_functions.find(name) != m_functions.end();
        }

        const Function& getFunction(const std::string& name) const
        {
            return m_functions.at(name);
        }

        const tuple* getNamedValue(const std::string& name) const
        {
            for (auto begin = m_namedValues.rbegin(); begin != m_namedValues.rend(); begin++)
            {
                if (auto result = begin->find(name);result != begin->end())
                {
                    return &result->second;
                }
            }
            auto result = m_globalValues.find(name);
            return result != m_globalValues.end() ? &result->second : nullptr;
        }

        void popScope()
        {
            m_namedValues.pop_back();
        }

        void pushScope()
        {
            m_namedValues.emplace_back();
        }

        void addValueToScope(const std::string& name, const tuple& value);

        void addGlobal(const std::string& name, tuple&& value)
        {
            auto[it, ins] = m_globalValues.insert({name, std::move(value)});
            if (!ins)
            {
                throw std::runtime_error("Redefinition of global symbol " + name);
            }
        }

        void addFunction(const std::string& name, Function&& function)
        {
            m_functions[name] = std::move(function);
        }

        void clearScope()
        {
            m_namedValues.clear();
            pushScope();
        }

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::Expression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PrimaryExpressionIdentifier& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PrimaryExpressionConstant& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PrimaryExpressionParenthese& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PostFixExpressionPrimaryExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PostFixExpressionSubscript& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PostFixExpressionIncrement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PostFixExpressionDecrement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PostFixExpressionDot& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PostFixExpressionArrow& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PostFixExpressionFunctionCall& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::PostFixExpressionTypeInitializer& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::AssignmentExpressionAssignment& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::UnaryExpressionPostFixExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::UnaryExpressionUnaryOperator& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::UnaryExpressionSizeOf& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::CastExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::Term& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::AdditiveExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::ShiftExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::RelationalExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::EqualityExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::BitAndExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::BitXorExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::BitOrExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::LogicalAndExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::LogicalOrExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::ConditionalExpression& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::ReturnStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::ExpressionStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::IfStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::SwitchStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::DefaultStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::CaseStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::CompoundStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::ForStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::InitializerList& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::Declaration& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::ForDeclarationStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::HeadWhileStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::FootWhileStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::BreakStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::ContinueStatement& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::StructOrUnionSpecifier& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::EnumSpecifier& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::FunctionDefinition& node) override;

        Syntax::StrongTypedef<NodeRetType>& visit(const Syntax::TranslationUnit& node) override;

        ReturnType& visit(const Syntax::PrimaryExpression& node) override;

        ReturnType& visit(const Syntax::PostFixExpression& node) override;

        ReturnType& visit(const Syntax::UnaryExpression& node) override;

        ReturnType& visit(const Syntax::AssignmentExpression& node) override;

        ReturnType& visit(const Syntax::Initializer& node) override;

        ReturnType& visit(const Syntax::CompoundItem& node) override;

        ReturnType& visit(const Syntax::Statement& node) override;

        ReturnType& visit(const Syntax::ExternalDeclaration& node) override;

        ReturnType& visit(const Syntax::TypeName& node) override;

        ReturnType& visit(const Syntax::Declarator& node) override;

        ReturnType& visit(const Syntax::EnumDeclaration& node) override;

        ReturnType& visit(const Syntax::TypeSpecifier& node) override;

        ReturnType& visit(const Syntax::DirectDeclarator& node) override;

        ReturnType& visit(const Syntax::DirectDeclaratorNoStaticOrAsterisk& node) override;

        ReturnType& visit(const Syntax::DirectDeclaratorStatic& node) override;

        ReturnType& visit(const Syntax::DirectDeclaratorAsterisk& node) override;

        ReturnType& visit(const Syntax::DirectDeclaratorParentheseParameters& node) override;

        ReturnType& visit(const Syntax::DirectDeclaratorParentheseIdentifiers& node) override;

        ReturnType& visit(const Syntax::DirectAbstractDeclarator& node) override;

        ReturnType& visit(const Syntax::DirectAbstractDeclaratorParameterTypeList& node) override;

        ReturnType& visit(const Syntax::DirectAbstractDeclaratorAssignmentExpression& node) override;

        ReturnType& visit(const Syntax::Pointer& node) override;

        ReturnType& visit(const Syntax::ParameterTypeList& node) override;

        ReturnType& visit(const Syntax::ParameterList& node) override;

        ReturnType& visit(const Syntax::LabelStatement& node) override;

        ReturnType& visit(const Syntax::GotoStatement& node) override;
    };
}

#endif //OPENCLPARSER_CODEGEN_HPP
