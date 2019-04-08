#ifndef OPENCLPARSER_CODEGEN_HPP
#define OPENCLPARSER_CODEGEN_HPP

#include "Syntax.hpp"
#include <map>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DIBuilder.h>

namespace OpenCL::Codegen
{
    using NodeRetType = std::pair<llvm::Value*,std::shared_ptr<Syntax::IType>>;

    using TypeRetType = llvm::Type*;

    class Context final : public OpenCL::Syntax::NodeVisitor<NodeRetType,TypeRetType>
    {
        using tuple = std::pair<llvm::Value*, std::shared_ptr<OpenCL::Syntax::IType>>;

        struct Function
        {
            std::shared_ptr<OpenCL::Syntax::IType> retType;
            std::vector<const OpenCL::Syntax::IType*> arguments;
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
        llvm::Function* currentFunction;
        const OpenCL::Syntax::IType* functionRetType = nullptr;
        std::vector<llvm::BasicBlock*> continueBlocks;
        std::vector<llvm::BasicBlock*> breakBlocks;
        std::vector<std::pair<llvm::SwitchInst*, bool>> switchStack;
        std::vector<llvm::DIScope*> debugScope;

        struct StructOrUnion
        {
            std::map<std::string, std::uint64_t> order;
            std::vector<std::shared_ptr<OpenCL::Syntax::IType>> types;
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

        tuple getNamedValue(const std::string& name) const
        {
            for (auto begin = m_namedValues.rbegin(); begin != m_namedValues.rend(); begin++)
            {
                if (auto result = begin->find(name);result != begin->end())
                {
                    return result->second;
                }
            }
            auto result = m_globalValues.find(name);
            return result != m_globalValues.end() ? result->second : tuple{};
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

        void addGlobal(const std::string& name, const tuple& value)
        {
            auto[it, ins] = m_globalValues.insert({name, value});
            if (!ins)
            {
                throw std::runtime_error("Redefinition of global symbol " + name);
            }
        }

        void addFunction(const std::string& name, const Function& function)
        {
            m_functions[name] = function;
        }

        void clearScope()
        {
            m_namedValues.clear();
            pushScope();
        }

        void visit(const Syntax::Expression& node) override;

        void visit(const Syntax::PrimaryExpressionIdentifier& node) override;

        void visit(const Syntax::PrimaryExpressionConstant& node) override;

        void visit(const Syntax::PrimaryExpressionParenthese& node) override;

        void visit(const Syntax::PostFixExpressionPrimaryExpression& node) override;

        void visit(const Syntax::PostFixExpressionSubscript& node) override;

        void visit(const Syntax::PostFixExpressionIncrement& node) override;

        void visit(const Syntax::PostFixExpressionDecrement& node) override;

        void visit(const Syntax::PostFixExpressionDot& node) override;

        void visit(const Syntax::PostFixExpressionArrow& node) override;

        void visit(const Syntax::PostFixExpressionFunctionCall& node) override;

        void visit(const Syntax::PostFixExpressionTypeInitializer& node) override;

        void visit(const Syntax::AssignmentExpressionAssignment& node) override;

        void visit(const Syntax::UnaryExpressionPostFixExpression& node) override;

        void visit(const Syntax::UnaryExpressionUnaryOperator& node) override;

        void visit(const Syntax::UnaryExpressionSizeOf& node) override;

        void visit(const Syntax::CastExpression& node) override;

        void visit(const Syntax::Term& node) override;

        void visit(const Syntax::AdditiveExpression& node) override;

        void visit(const Syntax::ShiftExpression& node) override;

        void visit(const Syntax::RelationalExpression& node) override;

        void visit(const Syntax::EqualityExpression& node) override;

        void visit(const Syntax::BitAndExpression& node) override;

        void visit(const Syntax::BitXorExpression& node) override;

        void visit(const Syntax::BitOrExpression& node) override;

        void visit(const Syntax::LogicalAndExpression& node) override;

        void visit(const Syntax::LogicalOrExpression& node) override;

        void visit(const Syntax::ConditionalExpression& node) override;

        void visit(const Syntax::ReturnStatement& node) override;

        void visit(const Syntax::ExpressionStatement& node) override;

        void visit(const Syntax::IfStatement& node) override;

        void visit(const Syntax::SwitchStatement& node) override;

        void visit(const Syntax::DefaultStatement& node) override;

        void visit(const Syntax::CaseStatement& node) override;

        void visit(const Syntax::BlockStatement& node) override;

        void visit(const Syntax::ForStatement& node) override;

        void visit(const Syntax::InitializerList& node) override;

        void visit(const Syntax::Declarations& node) override;

        void visit(const Syntax::ForDeclarationStatement& node) override;

        void visit(const Syntax::HeadWhileStatement& node) override;

        void visit(const Syntax::FootWhileStatement& node) override;

        void visit(const Syntax::BreakStatement& node) override;

        void visit(const Syntax::ContinueStatement& node) override;

        void visit(const Syntax::StructOrUnionDeclaration& node) override;

        void visit(const Syntax::EnumDeclaration& node) override;

        void visit(const Syntax::TypedefDeclaration& node) override;

        void visit(const Syntax::Function& node) override;

        void visit(const Syntax::GlobalDeclaration& node) override;

        void visit(const Syntax::Program& node) override;

        void visit(const Syntax::PrimitiveType& node) override;

        void visit(const Syntax::PointerType& node) override;

        void visit(const Syntax::ArrayType& node) override;

        void visit(const Syntax::StructType& node) override;

        void visit(const Syntax::UnionType& node) override;

        void visit(const Syntax::EnumType& node) override;
    };
}

#endif //OPENCLPARSER_CODEGEN_HPP
