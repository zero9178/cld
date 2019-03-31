#ifndef OPENCLPARSER_CODEGEN_HPP
#define OPENCLPARSER_CODEGEN_HPP

#include "Syntax.hpp"

namespace OpenCL::Codegen
{
    class Context final : public OpenCL::Syntax::NodeVisitor
    {
        using tuple = std::pair<llvm::Value*, std::shared_ptr<OpenCL::Syntax::Type>>;

        struct Function
        {
            std::shared_ptr<OpenCL::Syntax::Type> retType;
            std::vector<const OpenCL::Syntax::Type*> arguments;
        };

        std::map<std::string, Function> m_functions;
        std::map<std::string, tuple> m_globalValues;
        std::vector<std::map<std::string, tuple>> m_namedValues;

    public:

        llvm::LLVMContext context;
        llvm::IRBuilder<> builder{context};
        llvm::DIBuilder* debugBuilder;
        llvm::DIFile* debugUnit = nullptr;
        std::unique_ptr<llvm::Module> module;
        llvm::Function* currentFunction;
        const OpenCL::Syntax::Type* functionRetType = nullptr;
        std::vector<llvm::BasicBlock*> continueBlocks;
        std::vector<llvm::BasicBlock*> breakBlocks;
        std::vector<std::pair<llvm::SwitchInst*, bool>> switchStack;
        std::vector<llvm::DIScope*> debugScope;

        struct StructOrUnion
        {
            std::map<std::string, std::uint64_t> order;
            std::vector<std::shared_ptr<OpenCL::Syntax::Type>> types;
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

        using NodeRetType = OpenCL::Syntax::Node<void>::retType;

        NodeRetType* visit(const Syntax::Expression& node) override;

        NodeRetType* visit(const Syntax::PrimaryExpressionIdentifier& node) override;

        NodeRetType* visit(const Syntax::PrimaryExpressionConstant& node) override;

        NodeRetType* visit(const Syntax::PrimaryExpressionParenthese& node) override;

        NodeRetType* visit(const Syntax::PrimaryExpression& node) override;

        NodeRetType* visit(const Syntax::PostFixExpressionPrimaryExpression& node) override;

        NodeRetType* visit(const Syntax::PostFixExpressionSubscript& node) override;

        NodeRetType* visit(const Syntax::PostFixExpressionIncrement& node) override;

        NodeRetType* visit(const Syntax::PostFixExpressionDecrement& node) override;

        NodeRetType* visit(const Syntax::PostFixExpressionDot& node) override;

        NodeRetType* visit(const Syntax::PostFixExpressionArrow& node) override;

        NodeRetType* visit(const Syntax::PostFixExpressionFunctionCall& node) override;

        NodeRetType* visit(const Syntax::PostFixExpressionTypeInitializer& node) override;

        NodeRetType* visit(const Syntax::PostFixExpression& node) override;

        NodeRetType* visit(const Syntax::AssignmentExpression& node) override;

        NodeRetType* visit(const Syntax::UnaryExpressionPostFixExpression& node) override;

        NodeRetType* visit(const Syntax::UnaryExpressionUnaryOperator& node) override;

        NodeRetType* visit(const Syntax::UnaryExpressionSizeOf& node) override;

        NodeRetType* visit(const Syntax::UnaryExpression& node) override;

        NodeRetType* visit(const Syntax::CastExpression& node) override;

        NodeRetType* visit(const Syntax::Term& node) override;

        NodeRetType* visit(const Syntax::AdditiveExpression& node) override;

        NodeRetType* visit(const Syntax::ShiftExpression& node) override;

        NodeRetType* visit(const Syntax::RelationalExpression& node) override;

        NodeRetType* visit(const Syntax::EqualityExpression& node) override;

        NodeRetType* visit(const Syntax::BitAndExpression& node) override;

        NodeRetType* visit(const Syntax::BitXorExpression& node) override;

        NodeRetType* visit(const Syntax::BitOrExpression& node) override;

        NodeRetType* visit(const Syntax::LogicalAndExpression& node) override;

        NodeRetType* visit(const Syntax::LogicalOrExpression& node) override;

        NodeRetType* visit(const Syntax::ConditionalExpression& node) override;

        NodeRetType* visit(const Syntax::NonCommaExpression& node) override;

        NodeRetType* visit(const Syntax::ReturnStatement& node) override;

        NodeRetType* visit(const Syntax::ExpressionStatement& node) override;

        NodeRetType* visit(const Syntax::IfStatement& node) override;

        NodeRetType* visit(const Syntax::SwitchStatement& node) override;

        NodeRetType* visit(const Syntax::DefaultStatement& node) override;

        NodeRetType* visit(const Syntax::CaseStatement& node) override;

        NodeRetType* visit(const Syntax::BlockStatement& node) override;

        NodeRetType* visit(const Syntax::ForStatement& node) override;

        NodeRetType* visit(const Syntax::InitializerListScalarExpression& node) override;

        NodeRetType* visit(const Syntax::InitializerListBlock& node) override;

        NodeRetType* visit(const Syntax::InitializerList& node) override;

        NodeRetType* visit(const Syntax::Declarations& node) override;

        NodeRetType* visit(const Syntax::BlockItem& node) override;

        NodeRetType* visit(const Syntax::ForDeclarationStatement& node) override;

        NodeRetType* visit(const Syntax::HeadWhileStatement& node) override;

        NodeRetType* visit(const Syntax::FootWhileStatement& node) override;

        NodeRetType* visit(const Syntax::BreakStatement& node) override;

        NodeRetType* visit(const Syntax::ContinueStatement& node) override;

        NodeRetType* visit(const Syntax::Statement& node) override;

        NodeRetType* visit(const Syntax::StructOrUnionDeclaration& node) override;

        NodeRetType* visit(const Syntax::EnumDeclaration& node) override;

        NodeRetType* visit(const Syntax::TypedefDeclaration& node) override;

        NodeRetType* visit(const Syntax::Function& node) override;

        NodeRetType* visit(const Syntax::GlobalDeclaration& node) override;

        NodeRetType* visit(const Syntax::Global& node) override;

        NodeRetType* visit(const Syntax::Program& node) override;
    };
}

#endif //OPENCLPARSER_CODEGEN_HPP
