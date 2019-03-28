#ifndef OPENCLPARSER_CODEGEN_HPP
#define OPENCLPARSER_CODEGEN_HPP

#include "Syntax.hpp"

namespace OpenCL::Codegen
{
    class CodegenVisitor final : public Syntax::NodeVisitor
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

    public:

        void visit(Syntax::Expression& node) override;

        void visit(Syntax::PrimaryExpressionIdentifier& node) override;

        void visit(Syntax::PrimaryExpressionConstant& node) override;

        void visit(Syntax::PrimaryExpressionParenthese& node) override;

        void visit(Syntax::PrimaryExpression& node) override;

        void visit(Syntax::PostFixExpressionPrimaryExpression& node) override;

        void visit(Syntax::PostFixExpressionSubscript& node) override;

        void visit(Syntax::PostFixExpressionIncrement& node) override;

        void visit(Syntax::PostFixExpressionDecrement& node) override;

        void visit(Syntax::PostFixExpressionDot& node) override;

        void visit(Syntax::PostFixExpressionArrow& node) override;

        void visit(Syntax::PostFixExpressionFunctionCall& node) override;

        void visit(Syntax::PostFixExpressionTypeInitializer& node) override;

        void visit(Syntax::PostFixExpression& node) override;

        void visit(Syntax::AssignmentExpression& node) override;

        void visit(Syntax::UnaryExpressionPostFixExpression& node) override;

        void visit(Syntax::UnaryExpressionUnaryOperator& node) override;

        void visit(Syntax::UnaryExpressionSizeOf& node) override;

        void visit(Syntax::UnaryExpression& node) override;

        void visit(Syntax::CastExpression& node) override;

        void visit(Syntax::Term& node) override;

        void visit(Syntax::AdditiveExpression& node) override;

        void visit(Syntax::ShiftExpression& node) override;

        void visit(Syntax::RelationalExpression& node) override;

        void visit(Syntax::EqualityExpression& node) override;

        void visit(Syntax::BitAndExpression& node) override;

        void visit(Syntax::BitXorExpression& node) override;

        void visit(Syntax::BitOrExpression& node) override;

        void visit(Syntax::LogicalAndExpression& node) override;

        void visit(Syntax::LogicalOrExpression& node) override;

        void visit(Syntax::ConditionalExpression& node) override;

        void visit(Syntax::NonCommaExpression& node) override;

        void visit(Syntax::ReturnStatement& node) override;

        void visit(Syntax::ExpressionStatement& node) override;

        void visit(Syntax::IfStatement& node) override;

        void visit(Syntax::SwitchStatement& node) override;

        void visit(Syntax::DefaultStatement& node) override;

        void visit(Syntax::CaseStatement& node) override;

        void visit(Syntax::BlockStatement& node) override;

        void visit(Syntax::ForStatement& node) override;

        void visit(Syntax::InitializerListScalarExpression& node) override;

        void visit(Syntax::InitializerListBlock& node) override;

        void visit(Syntax::InitializerList& node) override;

        void visit(Syntax::Declarations& node) override;

        void visit(Syntax::BlockItem& node) override;

        void visit(Syntax::ForDeclarationStatement& node) override;

        void visit(Syntax::HeadWhileStatement& node) override;

        void visit(Syntax::FootWhileStatement& node) override;

        void visit(Syntax::BreakStatement& node) override;

        void visit(Syntax::ContinueStatement& node) override;

        void visit(Syntax::Statement& node) override;

        void visit(Syntax::StructOrUnionDeclaration& node) override;

        void visit(Syntax::EnumDeclaration& node) override;

        void visit(Syntax::TypedefDeclaration& node) override;

        void visit(Syntax::Function& node) override;

        void visit(Syntax::GlobalDeclaration& node) override;

        void visit(Syntax::Global& node) override;

        void visit(Syntax::Program& node) override;
    };
}

#endif //OPENCLPARSER_CODEGEN_HPP
