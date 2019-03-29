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

        retType visit(const Syntax::Expression& node) override;

        retType visit(const Syntax::PrimaryExpressionIdentifier& node) override;

        retType visit(const Syntax::PrimaryExpressionConstant& node) override;

        retType visit(const Syntax::PrimaryExpressionParenthese& node) override;

        retType visit(const Syntax::PrimaryExpression& node) override;

        retType visit(const Syntax::PostFixExpressionPrimaryExpression& node) override;

        retType visit(const Syntax::PostFixExpressionSubscript& node) override;

        retType visit(const Syntax::PostFixExpressionIncrement& node) override;

        retType visit(const Syntax::PostFixExpressionDecrement& node) override;

        retType visit(const Syntax::PostFixExpressionDot& node) override;

        retType visit(const Syntax::PostFixExpressionArrow& node) override;

        retType visit(const Syntax::PostFixExpressionFunctionCall& node) override;

        retType visit(const Syntax::PostFixExpressionTypeInitializer& node) override;

        retType visit(const Syntax::PostFixExpression& node) override;

        retType visit(const Syntax::AssignmentExpression& node) override;

        retType visit(const Syntax::UnaryExpressionPostFixExpression& node) override;

        retType visit(const Syntax::UnaryExpressionUnaryOperator& node) override;

        retType visit(const Syntax::UnaryExpressionSizeOf& node) override;

        retType visit(const Syntax::UnaryExpression& node) override;

        retType visit(const Syntax::CastExpression& node) override;

        retType visit(const Syntax::Term& node) override;

        retType visit(const Syntax::AdditiveExpression& node) override;

        retType visit(const Syntax::ShiftExpression& node) override;

        retType visit(const Syntax::RelationalExpression& node) override;

        retType visit(const Syntax::EqualityExpression& node) override;

        retType visit(const Syntax::BitAndExpression& node) override;

        retType visit(const Syntax::BitXorExpression& node) override;

        retType visit(const Syntax::BitOrExpression& node) override;

        retType visit(const Syntax::LogicalAndExpression& node) override;

        retType visit(const Syntax::LogicalOrExpression& node) override;

        retType visit(const Syntax::ConditionalExpression& node) override;

        retType visit(const Syntax::NonCommaExpression& node) override;

        retType visit(const Syntax::ReturnStatement& node) override;

        retType visit(const Syntax::ExpressionStatement& node) override;

        retType visit(const Syntax::IfStatement& node) override;

        retType visit(const Syntax::SwitchStatement& node) override;

        retType visit(const Syntax::DefaultStatement& node) override;

        retType visit(const Syntax::CaseStatement& node) override;

        retType visit(const Syntax::BlockStatement& node) override;

        retType visit(const Syntax::ForStatement& node) override;

        retType visit(const Syntax::InitializerListScalarExpression& node) override;

        retType visit(const Syntax::InitializerListBlock& node) override;

        retType visit(const Syntax::InitializerList& node) override;

        retType visit(const Syntax::Declarations& node) override;

        retType visit(const Syntax::BlockItem& node) override;

        retType visit(const Syntax::ForDeclarationStatement& node) override;

        retType visit(const Syntax::HeadWhileStatement& node) override;

        retType visit(const Syntax::FootWhileStatement& node) override;

        retType visit(const Syntax::BreakStatement& node) override;

        retType visit(const Syntax::ContinueStatement& node) override;

        retType visit(const Syntax::Statement& node) override;

        retType visit(const Syntax::StructOrUnionDeclaration& node) override;

        retType visit(const Syntax::EnumDeclaration& node) override;

        retType visit(const Syntax::TypedefDeclaration& node) override;

        retType visit(const Syntax::Function& node) override;

        retType visit(const Syntax::GlobalDeclaration& node) override;

        retType visit(const Syntax::Global& node) override;

        retType visit(const Syntax::Program& node) override;
    };
}

#endif //OPENCLPARSER_CODEGEN_HPP
