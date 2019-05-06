#ifndef OPENCLPARSER_CODEGEN_HPP
#define OPENCLPARSER_CODEGEN_HPP

#include "Expected.hpp"
#include "FailureReason.hpp"
#include "Representations.hpp"
#include "Syntax.hpp"

#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>
#include <map>

namespace OpenCL::Codegen
{
    using NodeRetType = Expected<std::pair<llvm::Value*, Representations::Type>, FailureReason>;

    using TypeRetType = llvm::Type*;

    class Context final
    {
        std::vector<std::map<std::string, std::pair<llvm::Value*, Representations::Type>>> m_namedValues{1};
        std::vector<std::map<std::string, Representations::Type>> m_structsUnions{1};
        std::vector<std::map<std::string, Representations::Type>> m_typedefs{1};

        llvm::LLVMContext context;
        llvm::IRBuilder<> builder{context};
        llvm::DIBuilder* debugBuilder;
        llvm::DIFile* debugUnit = nullptr;
        std::vector<llvm::BasicBlock*> continueBlocks;
        std::vector<llvm::BasicBlock*> breakBlocks;
        std::vector<std::pair<llvm::SwitchInst*, bool>> switchStack;
        std::vector<llvm::DIScope*> debugScope;
        const Representations::FunctionType* currentFunction;

        llvm::Constant* createZeroValue(llvm::Type* type);

        std::map<std::string, std::reference_wrapper<const Representations::Type>> gatherTypedefs() const;

        std::map<std::string, Representations::RecordType> gatherStructsAndUnions() const;

        void popScope()
        {
            m_namedValues.pop_back();
            m_structsUnions.pop_back();
            m_typedefs.pop_back();
        }

        const std::pair<llvm::Value*, Representations::Type>* findValue(const std::string& name) const
        {
            for (auto iter = m_namedValues.rbegin(); iter != m_namedValues.rend(); iter++)
            {
                auto result = iter->find(name);
                if (result != iter->end())
                {
                    return &result->second;
                }
            }
            return nullptr;
        }

        const Representations::Type* findStructUnionOrEnum(const std::string& name) const
        {
            for (auto iter = m_structsUnions.rbegin(); iter != m_structsUnions.rend(); iter++)
            {
                auto result = iter->find(name);
                if (result != iter->end())
                {
                    return &result->second;
                }
            }
            return nullptr;
        }

        void pushScope()
        {
            m_namedValues.emplace_back();
            m_structsUnions.emplace_back();
            m_typedefs.emplace_back();
        }

        void clearScope()
        {
            m_namedValues.clear();
            pushScope();
        }

        bool inGlobalScope() const
        {
            return m_namedValues.size() == 1;
        }

        NodeRetType makeDivide(Representations::Type leftType, llvm::Value* left, Representations::Type rightType,
                                 llvm::Value* right);

        NodeRetType makeRemainder(Representations::Type leftType, llvm::Value* left, Representations::Type rightType,
                                 llvm::Value* right);

        NodeRetType makeMultiply(Representations::Type leftType, llvm::Value* left, Representations::Type rightType,
                                 llvm::Value* right);

        NodeRetType makePlus(Representations::Type leftType, llvm::Value* left, Representations::Type rightType,
                             llvm::Value* right);

        NodeRetType makeMinus(Representations::Type leftType, llvm::Value* left, Representations::Type rightType,
                              llvm::Value* right);

        NodeRetType makeLeftShift(Representations::Type leftType, llvm::Value* left, Representations::Type rightType,
                                  llvm::Value* right);

        NodeRetType makeRightShift(Representations::Type leftType, llvm::Value* left, Representations::Type rightType,
                                   llvm::Value* right);

        NodeRetType makeBitAnd(Representations::Type leftType, llvm::Value* left, Representations::Type rightType,
                               llvm::Value* right);

        NodeRetType makeBitXor(Representations::Type leftType, llvm::Value* left, Representations::Type rightType,
                               llvm::Value* right);

        NodeRetType makeBitOr(Representations::Type leftType, llvm::Value* left, Representations::Type rightType,
                              llvm::Value* right);

        llvm::Value* castTo(const Representations::Type& sourceType, llvm::Value* source,
                            const Representations::Type& destinationType, bool explicitConversion = false);

        llvm::Value* toBool(llvm::Value* source);

        void arithmeticCast(Representations::Type& type, llvm::Value*& value, const Representations::Type& otherType);

        Representations::Type integerPromotion(const OpenCL::Representations::Type& type,
                                               llvm::Value** optionalValue = nullptr);

    public:
        std::unique_ptr<llvm::Module> module;

        NodeRetType visit(const Syntax::Expression& node);

        NodeRetType visit(const Syntax::PrimaryExpressionIdentifier& node);

        NodeRetType visit(const Syntax::PrimaryExpressionConstant& node);

        NodeRetType visit(const Syntax::PrimaryExpressionParenthese& node);

        NodeRetType visit(const Syntax::PostFixExpressionPrimaryExpression& node);

        NodeRetType visit(const Syntax::PostFixExpressionSubscript& node);

        NodeRetType visit(const Syntax::PostFixExpressionIncrement& node);

        NodeRetType visit(const Syntax::PostFixExpressionDecrement& node);

        NodeRetType visit(const Syntax::PostFixExpressionDot& node);

        NodeRetType visit(const Syntax::PostFixExpressionArrow& node);

        NodeRetType visit(const Syntax::PostFixExpressionFunctionCall& node);

        NodeRetType visit(const Syntax::PostFixExpressionTypeInitializer& node);

        NodeRetType visit(const Syntax::AssignmentExpressionAssignment& node);

        NodeRetType visit(const Syntax::UnaryExpressionPostFixExpression& node);

        NodeRetType visit(const Syntax::UnaryExpressionUnaryOperator& node);

        NodeRetType visit(const Syntax::UnaryExpressionSizeOf& node);

        NodeRetType visit(const Syntax::CastExpression& node);

        NodeRetType visit(const Syntax::Term& node);

        NodeRetType visit(const Syntax::AdditiveExpression& node);

        NodeRetType visit(const Syntax::ShiftExpression& node);

        NodeRetType visit(const Syntax::RelationalExpression& node);

        NodeRetType visit(const Syntax::EqualityExpression& node);

        NodeRetType visit(const Syntax::BitAndExpression& node);

        NodeRetType visit(const Syntax::BitXorExpression& node);

        NodeRetType visit(const Syntax::BitOrExpression& node);

        NodeRetType visit(const Syntax::LogicalAndExpression& node);

        NodeRetType visit(const Syntax::LogicalOrExpression& node);

        NodeRetType visit(const Syntax::ConditionalExpression& node);

        std::optional<FailureReason> visit(const Syntax::ReturnStatement& node);

        std::optional<FailureReason> visit(const Syntax::ExpressionStatement& node);

        std::optional<FailureReason> visit(const Syntax::IfStatement& node);

        std::optional<FailureReason> visit(const Syntax::SwitchStatement& node);

        std::optional<FailureReason> visit(const Syntax::DefaultStatement& node);

        std::optional<FailureReason> visit(const Syntax::CaseStatement& node);

        std::optional<OpenCL::FailureReason> visit(const OpenCL::Syntax::CompoundStatement& node,
                                                   bool pushScope = true);

        std::optional<FailureReason> visit(const Syntax::ForStatement& node);

        NodeRetType visit(const Syntax::InitializerList& node);

        std::optional<FailureReason> visit(const Syntax::Declaration& node);

        std::optional<FailureReason> visit(const Syntax::ForDeclarationStatement& node);

        std::optional<FailureReason> visit(const Syntax::HeadWhileStatement& node);

        std::optional<FailureReason> visit(const Syntax::FootWhileStatement& node);

        std::optional<FailureReason> visit(const Syntax::BreakStatement& node);

        std::optional<FailureReason> visit(const Syntax::ContinueStatement& node);

        std::optional<FailureReason> visit(const Syntax::FunctionDefinition& node);

        std::optional<FailureReason> visit(const Syntax::TranslationUnit& node);

        NodeRetType visit(const Syntax::PrimaryExpression& node);

        NodeRetType visit(const Syntax::PostFixExpression& node);

        NodeRetType visit(const Syntax::UnaryExpression& node);

        NodeRetType visit(const Syntax::AssignmentExpression& node);

        NodeRetType visit(const Syntax::Initializer& node);

        std::optional<FailureReason> visit(const Syntax::CompoundItem& node);

        std::optional<FailureReason> visit(const Syntax::Statement& node);

        std::optional<FailureReason> visit(const Syntax::ExternalDeclaration& node);

        NodeRetType visit(const Syntax::TypeName& node);

        NodeRetType visit(const Syntax::Declarator& node);

        NodeRetType visit(const Syntax::EnumDeclaration& node);

        NodeRetType visit(const Syntax::TypeSpecifier& node);

        NodeRetType visit(const Syntax::DirectDeclarator& node);

        NodeRetType visit(const Syntax::DirectDeclaratorNoStaticOrAsterisk& node);

        NodeRetType visit(const Syntax::DirectDeclaratorStatic& node);

        NodeRetType visit(const Syntax::DirectDeclaratorAsterisk& node);

        NodeRetType visit(const Syntax::DirectDeclaratorParentheseParameters& node);

        NodeRetType visit(const Syntax::DirectDeclaratorParentheseIdentifiers& node);

        NodeRetType visit(const Syntax::DirectAbstractDeclarator& node);

        NodeRetType visit(const Syntax::DirectAbstractDeclaratorParameterTypeList& node);

        NodeRetType visit(const Syntax::DirectAbstractDeclaratorAssignmentExpression& node);

        NodeRetType visit(const Syntax::Pointer& node);

        NodeRetType visit(const Syntax::ParameterTypeList& node);

        NodeRetType visit(const Syntax::ParameterList& node);

        std::optional<FailureReason> visit(const Syntax::LabelStatement& node);

        std::optional<FailureReason> visit(const Syntax::GotoStatement& node);

        llvm::Type* visit(const Representations::Type& node);

        llvm::Type* visit(const Representations::PrimitiveType& node);

        llvm::Type* visit(const Representations::ArrayType& node);

        llvm::Type* visit(const Representations::AbstractArrayType& node);

        llvm::Type* visit(const Representations::ValArrayType& node);

        llvm::Type* visit(const Representations::FunctionType& node);

        llvm::Type* visit(const Representations::RecordType& node);

        llvm::Type* visit(const Representations::EnumType& node);

        llvm::Type* visit(const Representations::PointerType& node);
    };
} // namespace OpenCL::Codegen

#endif // OPENCLPARSER_CODEGEN_HPP
