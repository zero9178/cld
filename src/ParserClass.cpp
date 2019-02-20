#include "Parser.hpp"

#include <algorithm>
#include <sstream>
#include <llvm/IR/Verifier.h>

OpenCL::Parser::Program::Program(std::vector<OpenCL::Parser::Function>&& functions) noexcept : m_functions(std::move(
    functions))
{

}

const std::vector<OpenCL::Parser::Function>& OpenCL::Parser::Program::getFunctions() const
{
    return m_functions;
}

const std::string& OpenCL::Parser::Function::getName() const
{
    return m_name;
}

const std::vector<std::string>& OpenCL::Parser::Function::getArguments() const
{
    return m_arguments;
}

const OpenCL::Parser::BlockStatement& OpenCL::Parser::Function::getBlockStatement() const
{
    return m_block;
}

OpenCL::Parser::Function::Function(std::string name,
                                   std::vector<std::string> arguments,
                                   BlockStatement&& blockItems) : m_name(std::move(
    name)), m_arguments(std::move(arguments)), m_block(std::move(blockItems))
{}

const std::string& OpenCL::Parser::Declaration::getName() const
{
    return m_name;
}

const OpenCL::Parser::Expression* OpenCL::Parser::Declaration::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

OpenCL::Parser::Declaration::Declaration(std::string name) : m_name(std::move(name))
{}

OpenCL::Parser::Declaration::Declaration(std::string name,
                                         std::unique_ptr<Expression>&& optionalExpression)
    : m_name(std::move(name)), m_optionalExpression(std::move(optionalExpression))
{}

const OpenCL::Parser::Expression& OpenCL::Parser::ReturnStatement::getExpression() const
{
    return m_expression;
}

OpenCL::Parser::ReturnStatement::ReturnStatement(Expression&& expression)
    : m_expression(std::move(expression))
{}

OpenCL::Parser::ExpressionStatement::ExpressionStatement(std::unique_ptr<Expression>&& optionalExpression)
    : m_optionalExpression(std::move(optionalExpression))
{}

const OpenCL::Parser::Expression* OpenCL::Parser::ExpressionStatement::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

std::unique_ptr<OpenCL::Parser::Expression> OpenCL::Parser::ExpressionStatement::moveOptionalExpression()
{
    return std::move(m_optionalExpression);
}

OpenCL::Parser::IfStatement::IfStatement(Expression&& expression,
                                         std::unique_ptr<Statement>&& branch,
                                         std::unique_ptr<Statement>&& elseBranch) : m_expression(
    std::move(expression)), m_branch(std::move(branch)), m_elseBranch(std::move(elseBranch))
{
    assert(m_branch);
}

const OpenCL::Parser::Expression& OpenCL::Parser::IfStatement::getExpression() const
{
    return m_expression;
}

const OpenCL::Parser::Statement& OpenCL::Parser::IfStatement::getBranch() const
{
    return *m_branch;
}

const OpenCL::Parser::Statement* OpenCL::Parser::IfStatement::getElseBranch() const
{
    return m_elseBranch.get();
}

OpenCL::Parser::BlockStatement::BlockStatement(std::vector<std::unique_ptr<OpenCL::Parser::BlockItem>> blockItems)
    : m_blockItems(std::move(blockItems))
{
    assert(std::all_of(m_blockItems.begin(),
                       m_blockItems.end(),
                       [](const std::unique_ptr<OpenCL::Parser::BlockItem>& ptr)
                       { return ptr.get(); }));
}

const std::vector<std::unique_ptr<OpenCL::Parser::BlockItem>>& OpenCL::Parser::BlockStatement::getBlockItems() const
{
    return m_blockItems;
}

OpenCL::Parser::ForStatement::ForStatement(std::unique_ptr<Expression>&& initial,
                                           std::unique_ptr<Expression>&& controlling,
                                           std::unique_ptr<Expression>&& post)
    : m_initial(std::move(initial)), m_controlling(std::move(controlling)), m_post(std::move(post))
{}

const OpenCL::Parser::Expression* OpenCL::Parser::ForStatement::getInitial() const
{
    return m_initial.get();
}

const OpenCL::Parser::Expression* OpenCL::Parser::ForStatement::getControlling() const
{
    return m_controlling.get();
}

const OpenCL::Parser::Expression* OpenCL::Parser::ForStatement::getPost() const
{
    return m_post.get();
}

OpenCL::Parser::ForDeclarationStatement::ForDeclarationStatement(Declaration&& initial,
                                                                 std::unique_ptr<Expression>&& controlling,
                                                                 std::unique_ptr<Expression>&& post)
    : m_initial(std::move(initial)), m_controlling(std::move(controlling)), m_post(std::move(post))
{}

const OpenCL::Parser::Declaration& OpenCL::Parser::ForDeclarationStatement::getInitial() const
{
    return m_initial;
}

const OpenCL::Parser::Expression* OpenCL::Parser::ForDeclarationStatement::getControlling() const
{
    return m_controlling.get();
}

const OpenCL::Parser::Expression* OpenCL::Parser::ForDeclarationStatement::getPost() const
{
    return m_post.get();
}

OpenCL::Parser::HeadWhileStatement::HeadWhileStatement(Expression&& expression,
                                                       std::unique_ptr<Statement>&& statement)
    : m_expression(std::move(expression)), m_statement(std::move(statement))
{
    assert(m_statement);
}

const OpenCL::Parser::Expression& OpenCL::Parser::HeadWhileStatement::getExpression() const
{
    return m_expression;
}

const OpenCL::Parser::Statement& OpenCL::Parser::HeadWhileStatement::getStatement() const
{
    return *m_statement;
}

OpenCL::Parser::FootWhileStatement::FootWhileStatement(std::unique_ptr<Statement>&& statement,
                                                       Expression&& expression) : m_statement(
    std::move(statement)), m_expression(std::move(expression))
{
    assert(m_statement);
}

const OpenCL::Parser::Statement& OpenCL::Parser::FootWhileStatement::getStatement() const
{
    return *m_statement;
}

const OpenCL::Parser::Expression& OpenCL::Parser::FootWhileStatement::getExpression() const
{
    return m_expression;
}

OpenCL::Parser::Expression::Expression(std::unique_ptr<NonCommaExpression>&& nonCommaExpression,
                                       std::unique_ptr<NonCommaExpression>&& optionalNonCommaExpression)
    : m_nonCommaExpression(std::move(nonCommaExpression)),
      m_optionalNonCommaExpression(std::move(optionalNonCommaExpression))
{
    assert(m_nonCommaExpression);
}

const OpenCL::Parser::NonCommaExpression& OpenCL::Parser::Expression::getNonCommaExpression() const
{
    return *m_nonCommaExpression;
}

const OpenCL::Parser::NonCommaExpression* OpenCL::Parser::Expression::getOptionalNonCommaExpression() const
{
    return m_optionalNonCommaExpression.get();
}

OpenCL::Parser::AssignmentExpression::AssignmentExpression(std::string identifier,
                                                           std::unique_ptr<NonCommaExpression>&& expression,
                                                           OpenCL::Parser::AssignmentExpression::AssignOperator assignOperator)
    : m_identifier(std::move(identifier)), m_expression(std::move(expression)), m_assignOperator(assignOperator)
{
    assert(m_expression);
}

const std::string& OpenCL::Parser::AssignmentExpression::getIdentifier() const
{
    return m_identifier;
}

const OpenCL::Parser::NonCommaExpression& OpenCL::Parser::AssignmentExpression::getExpression() const
{
    return *m_expression;
}

OpenCL::Parser::AssignmentExpression::AssignOperator OpenCL::Parser::AssignmentExpression::getAssignOperator() const
{
    return m_assignOperator;
}

OpenCL::Parser::Term::Term(std::unique_ptr<Factor>&& factor,
                           std::vector<std::pair<BinaryDotOperator, std::unique_ptr<Factor>>>&& optionalFactors)
    : m_factor(std::move(factor)), m_optionalFactors(std::move(optionalFactors))
{
    assert(m_factor);
}

const OpenCL::Parser::Factor& OpenCL::Parser::Term::getFactor() const
{
    return *m_factor;
}

const std::vector<std::pair<OpenCL::Parser::Term::BinaryDotOperator,
                            std::unique_ptr<OpenCL::Parser::Factor>>>& OpenCL::Parser::Term::getOptionalFactors() const
{
    return m_optionalFactors;
}

OpenCL::Parser::AdditiveExpression::AdditiveExpression(Term&& term,
                                                       std::vector<std::pair<BinaryDashOperator, Term>>&& optionalTerms)
    : m_term(std::move(term)), m_optionalTerms(std::move(optionalTerms))
{}

const OpenCL::Parser::Term& OpenCL::Parser::AdditiveExpression::getTerm() const
{
    return m_term;
}

const std::vector<std::pair<OpenCL::Parser::AdditiveExpression::BinaryDashOperator, OpenCL::Parser::Term>>&
OpenCL::Parser::AdditiveExpression::getOptionalTerms() const
{
    return m_optionalTerms;
}

OpenCL::Parser::ShiftExpression::ShiftExpression(AdditiveExpression&& additiveExpression,
                                                 std::vector<std::pair<ShiftOperator,
                                                                       AdditiveExpression>>&& optionalAdditiveExpressions)
    : m_additiveExpression(std::move(additiveExpression)),
      m_optionalAdditiveExpressions(std::move(optionalAdditiveExpressions))
{}

const OpenCL::Parser::AdditiveExpression& OpenCL::Parser::ShiftExpression::getAdditiveExpression() const
{
    return m_additiveExpression;
}

const std::vector<std::pair<OpenCL::Parser::ShiftExpression::ShiftOperator,
                            OpenCL::Parser::AdditiveExpression>>& OpenCL::Parser::ShiftExpression::getOptionalAdditiveExpressions() const
{
    return m_optionalAdditiveExpressions;
}

OpenCL::Parser::RelationalExpression::RelationalExpression(ShiftExpression&& shiftExpression,
                                                           std::vector<std::pair<RelationalOperator,
                                                                                 ShiftExpression>>&& optionalRelationalExpressions)
    : m_shiftExpression(std::move(shiftExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{}

const OpenCL::Parser::ShiftExpression& OpenCL::Parser::RelationalExpression::getShiftExpression() const
{
    return m_shiftExpression;
}

const std::vector<std::pair<OpenCL::Parser::RelationalExpression::RelationalOperator,
                            OpenCL::Parser::ShiftExpression>>& OpenCL::Parser::RelationalExpression::getOptionalRelationalExpressions() const
{
    return m_optionalRelationalExpressions;
}

OpenCL::Parser::EqualityExpression::EqualityExpression(RelationalExpression&& relationalExpression,
                                                       std::vector<std::pair<EqualityOperator,
                                                                             RelationalExpression>>&& optionalRelationalExpressions)
    : m_relationalExpression(std::move(relationalExpression)),
      m_optionalRelationalExpressions(std::move(optionalRelationalExpressions))
{}

const OpenCL::Parser::RelationalExpression& OpenCL::Parser::EqualityExpression::getRelationalExpression() const
{
    return m_relationalExpression;
}

const std::vector<std::pair<OpenCL::Parser::EqualityExpression::EqualityOperator,
                            OpenCL::Parser::RelationalExpression>>& OpenCL::Parser::EqualityExpression::getOptionalRelationalExpressions() const
{
    return m_optionalRelationalExpressions;
}

OpenCL::Parser::LogicalAndExpression::LogicalAndExpression(EqualityExpression&& equalityExpression,
                                                           std::vector<EqualityExpression>&& optionalEqualityExpressions)
    : m_equalityExpression(std::move(equalityExpression)),
      m_optionalEqualityExpressions(std::move(optionalEqualityExpressions))
{}

const OpenCL::Parser::EqualityExpression& OpenCL::Parser::LogicalAndExpression::getEqualityExpression() const
{
    return m_equalityExpression;
}

const std::vector<OpenCL::Parser::EqualityExpression>& OpenCL::Parser::LogicalAndExpression::getOptionalEqualityExpressions() const
{
    return m_optionalEqualityExpressions;
}

OpenCL::Parser::LogicalOrExpression::LogicalOrExpression(LogicalAndExpression&& andExpression,
                                                         std::vector<LogicalAndExpression>&& optionalAndExpressions)
    : m_andExpression(std::move(andExpression)), m_optionalAndExpressions(std::move(optionalAndExpressions))
{}

const OpenCL::Parser::LogicalAndExpression& OpenCL::Parser::LogicalOrExpression::getAndExpression() const
{
    return m_andExpression;
}

const std::vector<OpenCL::Parser::LogicalAndExpression>& OpenCL::Parser::LogicalOrExpression::getOptionalAndExpressions() const
{
    return m_optionalAndExpressions;
}

OpenCL::Parser::ConditionalExpression::ConditionalExpression(LogicalOrExpression&& logicalOrExpression,
                                                             std::unique_ptr<Expression>&& optionalExpression,
                                                             std::unique_ptr<ConditionalExpression>&& optionalConditionalExpression)
    : m_logicalOrExpression(std::move(logicalOrExpression)), m_optionalExpression(std::move(optionalExpression)),
      m_optionalConditionalExpression(std::move(optionalConditionalExpression))
{}

const OpenCL::Parser::LogicalOrExpression& OpenCL::Parser::ConditionalExpression::getLogicalOrExpression() const
{
    return m_logicalOrExpression;
}

const OpenCL::Parser::Expression* OpenCL::Parser::ConditionalExpression::getOptionalExpression() const
{
    return m_optionalExpression.get();
}

const OpenCL::Parser::ConditionalExpression* OpenCL::Parser::ConditionalExpression::getOptionalConditionalExpression() const
{
    return m_optionalConditionalExpression.get();
}

OpenCL::Parser::ParentheseFactor::ParentheseFactor(Expression&& expression) : m_expression(
    std::move(expression))
{}

const OpenCL::Parser::Expression& OpenCL::Parser::ParentheseFactor::getExpression() const
{
    return m_expression;
}

OpenCL::Parser::UnaryFactor::UnaryFactor(OpenCL::Parser::UnaryFactor::UnaryOperator unaryOperator,
                                         std::unique_ptr<Factor>&& factor) : m_unaryOperator(
    unaryOperator), m_factor(std::move(factor))
{
    assert(m_factor);
}

OpenCL::Parser::UnaryFactor::UnaryOperator OpenCL::Parser::UnaryFactor::getUnaryOperator() const
{
    return m_unaryOperator;
}

const OpenCL::Parser::Factor& OpenCL::Parser::UnaryFactor::getFactor() const
{
    return *m_factor;
}

OpenCL::Parser::ConstantFactor::ConstantFactor(std::string value) : m_value(std::move(value))
{}

const std::string& OpenCL::Parser::ConstantFactor::getValue() const
{
    return m_value;
}

OpenCL::Parser::VariableFactor::VariableFactor(std::string name) : m_name(std::move(name))
{}

const std::string& OpenCL::Parser::VariableFactor::getName() const
{
    return m_name;
}

OpenCL::Parser::PostIncrement::PostIncrement(std::string name) : m_name(std::move(name))
{}

const std::string& OpenCL::Parser::PostIncrement::getName() const
{
    return m_name;
}

OpenCL::Parser::PreIncrement::PreIncrement(std::string name) : m_name(std::move(name))
{}

const std::string& OpenCL::Parser::PreIncrement::getName() const
{
    return m_name;
}

OpenCL::Parser::PostDecrement::PostDecrement(std::string name) : m_name(std::move(name))
{}

const std::string& OpenCL::Parser::PostDecrement::getName() const
{
    return m_name;
}

OpenCL::Parser::PreDecrement::PreDecrement(std::string name) : m_name(std::move(name))
{}

const std::string& OpenCL::Parser::PreDecrement::getName() const
{
    return m_name;
}

OpenCL::Parser::FunctionCall::FunctionCall(std::string name,
                                           std::vector<std::unique_ptr<OpenCL::Parser::NonCommaExpression>> expressions)
    : m_name(std::move(name)), m_expressions(std::move(expressions))
{
    assert(std::all_of(m_expressions.begin(), m_expressions.end(), [](const std::unique_ptr<NonCommaExpression>& ptr)
    { return ptr.get(); }));
}

const std::string& OpenCL::Parser::FunctionCall::getName() const
{
    return m_name;
}

const std::vector<std::unique_ptr<OpenCL::Parser::NonCommaExpression>>& OpenCL::Parser::FunctionCall::getExpressions() const
{
    return m_expressions;
}

namespace
{
    llvm::AllocaInst* createEntryBlockAlloca(llvm::Function* function,const std::string& varname,OpenCL::Parser::Context& context)
    {
        llvm::IRBuilder<> tmpB(&function->getEntryBlock(),function->getEntryBlock().begin());
        return tmpB.CreateAlloca(llvm::Type::getInt32Ty(context.context),nullptr,varname);
    }
}

llvm::Value* OpenCL::Parser::UnaryFactor::codegen(OpenCL::Parser::Context& context) const
{
    auto* factor = getFactor().codegen(context);
    switch (getUnaryOperator())
    {
    case UnaryOperator::UnaryNegation:
        return context.builder.CreateNeg(factor,"beg");
    case UnaryOperator::UnaryBitWiseNegation:
        return context.builder.CreateNot(factor,"bitneg");
    case UnaryOperator::UnaryLogicalNegation:
        if(factor->getType()->getIntegerBitWidth() > 1)
        {
            factor = context.builder.CreateICmpNE(factor,context.builder.getIntN(factor->getType()->getIntegerBitWidth(),0),"boolconv");
        }
        return context.builder.CreateNot(factor,"boolneg");
    }
}

llvm::Value* OpenCL::Parser::Program::codegen(OpenCL::Parser::Context& context) const
{
    context.module = std::make_unique<llvm::Module>("main",context.context);
    for(auto& iter : getFunctions())
    {
        iter.codegen(context);
    }
    return nullptr;
}

llvm::Function* OpenCL::Parser::Function::codegen(OpenCL::Parser::Context& context) const
{
    std::vector<llvm::Type*> types(getArguments().size(),llvm::Type::getInt32Ty(context.context));
    auto* ft = llvm::FunctionType::get(llvm::Type::getInt32Ty(context.context),types,false);
    context.currentFunction = llvm::Function::Create(ft,llvm::Function::ExternalLinkage,getName(),context.module.get());
    std::size_t i = 0;
    for(auto& iter : context.currentFunction->args())
    {
        iter.setName(getArguments()[i++]);
    }

    auto* bb = llvm::BasicBlock::Create(context.context,"entry",context.currentFunction);
    context.builder.SetInsertPoint(bb);
    context.namedValues.clear();
    for(auto& iter : context.currentFunction->args())
    {
        auto* alloca = createEntryBlockAlloca(context.currentFunction,iter.getName(),context);
        context.builder.CreateStore(&iter,alloca);
        context.namedValues[iter.getName()] = alloca;
    }

    getBlockStatement().codegen(context);
    auto& block = context.currentFunction->back();
    if(block.empty() || !block.back().isTerminator())
    {
        context.builder.CreateRet(context.builder.getInt32(0));
    }

    if(llvm::verifyFunction(*context.currentFunction,&llvm::errs()))
    {
        context.currentFunction->print(llvm::outs());
        std::terminate();
    }

    return context.currentFunction;
}

llvm::Value* OpenCL::Parser::Declaration::codegen(OpenCL::Parser::Context& context) const
{
    auto* alloca = createEntryBlockAlloca(context.currentFunction,getName(),context);
    if(getOptionalExpression())
    {
        auto* value = getOptionalExpression()->codegen(context);
        context.builder.CreateStore(value,alloca);
    }
    context.namedValues[getName()] = alloca;
    return alloca;
}

llvm::Value* OpenCL::Parser::ReturnStatement::codegen(OpenCL::Parser::Context& context) const
{
    auto* value = getExpression().codegen(context);
    if(value->getType()->getIntegerBitWidth() != 32)
    {
        value = context.builder.CreateIntCast(value,llvm::Type::getInt32Ty(context.context),true);
    }
    return context.builder.CreateRet(value);
}

llvm::Value* OpenCL::Parser::ExpressionStatement::codegen(OpenCL::Parser::Context& context) const
{
    if(getOptionalExpression())
    {
        return getOptionalExpression()->codegen(context);
    }
    return nullptr;
}

llvm::Value* OpenCL::Parser::IfStatement::codegen(OpenCL::Parser::Context& context) const
{
    auto* value = getExpression().codegen(context);
    if(value->getType()->getIntegerBitWidth() > 1)
    {
        value = context.builder.CreateICmpNE(value,context.builder.getIntN(value->getType()->getIntegerBitWidth(),0));
    }
    auto* function = context.builder.GetInsertBlock()->getParent();

    auto* thenBB = llvm::BasicBlock::Create(context.context,"then",function);
    auto* elseBB = getElseBranch() ? llvm::BasicBlock::Create(context.context,"else") : nullptr;
    auto* mergeBB = llvm::BasicBlock::Create(context.context,"ifcont");

    context.builder.CreateCondBr(value,thenBB,elseBB ? elseBB : mergeBB);

    context.builder.SetInsertPoint(thenBB);
    getBranch().codegen(context);

    if(!thenBB->back().isTerminator())
    {
        context.builder.CreateBr(mergeBB);
    }

    if(elseBB)
    {
        function->getBasicBlockList().push_back(elseBB);
        context.builder.SetInsertPoint(elseBB);
        getElseBranch()->codegen(context);
        if(!elseBB->back().isTerminator())
        {
            context.builder.CreateBr(mergeBB);
        }
    }

    function->getBasicBlockList().push_back(mergeBB);
    context.builder.SetInsertPoint(mergeBB);

    return nullptr;
}

llvm::Value* OpenCL::Parser::BlockStatement::codegen(OpenCL::Parser::Context& context) const
{
    for(auto& iter : getBlockItems())
    {
        iter->codegen(context);
        if(dynamic_cast<ReturnStatement*>(iter.get()))
        {
            break;
        }
    }
    return nullptr;
}

namespace
{
    void doForLoop(const OpenCL::Parser::Expression* controlling,const OpenCL::Parser::Expression* post,OpenCL::Parser::Context& context)
    {
        auto* function = context.builder.GetInsertBlock()->getParent();

        auto* blockBB = llvm::BasicBlock::Create(context.context,"block",function);
        context.builder.CreateBr(blockBB);
    }
}

llvm::Value* OpenCL::Parser::ForStatement::codegen(OpenCL::Parser::Context& context) const
{
    getInitial()->codegen(context);
    doForLoop(getControlling(),getPost(),context);
    return nullptr;
}

llvm::Value* OpenCL::Parser::ForDeclarationStatement::codegen(OpenCL::Parser::Context& context) const
{
    getInitial().codegen(context);
    doForLoop(getControlling(),getPost(),context);
    return nullptr;
}

llvm::Value* OpenCL::Parser::HeadWhileStatement::codegen(OpenCL::Parser::Context& context) const
{
    auto* function = context.builder.GetInsertBlock()->getParent();

    auto* condBB = llvm::BasicBlock::Create(context.context,"cond",function);
    context.builder.CreateBr(condBB);
    auto* blockBB = llvm::BasicBlock::Create(context.context,"block");
    auto* endBB = llvm::BasicBlock::Create(context.context,"end");

    context.builder.SetInsertPoint(condBB);
    auto* value = getExpression().codegen(context);
    if(value->getType()->getIntegerBitWidth() > 1)
    {
        value = context.builder.CreateICmpNE(value,context.builder.getIntN(value->getType()->getIntegerBitWidth(),0));
    }
    context.builder.CreateCondBr(value,blockBB,endBB);

    function->getBasicBlockList().push_back(blockBB);
    context.builder.SetInsertPoint(blockBB);
    getStatement().codegen(context);
    context.builder.CreateBr(condBB);

    function->getBasicBlockList().push_back(endBB);
    context.builder.SetInsertPoint(endBB);
    return nullptr;
}

llvm::Value* OpenCL::Parser::FootWhileStatement::codegen(OpenCL::Parser::Context& context) const
{
    auto* function = context.builder.GetInsertBlock()->getParent();

    auto* blockBB = llvm::BasicBlock::Create(context.context,"block",function);
    context.builder.CreateBr(blockBB);
    auto* condBB = llvm::BasicBlock::Create(context.context,"cond");
    auto* endBB = llvm::BasicBlock::Create(context.context,"end");

    context.builder.SetInsertPoint(blockBB);
    getStatement().codegen(context);
    context.builder.CreateBr(condBB);

    function->getBasicBlockList().push_back(condBB);
    context.builder.SetInsertPoint(condBB);
    auto* value = getExpression().codegen(context);
    if(value->getType()->getIntegerBitWidth() > 1)
    {
        value = context.builder.CreateICmpNE(value,context.builder.getIntN(value->getType()->getIntegerBitWidth(),0));
    }
    context.builder.CreateCondBr(value,blockBB,endBB);

    function->getBasicBlockList().push_back(endBB);
    context.builder.SetInsertPoint(endBB);
    return nullptr;
}

llvm::Value* OpenCL::Parser::Expression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getNonCommaExpression().codegen(context);
    auto* right = getOptionalNonCommaExpression() ? getOptionalNonCommaExpression()->codegen(context) : nullptr;
    return right ? right : left;
}

llvm::Value* OpenCL::Parser::AssignmentExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = context.namedValues[getIdentifier()];
    if(!left)
    {
        return nullptr;
    }
    switch (getAssignOperator())
    {
    case AssignOperator::NoOperator:
    {
        context.builder.CreateStore(getExpression().codegen(context),left);
        break;
    }
    case AssignOperator::PlusAssign:
    {
        auto* current = context.builder.CreateLoad(left,getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateAdd(current,getExpression().codegen(context)),left);
        break;
    }
    case AssignOperator::MinusAssign:
    {
        auto* current = context.builder.CreateLoad(left,getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateSub(current,getExpression().codegen(context)),left);
        break;
    }
    case AssignOperator::DivideAssign:
    {
        auto* current = context.builder.CreateLoad(left,getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateSDiv(current,getExpression().codegen(context)),left);
        break;
    }
    case AssignOperator::MultiplyAssign:
    {
        auto* current = context.builder.CreateLoad(left,getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateMul(current,getExpression().codegen(context)),left);
        break;
    }
    case AssignOperator::ModuloAssign:
    {
        auto* current = context.builder.CreateLoad(left,getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateSRem(current,getExpression().codegen(context)),left);
        break;
    }
    case AssignOperator::LeftShiftAssign:
    {
        auto* current = context.builder.CreateLoad(left,getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateShl(current,getExpression().codegen(context)),left);
        break;
    }
    case AssignOperator::RightShiftAssign:
    {
        auto* current = context.builder.CreateLoad(left,getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateAShr(current,getExpression().codegen(context)),left);
        break;
    }
    case AssignOperator::BitAndAssign:
    {
        auto* current = context.builder.CreateLoad(left,getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateAnd(current,getExpression().codegen(context)),left);
        break;
    }
    case AssignOperator::BitOrAssign:
    {
        auto* current = context.builder.CreateLoad(left,getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateOr(current,getExpression().codegen(context)),left);
        break;
    }
    case AssignOperator::BitXorAssign:
    {
        auto* current = context.builder.CreateLoad(left,getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateXor(current,getExpression().codegen(context)),left);
        break;
    }
    }
    return context.builder.CreateLoad(left,getIdentifier().c_str());
}

llvm::Value* OpenCL::Parser::Term::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getFactor().codegen(context);
    for(auto& [op,factor] : getOptionalFactors())
    {
        auto* right = factor->codegen(context);
        if(!right || !left)
        {
            return nullptr;
        }

        switch(op)
        {
        case BinaryDotOperator::BinaryMultiply:
            left = context.builder.CreateMul(left,right,"multmp");
            break;
        case BinaryDotOperator::BinaryDivide:
            left = context.builder.CreateSDiv(left,right,"divtmp");
            break;
        case BinaryDotOperator::BinaryRemainder:
            left = context.builder.CreateSRem(left,right,"remtmp");
            break;
        }
    }
    return left;
}

llvm::Value* OpenCL::Parser::AdditiveExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getTerm().codegen(context);
    for(auto& [op,term] : getOptionalTerms())
    {
        auto* right = term.codegen(context);
        if(!right || !left)
        {
            return nullptr;
        }

        switch (op)
        {
        case BinaryDashOperator::BinaryPlus:
        {
            left = context.builder.CreateAdd(left,right,"addtmp");
            break;
        }
        case BinaryDashOperator::BinaryMinus:
        {
            left = context.builder.CreateSub(left,right,"subtmp");
            break;
        }
        }
    }
    return left;
}

llvm::Value* OpenCL::Parser::ShiftExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getAdditiveExpression().codegen(context);
    for(auto& [op,rel] : getOptionalAdditiveExpressions())
    {
        auto* right = rel.codegen(context);
        if(!right || !left)
        {
            return nullptr;
        }

        switch(op)
        {
        case ShiftOperator::Right:
            left = context.builder.CreateAShr(left,right);
            break;
        case ShiftOperator::Left:
            left = context.builder.CreateShl(left,right);
        }
    }
    return left;
}

llvm::Value* OpenCL::Parser::RelationalExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getShiftExpression().codegen(context);
    for(auto& [op,rel] : getOptionalRelationalExpressions())
    {
        auto* right = rel.codegen(context);
        if(!right || !left)
        {
            return nullptr;
        }

        switch(op)
        {
        case RelationalOperator::LessThan:
            left = context.builder.CreateICmpSLT(left,right);
            break;
        case RelationalOperator::LessThanOrEqual:
            left = context.builder.CreateICmpSLE(left,right);
            break;
        case RelationalOperator::GreaterThan:
            left = context.builder.CreateICmpSGT(left,right);
            break;
        case RelationalOperator::GreaterThanOrEqual:
            left = context.builder.CreateICmpSGE(left,right);
            break;
        }
    }
    return left;
}

llvm::Value* OpenCL::Parser::EqualityExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getRelationalExpression().codegen(context);
    for(auto& [op,factor] : getOptionalRelationalExpressions())
    {
        auto* right = factor.codegen(context);
        if(!right || !left)
        {
            return nullptr;
        }

        switch(op)
        {
        case EqualityOperator::Equal:
            left = context.builder.CreateICmpEQ(left,right,"cmpeqtmp");
            break;
        case EqualityOperator::NotEqual:
            left = context.builder.CreateICmpNE(left,right,"cmpnetmp");
            break;
        }
    }
    return left;
}

llvm::Value* OpenCL::Parser::LogicalAndExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getEqualityExpression().codegen(context);
    for(auto& factor : getOptionalEqualityExpressions())
    {
        if (left->getType()->getIntegerBitWidth() > 1)
        {
            left = context.builder.CreateICmpNE(left,context.builder.getInt32(0),"leftbooltmp");
        }
        auto* right = factor.codegen(context);
        if(right->getType()->getIntegerBitWidth() > 1)
        {
            right = context.builder.CreateICmpNE(right,context.builder.getInt32(0),"rightbooltmp");
        }
        if(!right || !left)
        {
            return nullptr;
        }

        left = context.builder.CreateAnd(left,right,"boolandtmp");
    }
    return left;
}

llvm::Value* OpenCL::Parser::LogicalOrExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getAndExpression().codegen(context);
    for(auto& factor : getOptionalAndExpressions())
    {
        if (left->getType()->getIntegerBitWidth() > 1)
        {
            left = context.builder.CreateICmpNE(left,context.builder.getInt32(0),"leftbooltmp");
        }
        auto* right = factor.codegen(context);
        if(right->getType()->getIntegerBitWidth() > 1)
        {
            right = context.builder.CreateICmpNE(right,context.builder.getInt32(0),"rightbooltmp");
        }
        if(!right || !left)
        {
            return nullptr;
        }

        left = context.builder.CreateOr(left,right,"boolortmp");
    }
    return left;
}

llvm::Value* OpenCL::Parser::ConditionalExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* value = getLogicalOrExpression().codegen(context);
    if(getOptionalExpression() && getOptionalConditionalExpression())
    {
        value = context.builder.CreateICmpNE(value,llvm::ConstantInt::get(context.context,{32,0}));
        auto* function = context.builder.GetInsertBlock()->getParent();

        auto* thenBB = llvm::BasicBlock::Create(context.context,"then",function);
        auto* elseBB = llvm::BasicBlock::Create(context.context,"else");
        auto* mergeBB = llvm::BasicBlock::Create(context.context,"ifcont");

        context.builder.CreateCondBr(value,thenBB,elseBB);

        context.builder.SetInsertPoint(thenBB);
        auto* thenV = getOptionalExpression()->codegen(context);

        context.builder.CreateBr(mergeBB);
        thenBB = context.builder.GetInsertBlock();

        function->getBasicBlockList().push_back(elseBB);
        context.builder.SetInsertPoint(elseBB);

        auto* elseV = getOptionalConditionalExpression()->codegen(context);

        context.builder.CreateBr(mergeBB);
        elseBB = context.builder.GetInsertBlock();

        function->getBasicBlockList().push_back(mergeBB);
        context.builder.SetInsertPoint(mergeBB);
        auto* pn = context.builder.CreatePHI(llvm::Type::getInt32Ty(context.context),2,"iftmp");
        pn->addIncoming(thenV,thenBB);
        pn->addIncoming(elseV,elseBB);
        return pn;
    }
    return value;
}

llvm::Value* OpenCL::Parser::ParentheseFactor::codegen(OpenCL::Parser::Context& context) const
{
    return getExpression().codegen(context);
}

llvm::Value* OpenCL::Parser::ConstantFactor::codegen(OpenCL::Parser::Context& context) const
{
    std::uint32_t value;
    std::istringstream ss(getValue());
    ss>>value;
    return llvm::ConstantInt::get(context.context,{32,value});
}

llvm::Value* OpenCL::Parser::VariableFactor::codegen(OpenCL::Parser::Context& context) const
{
    if(auto result = context.namedValues.find(getName()); result == context.namedValues.end())
    {
        throw std::runtime_error("Variable " + getName() + " not found");
    }
    else
    {
        return context.builder.CreateLoad(result->second,getName().c_str());
    }
}

llvm::Value* OpenCL::Parser::PostIncrement::codegen(OpenCL::Parser::Context& context) const
{
    return nullptr;
}

llvm::Value* OpenCL::Parser::PreIncrement::codegen(OpenCL::Parser::Context& context) const
{
    return nullptr;
}

llvm::Value* OpenCL::Parser::PostDecrement::codegen(OpenCL::Parser::Context& context) const
{
    return nullptr;
}

llvm::Value* OpenCL::Parser::PreDecrement::codegen(OpenCL::Parser::Context& context) const
{
    return nullptr;
}

llvm::Value* OpenCL::Parser::FunctionCall::codegen(OpenCL::Parser::Context& context) const
{
    auto* function = context.module->getFunction(this->getName());
    if(!function)
    {
        throw std::runtime_error("Unresolved reference to " + getName());
    }
    std::vector<llvm::Value*> arguments;
    for(auto& iter : getExpressions())
    {
        arguments.push_back(iter->codegen(context));
    }
    return context.builder.CreateCall(function,arguments,getName());
}

llvm::Value* OpenCL::Parser::BreakStatement::codegen(Context& context) const
{
    return nullptr;
}

llvm::Value* OpenCL::Parser::ContinueStatement::codegen(Context& context) const
{
    return nullptr;
}
