#include "Parser.hpp"

#include <sstream>
#include <llvm/IR/Verifier.h>

namespace
{
    llvm::AllocaInst* createEntryBlockAlloca(llvm::Function* function,
                                             const std::string& varname,
                                             OpenCL::Parser::Context& context)
    {
        llvm::IRBuilder<> tmpB(&function->getEntryBlock(), function->getEntryBlock().begin());
        return tmpB.CreateAlloca(llvm::Type::getInt32Ty(context.context), nullptr, varname);
    }
}

llvm::Value* OpenCL::Parser::UnaryFactor::codegen(OpenCL::Parser::Context& context) const
{
    auto* factor = getFactor().codegen(context);
    switch (getUnaryOperator())
    {
    case UnaryOperator::UnaryNegation:return context.builder.CreateNeg(factor, "beg");
    case UnaryOperator::UnaryBitWiseNegation:return context.builder.CreateNot(factor, "bitneg");
    case UnaryOperator::UnaryLogicalNegation:
        if (factor->getType()->getIntegerBitWidth() > 1)
        {
            factor = context.builder.CreateICmpNE(factor,
                                                  context.builder.getIntN(factor->getType()->getIntegerBitWidth(), 0),
                                                  "boolconv");
        }
        return context.builder.CreateNot(factor, "boolneg");
    }
    return nullptr;
}

llvm::Value* OpenCL::Parser::Program::codegen(OpenCL::Parser::Context& context) const
{
    context.module = std::make_unique<llvm::Module>("main", context.context);
    for (auto& iter : getGlobals())
    {
        iter->codegen(context);
    }
    return nullptr;
}

llvm::Value* OpenCL::Parser::GlobalDeclaration::codegen(OpenCL::Parser::Context& context) const
{
    auto* constant = getOptionalValue() ? getOptionalValue()->codegen(context) : llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.context),0,true);
    new llvm::GlobalVariable(*context.module,llvm::Type::getInt32Ty(context.context),false,
        llvm::GlobalVariable::LinkageTypes::WeakAnyLinkage,constant,getName());
    return nullptr;
}

llvm::Function* OpenCL::Parser::Function::codegen(OpenCL::Parser::Context& context) const
{
    std::vector<llvm::Type*> types(getArguments().size(), llvm::Type::getInt32Ty(context.context));
    auto* ft = llvm::FunctionType::get(llvm::Type::getInt32Ty(context.context), types, false);
    context.currentFunction = llvm::Function::Create(ft,
                                                     llvm::Function::ExternalLinkage,
                                                     getName(),
                                                     context.module.get());
    std::size_t i = 0;
    for (auto& iter : context.currentFunction->args())
    {
        iter.setName(getArguments()[i++]);
    }

    auto* bb = llvm::BasicBlock::Create(context.context, "entry", context.currentFunction);
    context.builder.SetInsertPoint(bb);
    context.clearScope();
    for (auto& iter : context.currentFunction->args())
    {
        auto* alloca = createEntryBlockAlloca(context.currentFunction, iter.getName(), context);
        context.builder.CreateStore(&iter, alloca);
        context.addValueToScope(iter.getName(),alloca);
    }

    getBlockStatement().codegen(context);
    auto& block = context.currentFunction->back();
    if (block.empty() || !block.back().isTerminator())
    {
        context.builder.CreateRet(context.builder.getInt32(0));
    }

    if (llvm::verifyFunction(*context.currentFunction, &llvm::errs()))
    {
        context.currentFunction->print(llvm::outs());
        std::terminate();
    }

    return context.currentFunction;
}

llvm::Value* OpenCL::Parser::Declaration::codegen(OpenCL::Parser::Context& context) const
{
    auto* alloca = createEntryBlockAlloca(context.currentFunction, getName(), context);
    if (getOptionalExpression())
    {
        auto* value = getOptionalExpression()->codegen(context);
        context.builder.CreateStore(value, alloca);
    }
    context.addValueToScope(getName(),alloca);
    return alloca;
}

llvm::Value* OpenCL::Parser::ReturnStatement::codegen(OpenCL::Parser::Context& context) const
{
    auto* value = getExpression().codegen(context);
    if (value->getType()->getIntegerBitWidth() != 32)
    {
        value = context.builder.CreateIntCast(value, llvm::Type::getInt32Ty(context.context), true);
    }
    return context.builder.CreateRet(value);
}

llvm::Value* OpenCL::Parser::ExpressionStatement::codegen(OpenCL::Parser::Context& context) const
{
    if (getOptionalExpression())
    {
        return getOptionalExpression()->codegen(context);
    }
    return nullptr;
}

llvm::Value* OpenCL::Parser::IfStatement::codegen(OpenCL::Parser::Context& context) const
{
    auto* value = getExpression().codegen(context);
    if (value->getType()->getIntegerBitWidth() > 1)
    {
        value = context.builder.CreateICmpNE(value, context.builder.getIntN(value->getType()->getIntegerBitWidth(), 0));
    }
    auto* function = context.builder.GetInsertBlock()->getParent();

    auto* thenBB = llvm::BasicBlock::Create(context.context, "then", function);
    auto* elseBB = getElseBranch() ? llvm::BasicBlock::Create(context.context, "else") : nullptr;
    auto* mergeBB = llvm::BasicBlock::Create(context.context, "ifcont");

    context.builder.CreateCondBr(value, thenBB, elseBB ? elseBB : mergeBB);

    context.builder.SetInsertPoint(thenBB);
    getBranch().codegen(context);

    if (!thenBB->back().isTerminator())
    {
        context.builder.CreateBr(mergeBB);
    }

    if (elseBB)
    {
        function->getBasicBlockList().push_back(elseBB);
        context.builder.SetInsertPoint(elseBB);
        getElseBranch()->codegen(context);
        if (!elseBB->back().isTerminator())
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
    context.pushScope();
    for (auto& iter : getBlockItems())
    {
        iter->codegen(context);
        if (dynamic_cast<ReturnStatement*>(iter.get())
        || dynamic_cast<BreakStatement*>(iter.get())
        || dynamic_cast<ContinueStatement*>(iter.get()))
        {
            break;
        }
    }
    context.popScope();
    return nullptr;
}

namespace
{
    void doForLoop(const OpenCL::Parser::Expression* controlling,
                   const OpenCL::Parser::Expression* post,
                   const OpenCL::Parser::Statement& statement,
                   OpenCL::Parser::Context& context)
    {
        auto* function = context.builder.GetInsertBlock()->getParent();

        auto* postBB = llvm::BasicBlock::Create(context.context,"post",function);
        auto* condBB = llvm::BasicBlock::Create(context.context,"cond");
        context.builder.CreateBr(condBB);
        auto* blockBB = llvm::BasicBlock::Create(context.context, "block");
        auto* endBB = llvm::BasicBlock::Create(context.context,"end");

        context.breakBlocks.push_back(endBB);
        context.continueBlocks.push_back(postBB);

        context.builder.SetInsertPoint(postBB);
        if(post)
        {
            post->codegen(context);
        }
        context.builder.CreateBr(condBB);

        function->getBasicBlockList().push_back(condBB);
        context.builder.SetInsertPoint(condBB);
        auto* value = controlling ? controlling->codegen(context) : context.builder.getInt1(true);
        if(value->getType()->getIntegerBitWidth() > 1)
        {
            value = context.builder.CreateICmpNE(value,context.builder.getIntN(value->getType()->getIntegerBitWidth(),0));
        }
        context.builder.CreateCondBr(value,blockBB,endBB);

        function->getBasicBlockList().push_back(blockBB);
        context.builder.SetInsertPoint(blockBB);
        statement.codegen(context);
        context.builder.CreateBr(postBB);

        function->getBasicBlockList().push_back(endBB);
        context.builder.SetInsertPoint(endBB);
        context.breakBlocks.pop_back();
        context.continueBlocks.pop_back();
    }
}

llvm::Value* OpenCL::Parser::ForStatement::codegen(OpenCL::Parser::Context& context) const
{
    if(getInitial())
    {
        getInitial()->codegen(context);
    }
    doForLoop(getControlling(), getPost(),getStatement(),context);
    return nullptr;
}

llvm::Value* OpenCL::Parser::ForDeclarationStatement::codegen(OpenCL::Parser::Context& context) const
{
    context.pushScope();
    getInitial().codegen(context);
    doForLoop(getControlling(), getPost(),getStatement(), context);
    context.popScope();
    return nullptr;
}

llvm::Value* OpenCL::Parser::HeadWhileStatement::codegen(OpenCL::Parser::Context& context) const
{
    auto* function = context.builder.GetInsertBlock()->getParent();

    auto* condBB = llvm::BasicBlock::Create(context.context, "cond", function);
    context.builder.CreateBr(condBB);
    auto* blockBB = llvm::BasicBlock::Create(context.context, "block");
    auto* endBB = llvm::BasicBlock::Create(context.context, "end");

    context.breakBlocks.push_back(endBB);
    context.continueBlocks.push_back(condBB);

    context.builder.SetInsertPoint(condBB);
    auto* value = getExpression().codegen(context);
    if (value->getType()->getIntegerBitWidth() > 1)
    {
        value = context.builder.CreateICmpNE(value, context.builder.getIntN(value->getType()->getIntegerBitWidth(), 0));
    }
    context.builder.CreateCondBr(value, blockBB, endBB);

    function->getBasicBlockList().push_back(blockBB);
    context.builder.SetInsertPoint(blockBB);
    getStatement().codegen(context);
    context.builder.CreateBr(condBB);

    function->getBasicBlockList().push_back(endBB);
    context.builder.SetInsertPoint(endBB);
    context.breakBlocks.pop_back();
    context.continueBlocks.pop_back();
    return nullptr;
}

llvm::Value* OpenCL::Parser::FootWhileStatement::codegen(OpenCL::Parser::Context& context) const
{
    auto* function = context.builder.GetInsertBlock()->getParent();

    auto* blockBB = llvm::BasicBlock::Create(context.context, "block", function);
    context.builder.CreateBr(blockBB);
    auto* condBB = llvm::BasicBlock::Create(context.context, "cond");
    auto* endBB = llvm::BasicBlock::Create(context.context, "end");

    context.breakBlocks.push_back(endBB);
    context.continueBlocks.push_back(condBB);

    context.builder.SetInsertPoint(blockBB);
    getStatement().codegen(context);
    context.builder.CreateBr(condBB);

    function->getBasicBlockList().push_back(condBB);
    context.builder.SetInsertPoint(condBB);
    auto* value = getExpression().codegen(context);
    if (value->getType()->getIntegerBitWidth() > 1)
    {
        value = context.builder.CreateICmpNE(value, context.builder.getIntN(value->getType()->getIntegerBitWidth(), 0));
    }
    context.builder.CreateCondBr(value, blockBB, endBB);

    function->getBasicBlockList().push_back(endBB);
    context.builder.SetInsertPoint(endBB);
    context.breakBlocks.pop_back();
    context.continueBlocks.pop_back();
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
    auto* left = context.getNamedValue(getIdentifier());
    if (!left)
    {
        return nullptr;
    }
    switch (getAssignOperator())
    {
    case AssignOperator::NoOperator:
    {
        context.builder.CreateStore(getExpression().codegen(context), left);
        break;
    }
    case AssignOperator::PlusAssign:
    {
        auto* current = context.builder.CreateLoad(left, getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateAdd(current, getExpression().codegen(context)), left);
        break;
    }
    case AssignOperator::MinusAssign:
    {
        auto* current = context.builder.CreateLoad(left, getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateSub(current, getExpression().codegen(context)), left);
        break;
    }
    case AssignOperator::DivideAssign:
    {
        auto* current = context.builder.CreateLoad(left, getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateSDiv(current, getExpression().codegen(context)), left);
        break;
    }
    case AssignOperator::MultiplyAssign:
    {
        auto* current = context.builder.CreateLoad(left, getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateMul(current, getExpression().codegen(context)), left);
        break;
    }
    case AssignOperator::ModuloAssign:
    {
        auto* current = context.builder.CreateLoad(left, getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateSRem(current, getExpression().codegen(context)), left);
        break;
    }
    case AssignOperator::LeftShiftAssign:
    {
        auto* current = context.builder.CreateLoad(left, getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateShl(current, getExpression().codegen(context)), left);
        break;
    }
    case AssignOperator::RightShiftAssign:
    {
        auto* current = context.builder.CreateLoad(left, getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateAShr(current, getExpression().codegen(context)), left);
        break;
    }
    case AssignOperator::BitAndAssign:
    {
        auto* current = context.builder.CreateLoad(left, getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateAnd(current, getExpression().codegen(context)), left);
        break;
    }
    case AssignOperator::BitOrAssign:
    {
        auto* current = context.builder.CreateLoad(left, getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateOr(current, getExpression().codegen(context)), left);
        break;
    }
    case AssignOperator::BitXorAssign:
    {
        auto* current = context.builder.CreateLoad(left, getIdentifier().c_str());
        context.builder.CreateStore(context.builder.CreateXor(current, getExpression().codegen(context)), left);
        break;
    }
    }
    return context.builder.CreateLoad(left, getIdentifier().c_str());
}

llvm::Value* OpenCL::Parser::Term::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getFactor().codegen(context);
    for (auto&[op, factor] : getOptionalFactors())
    {
        auto* right = factor->codegen(context);
        if (!right || !left)
        {
            return nullptr;
        }

        switch (op)
        {
        case BinaryDotOperator::BinaryMultiply:left = context.builder.CreateMul(left, right, "multmp");
            break;
        case BinaryDotOperator::BinaryDivide:left = context.builder.CreateSDiv(left, right, "divtmp");
            break;
        case BinaryDotOperator::BinaryRemainder:left = context.builder.CreateSRem(left, right, "remtmp");
            break;
        }
    }
    return left;
}

llvm::Value* OpenCL::Parser::AdditiveExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getTerm().codegen(context);
    for (auto&[op, term] : getOptionalTerms())
    {
        auto* right = term.codegen(context);
        if (!right || !left)
        {
            return nullptr;
        }

        switch (op)
        {
        case BinaryDashOperator::BinaryPlus:
        {
            left = context.builder.CreateAdd(left, right, "addtmp");
            break;
        }
        case BinaryDashOperator::BinaryMinus:
        {
            left = context.builder.CreateSub(left, right, "subtmp");
            break;
        }
        }
    }
    return left;
}

llvm::Value* OpenCL::Parser::ShiftExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getAdditiveExpression().codegen(context);
    for (auto&[op, rel] : getOptionalAdditiveExpressions())
    {
        auto* right = rel.codegen(context);
        if (!right || !left)
        {
            return nullptr;
        }

        switch (op)
        {
        case ShiftOperator::Right:left = context.builder.CreateAShr(left, right);
            break;
        case ShiftOperator::Left:left = context.builder.CreateShl(left, right);
        }
    }
    return left;
}

llvm::Value* OpenCL::Parser::RelationalExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getShiftExpression().codegen(context);
    for (auto&[op, rel] : getOptionalRelationalExpressions())
    {
        auto* right = rel.codegen(context);
        if (!right || !left)
        {
            return nullptr;
        }

        switch (op)
        {
        case RelationalOperator::LessThan:left = context.builder.CreateICmpSLT(left, right);
            break;
        case RelationalOperator::LessThanOrEqual:left = context.builder.CreateICmpSLE(left, right);
            break;
        case RelationalOperator::GreaterThan:left = context.builder.CreateICmpSGT(left, right);
            break;
        case RelationalOperator::GreaterThanOrEqual:left = context.builder.CreateICmpSGE(left, right);
            break;
        }
    }
    return left;
}

llvm::Value* OpenCL::Parser::EqualityExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getRelationalExpression().codegen(context);
    for (auto&[op, factor] : getOptionalRelationalExpressions())
    {
        auto* right = factor.codegen(context);
        if (!right || !left)
        {
            return nullptr;
        }

        switch (op)
        {
        case EqualityOperator::Equal:left = context.builder.CreateICmpEQ(left, right, "cmpeqtmp");
            break;
        case EqualityOperator::NotEqual:left = context.builder.CreateICmpNE(left, right, "cmpnetmp");
            break;
        }
    }
    return left;
}

llvm::Value* OpenCL::Parser::LogicalAndExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getBitOrExpression().codegen(context);
    for (auto& factor : getOptionalBitOrExpressions())
    {
        if (left->getType()->getIntegerBitWidth() > 1)
        {
            left = context.builder.CreateICmpNE(left, context.builder.getInt32(0), "leftbooltmp");
        }
        auto* right = factor.codegen(context);
        if (right->getType()->getIntegerBitWidth() > 1)
        {
            right = context.builder.CreateICmpNE(right, context.builder.getInt32(0), "rightbooltmp");
        }
        if (!right || !left)
        {
            return nullptr;
        }

        left = context.builder.CreateAnd(left, right, "boolandtmp");
    }
    return left;
}

llvm::Value* OpenCL::Parser::LogicalOrExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getAndExpression().codegen(context);
    for (auto& factor : getOptionalAndExpressions())
    {
        if (left->getType()->getIntegerBitWidth() > 1)
        {
            left = context.builder.CreateICmpNE(left, context.builder.getInt32(0), "leftbooltmp");
        }
        auto* right = factor.codegen(context);
        if (right->getType()->getIntegerBitWidth() > 1)
        {
            right = context.builder.CreateICmpNE(right, context.builder.getInt32(0), "rightbooltmp");
        }
        if (!right || !left)
        {
            return nullptr;
        }

        left = context.builder.CreateOr(left, right, "boolortmp");
    }
    return left;
}

llvm::Value* OpenCL::Parser::ConditionalExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* value = getLogicalOrExpression().codegen(context);
    if (getOptionalExpression() && getOptionalConditionalExpression())
    {
        value = context.builder.CreateICmpNE(value, llvm::ConstantInt::get(context.context, {32, 0}));
        auto* function = context.builder.GetInsertBlock()->getParent();

        auto* thenBB = llvm::BasicBlock::Create(context.context, "then", function);
        auto* elseBB = llvm::BasicBlock::Create(context.context, "else");
        auto* mergeBB = llvm::BasicBlock::Create(context.context, "ifcont");

        context.builder.CreateCondBr(value, thenBB, elseBB);

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
        auto* pn = context.builder.CreatePHI(llvm::Type::getInt32Ty(context.context), 2, "iftmp");
        pn->addIncoming(thenV, thenBB);
        pn->addIncoming(elseV, elseBB);
        return pn;
    }
    return value;
}

llvm::Value* OpenCL::Parser::ParentheseFactor::codegen(OpenCL::Parser::Context& context) const
{
    return getExpression().codegen(context);
}

llvm::Constant* OpenCL::Parser::ConstantFactor::codegen(OpenCL::Parser::Context& context) const
{
    std::uint32_t value;
    std::istringstream ss(getValue());
    ss >> value;
    return llvm::ConstantInt::get(context.context, {32, value});
}

llvm::Value* OpenCL::Parser::VariableFactor::codegen(OpenCL::Parser::Context& context) const
{
    auto* value = context.getNamedValue(getName());
    if(!value)
    {
        throw std::runtime_error("Undefined reference to variable " + getName());
    }
    return context.builder.CreateLoad(value, getName().c_str());
}

llvm::Value* OpenCL::Parser::PostIncrement::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = context.getNamedValue(getName());
    if (!left)
    {
        throw std::runtime_error("Undefined reference to variable " + getName());
    }
    auto* value = context.builder.CreateLoad(left,"prevVal");
    context.builder.CreateStore(context.builder.CreateAdd(value,context.builder.getIntN(value->getType()->getIntegerBitWidth(),1)),left);
    return value;
}

llvm::Value* OpenCL::Parser::PreIncrement::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = context.getNamedValue(getName());
    if (!left)
    {
        throw std::runtime_error("Undefined reference to variable " + getName());
    }
    auto* load = context.builder.CreateLoad(left,"prevVal");
    auto* newValue = context.builder.CreateAdd(load,context.builder.getIntN(load->getType()->getIntegerBitWidth(),1));
    context.builder.CreateStore(newValue,left);
    return newValue;
}

llvm::Value* OpenCL::Parser::PostDecrement::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = context.getNamedValue(getName());
    if (!left)
    {
        throw std::runtime_error("Undefined reference to variable " + getName());
    }
    auto* value = context.builder.CreateLoad(left,"prevVal");
    context.builder.CreateStore(context.builder.CreateSub(value,context.builder.getIntN(value->getType()->getIntegerBitWidth(),1)),left);
    return value;
}

llvm::Value* OpenCL::Parser::PreDecrement::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = context.getNamedValue(getName());
    if (!left)
    {
        throw std::runtime_error("Undefined reference to variable " + getName());
    }
    auto* load = context.builder.CreateLoad(left,"prevVal");
    auto* newValue = context.builder.CreateSub(load,context.builder.getIntN(load->getType()->getIntegerBitWidth(),1));
    context.builder.CreateStore(newValue,left);
    return newValue;
}

llvm::Value* OpenCL::Parser::FunctionCall::codegen(OpenCL::Parser::Context& context) const
{
    auto* function = context.module->getFunction(this->getName());
    if (!function)
    {
        throw std::runtime_error("Unresolved reference to " + getName());
    }
    std::vector<llvm::Value*> arguments;
    for (auto& iter : getExpressions())
    {
        arguments.push_back(iter->codegen(context));
    }
    return context.builder.CreateCall(function, arguments, getName());
}

llvm::Value* OpenCL::Parser::BreakStatement::codegen(Context& context) const
{
    context.builder.CreateBr(context.breakBlocks.back());
    return nullptr;
}

llvm::Value* OpenCL::Parser::ContinueStatement::codegen(Context& context) const
{
    context.builder.CreateBr(context.continueBlocks.back());
    return nullptr;
}

llvm::Value* OpenCL::Parser::BitAndExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getEqualityExpression().codegen(context);
    for (auto& factor : getOptionalEqualityExpressions())
    {
        auto* right = factor.codegen(context);
        if (!right || !left)
        {
            return nullptr;
        }

        left = context.builder.CreateAnd(left, right, "bitandtmp");
    }
    return left;
}

llvm::Value* OpenCL::Parser::BitXorExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getBitAndExpression().codegen(context);
    for (auto& factor : getOptionalBitAndExpressions())
    {
        auto* right = factor.codegen(context);
        if (!right || !left)
        {
            return nullptr;
        }

        left = context.builder.CreateXor(left, right, "bitandtmp");
    }
    return left;
}

llvm::Value* OpenCL::Parser::BitOrExpression::codegen(OpenCL::Parser::Context& context) const
{
    auto* left = getBitXorExpression().codegen(context);
    for (auto& factor : getOptionalBitXorExpressions())
    {
        auto* right = factor.codegen(context);
        if (!right || !left)
        {
            return nullptr;
        }

        left = context.builder.CreateOr(left, right, "bitandtmp");
    }
    return left;
}
