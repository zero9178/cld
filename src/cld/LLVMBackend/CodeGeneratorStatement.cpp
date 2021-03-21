#include "CodeGenerator.hpp"

void cld::CGLLVM::CodeGenerator::visit(const Semantics::CompoundStatement& compoundStatement)
{
    std::optional<cld::ValueReset<llvm::DIScope*>> reset;
    if (m_options.debugEmission != cld::CGLLVM::DebugEmission::None)
    {
        if (!m_scopeIdToScope[compoundStatement.getScope()])
        {
            auto* parent = m_scopeIdToScope[m_programInterface.getScopes()[compoundStatement.getScope()].previousScope];
            CLD_ASSERT(parent);
            m_scopeIdToScope[compoundStatement.getScope()] = m_debugInfo->createLexicalBlock(
                parent, getFile(compoundStatement.getOpenBrace()), getLine(compoundStatement.getOpenBrace()),
                getColumn(compoundStatement.getOpenBrace()));
        }
        reset.emplace(m_currentDebugScope, m_currentDebugScope);
        m_currentDebugScope = m_scopeIdToScope[compoundStatement.getScope()];
    }
    for (auto& iter : compoundStatement.getCompoundItems())
    {
        cld::match(
            iter,
            [&](const std::shared_ptr<const Semantics::ExpressionBase>& expr) {
                auto result = m_valSizes.emplace(
                    expr, m_builder.CreateIntCast(
                              visit(*expr).value, m_builder.getInt64Ty(),
                              cld::get<Semantics::PrimitiveType>(expr->getType().getVariant()).isSigned()));
                (void)result;
                CLD_ASSERT(result.second);
            },
            [&](const cld::IntrVarPtr<Semantics::Declaration>& decl) {
                if (auto* varDecl = decl->get_if<Semantics::VariableDeclaration>())
                {
                    visit(*varDecl);
                }
                else if (auto* funcDecl = decl->get_if<Semantics::FunctionDeclaration>())
                {
                    visit(*funcDecl);
                }
            },
            [&](const cld::IntrVarPtr<Semantics::Statement>& statement) { visit(*statement); });
    }
    if (m_builder.GetInsertBlock())
    {
        runDestructors(compoundStatement.getScope());
    }
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::Statement& statement)
{
    statement.match([&](const auto& statement) { visit(statement); },
                    [&](const Semantics::ExpressionStatement& expressionStatement) {
                        if (!expressionStatement.getExpression() || !m_builder.GetInsertBlock())
                        {
                            return;
                        }
                        visitVoidExpression(*expressionStatement.getExpression());
                    });
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::ReturnStatement& returnStatement)
{
    if (!m_builder.GetInsertBlock())
    {
        return;
    }
    if (!returnStatement.getExpression())
    {
        runDestructors(returnStatement.getScope(), 0);
        m_builder.CreateRetVoid();
        m_builder.ClearInsertionPoint();
        return;
    }

    // auto* function = m_currentFunction;
    auto value = visit(*returnStatement.getExpression());
    auto* ret = m_abi->generateValueReturn(*this, value);
    runDestructors(returnStatement.getScope(), 0);
    if (ret)
    {
        m_builder.CreateRet(ret);
    }
    else
    {
        m_builder.CreateRetVoid();
    }
    m_builder.ClearInsertionPoint();
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::ForStatement& forStatement)
{
    std::optional<cld::ValueReset<llvm::DIScope*>> reset;
    cld::match(
        forStatement.getInitial(), [](std::monostate) {},
        [&](const std::vector<cld::IntrVarPtr<Semantics::Declaration>>& declaration) {
            if (m_options.debugEmission != cld::CGLLVM::DebugEmission::None)
            {
                if (!m_scopeIdToScope[forStatement.getScope()])
                {
                    auto* parent =
                        m_scopeIdToScope[m_programInterface.getScopes()[forStatement.getScope()].previousScope];
                    CLD_ASSERT(parent);
                    m_scopeIdToScope[forStatement.getScope()] = llvm::DILexicalBlock::get(
                        m_module.getContext(), parent, getFile(forStatement.getForToken()),
                        getLine(forStatement.getForToken()), getColumn(forStatement.getForToken()));
                }
                reset.emplace(m_currentDebugScope, m_currentDebugScope);
                m_currentDebugScope = m_scopeIdToScope[forStatement.getScope()];
            }
            for (auto& iter : declaration)
            {
                if (auto* varDecl = iter->get_if<Semantics::VariableDeclaration>())
                {
                    visit(*varDecl);
                }
                else if (auto* funcDecl = iter->get_if<Semantics::FunctionDeclaration>())
                {
                    visit(*funcDecl);
                }
            }
        },
        [&](const cld::IntrVarPtr<Semantics::ExpressionBase>& expression) { visitVoidExpression(*expression); });
    auto* controlling = llvm::BasicBlock::Create(m_module.getContext(), "for.controlling", m_currentFunction);
    if (m_builder.GetInsertBlock())
    {
        m_builder.CreateBr(controlling);
    }
    m_builder.SetInsertPoint(controlling);
    auto* body = llvm::BasicBlock::Create(m_module.getContext(), "for.body", m_currentFunction);
    llvm::BasicBlock* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "for.continue", m_currentFunction);
    m_breakTargets[&forStatement] = contBlock;
    if (forStatement.getControlling())
    {
        auto value = visit(*forStatement.getControlling());
        value = boolToi1(value);
        m_builder.CreateCondBr(value.value, body, contBlock);
    }
    else
    {
        m_builder.CreateBr(body);
    }
    m_builder.SetInsertPoint(body);
    auto* iteration = forStatement.getIteration() ?
                          llvm::BasicBlock::Create(m_module.getContext(), "for.iteration", m_currentFunction) :
                          controlling;
    m_continueTargets[&forStatement] = iteration;
    visit(forStatement.getStatement());
    if (m_builder.GetInsertBlock())
    {
        m_builder.CreateBr(iteration);
    }
    if (forStatement.getIteration())
    {
        m_builder.SetInsertPoint(iteration);
        visitVoidExpression(*forStatement.getIteration());
        m_builder.CreateBr(controlling);
    }
    m_builder.SetInsertPoint(contBlock);
    if (std::holds_alternative<std::vector<cld::IntrVarPtr<Semantics::Declaration>>>(forStatement.getInitial()))
    {
        // If the for statement held declarations we must run the destructors for those declarations as soon as we
        // leave the statement
        runDestructors(forStatement.getScope());
    }
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::IfStatement& ifStatement)
{
    Value expression = nullptr;
    llvm::BasicBlock* trueBranch = nullptr;
    if (m_builder.GetInsertBlock())
    {
        expression = visit(ifStatement.getExpression());
        expression = boolToi1(expression);
        trueBranch = llvm::BasicBlock::Create(m_module.getContext(), "if.true", m_currentFunction);
    }
    if (!ifStatement.getFalseBranch())
    {
        auto* contBranch = llvm::BasicBlock::Create(m_module.getContext(), "if.continue", m_currentFunction);
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateCondBr(expression.value, trueBranch, contBranch);
            m_builder.SetInsertPoint(trueBranch);
        }
        visit(ifStatement.getTrueBranch());
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(contBranch);
        }
        m_builder.SetInsertPoint(contBranch);
        return;
    }
    llvm::BasicBlock* falseBranch = nullptr;
    if (m_builder.GetInsertBlock())
    {
        falseBranch = llvm::BasicBlock::Create(m_module.getContext(), "if.false", m_currentFunction);
        m_builder.CreateCondBr(expression.value, trueBranch, falseBranch);
        m_builder.SetInsertPoint(trueBranch);
    }
    visit(ifStatement.getTrueBranch());
    auto* trueBlock = m_builder.GetInsertBlock();
    auto* trueTerminator = m_builder.GetInsertBlock() ? m_builder.GetInsertBlock()->getTerminator() : nullptr;
    if (falseBranch)
    {
        m_builder.SetInsertPoint(falseBranch);
    }
    visit(*ifStatement.getFalseBranch());
    auto* falseBlock = m_builder.GetInsertBlock();
    auto* falseTerminator = m_builder.GetInsertBlock() ? m_builder.GetInsertBlock()->getTerminator() : nullptr;
    if ((trueBlock && !trueTerminator) || (falseBlock && !falseTerminator))
    {
        auto* contBranch = llvm::BasicBlock::Create(m_module.getContext(), "if.continue", m_currentFunction);
        if (trueBlock && !trueTerminator)
        {
            m_builder.SetInsertPoint(trueBlock);
            m_builder.CreateBr(contBranch);
        }
        if (falseBlock && !falseTerminator)
        {
            m_builder.SetInsertPoint(falseBlock);
            m_builder.CreateBr(contBranch);
        }
        m_builder.SetInsertPoint(contBranch);
    }
    else
    {
        m_builder.ClearInsertionPoint();
    }
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::HeadWhileStatement& headWhileStatement)
{
    auto* controlling = llvm::BasicBlock::Create(m_module.getContext(), "while.controlling", m_currentFunction);
    if (m_builder.GetInsertBlock())
    {
        m_builder.CreateBr(controlling);
    }
    m_continueTargets[&headWhileStatement] = controlling;
    m_builder.SetInsertPoint(controlling);
    auto expression = visit(headWhileStatement.getExpression());
    expression = boolToi1(expression);
    auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "while.continue", m_currentFunction);
    auto* body = llvm::BasicBlock::Create(m_module.getContext(), "while.body", m_currentFunction);
    m_builder.CreateCondBr(expression.value, body, contBlock);
    m_builder.SetInsertPoint(body);
    m_breakTargets[&headWhileStatement] = contBlock;
    visit(headWhileStatement.getStatement());
    if (m_builder.GetInsertBlock())
    {
        m_builder.CreateBr(controlling);
    }
    m_builder.SetInsertPoint(contBlock);
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::FootWhileStatement& footWhileStatement)
{
    auto* controlling = llvm::BasicBlock::Create(m_module.getContext(), "do_while.controlling", m_currentFunction);
    auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "do_while.continue", m_currentFunction);
    auto* body = llvm::BasicBlock::Create(m_module.getContext(), "do_while.body", m_currentFunction);
    if (m_builder.GetInsertBlock())
    {
        m_builder.CreateBr(body);
    }
    m_continueTargets[&footWhileStatement] = controlling;
    m_breakTargets[&footWhileStatement] = contBlock;
    m_builder.SetInsertPoint(body);
    visit(footWhileStatement.getStatement());
    if (m_builder.GetInsertBlock())
    {
        m_builder.CreateBr(controlling);
    }
    m_builder.SetInsertPoint(controlling);
    auto expression = visit(footWhileStatement.getExpression());
    expression = boolToi1(expression);
    m_builder.CreateCondBr(expression.value, body, contBlock);
    m_builder.SetInsertPoint(contBlock);
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::BreakStatement& breakStatement)
{
    if (!m_builder.GetInsertBlock())
    {
        return;
    }
    runDestructors(breakStatement.getScope(),
                   cld::match(breakStatement.getBreakableStatement(), [](auto* ptr) { return ptr->getScope(); }));
    m_builder.CreateBr(m_breakTargets[breakStatement.getBreakableStatement()]);
    m_builder.ClearInsertionPoint();
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::ContinueStatement& continueStatement)
{
    if (!m_builder.GetInsertBlock())
    {
        return;
    }
    runDestructors(continueStatement.getScope(),
                   cld::match(continueStatement.getLoopStatement(), [](auto* ptr) { return ptr->getScope(); }));
    m_builder.CreateBr(m_continueTargets[continueStatement.getLoopStatement()]);
    m_builder.ClearInsertionPoint();
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::SwitchStatement& switchStatement)
{
    auto expression = m_builder.GetInsertBlock() ? visit(switchStatement.getExpression()) : nullptr;
    auto& switchData = m_switches[&switchStatement];
    auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "switch.continue", m_currentFunction);
    if (switchStatement.getDefaultStatement())
    {
        switchData.defaultBlock = llvm::BasicBlock::Create(m_module.getContext(), "switch.default", m_currentFunction);
    }
    auto* switchStmt =
        expression.value ?
            m_builder.CreateSwitch(expression.value, switchData.defaultBlock ? switchData.defaultBlock : contBlock,
                                   switchStatement.getCases().size()) :
            nullptr;
    switchData.llvmSwitch = switchStmt;
    m_builder.ClearInsertionPoint();
    m_breakTargets[&switchStatement] = contBlock;
    visit(switchStatement.getStatement());
    if (m_builder.GetInsertBlock())
    {
        m_builder.CreateBr(contBlock);
    }
    m_builder.SetInsertPoint(contBlock);
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::DefaultStatement& defaultStatement)
{
    auto& switchData = m_switches[&defaultStatement.getSwitchStatement()];
    auto* bb = switchData.defaultBlock;
    if (m_builder.GetInsertBlock())
    {
        m_builder.CreateBr(bb);
    }
    m_builder.SetInsertPoint(bb);
    visit(defaultStatement.getStatement());
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::CaseStatement& caseStatement, llvm::BasicBlock* bb)
{
    auto& switchData = m_switches[&caseStatement.getSwitchStatement()];
    if (!bb && (switchData.llvmSwitch || m_builder.GetInsertBlock()))
    {
        bb = llvm::BasicBlock::Create(m_module.getContext(), "switch.case", m_currentFunction);
        if (m_builder.GetInsertBlock())
        {
            m_builder.CreateBr(bb);
        }
    }
    if (caseStatement.getStatement().is<Semantics::CaseStatement>())
    {
        if (switchData.llvmSwitch)
        {
            llvm::Constant* val =
                llvm::ConstantInt::get(switchData.llvmSwitch->getCondition()->getType(), caseStatement.getConstant());
            switchData.llvmSwitch->addCase(llvm::cast<llvm::ConstantInt>(val), bb);
        }
        visit(caseStatement.getStatement().cast<Semantics::CaseStatement>(), bb);
        return;
    }
    if (bb)
    {
        if (switchData.llvmSwitch)
        {
            llvm::Constant* val =
                llvm::ConstantInt::get(switchData.llvmSwitch->getCondition()->getType(), caseStatement.getConstant());
            switchData.llvmSwitch->addCase(llvm::cast<llvm::ConstantInt>(val), bb);
        }
        m_builder.SetInsertPoint(bb);
    }
    visit(caseStatement.getStatement());
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::GotoStatement& gotoStatement)
{
    if (!m_builder.GetInsertBlock())
    {
        return;
    }
    // Unlike in loops and other constructs a label can be anywhere in the whole function and the goto
    // can be anywhere in the whole function. Therefore we must find the first scope that both are part of.
    // That scope is the exclusive end of all scopes whose declarations must be destructed
    std::unordered_set<std::size_t> labelScopes;
    {
        auto currScope = gotoStatement.getLabel()->getScope();
        while (currScope != static_cast<std::size_t>(-1))
        {
            labelScopes.insert(currScope);
            currScope = m_programInterface.getScopes()[currScope].previousScope;
        }
    }
    auto commonScope = gotoStatement.getScope();
    while (commonScope != static_cast<std::size_t>(-1) && labelScopes.count(commonScope) == 0)
    {
        commonScope = m_programInterface.getScopes()[commonScope].previousScope;
    }

    runDestructors(gotoStatement.getScope(), commonScope);
    auto* bb = m_labels[gotoStatement.getLabel()];
    if (!bb)
    {
        bb = m_labels[gotoStatement.getLabel()] = llvm::BasicBlock::Create(
            m_module.getContext(), llvm::StringRef{gotoStatement.getLabel()->getIdentifier()->getText()},
            m_currentFunction);
    }
    m_builder.CreateBr(bb);
    m_builder.ClearInsertionPoint();
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::LabelStatement& labelStatement)
{
    auto* bb = m_labels[&labelStatement];
    if (!bb)
    {
        bb = m_labels[&labelStatement] = llvm::BasicBlock::Create(
            m_module.getContext(), llvm::StringRef{labelStatement.getIdentifier()->getText()}, m_currentFunction);
    }
    if (m_builder.GetInsertBlock())
    {
        m_builder.CreateBr(bb);
    }
    m_builder.SetInsertPoint(bb);
    visit(labelStatement.getStatement());
}

void cld::CGLLVM::CodeGenerator::visit(const Semantics::GNUASMStatement&)
{
    // TODO:
    llvm::errs() << "GNU ASM Statements are not yet implemented. Sorry";
    std::terminate();
}
