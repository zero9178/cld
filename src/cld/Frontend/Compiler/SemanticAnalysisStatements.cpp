#include "SemanticAnalysis.hpp"

#include "ErrorMessages.hpp"

std::unique_ptr<cld::Semantics::ReturnStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::ReturnStatement& node)
{
    auto& ft = getCurrentFunctionScope()->currentFunction->getType().cast<FunctionType>();
    if (!node.getExpression())
    {
        if (!isVoid(ft.getReturnType()))
        {
            log(Errors::Semantics::CANNOT_RETURN_NO_VALUE_FROM_FUNCTION_N_WITH_RETURN_TYPE_N.args(
                node, m_sourceInterface, *getCurrentFunctionScope()->currentFunction->getNameToken(),
                ft.getReturnType(), node));
        }
        return std::make_unique<ReturnStatement>(m_currentScope, nullptr);
    }
    if (isVoid(ft.getReturnType()))
    {
        log(Errors::Semantics::CANNOT_RETURN_VALUE_FROM_FUNCTION_N_WITH_VOID_RETURN_TYPE.args(
            node, m_sourceInterface, *getCurrentFunctionScope()->currentFunction->getNameToken(), node));
    }
    auto value = lvalueConversion(visit(*node.getExpression()));
    doAssignmentLikeConstraints(
        ft.getReturnType(), value,
        [&] {
            log(Errors::Semantics::EXPECTED_RETURN_VALUE_TO_BE_AN_ARITHMETIC_TYPE.args(*value, m_sourceInterface,
                                                                                       *node.begin(), *value));
        },
        [&] {
            log(Errors::Semantics::EXPECTED_RETURN_VALUE_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                *value, m_sourceInterface, *node.begin(), *value));
        },
        [&] {},
        [&] {
            log(Errors::Semantics::CANNOT_RETURN_VARIABLE_OF_TYPE_N_TO_INCOMPATIBLE_RETURN_TYPE_N.args(
                *value, m_sourceInterface, *value, ft.getReturnType(), *node.begin()));
        },
        [&] {
            log(Errors::Semantics::EXPECTED_RETURN_VALUE_TO_BE_NULL_2.args(*value, m_sourceInterface, *node.begin(),
                                                                           *value));
        },
        [&](const ConstValue& constant) {
            log(Errors::Semantics::EXPECTED_RETURN_VALUE_TO_BE_NULL.args(*value, m_sourceInterface, *node.begin(),
                                                                         *value, constant));
        },
        [&] {
            log(Errors::Semantics::EXPECTED_RETURN_VALUE_TO_BE_A_POINTER_TYPE.args(*value, m_sourceInterface,
                                                                                   *node.begin(), *value));
        },
        [&] {
            if (isVoid(ft.getReturnType()))
            {
                log(Errors::Semantics::CANNOT_RETURN_FUNCTION_POINTER_WITH_VOID_POINTER_RETURN_TYPE.args(
                    *value, m_sourceInterface, *node.begin(), *value));
            }
            else
            {
                log(Errors::Semantics::CANNOT_RETURN_VOID_POINTER_WITH_FUNCTION_POINTER_RETURN_TYPE.args(
                    *value, m_sourceInterface, *node.begin(), *value));
            }
        });
    return std::make_unique<ReturnStatement>(m_currentScope, std::move(value));
}

std::unique_ptr<cld::Semantics::IfStatement> cld::Semantics::SemanticAnalysis::visit(const Syntax::IfStatement& node)
{
    auto value = lvalueConversion(visit(node.getExpression()));
    if (!value->getType().isUndefined() && !isScalar(value->getType()))
    {
        log(Errors::Semantics::CONTROLLING_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            *value, m_sourceInterface, *value));
    }
    value = toBool(std::move(value));
    auto trueBranch = visit(node.getBranch());
    if (!node.getElseBranch())
    {
        return std::make_unique<IfStatement>(m_currentScope, std::move(value), std::move(trueBranch), nullptr);
    }
    auto falseBranch = visit(*node.getElseBranch());
    return std::make_unique<IfStatement>(m_currentScope, std::move(value), std::move(trueBranch),
                                         std::move(falseBranch));
}

std::unique_ptr<cld::Semantics::ForStatement> cld::Semantics::SemanticAnalysis::visit(const Syntax::ForStatement& node)
{
    std::optional<decltype(pushScope())> scope;
    ForStatement::Variant initial;
    if (std::holds_alternative<Syntax::Declaration>(node.getInitial()))
    {
        scope.emplace(pushScope());
        auto& declarations = cld::get<Syntax::Declaration>(node.getInitial());
        for (auto& iter : declarations.getDeclarationSpecifiers())
        {
            auto* storageClassSpecifier = std::get_if<Syntax::StorageClassSpecifier>(&iter);
            if (!storageClassSpecifier)
            {
                continue;
            }
            if (storageClassSpecifier->getSpecifier() != Syntax::StorageClassSpecifier::Auto
                && storageClassSpecifier->getSpecifier() != Syntax::StorageClassSpecifier::Register)
            {
                log(Errors::Semantics::ONLY_AUTO_OR_REGISTER_ALLOWED_IN_FOR_STATEMENTS_DECLARATION.args(
                    *storageClassSpecifier, m_sourceInterface, *storageClassSpecifier));
            }
        }
        auto result = visit(declarations);
        auto& vector = initial.emplace<std::vector<IntrVarPtr<Declaration>>>();
        for (auto& iter : result)
        {
            if (std::holds_alternative<IntrVarPtr<Declaration>>(iter))
            {
                vector.push_back(cld::get<IntrVarPtr<Declaration>>(std::move(iter)));
            }
        }
    }
    else
    {
        auto& expression = cld::get<std::unique_ptr<Syntax::Expression>>(node.getInitial());
        if (expression)
        {
            initial = visit(*expression);
        }
    }

    IntrVarPtr<ExpressionBase> controlling;
    if (node.getControlling())
    {
        auto result = lvalueConversion(visit(*node.getControlling()));
        if (!result->getType().isUndefined() && !isScalar(result->getType()))
        {
            log(Errors::Semantics::CONTROLLING_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                *result, m_sourceInterface, *result));
        }
        controlling = toBool(std::move(result));
    }

    IntrVarPtr<ExpressionBase> iteration;
    if (node.getPost())
    {
        iteration = visit(*node.getPost());
    }

    auto forStatement = std::make_unique<ForStatement>(m_currentScope, node.begin(), ForStatement::Variant{}, nullptr,
                                                       nullptr, nullptr);
    auto loopGuard = pushLoop(forStatement.get());
    auto statement = visit(node.getStatement());
    *forStatement = ForStatement(m_currentScope, node.begin(), std::move(initial), std::move(controlling),
                                 std::move(iteration), std::move(statement));
    return forStatement;
}

std::unique_ptr<cld::Semantics::HeadWhileStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::HeadWhileStatement& node)
{
    auto expression = lvalueConversion(visit(node.getExpression()));
    if (!expression->getType().isUndefined() && !isScalar(expression->getType()))
    {
        log(Errors::Semantics::CONTROLLING_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            *expression, m_sourceInterface, *expression));
    }
    auto loop = std::make_unique<HeadWhileStatement>(m_currentScope, nullptr, nullptr);
    auto loopGuard = pushLoop(loop.get());
    auto statement = visit(node.getStatement());
    *loop = HeadWhileStatement(m_currentScope, toBool(std::move(expression)), std::move(statement));
    return loop;
}

std::unique_ptr<cld::Semantics::FootWhileStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::FootWhileStatement& node)
{
    auto loop = std::make_unique<FootWhileStatement>(m_currentScope, nullptr, std::make_unique<ErrorExpression>(node));
    auto guard = pushLoop(loop.get());
    auto statement = visit(node.getStatement());
    auto expression = lvalueConversion(visit(node.getExpression()));
    if (!expression->getType().isUndefined() && !isScalar(expression->getType()))
    {
        log(Errors::Semantics::CONTROLLING_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            *expression, m_sourceInterface, *expression));
    }
    *loop = FootWhileStatement(m_currentScope, std::move(statement), toBool(std::move(expression)));
    return loop;
}

std::unique_ptr<cld::Semantics::BreakStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::BreakStatement& node)
{
    if (m_breakableStatements.empty())
    {
        log(Errors::Semantics::BREAK_MUST_BE_WITHIN_A_SWITCH_OR_LOOP_STATEMENT.args(node, m_sourceInterface, node));
        return {};
    }
    return std::make_unique<BreakStatement>(m_currentScope, m_breakableStatements.back());
}

std::unique_ptr<cld::Semantics::ContinueStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::ContinueStatement& node)
{
    if (m_loopStatements.empty())
    {
        log(Errors::Semantics::CONTINUE_MUST_BE_WITHIN_A_LOOP_STATEMENT.args(node, m_sourceInterface, node));
        return {};
    }
    return std::make_unique<ContinueStatement>(m_currentScope, m_loopStatements.back());
}

std::unique_ptr<cld::Semantics::SwitchStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::SwitchStatement& node)
{
    auto expression = integerPromotion(visit(node.getExpression()));
    if (!expression->getType().isUndefined() && !isInteger(expression->getType()))
    {
        log(Errors::Semantics::CONTROLLING_EXPRESSION_MUST_BE_AN_INTEGER_TYPE.args(*expression, m_sourceInterface,
                                                                                   *expression));
        expression = std::make_unique<ErrorExpression>(node);
    }
    auto switchStmt = std::make_unique<SwitchStatement>(m_currentScope, std::move(expression), nullptr);
    auto guard = pushSwitch(*switchStmt);
    auto statement = visit(node.getStatement());
    *switchStmt = SwitchStatement(m_currentScope, std::move(*switchStmt).getExpression(), std::move(statement),
                                  std::move(m_switchStatements.back().cases), m_switchStatements.back().defaultStmt);
    return switchStmt;
}

void cld::Semantics::SemanticAnalysis::checkForIllegalSwitchJumps(
    std::tuple<const Lexer::CToken&, const Lexer::CToken&> loc, const cld::Semantics::SwitchStatement& switchStatement,
    bool isCaseOrDefault)
{
    bool illegalJump = false;
    bool isTypedef = false;
    const Lexer::CToken* identifier = nullptr;
    const Semantics::Type* type = nullptr;
    auto curr = m_currentScope;
    while (!illegalJump && curr != switchStatement.getScope())
    {
        for (auto& [name, decl] : m_scopes[curr].declarations)
        {
            if (auto* val = std::get_if<VariableDeclaration*>(&decl.declared))
            {
                if (isVariablyModified((*val)->getType()))
                {
                    illegalJump = true;
                    identifier = decl.identifier;
                    type = &(*val)->getType();
                    break;
                }
            }
            else if (auto* typeDef = std::get_if<IntrVarValue<Type>>(&decl.declared))
            {
                if (isVariablyModified(**typeDef))
                {
                    isTypedef = true;
                    illegalJump = true;
                    identifier = decl.identifier;
                    type = &**typeDef;
                    break;
                }
            }
        }
        curr = m_scopes[curr].previousScope;
    }

    if (!illegalJump)
    {
        return;
    }
    if (!isTypedef)
    {
        if (isCaseOrDefault)
        {
            log(Errors::Semantics::CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N.args(
                loc, m_sourceInterface, *identifier, loc));
        }
        else
        {
            log(Errors::Semantics::DEFAULT_CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N.args(
                loc, m_sourceInterface, *identifier, loc));
        }
        log(Notes::Semantics::VARIABLY_MODIFIED_VARIABLE_N_WITH_TYPE_N_HERE.args(*identifier, m_sourceInterface,
                                                                                 *identifier, *type));
    }
    else
    {
        if (isCaseOrDefault)
        {
            log(Errors::Semantics::CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_TYPEDEF_N.args(
                loc, m_sourceInterface, *identifier, loc));
        }
        else
        {
            log(Errors::Semantics::DEFAULT_CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_TYPEDEF_N.args(
                loc, m_sourceInterface, *identifier, loc));
        }
        log(Notes::Semantics::VARIABLY_MODIFIED_TYPEDEF_N_HERE.args(*identifier, m_sourceInterface, *identifier));
    }
}

std::unique_ptr<cld::Semantics::DefaultStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::DefaultStatement& node)
{
    if (m_switchStatements.empty())
    {
        log(Errors::Semantics::DEFAULT_MUST_BE_WITHIN_A_SWITCH_STATEMENT.args(
            std::forward_as_tuple(node.getDefaultToken(), node.getColonToken()), m_sourceInterface,
            std::forward_as_tuple(node.getDefaultToken(), node.getColonToken())));
        return nullptr;
    }
    auto& switchStmt = *m_switchStatements.back().switchStatement;
    checkForIllegalSwitchJumps(std::forward_as_tuple(*node.getDefaultToken(), *node.getColonToken()), switchStmt,
                               false);
    auto defaultStmt = std::make_unique<DefaultStatement>(m_currentScope, node.getDefaultToken(), node.getColonToken(),
                                                          nullptr, switchStmt);
    if (m_switchStatements.back().defaultStmt)
    {
        log(Errors::Semantics::REDEFINITION_OF_DEFAULT.args(
            std::forward_as_tuple(node.getDefaultToken(), node.getColonToken()), m_sourceInterface,
            std::forward_as_tuple(node.getDefaultToken(), node.getColonToken())));
        log(Notes::Semantics::PREVIOUS_DEFAULT_HERE.args(
            std::forward_as_tuple(m_switchStatements.back().defaultStmt->getDefaultToken(),
                                  m_switchStatements.back().defaultStmt->getColonToken()),
            m_sourceInterface,
            std::forward_as_tuple(m_switchStatements.back().defaultStmt->getDefaultToken(),
                                  m_switchStatements.back().defaultStmt->getColonToken())));
    }
    else
    {
        m_switchStatements.back().defaultStmt = defaultStmt.get();
    }
    auto statement = visit(node.getStatement());
    *defaultStmt = DefaultStatement(m_currentScope, node.getDefaultToken(), node.getColonToken(), std::move(statement),
                                    switchStmt);
    return defaultStmt;
}

std::unique_ptr<cld::Semantics::CaseStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::CaseStatement& node)
{
    auto expr = visit(node.getConstantExpression());
    if (!expr->getType().isUndefined() && !isInteger(expr->getType()))
    {
        log(Errors::Semantics::ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS.args(*expr, m_sourceInterface,
                                                                                          *expr));
    }
    auto constant = evaluateConstantExpression(*expr);
    if (!constant)
    {
        for (auto& iter : constant.error())
        {
            log(iter);
        }
    }
    if (m_switchStatements.empty())
    {
        log(Errors::Semantics::CASE_MUST_BE_WITHIN_A_SWITCH_STATEMENT.args(
            std::forward_as_tuple(node.getCaseToken(), node.getColonToken()), m_sourceInterface,
            std::forward_as_tuple(node.getCaseToken(), node.getColonToken())));
        return {};
    }
    auto& switchStmt = *m_switchStatements.back().switchStatement;
    checkForIllegalSwitchJumps(std::forward_as_tuple(*node.getCaseToken(), *node.getColonToken()), switchStmt, true);
    if (!constant || !std::holds_alternative<llvm::APSInt>(constant->getValue()))
    {
        return {};
    }
    if (switchStmt.getExpression().isUndefined())
    {
        return {};
    }
    constant = constant->castTo(switchStmt.getExpression().getType(), this, m_sourceInterface.getLanguageOptions());
    auto caseStmt = std::make_unique<CaseStatement>(m_currentScope, node.getCaseToken(),
                                                    cld::get<llvm::APSInt>(constant->getValue()), node.getColonToken(),
                                                    nullptr, switchStmt);
    auto [prev, notRedefinition] = m_switchStatements.back().cases.emplace(caseStmt->getConstant(), caseStmt.get());
    if (!notRedefinition)
    {
        log(Errors::Semantics::REDEFINITION_OF_CASE_WITH_VALUE_N.args(
            std::forward_as_tuple(node.getCaseToken(), node.getColonToken()), m_sourceInterface, *constant, *expr));
        log(Notes::Semantics::PREVIOUS_CASE_HERE.args(
            std::forward_as_tuple(prev->second->getCaseToken(), prev->second->getColonToken()), m_sourceInterface,
            std::forward_as_tuple(prev->second->getCaseToken() + 1, prev->second->getColonToken() - 1), *constant));
    }
    auto statement = visit(node.getStatement());
    *caseStmt = CaseStatement(m_currentScope, node.getCaseToken(), caseStmt->getConstant(), node.getColonToken(),
                              std::move(statement), switchStmt);
    return caseStmt;
}

void cld::Semantics::SemanticAnalysis::resolveGotos()
{
    for (auto& [token, gotoStmt] : m_scheduledGotos)
    {
        auto result = getCurrentFunctionScope()->labels.find(token->getText());
        if (result == getCurrentFunctionScope()->labels.end())
        {
            log(Errors::Semantics::NO_LABEL_CALLED_N_FOUND_IN_FUNCTION_N.args(
                *token, m_sourceInterface, *token, *getCurrentFunctionScope()->currentFunction->getNameToken()));
            continue;
        }

        // The current scope always needs to be checked as different declarations might be visible at the point of both
        // the label and the goto even if they're in the same scope
        std::unordered_set<std::size_t> scopes;
        auto curr = m_scopes[m_currentScope].previousScope;
        while (curr != static_cast<std::size_t>(-1))
        {
            scopes.insert(curr);
            curr = m_scopes[curr].previousScope;
        }

        bool illegalJump = false;
        bool isTypedef = false;
        const Lexer::CToken* identifier = nullptr;
        const Semantics::Type* type = nullptr;
        auto labelScope = result->second->getScope();
        while (!illegalJump && scopes.count(labelScope) == 0)
        {
            for (auto iter = m_scopes[labelScope].declarations.begin(); iter != m_scopes[labelScope].declarations.end();
                 iter++)
            {
                if (labelScope == result->second->getScope()
                    && static_cast<std::size_t>(iter - m_scopes[labelScope].declarations.begin())
                           >= result->second->getSizeOfCurrentScope())
                {
                    break;
                }
                auto& [name, decl] = *iter;
                if (auto* val = std::get_if<VariableDeclaration*>(&decl.declared))
                {
                    if (isVariablyModified((*val)->getType()))
                    {
                        illegalJump = true;
                        identifier = decl.identifier;
                        type = &(*val)->getType();
                        break;
                    }
                }
                if (auto* typeDef = std::get_if<IntrVarValue<Type>>(&decl.declared))
                {
                    if (isVariablyModified(**typeDef))
                    {
                        isTypedef = true;
                        illegalJump = true;
                        identifier = decl.identifier;
                        type = &**typeDef;
                        break;
                    }
                }
            }
            labelScope = m_scopes[labelScope].previousScope;
        }

        if (illegalJump)
        {
            if (!isTypedef)
            {
                log(Errors::Semantics::GOTO_TO_LABEL_N_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N.args(
                    *result->second->getIdentifier(), m_sourceInterface, *result->second->getIdentifier(),
                    *identifier));
                log(Notes::Semantics::VARIABLY_MODIFIED_VARIABLE_N_WITH_TYPE_N_HERE.args(*identifier, m_sourceInterface,
                                                                                         *identifier, *type));
            }
            else
            {
                log(Errors::Semantics::GOTO_TO_LABEL_N_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_TYPEDEF_N.args(
                    *result->second->getIdentifier(), m_sourceInterface, *result->second->getIdentifier(),
                    *identifier));
                log(Notes::Semantics::VARIABLY_MODIFIED_TYPEDEF_N_HERE.args(*identifier, m_sourceInterface,
                                                                            *identifier));
            }
            log(Notes::Semantics::LABEL_N_HERE.args(*result->second->getIdentifier(), m_sourceInterface,
                                                    *result->second->getIdentifier()));
        }

        *gotoStmt = GotoStatement(m_currentScope, result->second);
    }
    m_scheduledGotos.clear();
}

std::unique_ptr<cld::Semantics::GotoStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::GotoStatement& node)
{
    auto gotoStatement = std::make_unique<GotoStatement>(m_currentScope, nullptr);
    m_scheduledGotos.emplace_back(node.getIdentifier(), gotoStatement.get());
    return gotoStatement;
}

std::unique_ptr<cld::Semantics::LabelStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::LabelStatement& node)
{
    if (node.getIdentifierToken()->getText() == "__func__")
    {
        log(Errors::Semantics::DECLARING_LABEL_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR.args(
            *node.getIdentifierToken(), m_sourceInterface, *node.getIdentifierToken()));
        return {};
    }

    auto storage = std::make_unique<LabelStatement>(m_currentScope, node.getIdentifierToken(),
                                                    getCurrentScope().declarations.size(), nullptr);
    auto [prev, notRedefinition] =
        getCurrentFunctionScope()->labels.emplace(node.getIdentifierToken()->getText(), storage.get());
    if (!notRedefinition)
    {
        log(Errors::Semantics::REDEFINITION_OF_LABEL_N.args(*node.getIdentifierToken(), m_sourceInterface,
                                                            *node.getIdentifierToken()));
        log(Notes::PREVIOUSLY_DECLARED_HERE.args(*prev->second->getIdentifier(), m_sourceInterface,
                                                 *prev->second->getIdentifier()));
    }
    else if (auto* statement = std::get_if<std::unique_ptr<Syntax::Statement>>(&node.getStatementOrAttribute()))
    {
        *storage = LabelStatement(m_currentScope, node.getIdentifierToken(), getCurrentScope().declarations.size(),
                                  visit(**statement));
    }

    return storage;
}

std::unique_ptr<cld::Semantics::GNUASMStatement>
    cld::Semantics::SemanticAnalysis::visit(const cld::Syntax::GNUASMStatement&)
{
    // TODO:
    return std::make_unique<GNUASMStatement>(m_currentScope);
}
