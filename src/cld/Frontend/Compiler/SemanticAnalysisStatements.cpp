#include "SemanticAnalysis.hpp"

#include "ErrorMessages.hpp"

std::unique_ptr<cld::Semantics::CompoundStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::CompoundStatement& node, bool pushScope)
{
    std::optional<decltype(this->pushScope())> scope;
    if (pushScope)
    {
        scope.emplace(this->pushScope());
    }
    std::vector<CompoundStatement::Variant> result;
    for (auto& iter : node.getBlockItems())
    {
        auto tmp = visit(iter);
        result.insert(result.end(), std::move_iterator(tmp.begin()), std::move_iterator(tmp.end()));
    }
    return std::make_unique<CompoundStatement>(getCurrentScopePoint(), node.begin(), std::move(result), node.end() - 1);
}

std::vector<cld::Semantics::CompoundStatement::Variant>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::CompoundItem& node)
{
    std::vector<CompoundStatement::Variant> result;
    cld::match(
        node,
        [&](const Syntax::Declaration& declaration)
        {
            auto tmp = visit(declaration);
            result.reserve(result.size() + tmp.size());
            std::transform(std::move_iterator(tmp.begin()), std::move_iterator(tmp.end()), std::back_inserter(result),
                           [](DeclRetVariant&& variant)
                           {
                               return cld::match(std::move(variant),
                                                 [](auto&& value) -> CompoundStatement::Variant
                                                 { return {std::move(value)}; });
                           });
        },
        [&](const Syntax::Statement& statement) { result.emplace_back(visit(statement)); });
    return result;
}

cld::IntrVarPtr<cld::Semantics::Statement> cld::Semantics::SemanticAnalysis::visit(const Syntax::Statement& node)
{
    return cld::match(
        node, [&](const auto& node) -> cld::IntrVarPtr<Statement> { return visit(node); },
        [&](const Syntax::ExpressionStatement& node) -> cld::IntrVarPtr<Statement>
        {
            if (!node.getOptionalExpression())
            {
                return std::make_unique<ExpressionStatement>(getCurrentScopePoint(), nullptr);
            }
            auto expression = visit(*node.getOptionalExpression());
            checkUnusedResult(*expression);
            return std::make_unique<ExpressionStatement>(getCurrentScopePoint(), std::move(expression));
        });
}

std::unique_ptr<cld::Semantics::ReturnStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::ReturnStatement& node)
{
    auto& ft = getCurrentFunctionScope()->currentFunction->getType();
    if (!node.getExpression())
    {
        if (!isVoid(ft.getReturnType()))
        {
            log(Errors::Semantics::CANNOT_RETURN_NO_VALUE_FROM_FUNCTION_N_WITH_RETURN_TYPE_N.args(
                node, m_sourceInterface, *getCurrentFunctionScope()->currentFunction->getNameToken(),
                ft.getReturnType(), node));
        }
        return std::make_unique<ReturnStatement>(getCurrentScopePoint(), nullptr);
    }
    if (isVoid(ft.getReturnType()))
    {
        log(Errors::Semantics::CANNOT_RETURN_VALUE_FROM_FUNCTION_N_WITH_VOID_RETURN_TYPE.args(
            node, m_sourceInterface, *getCurrentFunctionScope()->currentFunction->getNameToken(), node));
    }
    auto value = lvalueConversion(visit(*node.getExpression()));
    doAssignmentLikeConstraints(
        ft.getReturnType(), value,
        [&]
        {
            log(Errors::Semantics::EXPECTED_RETURN_VALUE_TO_BE_AN_ARITHMETIC_TYPE.args(*value, m_sourceInterface,
                                                                                       *node.begin(), *value));
        },
        [&]
        {
            log(Errors::Semantics::EXPECTED_RETURN_VALUE_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
                *value, m_sourceInterface, *node.begin(), *value));
        },
        [&] {},
        [&]
        {
            log(Errors::Semantics::CANNOT_RETURN_VARIABLE_OF_TYPE_N_TO_INCOMPATIBLE_RETURN_TYPE_N.args(
                *value, m_sourceInterface, *value, ft.getReturnType(), *node.begin()));
        },
        [&] {
            log(Errors::Semantics::EXPECTED_RETURN_VALUE_TO_BE_NULL_2.args(*value, m_sourceInterface, *node.begin(),
                                                                           *value));
        },
        [&](const ConstValue& constant)
        {
            log(Errors::Semantics::EXPECTED_RETURN_VALUE_TO_BE_NULL.args(*value, m_sourceInterface, *node.begin(),
                                                                         *value, constant));
        },
        [&]
        {
            log(Errors::Semantics::EXPECTED_RETURN_VALUE_TO_BE_A_POINTER_TYPE.args(*value, m_sourceInterface,
                                                                                   *node.begin(), *value));
        },
        [&]
        {
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
    return std::make_unique<ReturnStatement>(getCurrentScopePoint(), std::move(value));
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
        return std::make_unique<IfStatement>(getCurrentScopePoint(), std::move(value), std::move(trueBranch), nullptr);
    }
    auto falseBranch = visit(*node.getElseBranch());
    return std::make_unique<IfStatement>(getCurrentScopePoint(), std::move(value), std::move(trueBranch),
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
            checkUnusedResult(*cld::get<IntrVarPtr<ExpressionBase>>(initial));
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
        checkUnusedResult(*iteration);
    }

    auto forStatement = std::make_unique<ForStatement>(getCurrentScopePoint(), node.begin(), ForStatement::Variant{},
                                                       nullptr, nullptr, nullptr);
    auto loopGuard = pushLoop(forStatement.get());
    auto statement = visit(node.getStatement());
    *forStatement = ForStatement(getCurrentScopePoint(), node.begin(), std::move(initial), std::move(controlling),
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
    auto loop = std::make_unique<HeadWhileStatement>(getCurrentScopePoint(), nullptr, nullptr);
    auto loopGuard = pushLoop(loop.get());
    auto statement = visit(node.getStatement());
    *loop = HeadWhileStatement(getCurrentScopePoint(), toBool(std::move(expression)), std::move(statement));
    return loop;
}

std::unique_ptr<cld::Semantics::FootWhileStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::FootWhileStatement& node)
{
    auto loop =
        std::make_unique<FootWhileStatement>(getCurrentScopePoint(), nullptr, std::make_unique<ErrorExpression>(node));
    auto guard = pushLoop(loop.get());
    auto statement = visit(node.getStatement());
    auto expression = lvalueConversion(visit(node.getExpression()));
    if (!expression->getType().isUndefined() && !isScalar(expression->getType()))
    {
        log(Errors::Semantics::CONTROLLING_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE.args(
            *expression, m_sourceInterface, *expression));
    }
    *loop = FootWhileStatement(getCurrentScopePoint(), std::move(statement), toBool(std::move(expression)));
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
    return std::make_unique<BreakStatement>(getCurrentScopePoint(), m_breakableStatements.back());
}

std::unique_ptr<cld::Semantics::ContinueStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::ContinueStatement& node)
{
    if (m_loopStatements.empty())
    {
        log(Errors::Semantics::CONTINUE_MUST_BE_WITHIN_A_LOOP_STATEMENT.args(node, m_sourceInterface, node));
        return {};
    }
    return std::make_unique<ContinueStatement>(getCurrentScopePoint(), m_loopStatements.back());
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
    auto switchStmt = std::make_unique<SwitchStatement>(getCurrentScopePoint(), std::move(expression), nullptr);
    auto guard = pushSwitch(*switchStmt);
    auto statement = visit(node.getStatement());
    *switchStmt = SwitchStatement(getCurrentScopePoint(), std::move(*switchStmt).getExpression(), std::move(statement),
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
    for (auto& iter : scopeIterators(m_currentScope))
    {
        if (iter.thisScopeId == switchStatement.getScopePoint().getScopeId() || illegalJump)
        {
            break;
        }
        for (auto& [name, decl] : iter.declarations)
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
            else if (auto* typeDef = std::get_if<TypedefInfo*>(&decl.declared))
            {
                if (isVariablyModified((*typeDef)->type))
                {
                    isTypedef = true;
                    illegalJump = true;
                    identifier = decl.identifier;
                    type = (*typeDef)->type.data();
                    break;
                }
            }
        }
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
    auto defaultStmt = std::make_unique<DefaultStatement>(getCurrentScopePoint(), node.getDefaultToken(),
                                                          node.getColonToken(), nullptr, switchStmt);
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
    *defaultStmt = DefaultStatement(getCurrentScopePoint(), node.getDefaultToken(), node.getColonToken(),
                                    std::move(statement), switchStmt);
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
    constant = constant->castTo(switchStmt.getExpression().getType(), this, getLanguageOptions());
    auto caseStmt = std::make_unique<CaseStatement>(getCurrentScopePoint(), node.getCaseToken(),
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
    *caseStmt = CaseStatement(getCurrentScopePoint(), node.getCaseToken(), caseStmt->getConstant(),
                              node.getColonToken(), std::move(statement), switchStmt);
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
        auto scopeIter = scopeIterators(m_currentScope);
        std::unordered_set<std::size_t> scopes;
        std::transform(std::next(scopeIter.begin()), scopeIter.end(), std::inserter(scopes, scopes.begin()),
                       std::mem_fn(&Scope::thisScopeId));

        bool illegalJump = false;
        bool isTypedef = false;
        const Lexer::CToken* identifier = nullptr;
        const Semantics::Type* type = nullptr;
        auto iterators = scopeIterators(result->second->getScopePoint().getScopeId());
        auto end = std::find_if(iterators.begin(), iterators.end(),
                                [&](const Scope& scope) { return scopes.count(scope.thisScopeId); });
        CLD_ASSERT(end != iterators.end());
        for (auto& decl : declIterators(result->second->getScopePoint(), end->thisScopeId))
        {
            if (auto* val = std::get_if<VariableDeclaration*>(&decl.declared);
                val && isVariablyModified((*val)->getType()))
            {
                illegalJump = true;
                identifier = decl.identifier;
                type = &(*val)->getType();
                break;
            }
            if (auto* typeDef = std::get_if<TypedefInfo*>(&decl.declared);
                typeDef && isVariablyModified((*typeDef)->type))
            {
                illegalJump = true;
                isTypedef = true;
                identifier = decl.identifier;
                type = (*typeDef)->type.data();
                break;
            }
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

        *gotoStmt = GotoStatement(getCurrentScopePoint(), result->second);
    }
    m_scheduledGotos.clear();
}

std::unique_ptr<cld::Semantics::GotoStatement>
    cld::Semantics::SemanticAnalysis::visit(const Syntax::GotoStatement& node)
{
    auto gotoStatement = std::make_unique<GotoStatement>(getCurrentScopePoint(), nullptr);
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

    auto storage = std::make_unique<LabelStatement>(getCurrentScopePoint(), node.getIdentifierToken(),
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
        *storage = LabelStatement(getCurrentScopePoint(), node.getIdentifierToken(),
                                  getCurrentScope().declarations.size(), visit(**statement));
    }

    return storage;
}

std::unique_ptr<cld::Semantics::GNUASMStatement>
    cld::Semantics::SemanticAnalysis::visit(const cld::Syntax::GNUASMStatement&)
{
    // TODO:
    return std::make_unique<GNUASMStatement>(getCurrentScopePoint());
}

void cld::Semantics::SemanticAnalysis::checkUnusedResult(const cld::Semantics::ExpressionBase& expression)
{
    auto* callExpression = expression.tryAs<CallExpression>();
    if (!callExpression)
    {
        return;
    }
    auto* conversion = callExpression->getFunctionExpression().tryAs<Conversion>();
    if (!conversion || conversion->getKind() != Conversion::LValue)
    {
        return;
    }
    auto* function = conversion->getExpression().tryAs<DeclarationRead>();
    if (!function
        || (!function->getDeclRead().is<FunctionDefinition>() && !function->getDeclRead().is<FunctionDeclaration>()
            && !function->getDeclRead().is<BuiltinFunction>()))
    {
        return;
    }
    auto& attributes = function->getDeclRead().match(
        [](const auto& value) -> const AttributeHolder<FunctionAttribute>& { return value; },
        [](const VariableDeclaration&) -> const AttributeHolder<FunctionAttribute>& { CLD_UNREACHABLE; });
    if (!attributes.hasAttribute<WarnUnusedResultAttribute>())
    {
        return;
    }
    log(Warnings::Semantics::RESULT_OF_CALL_TO_FUNCTION_N_UNUSED.args(
        *function->getIdentifierToken(), m_sourceInterface, *function->getIdentifierToken()));
}
