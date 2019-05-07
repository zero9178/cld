#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include "Representations.hpp"
#include "Syntax.hpp"

#include <map>

namespace OpenCL::Parser
{
    using Tokens = std::vector<OpenCL::Lexer::Token>;

    class ParsingContext final
    {
        std::vector<std::set<std::string>> m_currentScope;
        std::vector<std::map<std::string, std::int32_t>> m_enumConstants;

    public:
        std::vector<std::set<std::string>> typedefs;
        std::set<std::string> functions;
        std::map<std::string, Representations::RecordType> structOrUnions;
        std::vector<std::map<std::string, std::pair<std::size_t, std::size_t>>> enumDefinitions;

        bool isTypedef(const std::string& name);

        ParsingContext()
        {
            m_currentScope.emplace_back();
            typedefs.emplace_back();
            m_enumConstants.emplace_back();
            enumDefinitions.emplace_back();
        }

        ~ParsingContext() = default;

        ParsingContext(const ParsingContext&) = delete;

        ParsingContext(ParsingContext&&) = delete;

        ParsingContext& operator=(const ParsingContext&) = delete;

        ParsingContext& operator=(ParsingContext&&) = delete;

        void addToScope(std::string name)
        {
            if (auto result = m_currentScope.back().insert(std::move(name)); !result.second)
            {
                throw std::runtime_error(*result.first + " already exists in this scope");
            }
        }

        bool isInScope(const std::string& name)
        {
            for (auto& iter : m_currentScope)
            {
                if (iter.count(name))
                {
                    return true;
                }
            }
            return false;
        }

        void pushScope()
        {
            m_currentScope.emplace_back();
            typedefs.emplace_back();
            m_enumConstants.emplace_back();
            enumDefinitions.emplace_back();
        }

        void popScope()
        {
            m_currentScope.pop_back();
            typedefs.emplace_back();
            m_enumConstants.pop_back();
            enumDefinitions.pop_back();
        }

        void addEnumConstant(const std::string& name, std::int32_t value)
        {
            if (!m_enumConstants.back().insert({name, value}).second)
            {
                throw std::runtime_error("Enum constant called " + name + " alreaedy exists");
            }
        }

        std::int32_t* getEnumConstant(const std::string& name)
        {
            for (auto iter = m_enumConstants.rbegin(); iter != m_enumConstants.rend(); iter++)
            {
                auto result = iter->find(name);
                if (result != iter->end())
                {
                    return &result->second;
                }
            }
            return nullptr;
        }
    };

    Expected<Syntax::TranslationUnit, FailureReason> buildTree(const std::vector<Lexer::Token>& tokens);

    Expected<Syntax::TranslationUnit, FailureReason>
        parseTranslationUnit(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::ExternalDeclaration, FailureReason>
        parseExternalDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::FunctionDefinition, FailureReason>
        parseFunctionDefinition(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::Declaration, FailureReason> parseDeclaration(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::DeclarationSpecifier, OpenCL::FailureReason>
        parseDeclarationSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::SpecifierQualifier, FailureReason>
        parseSpecifierQualifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::Declarator, FailureReason> parseDeclarator(Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::DirectDeclarator, FailureReason>
        parseDirectDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::ParameterTypeList, FailureReason>
        parseParameterTypeList(Tokens::const_iterator& tokens, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::AbstractDeclarator, FailureReason>
        parseAbstractDeclarator(Tokens::const_iterator& tokens, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::DirectAbstractDeclarator, FailureReason>
        parseDirectAbstractDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                      ParsingContext& context);

    Expected<Syntax::ParameterList, FailureReason>
        parseParameterList(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::Pointer, FailureReason> parsePointer(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                          ParsingContext& context);

    Expected<Syntax::StructOrUnionSpecifier, FailureReason>
        parseStructOrUnionSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::EnumSpecifier, FailureReason>
        parseEnumSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::EnumDeclaration, FailureReason>
        parseEnumDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::CompoundStatement, FailureReason>
        parseCompoundStatement(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::CompoundItem, FailureReason>
        parseCompoundItem(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::Initializer, FailureReason> parseInitializer(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::InitializerList, FailureReason>
        parseInitializerList(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::Statement, FailureReason> parseStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                              ParsingContext& context);

    Expected<Syntax::Expression, FailureReason> parseExpression(Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::AssignmentExpression, FailureReason>
        parseAssignmentExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::ConditionalExpression, FailureReason>
        parseConditionalExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::LogicalOrExpression, FailureReason>
        parseLogicalOrExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::LogicalAndExpression, FailureReason>
        parseLogicalAndExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::BitOrExpression, FailureReason>
        parseBitOrExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::BitXorExpression, FailureReason>
        parseBitXorExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::BitAndExpression, FailureReason>
        parseBitAndExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::EqualityExpression, FailureReason>
        parseEqualityExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::RelationalExpression, FailureReason>
        parseRelationalExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::ShiftExpression, FailureReason>
        parseShiftExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::AdditiveExpression, FailureReason>
        parseAdditiveExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::Term, FailureReason> parseTerm(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                    ParsingContext& context);

    Expected<Syntax::TypeName, FailureReason> parseTypeName(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                            ParsingContext& context);

    Expected<Syntax::CastExpression, FailureReason>
        parseCastExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::UnaryExpression, FailureReason>
        parseUnaryExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::PostFixExpression, FailureReason>
        parsePostFixExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);

    Expected<Syntax::PrimaryExpression, FailureReason>
        parsePrimaryExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, ParsingContext& context);
} // namespace OpenCL::Parser

#endif // OPENCLPARSER_PARSER_HPP
