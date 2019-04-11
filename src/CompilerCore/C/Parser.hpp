#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include "Syntax.hpp"
#include <map>

namespace OpenCL::Parser
{
    using Tokens = std::vector<OpenCL::Lexer::Token>;

    class ParsingContext final
    {
        std::vector<std::set<std::string>> m_currentScope;
        std::map<std::string,std::int32_t> m_enumConstants;

    public:

        std::set<std::string> typedefs;
        std::map<std::string,const OpenCL::Syntax::StructOrUnionSpecifier*> structOrUnions;
        std::set<std::string> functions;

        ParsingContext()
        {
            m_currentScope.emplace_back();
        }

        ~ParsingContext() = default;

        ParsingContext(const ParsingContext&) = delete;

        ParsingContext(ParsingContext&&) = delete;

        ParsingContext& operator=(const ParsingContext&) = delete;

        ParsingContext& operator=(ParsingContext&&) = delete;

        void addToScope(std::string name)
        {
            if (auto result = m_currentScope.back().insert(std::move(name));!result.second)
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
        }

        void popScope()
        {
            m_currentScope.pop_back();
        }

        void addEnumConstant(const std::string& name,std::int32_t value)
        {
            if(!m_enumConstants.insert({name,value}).second)
            {
                throw std::runtime_error("Enum constant called " + name + " alreaedy exists");
            }
            if(functions.count(name))
            {
                throw std::runtime_error("Symbol " + name + " previously declared as function");
            }
        }

        std::int32_t* getEnumConstant(const std::string& name)
        {
            auto result = m_enumConstants.find(name);
            return result == m_enumConstants.end() ? nullptr : &result->second;
        }
    };

    OpenCL::Syntax::TranslationUnit buildTree(std::vector<Lexer::Token>&& tokens);

    OpenCL::Syntax::TranslationUnit parseTranslationUnit(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::ExternalDeclaration parseExternalDeclaration(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::Declaration parseDeclaration(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::DeclarationSpecifier parseDeclarationSpecifier(Tokens& tokens,ParsingContext& context);

    OpenCL::Syntax::StructOrUnionSpecifier parseStructOrUnionSpecifier(Tokens& tokens,ParsingContext& context);

    OpenCL::Syntax::EnumSpecifier parseEnumSpecifier(Tokens& tokens,ParsingContext& context);

    OpenCL::Syntax::FunctionDefinition parseFunctionDefinition(Tokens& tokens, ParsingContext& context);

    Syntax::CompoundItem parseCompoundItem(Tokens& tokens, ParsingContext& context);

    Syntax::Initializer parseInitializer(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::InitializerList parseInitializerList(Tokens& tokens, ParsingContext& context);

    Syntax::Statement parseStatement(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::Expression parseExpression(Tokens& tokens, ParsingContext& context);

    Syntax::AssignmentExpression parseAssignmentExpression(Tokens& tokens,
                                                           ParsingContext& context);

    OpenCL::Syntax::ConditionalExpression parseConditionalExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::LogicalOrExpression parseLogicalOrExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::LogicalAndExpression parseLogicalAndExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::BitOrExpression parseBitOrExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::BitXorExpression parseBitXorExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::BitAndExpression parseBitAndExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::EqualityExpression parseEqualityExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::RelationalExpression parseRelationalExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::ShiftExpression parseShiftExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::AdditiveExpression parseAdditiveExpression(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::Term parseTerm(Tokens& tokens, ParsingContext& context);

    OpenCL::Syntax::CastExpression parseCastExpression(Tokens& tokens, ParsingContext& context);

    Syntax::UnaryExpression parseUnaryExpression(Tokens& tokens, ParsingContext& context);

    Syntax::PostFixExpression parsePostFixExpression(Tokens& tokens, ParsingContext& context);

    Syntax::PrimaryExpression parsePrimaryExpression(Tokens& tokens, ParsingContext& context);
}

#endif //OPENCLPARSER_PARSER_HPP
