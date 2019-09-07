#ifndef OPENCLPARSER_PARSER_HPP
#define OPENCLPARSER_PARSER_HPP

#include <tl/function_ref.hpp>

#include "../Common/UniqueResource.hpp"
#include "Message.hpp"
#include "Semantics.hpp"
#include "Syntax.hpp"

namespace OpenCL::Parser
{
    using Tokens = std::vector<Lexer::Token>;

    using InRecoverySet = tl::function_ref<bool(const Lexer::Token&)>;

    class Context final
    {
        std::ostream* m_reporter;
        struct DeclarationLocation
        {
            Tokens::const_iterator begin;
            Tokens::const_iterator end;
            Tokens::const_iterator identifier;
        };

        struct Declaration
        {
            DeclarationLocation location;
            bool isTypedef{};
        };
        std::vector<std::map<std::string, Declaration>> m_currentScope{1};

        static bool tokenCompare(const Lexer::Token& lhs, const Lexer::Token& rhs);

        std::map<Lexer::Token, std::pair<Tokens::const_iterator, Tokens::const_iterator>, decltype(&tokenCompare)>
            m_lines{tokenCompare};
        std::size_t m_errorCount = 0;

    public:
        explicit Context(Tokens::const_iterator sourceBegin, Tokens::const_iterator sourceEnd,
                         std::ostream* reporter = &std::cerr);

        ~Context() = default;

        Context(const Context&) = delete;

        Context(Context&&) = delete;

        Context& operator=(const Context&) = delete;

        Context& operator=(Context&&) = delete;

        void addTypedef(const std::string& name, DeclarationLocation declarator);

        [[nodiscard]] bool isTypedef(const std::string& name) const;

        [[nodiscard]] bool isTypedefInScope(const std::string& name) const;

        void log(std::vector<Message> messages);

        [[nodiscard]] Tokens::const_iterator getLineStart(Tokens::const_iterator iter) const;

        [[nodiscard]] Tokens::const_iterator getLineEnd(Tokens::const_iterator iter) const;

        void addToScope(const std::string& name, DeclarationLocation declarator);

        [[nodiscard]] const Parser::Context::DeclarationLocation* getLocationOf(const std::string& name) const;

        void pushScope();

        void popScope();

        [[nodiscard]] std::size_t getCurrentErrorCount() const;
    };

    std::pair<OpenCL::Syntax::TranslationUnit, bool> buildTree(const std::vector<Lexer::Token>& tokens,
                                                               std::ostream* reporter = &std::cerr);

    OpenCL::Syntax::TranslationUnit parseTranslationUnit(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                         Context& context);

    std::optional<Syntax::ExternalDeclaration> parseExternalDeclaration(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end, Context& context,
                                                                        InRecoverySet recoverySet);

    std::optional<Syntax::Declaration> parseDeclaration(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                        Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::DeclarationSpecifier> parseDeclarationSpecifier(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end, Context& context,
                                                                          InRecoverySet recoverySet);

    std::optional<Syntax::SpecifierQualifier> parseSpecifierQualifier(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end, Context& context,
                                                                      InRecoverySet recoverySet);

    std::vector<Syntax::SpecifierQualifier> parseSpecifierQualifierList(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end, Context& context,
                                                                        InRecoverySet recoverySet);

    std::optional<Syntax::Declarator> parseDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                      Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::DirectDeclarator> parseDirectDeclarator(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end, Context& context,
                                                                  InRecoverySet recoverySet);

    Syntax::ParameterTypeList parseParameterTypeList(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                     Context& context, InRecoverySet recoverySet);

    Syntax::AbstractDeclarator parseAbstractDeclarator(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                       Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::DirectAbstractDeclarator> parseDirectAbstractDeclarator(Tokens::const_iterator& begin,
                                                                                  Tokens::const_iterator end,
                                                                                  Context& context,
                                                                                  InRecoverySet recoverySet);

    Syntax::ParameterList parseParameterList(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                             Context& context, InRecoverySet recoverySet);

    Syntax::Pointer parsePointer(Tokens::const_iterator& begin, Tokens::const_iterator end, Context& context,
                                 InRecoverySet recoverySet);

    std::optional<Syntax::StructOrUnionSpecifier> parseStructOrUnionSpecifier(Tokens::const_iterator& begin,
                                                                              Tokens::const_iterator end,
                                                                              Context& context,
                                                                              InRecoverySet recoverySet);

    std::optional<Syntax::EnumSpecifier> parseEnumSpecifier(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                            Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::CompoundStatement> parseCompoundStatement(OpenCL::Parser::Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end,
                                                                    OpenCL::Parser::Context& context,
                                                                    InRecoverySet recoverySet, bool pushScope = true);

    std::optional<Syntax::CompoundItem> parseCompoundItem(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                          Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::Initializer> parseInitializer(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                        Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::InitializerList> parseInitializerList(Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end, Context& context,
                                                                InRecoverySet recoverySet);

    std::optional<Syntax::Statement> parseStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                    Context& context, InRecoverySet recoverySet);

    Syntax::ReturnStatement parseReturnStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                 Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::IfStatement> parseIfStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                        Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::SwitchStatement> parseSwitchStatement(Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end, Context& context,
                                                                InRecoverySet recoverySet);

    std::optional<Syntax::ForStatement> parseForStatement(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                          Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::HeadWhileStatement> parseHeadWhileStatement(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end, Context& context,
                                                                      InRecoverySet recoverySet);

    std::optional<Syntax::FootWhileStatement> parseFootWhileStatement(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end, Context& context,
                                                                      InRecoverySet recoverySet);

    Syntax::Expression parseExpression(Tokens::const_iterator& begin, Tokens::const_iterator end, Context& context,
                                       InRecoverySet recoverySet);

    std::optional<Syntax::AssignmentExpression> parseAssignmentExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end, Context& context,
                                                                          InRecoverySet recoverySet);

    std::optional<Syntax::ConditionalExpression> parseConditionalExpression(Tokens::const_iterator& begin,
                                                                            Tokens::const_iterator end,
                                                                            Context& context,
                                                                            InRecoverySet recoverySet);

    std::optional<Syntax::LogicalOrExpression> parseLogicalOrExpression(Tokens::const_iterator& begin,
                                                                        Tokens::const_iterator end, Context& context,
                                                                        InRecoverySet recoverySet);

    std::optional<Syntax::LogicalAndExpression> parseLogicalAndExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end, Context& context,
                                                                          InRecoverySet recoverySet);

    std::optional<Syntax::BitOrExpression> parseBitOrExpression(Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end, Context& context,
                                                                InRecoverySet recoverySet);

    std::optional<Syntax::BitXorExpression> parseBitXorExpression(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end, Context& context,
                                                                  InRecoverySet recoverySet);

    std::optional<Syntax::BitAndExpression> parseBitAndExpression(Tokens::const_iterator& begin,
                                                                  Tokens::const_iterator end, Context& context,
                                                                  InRecoverySet recoverySet);

    std::optional<Syntax::EqualityExpression> parseEqualityExpression(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end, Context& context,
                                                                      InRecoverySet recoverySet);

    std::optional<Syntax::RelationalExpression> parseRelationalExpression(Tokens::const_iterator& begin,
                                                                          Tokens::const_iterator end, Context& context,
                                                                          InRecoverySet recoverySet);

    std::optional<Syntax::ShiftExpression> parseShiftExpression(Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end, Context& context,
                                                                InRecoverySet recoverySet);

    std::optional<Syntax::AdditiveExpression> parseAdditiveExpression(Tokens::const_iterator& begin,
                                                                      Tokens::const_iterator end, Context& context,
                                                                      InRecoverySet recoverySet);

    std::optional<Syntax::Term> parseTerm(Tokens::const_iterator& begin, Tokens::const_iterator end, Context& context,
                                          InRecoverySet recoverySet);

    template <class... Args>
    using VariantOfOptionals = std::variant<std::monostate, std::optional<Args>...>;

    using StateVariant = VariantOfOptionals<Syntax::Term, Syntax::AdditiveExpression, Syntax::ShiftExpression,
                                            Syntax::RelationalExpression, Syntax::EqualityExpression,
                                            Syntax::BitAndExpression, Syntax::BitXorExpression, Syntax::BitOrExpression,
                                            Syntax::LogicalAndExpression, Syntax::LogicalOrExpression>;

    enum class EndState
    {
        Term,
        Additive,
        Shift,
        Relational,
        Equality,
        BitAnd,
        BitXor,
        BitOr,
        LogicalAnd,
        LogicalOr
    };

    StateVariant parseBinaryOperators(EndState endState, Tokens::const_iterator& begin, Tokens::const_iterator end,
                                      Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::TypeName> parseTypeName(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                  Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::CastExpression> parseCastExpression(Tokens::const_iterator& begin, Tokens::const_iterator end,
                                                              Context& context, InRecoverySet recoverySet);

    std::optional<Syntax::UnaryExpression> parseUnaryExpression(Tokens::const_iterator& begin,
                                                                Tokens::const_iterator end, Context& context,
                                                                InRecoverySet recoverySet);

    std::optional<Syntax::PostFixExpression> parsePostFixExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end, Context& context,
                                                                    InRecoverySet recoverySet);

    std::optional<Syntax::PrimaryExpression> parsePrimaryExpression(Tokens::const_iterator& begin,
                                                                    Tokens::const_iterator end, Context& context,
                                                                    InRecoverySet recoverySet);
} // namespace OpenCL::Parser

#endif // OPENCLPARSER_PARSER_HPP
