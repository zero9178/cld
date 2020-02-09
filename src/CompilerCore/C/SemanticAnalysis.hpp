#ifndef OPENCLPARSER_SEMANTICANALYSIS_HPP
#define OPENCLPARSER_SEMANTICANALYSIS_HPP

#include <map>

#include "Message.hpp"
#include "Semantics.hpp"
#include "Syntax.hpp"

namespace OpenCL::Semantics
{
using PossiblyAbstractQualifierRef =
    std::variant<const Syntax::AbstractDeclarator*, std::reference_wrapper<const Syntax::Declarator>>;

class ConstantEvaluator;

class SemanticAnalysis final
{
    const SourceObject& m_sourceObject;
    llvm::raw_ostream* m_reporter;
    std::vector<std::map<std::string, Semantics::RecordType>> m_structsUnions{1};
    std::vector<std::map<std::string, DeclarationTypedefEnums>> m_declarations{1};

    void popScope()
    {
        m_declarations.pop_back();
        m_structsUnions.pop_back();
    }

    void pushScope()
    {
        m_declarations.emplace_back();
        m_structsUnions.emplace_back();
    }

    ConstantEvaluator makeEvaluator(std::vector<OpenCL::Lexer::Token>::const_iterator exprBegin,
                                    std::vector<OpenCL::Lexer::Token>::const_iterator exprEnd);

    [[nodiscard]] bool isTypedef(const std::string& name) const;

    [[nodiscard]] bool isTypedefInScope(const std::string& name) const;

    [[nodiscard]] const Semantics::Type* getTypedef(const std::string& name) const;

    void logError(std::vector<Message> messages);

    OpenCL::Semantics::Type
        primitivesToType(std::vector<OpenCL::Lexer::Token>::const_iterator declStart,
                         std::vector<OpenCL::Lexer::Token>::const_iterator declEnd,
                         const std::vector<OpenCL::Syntax::TypeSpecifier::PrimitiveTypeSpecifier>& primitives,
                         bool isConst, bool isVolatile);

    OpenCL::Semantics::Type
        typeSpecifiersToType(std::vector<OpenCL::Lexer::Token>::const_iterator declStart,
                             std::vector<OpenCL::Lexer::Token>::const_iterator declEnd,
                             const std::vector<const OpenCL::Syntax::TypeSpecifier*>& typeSpecifiers, bool isConst,
                             bool isVolatile);

    OpenCL::Semantics::Type apply(std::vector<Lexer::Token>::const_iterator declStart,
                                  std::vector<Lexer::Token>::const_iterator declEnd,
                                  PossiblyAbstractQualifierRef declarator, Type&& baseType,
                                  const std::vector<Syntax::Declaration>& declarations);

    OpenCL::Semantics::Type apply(std::vector<Lexer::Token>::const_iterator declStart,
                                  std::vector<Lexer::Token>::const_iterator declEnd,
                                  const Syntax::DirectAbstractDeclarator& abstractDeclarator, Type&& baseType,
                                  const std::vector<Syntax::Declaration>& declarations);

    OpenCL::Semantics::Type apply(std::vector<Lexer::Token>::const_iterator declStart,
                                  std::vector<Lexer::Token>::const_iterator declEnd,
                                  const Syntax::DirectDeclarator& directDeclarator, Type&& baseType,
                                  const std::vector<Syntax::Declaration>& declarations);

    static std::tuple<bool, bool, bool> getQualifiers(const std::vector<Syntax::TypeQualifier>& typeQualifiers);

    std::vector<std::pair<OpenCL::Semantics::Type, std::string>>
        parameterListToArguments(std::vector<OpenCL::Lexer::Token>::const_iterator declStart,
                                 std::vector<OpenCL::Lexer::Token>::const_iterator declEnd,
                                 const std::vector<OpenCL::Syntax::ParameterDeclaration>& parameterDeclarations,
                                 const std::vector<OpenCL::Syntax::Declaration>& declarations);

public:
    explicit SemanticAnalysis(const SourceObject& sourceObject, llvm::raw_ostream* reporter = nullptr)
        : m_sourceObject(sourceObject), m_reporter(reporter)
    {
    }

    using DeclarationOrSpecifierQualifier =
        std::variant<std::reference_wrapper<const OpenCL::Syntax::DeclarationSpecifier>,
                     std::reference_wrapper<const OpenCL::Syntax::SpecifierQualifier>>;

    Type declaratorsToType(const std::vector<DeclarationOrSpecifierQualifier>& declarationOrSpecifierQualifiers,
                           const PossiblyAbstractQualifierRef& declarator = {},
                           const std::vector<Syntax::Declaration>& declarations = {});

    TranslationUnit visit(const Syntax::TranslationUnit& node);

    std::optional<OpenCL::Semantics::FunctionDefinition> visit(const Syntax::FunctionDefinition& node);

    std::vector<Declaration> visit(const Syntax::Declaration& node);
};
} // namespace OpenCL::Semantics

#endif // OPENCLPARSER_SEMANTICANALYSIS_HPP
