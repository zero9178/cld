#include "Preprocessor.hpp"

#include <CompilerCore/C/SourceObject.hpp>
#include <CompilerCore/Common/Util.hpp>

#include "Parser.hpp"

namespace
{
class Preprocessor final
{
    llvm::raw_ostream* m_report;
    const cld::SourceObject& m_sourceObject;
    std::vector<cld::Lexer::Token> m_result;
    cld::PPSourceObject::SubstitutionMap m_substitutions;
    std::vector<std::uint64_t> m_ppStarts;

public:
    Preprocessor(llvm::raw_ostream* report, const cld::SourceObject& sourceObject) noexcept
        : m_report(report), m_sourceObject(sourceObject)
    {
    }

    std::vector<cld::Lexer::Token>& getResult() noexcept
    {
        return m_result;
    }

    cld::PPSourceObject::SubstitutionMap& getSubstitutions() noexcept
    {
        return m_substitutions;
    }

    std::vector<std::uint64_t>& getPpStarts() noexcept
    {
        return m_ppStarts;
    }

    void visit(const cld::PP::File& file)
    {
        for (auto& iter : file.groups)
        {
            visit(iter);
        }
    }

    void visit(const cld::PP::Group& group)
    {
        for (auto& iter : group.groupPart)
        {
            visit(iter);
        }
    }

    void visit(const cld::PP::GroupPart& groupPart)
    {
        cld::match(groupPart, [this](auto&& value) { this->visit(value); });
    }

    void visit(const std::vector<cld::Lexer::Token>& text) {}

    void visit(const cld::PP::IfSection& ifSection) {}

    void visit(const cld::PP::ControlLine& controlLine)
    {
        cld::match(controlLine.variant, [this](auto&& value) { this->visit(value); });
    }

    void visit(const cld::PP::NonDirective&) {}

    void visit(const cld::PP::ControlLine::IncludeTag& includeTag) {}

    void visit(const cld::PP::ControlLine::LineTag& lineTag) {}

    void visit(const cld::PP::ControlLine::ErrorTag& errorTag) {}

    void visit(const cld::PP::ControlLine::PragmaTag& pragmaTag) {}

    void visit(const std::string& undef) {}

    void visit(const cld::PP::DefineDirective& defineDirective)
    {

    }
};
} // namespace

cld::PPSourceObject cld::PP::preprocess(const cld::SourceObject& sourceObject, llvm::raw_ostream* reporter) noexcept
{
    auto [tree, ok] = buildTree(sourceObject, reporter);
    if (!ok)
    {
        return cld::PPSourceObject(sourceObject);
    }
    Preprocessor preprocessor(reporter, sourceObject);
    preprocessor.visit(tree);
    return PPSourceObject(sourceObject, std::move(preprocessor.getResult()), std::move(preprocessor.getSubstitutions()),
                          std::move(preprocessor.getPpStarts()));
}
