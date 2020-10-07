#include "Parser.hpp"

#include <algorithm>

#include "ErrorMessages.hpp"
#include "ParserUtil.hpp"
#include "SourceObject.hpp"

cld::Syntax::TranslationUnit cld::Parser::buildTree(const CSourceObject& sourceObject, llvm::raw_ostream* reporter,
                                                    bool* errors)
{
    Context context(sourceObject, reporter);
    const auto* begin = sourceObject.data().data();
    auto result = parseTranslationUnit(begin, sourceObject.data().data() + sourceObject.data().size(), context);
    if (errors)
    {
        *errors = context.getCurrentErrorCount();
    }
    return result;
}

void cld::Parser::Context::addTypedef(std::string_view name, DeclarationLocation declarator)
{
    m_currentScope.back().emplace(name, Declaration{declarator, true});
}

bool cld::Parser::Context::isTypedef(std::string_view name) const
{
    for (auto& iter : m_currentScope)
    {
        if (auto result = iter.find(name); result != iter.end() && result->second.isTypedef)
        {
            return true;
        }
    }
    return false;
}

bool cld::Parser::Context::log(const Message& message)
{
    if (m_reporter)
    {
        *m_reporter << message;
    }
    if (message.getSeverity() == Severity::Error)
    {
        m_errorCount++;
    }
    return message.getSeverity() != Severity::None;
}

void cld::Parser::Context::addToScope(std::string_view name, DeclarationLocation declarator)
{
    CLD_ASSERT(!name.empty());
    m_currentScope.back().emplace(name, Declaration{declarator, false});
}

void cld::Parser::Context::pushScope()
{
    m_currentScope.emplace_back();
}

void cld::Parser::Context::popScope()
{
    m_currentScope.pop_back();
}

std::size_t cld::Parser::Context::getCurrentErrorCount() const
{
    return m_errorCount;
}

const cld::Parser::Context::DeclarationLocation* cld::Parser::Context::getLocationOf(std::string_view name) const
{
    for (auto iter = m_currentScope.rbegin(); iter != m_currentScope.rend(); iter++)
    {
        if (auto result = iter->find(name); result != iter->end())
        {
            return &result->second.location;
        }
    }
    return nullptr;
}

bool cld::Parser::Context::isTypedefInScope(std::string_view name) const
{
    for (auto iter = m_currentScope.rbegin(); iter != m_currentScope.rend(); iter++)
    {
        if (auto result = iter->find(name); result != iter->end())
        {
            return result->second.isTypedef;
        }
    }
    return false;
}

bool cld::Parser::Context::isBuiltin(std::string_view name) const
{
    if (name == "__builtin_va_list")
    {
        return true;
    }
    return false;
}

cld::Parser::Context::Context(const SourceInterface& sourceInterface, llvm::raw_ostream* reporter, bool inPreprocessor)
    : m_sourceInterface(sourceInterface), m_reporter(reporter), m_inPreprocessor(inPreprocessor)
{
}

cld::Parser::Context::TokenBitReseter cld::Parser::Context::withRecoveryTokens(const TokenBitSet& tokenBitSet)
{
    auto oldSet = m_recoverySet;
    m_recoverySet |= tokenBitSet;
    return cld::Parser::Context::TokenBitReseter(oldSet, *this);
}

cld::Parser::Context::TokenBitReseter::TokenBitReseter(TokenBitSet original, Context& context)
    : m_original(original), m_context(context)
{
}

cld::Parser::Context::TokenBitReseter::~TokenBitReseter()
{
    m_context.m_recoverySet = m_original;
}

void cld::Parser::Context::skipUntil(Lexer::CTokenIterator& begin, Lexer::CTokenIterator end, TokenBitSet additional)
{
    begin = std::find_if(begin, end, [bitset = m_recoverySet | additional](const Lexer::CToken& token) {
        return bitset[static_cast<std::underlying_type_t<Lexer::TokenType>>(token.getTokenType())];
    });
}

cld::ValueReset<std::uint64_t> cld::Parser::Context::parenthesesEntered(Lexer::CTokenIterator bracket)
{
    if (++m_parenthesesDepth <= m_bracketMax)
    {
        return cld::ValueReset<std::uint64_t>(m_parenthesesDepth, m_parenthesesDepth - 1);
    }
    log(Errors::Parser::MAXIMUM_BRACKET_DEPTH_OF_N_EXCEEDED.args(*bracket, m_sourceInterface, m_bracketMax, *bracket));
#ifdef LLVM_ENABLE_EXCEPTIONS
    throw FatalParserError();
#else
    std::terminate();
#endif
}

cld::ValueReset<std::uint64_t> cld::Parser::Context::squareBracketEntered(Lexer::CTokenIterator bracket)
{
    if (++m_squareBracketDepth <= m_bracketMax)
    {
        return cld::ValueReset<std::uint64_t>(m_squareBracketDepth, m_squareBracketDepth - 1);
    }
    log(Errors::Parser::MAXIMUM_BRACKET_DEPTH_OF_N_EXCEEDED.args(*bracket, m_sourceInterface, m_bracketMax, *bracket));
#ifdef LLVM_ENABLE_EXCEPTIONS
    throw FatalParserError();
#else
    std::terminate();
#endif
}

cld::ValueReset<std::uint64_t> cld::Parser::Context::braceEntered(Lexer::CTokenIterator bracket)
{
    if (++m_braceDepth <= m_bracketMax)
    {
        return cld::ValueReset<std::uint64_t>(m_braceDepth, m_braceDepth - 1);
    }
    log(Errors::Parser::MAXIMUM_BRACKET_DEPTH_OF_N_EXCEEDED.args(*bracket, m_sourceInterface, m_bracketMax, *bracket));
#ifdef LLVM_ENABLE_EXCEPTIONS
    throw FatalParserError();
#else
    std::terminate();
#endif
}

const cld::SourceInterface& cld::Parser::Context::getSourceInterface() const
{
    return m_sourceInterface;
}

bool cld::Parser::Context::isInPreprocessor() const
{
    return m_inPreprocessor;
}

std::uint64_t cld::Parser::Context::getBracketMax() const
{
    return m_bracketMax;
}

void cld::Parser::Context::setBracketMax(std::uint64_t bracketMax)
{
    m_bracketMax = bracketMax;
}

cld::ValueReset<bool> cld::Parser::Context::enableExtensions(bool extensions)
{
    auto prevValue = m_inExtension;
    m_inExtension = extensions || m_inExtension;
    return cld::ValueReset<bool>(m_inExtension, prevValue);
}
