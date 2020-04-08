#include "Parser.hpp"

#include <CompilerCore/Common/Util.hpp>

#include <algorithm>

#include "ParserUtil.hpp"
#include "SourceObject.hpp"

std::pair<cld::Syntax::TranslationUnit, bool> cld::Parser::buildTree(const SourceObject& sourceObject,
                                                                     llvm::raw_ostream* reporter)
{
    Context context(sourceObject, reporter);
    auto begin = sourceObject.data().cbegin();
    return {parseTranslationUnit(begin, sourceObject.data().cend(), context), context.getCurrentErrorCount() == 0};
}

void cld::Parser::Context::addTypedef(const std::string& name, DeclarationLocation declarator)
{
    auto [iter, inserted] = m_currentScope.back().emplace(name, Declaration{declarator, true});
    if (!inserted && iter->second.isTypedef)
    {
        log({Message::error(ErrorMessages::REDEFINITION_OF_SYMBOL_N.args('\'' + name + '\''), declarator.begin,
                            declarator.end, Modifier(declarator.identifier, declarator.identifier + 1)),
             Message::note(Notes::PREVIOUSLY_DECLARED_HERE, iter->second.location.begin, iter->second.location.end,
                           Modifier(iter->second.location.identifier, iter->second.location.identifier + 1))});
    }
}

bool cld::Parser::Context::isTypedef(const std::string& name) const
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

void cld::Parser::Context::log(std::vector<Message> messages)
{
    for (auto& iter : messages)
    {
        if (iter.getSeverity() == Message::Error)
        {
            m_errorCount++;
        }
        if (this->m_reporter)
        {
            iter.print(*this->m_reporter, getSourceObject());
        }
    }
}

void cld::Parser::Context::addToScope(const std::string& name, DeclarationLocation declarator)
{
    CLD_ASSERT(!name.empty());
    auto [iter, inserted] = m_currentScope.back().emplace(name, Declaration{declarator, false});
    if (!inserted && iter->second.isTypedef)
    {
        log({Message::error(ErrorMessages::REDEFINITION_OF_SYMBOL_N.args('\'' + name + '\''), declarator.begin,
                            declarator.end, Modifier(declarator.identifier, declarator.identifier + 1)),
             Message::note(Notes::PREVIOUSLY_DECLARED_HERE, iter->second.location.begin, iter->second.location.end,
                           Modifier(iter->second.location.identifier, iter->second.location.identifier + 1))});
    }
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

const cld::Parser::Context::DeclarationLocation* cld::Parser::Context::getLocationOf(const std::string& name) const
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

bool cld::Parser::Context::isTypedefInScope(const std::string& name) const
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

cld::Parser::Context::Context(const SourceObject& sourceObject, llvm::raw_ostream* reporter, bool inPreprocessor)
    : m_sourceObject(sourceObject), m_reporter(reporter), m_inPreprocessor(inPreprocessor)
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

void cld::Parser::Context::skipUntil(Lexer::TokenIterator& begin, Lexer::TokenIterator end, TokenBitSet additional)
{
    begin = std::find_if(begin, end, [bitset = m_recoverySet | additional](const Lexer::Token& token) {
        return bitset[static_cast<std::underlying_type_t<Lexer::TokenType>>(token.getTokenType())];
    });
}

void cld::Parser::Context::parenthesesEntered(Lexer::TokenIterator bracket)
{
    if (++m_parenthesesDepth <= m_bracketMax)
    {
        return;
    }
    log({Message::error(ErrorMessages::Parser::MAXIMUM_N_DEPTH_OF_N_EXCEEDED.args("bracket", m_bracketMax), bracket,
                        bracket, Modifier(bracket, bracket + 1, Modifier::PointAtBeginning))});
    throw FatalParserError();
}

void cld::Parser::Context::parenthesesLeft()
{
    m_parenthesesDepth--;
}

void cld::Parser::Context::squareBracketEntered(Lexer::TokenIterator bracket)
{
    if (++m_squareBracketDepth <= m_bracketMax)
    {
        return;
    }
    log({Message::error(ErrorMessages::Parser::MAXIMUM_N_DEPTH_OF_N_EXCEEDED.args("bracket", m_bracketMax), bracket,
                        bracket, Modifier(bracket, bracket + 1, Modifier::PointAtBeginning))});
    throw FatalParserError();
}

void cld::Parser::Context::squareBracketLeft()
{
    m_squareBracketDepth--;
}

void cld::Parser::Context::braceEntered(Lexer::TokenIterator bracket)
{
    if (++m_braceDepth <= m_bracketMax)
    {
        return;
    }
    log({Message::error(ErrorMessages::Parser::MAXIMUM_N_DEPTH_OF_N_EXCEEDED.args("bracket", m_bracketMax), bracket,
                        bracket, Modifier(bracket, bracket + 1, Modifier::PointAtBeginning))});
    throw FatalParserError();
}

void cld::Parser::Context::braceLeft()
{
    m_braceDepth--;
}

const cld::SourceObject& cld::Parser::Context::getSourceObject() const
{
    return m_sourceObject;
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
