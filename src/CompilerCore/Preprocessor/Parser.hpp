#pragma once

#include <CompilerCore/C/Lexer.hpp>

#include "Syntax.hpp"

namespace cld
{
class Message;

namespace PP
{
class Context final
{
    const SourceObject& m_sourceObject;
    llvm::raw_ostream* m_reporter;
    std::size_t m_errorCount = 0;

public:
    Context(const SourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs());

    Context(const Context&) = delete;
    Context& operator=(const Context&) = delete;
    Context(Context&&) = delete;
    Context& operator=(Context&&) = delete;

    void log(std::vector<Message> messages);

    std::size_t getErrorCount() const;
};

std::pair<File, bool> buildTree(const SourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs());

File parseFile(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context);

Group parseGroup(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context, bool inIf = false);

ControlLine parseControlLine(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context);

DefineDirective parseDefineDirective(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context);

IfSection parseIfSection(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context);

ElseGroup parseElseGroup(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context);

ElIfGroup parseElIfGroup(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context);

IfGroup parseIfGroup(Lexer::TokenIterator& begin, Lexer::TokenIterator end, Context& context);
} // namespace PP
} // namespace cld
