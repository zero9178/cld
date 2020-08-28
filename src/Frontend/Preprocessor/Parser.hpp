#pragma once

#include <Frontend/Compiler/Lexer.hpp>

#include "Syntax.hpp"

namespace cld
{
class Message;

namespace PP
{
class Context final
{
    const SourceInterface& m_sourceInterface;
    llvm::raw_ostream* m_reporter;
    std::size_t m_errorCount = 0;

public:
    Context(const SourceInterface& sourceInterface, llvm::raw_ostream* reporter = &llvm::errs());

    Context(const Context&) = delete;
    Context& operator=(const Context&) = delete;
    Context(Context&&) = delete;
    Context& operator=(Context&&) = delete;

    void log(const Message& message);

    std::size_t getErrorCount() const;

    const SourceInterface& getSourceInterface() const;
};

File buildTree(const PPSourceObject& sourceObject, llvm::raw_ostream* reporter = &llvm::errs(), bool* errors = nullptr);

File parseFile(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context);

Group parseGroup(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context, bool inIf = false);

ControlLine parseControlLine(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context);

IfSection parseIfSection(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context);

ElseGroup parseElseGroup(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context);

ElIfGroup parseElIfGroup(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context);

IfGroup parseIfGroup(Lexer::PPTokenIterator& begin, Lexer::PPTokenIterator end, Context& context);
} // namespace PP
} // namespace cld
