#include "SourceObject.hpp"

#include "Lexer.hpp"

template class cld::SourceObject<cld::Lexer::CToken>;
template class cld::SourceObject<cld::Lexer::PPToken>;
