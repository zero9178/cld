#include "Message.hpp"

llvm::raw_ostream& cld::operator<<(llvm::raw_ostream& os, const cld::Message& message)

{
    os << message.m_text;
    os.flush();
    return os;
}
