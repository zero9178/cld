#include "Lexer.hpp"
#include "Parser.hpp"

auto SOURCE
    = "int fib(int n){if (n == 0 || n == 1){return n;} else {return fib(n - 1) + fib(n - 2);}}"
      "int main() {int n = 5;return fib(n);}";

int main()
{
    auto result = OpenCL::Lexer::tokenize(SOURCE);
    auto node = OpenCL::Parser::buildTree(std::move(result));
    return 0;
}
