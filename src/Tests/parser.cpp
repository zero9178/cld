#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/Preprocessor/Preprocessor.hpp>
#include <fstream>
#include "catch.hpp"

auto start = R"(typedef struct Point
{
    struct Point* next;
    int array[5];
} Point;

unsigned long long getListCount(const Point* first)
{
    const Point* current = first;
    unsigned long long i = 0;
    while(current)
    {
        current = current->next;
        i++;
    }
    return i;
}

int main()
{
    Point one,two,three;
    one.next = &two;
    two.next = &three;
    three.next = 0;
    return getListCount(&one);
})";

TEST_CASE("Failure1", "[parser]")
{
    auto failure1 =
        "const * & ; main ] two next { next ; i typedef { ( current next Point . array ++ ) ( ; one } i } struct . , ) current three Point ( ; one = two . -> int getListCount = long first unsigned 5 long & one int current = three unsigned ; i = return ; ; Point ) next ) ; struct three long { ; 0 while [ current Point Point ; next ; const = two first = ; } } Point & 0 long * ( * { ; getListCount , return ";
    auto tokens = OpenCL::Lexer::tokenize(failure1);
    REQUIRE_FALSE(OpenCL::Parser::buildTree(tokens));
}

TEST_CASE("Failure2", "[parser]")
{
    auto failure2 =
        "struct main { } next ; int first ( & } ) three ; two long ; ( ; one . current next getListCount ; -> first next ; 5 = 0 long unsigned } { ; { long three getListCount array = unsigned next = ) * * ( ; struct current ] Point { i long current ; two = 0 one return , return i two ++ Point const const one next Point . , * ; = ; Point . typedef int = i [ Point } while ; ) ; & Point current three & ) ( ";
    auto tokens = OpenCL::Lexer::tokenize(failure2);
    REQUIRE_FALSE(OpenCL::Parser::buildTree(tokens));
}

TEST_CASE("Declaration parsing", "[parser]")
{
    auto program = R"(int main()
{
    int r;
    int *i = &r,*f = i;
}
)";
    auto tokens = OpenCL::Lexer::tokenize(program);
    auto expected = OpenCL::Parser::buildTree(tokens);
    INFO("The error is:\"" << (expected.hasError() ? expected.error().getText() : "") << '\\');
    REQUIRE(expected);
}

TEST_CASE("Traditional argument scope","[parser]")
{
    std::array sources  = {
        std::pair{"int x(a) int a; {return a;}\n",true},
        std::pair{"int y(b) int b; {return a;}\n",false},
        std::pair{"int a(a)int a;{a=10;return a;}",true}
    };
    for(auto [source,succeed] : sources)
    {
        DYNAMIC_SECTION(source)
        {
            auto expected = OpenCL::Parser::buildTree(OpenCL::Lexer::tokenize(source));
            REQUIRE(static_cast<bool>(expected) == succeed);
        }
    }
}
