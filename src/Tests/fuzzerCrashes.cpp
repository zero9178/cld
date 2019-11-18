#include "catch.hpp"

#include <CompilerCore/C/Parser.hpp>
#include <CompilerCore/C/SourceObject.hpp>

namespace
{
    void parse(const std::string& source)
    {
        std::string storage;
        llvm::raw_string_ostream ss(storage);
        auto tokens = OpenCL::Lexer::tokenize(source, OpenCL::LanguageOptions::native(), false, &ss);
        if (!ss.str().empty() || tokens.data().empty())
        {
            return;
        }

        OpenCL::Parser::buildTree(tokens, &ss);
    }

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-function"
    void excludeFromAddressSanitizer()
    {
        parse(
            "*l=((((((((((((((((((((((((((((((((((((((((((((((((((((((( (((((((((((((((((((li(((((((((((((( (((((((((((((((((((( ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( (((((((((li(((((((((((((( (((((((((((((((((((( ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((({(((((((((((((((((((((((((h((((((((((((((((((((((((((((((((((((((((({(((((((((((((((((((((((((h((((((((((((((((((((((((( (((((((((((((((((((({((((((((((((((((((((((((u");
        parse(
            "([([[(([([((n([([[([[(([([(([([8[[(([[([(([([([([[[([[([([[(([([(([([8[[(([[([([[(([([(([([8[[(([[([[([[([[(([([(([([8[[([([[([[(([([(([([8[[(([([8[[(([[([(([([([([[[(([([[[[(([([(([([([[([([[(([([(([[([([(([([8[[((A[[[[(([[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[([([-[[([[([[(([(?[(([([8[[([[[([(([([([([[[(([([(([([([[[[([8[[((A[[[[(([[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[([(([([([[[(([([(([([([[([([[(([([(([([-[[([8[[(([[([(([([([([[[(([([[[[(([([(([([([[([([[(([([(([[([([(([([8[[((A[[[[(([[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[([(([([([[[(([([(([([([[([([[(([([(([([-[[([[([[(([(?[(([([(([([8[[(([([8[[(([[([[([(([([([[[[([((([([(([([7[[((-[[([[([[(([(?[(([([8[[([[[([(([([([([[[(([([(([[[[(([([(([([([[([([7[[(([[([((([(");
        parse(
            "([([[(([([((n([([[([[(8[[(([([8[[(([[([(([([(([([([[([([[(([([(([[([([(([([8[[((A[[[[(([[[[>=[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[(?[(([([8[[([[[([(([([([([[[(([([(([([([[[[([(((((A[[[[(([[[[[[([[([[(([([(([([8[[(([([8[[(([[([(([([([([[[([([([([[[(([([[[[(([([(([([([[([([[(([([[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[([(([([([[[(([([(([([([[([([[(([([(([([-[[([[([[(([(?[(([([8[[([[[([(([([([([[[(([([(([([([[[[(k[(([[([([(([([8[[((A[[[[(([[[[>=[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[(?[(([([8[[([[[([(([([([([[[(([([(([([([[[[([(((((A[[[[(([[[[[[([[([[(([([(([([8[[(([([8[[(([[([(([([([([[[(([([[[[(([([(([([([[([([[(([([(([[([([(([([8[[((A[[[[(([[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[([(([([([[[(([([(([([([[([([[(([([(([([-[[([[([[(([(?[(([([8[[([[[([(([([([([[[(([([(([([([[[[(k[((([([(([([7[[((-[[([[([[(([(?[(([([8[[([[[([(([([([([[[(([([(([[[[(([([(([([([[([([7[[(([[([((([(");
    }
#pragma clang diagnostic pop
} // namespace

TEST_CASE("Fuzzer discoveries", "[fuzzer]")
{
    parse("Y'\x0a\x0a");
    parse("(auto:");
    parse("\x8e.8..");
    parse(R"(('


I=')");
    parse("YYu{\x0a:");
    parse("(n{-((do(n");
    parse("+goto2n[0(sizeof(");
    parse("({(switch");
    parse("({if\"{");
    parse("\xd5.2f2f");
    parse("d{while");
    parse("*[( \x09(5ev");
    parse("Ao(int,{,[");
    parse("[1 U( \x091x");
    parse("v\x09(3.f.");
    parse("\x09(=[9l8lu");
    parse("enum {*(");
    parse("k{struct{{;");
    parse("it(\"{}%\"    ");
    parse("*{(enum{)=9");
    parse("(=({A{enum k{=(");
    parse("\"{}Hy\"([H{");
    parse("typedef(z;H");
    parse(R"(("""""""""\""""""""_=)");
    parse("(+cittsa=%=[([(typedef(( ( D;D[(");
    parse("( ({o[(void(o[(void([((c(([[((Tm(c(([[(enum[(T(,*(=_%(void(o[a[in{=");
    parse("id(o[(voi ([([(enum[(..,(void(+k{m++(({([(enum[(_id(+k{m++(({([(enum[(_%[(volatile([(e(vp==iivp==i");
    parse("(const(6(*foro([void(o,(void([) 6(*foro([void(o,(void([) [(void(_enum[(void%=l{[*(cona");
    parse(R"(f{u&elsaeifyur{u{y""y--y-Na(N[ "\\\\\\\\\\\\\\\\\\\\\\\\\\X\\\\\\\\\\\"yN[&ry ""&ry "volatilegis-)");
    parse("z{|if{iw,_if  (long(:U [o[(long(([o((else(long(:U [o[(lo(([o((long(((((_||f((");
    parse(
        "F>>=u{cc mase enum mase   q{enum mase en   q{enum mase enum ase enum mase   q{enum mase en  mase enum mau{+r++++++++se[   Kq{enum mase en   q{enmase en   u{case-.sh{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{->{{{{{{{A{ {{{{ort!s if .l{{{{{{{{{{{{{{{{((union{e((((((union\n"
        "\n"
        "\n"
        "{e((((((union\n"
        "\n"
        "\n"
        "                                                                                                                                                                                               ");
    parse("IN[\"*[\\* 8F*\n"
          "\"(] 4");
    // Causes stack overflow when using address sanitizer due to address sanitizer possibly using 3x as much stack space
    // according to documentations
    // Also causes __chckstk to throw on windows when compiling in debug mode
#if defined(NDEBUG) || !defined(_WIN32)
    #if !defined(__has_feature)
    excludeFromAddressSanitizer();
    #else
        #if !__has_feature(address_sanitizer)
    excludeFromAddressSanitizer();
        #endif
    #endif
#endif
    parse("V=V==L+E");
}
