# cld - A C99 Compiler

cld is a fully C99 compliant (minus bugs) Compiler written in C++. The frontend was written from scratch and LLVM is
currently used as a backend for code generation. The command line interface is designed to match the one of GCC but is
currently still under work.

Currently supported targets for code generation are x64 on Windows and Linux (although ABI wise other x64 Operating
Systems should work).

The compilers source code is designed to be usable as a library. Each phase of the compiler is cleanly separated from
each other but builds on top of the previous phase.

As an example, checking if C source code is valid and emit errors to stderr is simply:

```cpp
bool errors = false;
auto pptokens = cld::Lexer::tokenize(std::move(input),options,&llvm::errs(),&errors);
if (errors)
{
    return -1;    
}
pptokens = cld::PP::preprocess(std::move(pptokens),&llvm::errs(),&errors);
if (errors)
{
    return -1;
}
auto ctokens = cld::Lexer::toCTokens(pptokens,&llvm::errs(),&errors);
if (errors)
{
    return -1;
}
auto tree = cld::Parser::buildTree(ctokens,&llvm::errs(),&errors);
if (errors)
{
    return -1;
}
cld::Semantics::analyse(tree,std::move(ctokens),&llvm::errs(),&errors);
```

Compiling C source code can be done just like with GCC:
`cld -c test.c` which will then produce a `test.o` object file.

Projects that have already successfully been compiled include zlib and sqlite3 using both glibc and MinGW as libc.

## WIP

Things that are currently still Work in Progress are:

* More commandline compatibility with GCC
* Ability to link
* Find sysroots for the target similar to clang
* Improve kind of broken preprocessor tokens to text conversion
* Implement more and more GCC Extensions and builtins

## Building

To build cld and it's libraries do enter following commands from the project root:

```shell
mkdir build
cd build
cmake .. -G<GENERATOR> -DCMAKE_INSTALL_PREFIX=<DIRECTORY>
```

with the generators, install directory, C++ compilers and more of your choice.

Then use the install target with the generator of your choice (`make install`,`ninja install`,etc...).

Additional options are:

* `CLD_BUILD_TESTS` (default ON): Also build tests and enable the `test` target
* `CLD_FUZZER` (default OFF): Enable fuzzer instrumentation and also build fuzzers
* `CLD_ENABLE_ASSERTS` (default OFF): Enable asserts in release mode as well (always active in debug mode)
* `CLD_USE_EXCEPTIONS` (default ON): Compile with exceptions
* `CLD_COVERAGE` (default OFF): Compile with Coverage
* `CLD_DEFAULT_TARGET` (default empty): String of the default target

Additionally cld currently unconditionally depends on LLVM 11. If your LLVM installation is not in a default location
for cmake to find you might need to append the path to CMAKE_PREFIX_PATH.

Supported Compilers are newer versions of Clang, GCC and MSVC. The compiler needs C++17 supports and good constexpr
support. The source code was tested and written using Clang 9 to 11, GCC 9 to 11 and MSVC 19.24 to 19.27 on Windows and
Linux. It should theoretically also compile on Mac OS

## Using cld with your own projects

Simply add the following to your CMakeLists.txt:

```cmake
find_package(Threads::Threads REQUIRED)
find_package(LLVM REQUIRED 11)
find_package(cld REQUIRED)
```

Make sure to use and append to CMAKE_PREFIX_PATH the same LLVM installation you used when building cld. You may list
multiple paths to CMAKE_PREFIX_PATH by separating them with `;`.

Following CMake targets are then available and must simply be linked against. The targets are:

* `cld::Frontend`. This includes all code related to the compiler frontend. All headers within cld/Frontend/* may be
  used without linking issues
* `cld::LLVMBackend`, has a dependency on `cld::Frontend`. This includes the LLVMBackend and allows LLVM IR to be
  generated from the Frontends AST. All headers within cld/LLVMBackend may be used without linker errors
* `cld::cldMain`, has a dependency on `cld::LLVMBackend`. This includes the main program used by the compiler. Linking
  against this target allows the usage of the cld/cldmain.hpp header.

Additionally all of the above targets depend on `cld::Support`, allowing all headers in cld/Support to be used.

