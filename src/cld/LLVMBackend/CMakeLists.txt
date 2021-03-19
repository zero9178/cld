
llvm_map_components_to_libnames(llvm_libs native)

add_library(LLVMBackend Codegen.cpp CodeGenerator.cpp CodeGenerator.hpp CodeGeneratorExpr.cpp CodeGeneratorDecl.cpp CodeGeneratorStatement.cpp ABIImplementation.cpp ABIImplementation.hpp WinX64ABI.cpp WinX64ABI.hpp X64ABI.cpp X64ABI.hpp)
target_link_libraries(LLVMBackend PUBLIC Frontend ${llvm_libs})
