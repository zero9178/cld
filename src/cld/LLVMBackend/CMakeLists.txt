
llvm_map_components_to_libnames(llvm_libs native)

add_library(LLVMBackend Codegen.cpp)
target_link_libraries(LLVMBackend PUBLIC Frontend ${llvm_libs})
