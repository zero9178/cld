
llvm_map_components_to_libnames(llvm_all ${LLVM_TARGETS_TO_BUILD} ipo)

message(STATUS "${llvm_all}")

add_library(cldMain cldMain.cpp CommandLine.cpp)
target_link_libraries(cldMain LLVMBackend ${llvm_all})
target_compile_definitions(cldMain PRIVATE CLD_VERSION="${PROJECT_VERSION}")
