
llvm_map_components_to_libnames(llvm_support Support)
add_library(Support Text.cpp Util.cpp)
target_link_libraries(Support PRIVATE ${llvm_support})
