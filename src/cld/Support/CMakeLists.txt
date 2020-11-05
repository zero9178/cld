
llvm_map_components_to_libnames(llvm_support Support)
add_library(Support Text.cpp Util.cpp Filesystem.cpp Triple.cpp)
target_link_libraries(Support PUBLIC ${llvm_support})
if (CLD_DEFAULT_TARGET)
    target_compile_definitions(Support PRIVATE CLD_DEFAULT_TARGET=${CLD_DEFAULT_TARGET})
endif ()
target_include_directories(Support SYSTEM PUBLIC ${LLVM_INCLUDE_DIRS})
