
add_compile_definitions(CATCH_CONFIG_ENABLE_ALL_STRINGMAKERS)
if (MSVC AND ("${CLD_SANITIZER}" MATCHES ".*address.*"))
    add_compile_definitions(CATCH_CONFIG_NO_WINDOWS_SEH)
endif ()

add_library(testCommon STATIC TestConfig.cpp)
target_include_directories(testCommon PUBLIC .)

add_subdirectory(Frontend)
add_subdirectory(LLVMBackend)
add_subdirectory(cldMain)
