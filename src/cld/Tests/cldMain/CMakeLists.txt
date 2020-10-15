

add_executable(cldMainTests main.cpp commandline.cpp)
target_link_libraries(cldMainTests cldMain testCommon)
set_source_files_properties(main.cpp PROPERTIES COMPILE_DEFINITIONS CATCH_CONFIG_RUNNER SKIP_PRECOMPILE_HEADERS ON)
if (CLD_USE_PCH)
    target_precompile_headers(cldMainTests PRIVATE <cld/Frontend/Compiler/ErrorMessages.hpp> <catch.hpp>)
endif ()
catch_discover_tests(cldMainTests)
