#ifndef OPENCLPARSER_ERRORMESSAGES_HPP
#define OPENCLPARSER_ERRORMESSAGES_HPP

#include "Message.hpp"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-const-variable"

namespace OpenCL
{
    namespace ErrorMessages
    {
        namespace Parser
        {
            constexpr auto EXPECTED_N = Format("Expected {}");

            constexpr auto EXPECTED_N_BEFORE_N = Format("Expected {} before {}");

            constexpr auto EXPECTED_N_AFTER_N = Format("Expected {} after {}");

            constexpr auto EXPECTED_N_INSTEAD_OF_N = Format("Expected {} instead of {}");

            constexpr auto MISSING_PARAMETER_NAME = "Parameter name omitted in function definition";
        }

        constexpr auto REDEFINITION_OF_SYMBOL_N = Format("Redefinition of symbol {}");

        namespace Semantics
        {
            constexpr auto ONLY_ONE_STORAGE_SPECIFIER = "Only one storage specifier allowed in declaration";

            constexpr auto ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION
                = "Only 'static' or 'extern' are allowed in function definition";

            constexpr auto ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DECLARATION
                = "Only 'static' or 'extern' are allowed in function definition";

            constexpr auto AT_LEAST_ONE_TYPE_SPECIFIER_REQUIRED = "At least one type specifier required";

            constexpr auto EXPECTED_ONLY_PRIMITIVES = Format("Expected only primitives after {}");

            constexpr auto N_APPEARING_MORE_THAN_N = Format("{} appearing more than {}");

            constexpr auto CANNOT_COMBINE_N_WITH_N = Format("Cannot combine {} with {}");

            constexpr auto EXPECTED_NO_FURTHER_N_AFTER_N = Format("Expected no further {} after {}");

            constexpr auto ONLY_POINTERS_CAN_BE_RESTRICTED = "Only pointers can be restricted";

            constexpr auto ARRAY_SIZE_MUST_BE_AN_INTEGER = "Array size must be an integer";

            constexpr auto ONLY_REGISTER_ALLOWED_IN_FUNCTION_ARGUMENT
                = "Only 'register' allowed as storage specifier for function argument";

            constexpr auto INLINE_ONLY_ALLOWED_IN_FRONT_OF_FUNCTION = "'inline' only allowed in front of a function";

            constexpr auto FUNCTION_PARAMETER_CANNOT_BE_VOID = "Function parameter cannot be 'void'";

            constexpr auto PARAMETER_IN_FUNCTION_NOT_ALLOWED_TO_HAVE_INITIALIZER
                = "Parameter in function not allowed to have initializer";

            constexpr auto FUNCTION_DECLARATION_NOT_ALLOWED_TO_HAVE_INITIALIZER
                = "Function declaration not allowed to have initializer";

            constexpr auto DECLARATION_CANNNOT_BE_VOID = "Declaration cannot have type 'void'";

            constexpr auto N_APPEARING_IN_N_BUT_NOT_IN_N = Format("{} appearing in {} but not in {}");

            constexpr auto
                EXPECTED_PARAMETER_LIST_IN_FUNCTION_DEFINITION = "Expected parameter list in function definition";

            constexpr auto
                DECLARATIONS_ONLY_ALLOWED_WITH_IDENTIFIER_LIST = "Declarations only allowed with identifier list";

            constexpr auto DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO_OR_REGISTER
                = "Declarations at file scope cannot be 'auto' or 'register'";

            constexpr auto STATIC_ONLY_ALLOWED_FOR_FUNCTION_DECLARATION_AT_FILE_SCOPE
                = "'static' only allowed for function declaration at file scope";

            constexpr auto IDENTIFIER_LIST_NOT_ALLOWED_IN_FUNCTION_DECLARATION
                = "Identifier list not allowed in function declaration";

            constexpr auto INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF = Format("Incomplete type {} in alignment of");

            constexpr auto INCOMPLETE_TYPE_N_IN_SIZE_OF = Format("Incomplete type {} in size of");

            constexpr auto FUNCTION_TYPE_NOT_ALLOWED_IN_ALIGNMENT_OF = "Function type not allowed in alignment of";

            constexpr auto FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF = "Function type not not allowed in size of";

            constexpr auto SIZEOF_VAL_ARRAY_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION
                = "Size of Valarray cannot be determined in constant expression";

            constexpr auto N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION
                = Format("{} not allowed in constant expression");
        }
    }

    namespace Notes
    {
        constexpr auto
            TYPEDEF_OVERSHADOWED_BY_DECLARATION = Format("{} is a typedef but overshadowed by declaration here:");

        constexpr auto
            IDENTIFIER_IS_TYPDEF = Format("{} is a typename and not an identifier due to typedef declaration here:");

        constexpr auto TO_MATCH_N_HERE = Format("To match {} here:");

        constexpr auto PREVIOUSLY_DECLARED_HERE = "Previously declared here:";

        constexpr auto PREVIOUS_STORAGE_SPECIFIER_HERE = "Previous storage specifier encountered here:";
    }
}

#pragma clang diagnostic pop

#endif //OPENCLPARSER_ERRORMESSAGES_HPP
