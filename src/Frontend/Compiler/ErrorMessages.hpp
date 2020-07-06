#pragma once

#include "Diagnostic.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-const-variable"
#pragma clang diagnostic ignored "-Wgnu-zero-variadic-macro-arguments"

#define CREATE_NOTE(variableName, format, ...)                        \
    namespace detail                                                  \
    {                                                                 \
    constexpr auto variableName##Text = ::ctll::fixed_string{format}; \
    }                                                                 \
    constexpr auto variableName =                                     \
        ::cld::makeDiagnostic<detail::variableName##Text, ##__VA_ARGS__>(::cld::Severity::Note, "")

#define CREATE_WARNING(variableName, cliName, format, ...)            \
    namespace detail                                                  \
    {                                                                 \
    constexpr auto variableName##Text = ::ctll::fixed_string{format}; \
    }                                                                 \
    constexpr auto variableName =                                     \
        ::cld::makeDiagnostic<detail::variableName##Text, ##__VA_ARGS__>(::cld::Severity::Warning, cliName)

#define CREATE_ERROR(variableName, format, ...)                       \
    namespace detail                                                  \
    {                                                                 \
    constexpr auto variableName##Text = ::ctll::fixed_string{format}; \
    }                                                                 \
    constexpr auto variableName =                                     \
        ::cld::makeDiagnostic<detail::variableName##Text, ##__VA_ARGS__>(::cld::Severity::Error, "")

namespace cld
{
namespace Errors
{
namespace Parser
{
CREATE_ERROR(EXPECTED_N, "Expected %0", InsertAfter<0>);

CREATE_ERROR(EXPECTED_N_BEFORE_N, "Expected %0 before %1");

CREATE_ERROR(EXPECTED_N_AFTER_N, "Expected %0 after %1");

CREATE_ERROR(EXPECTED_N_INSTEAD_OF_N, "Expected %0 instead of %1");

CREATE_ERROR(MISSING_PARAMETER_NAME, "Parameter name omitted in function definition");

CREATE_ERROR(N_REQUIRES_AT_LEAST_ONE_N, "%0 requires at least one %1");

CREATE_ERROR(MAXIMUM_N_DEPTH_OF_N_EXCEEDED, "Maximum {} depth of {} exceeded");
} // namespace Parser

CREATE_ERROR(REDEFINITION_OF_SYMBOL_N, "Redefinition of symbol {}");

namespace Semantics
{
CREATE_ERROR(ONLY_ONE_STORAGE_SPECIFIER, "Only one storage specifier allowed in declaration");

CREATE_ERROR(ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION,
             "Only 'static' or 'extern' are allowed in function definition");

CREATE_ERROR(ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DECLARATION,
             "Only 'static' or 'extern' are allowed in function definition");

CREATE_ERROR(AT_LEAST_ONE_TYPE_SPECIFIER_REQUIRED, "At least one type specifier required");

CREATE_ERROR(EXPECTED_ONLY_PRIMITIVES, "Expected only primitives after {}");

CREATE_ERROR(N_APPEARING_MORE_THAN_N, "{} appearing more than {}");

CREATE_ERROR(CANNOT_COMBINE_N_WITH_N, "Cannot combine {} with {}");

CREATE_ERROR(EXPECTED_NO_FURTHER_N_AFTER_N, "Expected no further {} after {}");

CREATE_ERROR(ONLY_POINTERS_CAN_BE_RESTRICTED, "Only pointers can be restricted");

CREATE_ERROR(ARRAY_SIZE_MUST_BE_AN_INTEGER, "Array size must be an integer");

CREATE_ERROR(ONLY_REGISTER_ALLOWED_IN_FUNCTION_ARGUMENT,
             "Only 'register' allowed as storage specifier for function argument");

CREATE_ERROR(INLINE_ONLY_ALLOWED_IN_FRONT_OF_FUNCTION, "'inline' only allowed in front of a function");

CREATE_ERROR(FUNCTION_PARAMETER_CANNOT_BE_VOID, "Function parameter cannot be 'void'");

CREATE_ERROR(PARAMETER_IN_FUNCTION_NOT_ALLOWED_TO_HAVE_INITIALIZER,
             "Parameter in function not allowed to have initializer");

CREATE_ERROR(FUNCTION_DECLARATION_NOT_ALLOWED_TO_HAVE_INITIALIZER,
             "Function declaration not allowed to have initializer");

CREATE_ERROR(DECLARATION_CANNNOT_BE_VOID, "Declaration cannot have type 'void'");

CREATE_ERROR(N_APPEARING_IN_N_BUT_NOT_IN_N, "{} appearing in {} but not in {}");

CREATE_ERROR(EXPECTED_PARAMETER_LIST_IN_FUNCTION_DEFINITION, "Expected parameter list in function definition");

CREATE_ERROR(DECLARATIONS_ONLY_ALLOWED_WITH_IDENTIFIER_LIST, "Declarations only allowed with identifier list");

CREATE_ERROR(DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO_OR_REGISTER,
             "Declarations at file scope cannot be 'auto' or 'register'");

CREATE_ERROR(STATIC_ONLY_ALLOWED_FOR_FUNCTION_DECLARATION_AT_FILE_SCOPE,
             "'static' only allowed for function declaration at file scope");

CREATE_ERROR(IDENTIFIER_LIST_NOT_ALLOWED_IN_FUNCTION_DECLARATION,
             "Identifier list not allowed in function declaration");

CREATE_ERROR(INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF, "Incomplete type {} in 'alignof'");

CREATE_ERROR(INCOMPLETE_TYPE_N_IN_SIZE_OF, "Incomplete type {} in 'sizeof'");

CREATE_ERROR(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "Incomplete type {} used in pointer arithmetic");

CREATE_ERROR(FUNCTION_TYPE_NOT_ALLOWED_IN_ALIGNMENT_OF, "Function type not allowed in 'alignof'");

CREATE_ERROR(FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF, "Function type not allowed in 'sizeof'");

CREATE_ERROR(SIZEOF_VAL_ARRAY_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION,
             "Size of Valarray cannot be determined in constant expression");

CREATE_ERROR(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "{} not allowed in constant expression");

CREATE_ERROR(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
             "Only integers allowed in integer constant expressions");

CREATE_ERROR(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N, "Cannot apply unary operator '{}' to value of type {}");

CREATE_ERROR(CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION,
             "Can only cast to integers in integer constant expression");

CREATE_ERROR(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N,
             "Cannot apply binary operator '{}' to values of type {} and {}");

CREATE_ERROR(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_INCOMPATIBLE_TYPES_N_AND_N,
             "Cannot apply binary operator '{}' to values of incompatible types {} and {}");

CREATE_ERROR(INVALID_CAST_FROM_TYPE_N_TO_TYPE_N, "Invalid cast from type {} to type {}");

CREATE_ERROR(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARED_WITH_POINTER,
             "Integer must evaluate to null to be compared with pointer");

CREATE_ERROR(UNKNOWN_TYPE_N, "Unknown type '{}'");
} // namespace Semantics

namespace Lexer
{
CREATE_ERROR(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED, "At least one hexadecimal digit required");

CREATE_ERROR(INVALID_OCTAL_CHARACTER, "Invalid octal character '{}'");

CREATE_ERROR(INVALID_LITERAL_SUFFIX, "Invalid literal suffix '{}'");

CREATE_ERROR(INVALID_ESCAPE_SEQUENCE_N, "Invalid escape sequence '{}'");

CREATE_ERROR(INVALID_UNIVERSAL_CHARACTER_EXPECTED_N_MORE_DIGITS,
             "Invalid universal character. Expected {} more hex digits");

CREATE_ERROR(INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N,
             "Invalid universal character. Illegal value of 0x{} ({})");

CREATE_ERROR(VALUE_MUSTNT_BE_LESS_THAN_A0, "Value mustn't be less than 0x00A0");

CREATE_ERROR(VALUE_MUSTNT_BE_IN_RANGE, "Value mustn't be in range of 0xD800 to 0xDFFF");

CREATE_ERROR(INVALID_HEX_ESCAPE_SEQUENCE_N, "Invalid hex escape sequence {}");

CREATE_ERROR(VALUE_MUST_FIT_IN_UTF32, "Value must fit in UTF32");

CREATE_ERROR(EXPECTED_CHARACTER_AFTER_BACKSLASH, "Expected character after \\");

CREATE_ERROR(BINARY_FLOATING_POINT_MUST_CONTAIN_EXPONENT, "Binary floating point literal must contain an exponent");

CREATE_ERROR(EXPECTED_DIGITS_AFTER_EXPONENT, "Expected digits after exponent");

CREATE_ERROR(UNEXPECTED_CHARACTER, "Unexpected character");

CREATE_ERROR(NON_PRINTABLE_CHARACTER_N, "Non printable character '{}'");

CREATE_ERROR(NEWLINE_IN_N_USE_BACKLASH_N, "Newline in {} use \\n instead");

CREATE_ERROR(CHARACTER_LITERAL, "Character literal");

CREATE_ERROR(STRING_LITERAL, "String literal");

CREATE_ERROR(UNTERMINATED_N, "Unterminated {}");

CREATE_ERROR(BLOCK_COMMENT, "Block comment");

CREATE_ERROR(INCLUDE_DIRECTIVE, "Include directive");

CREATE_ERROR(INVALID_UTF8_SEQUENCE, "Invalid UTF-8 Sequence");

CREATE_ERROR(CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE, "Character too large for literal type");

CREATE_ERROR(CHARACTER_LITERAL_CANNOT_BE_EMPTY, "Character literal cannot be empty");

CREATE_ERROR(DISCARDING_ALL_BUT_FIRST_CHARACTER, "Discarding all but first character");

CREATE_ERROR(STRAY_N_IN_PROGRAM, "Stray '{}' in program");

CREATE_ERROR(INTEGER_VALUE_TOO_BIG_TO_BE_REPRESENTABLE, "Integer value too big to be representable");
} // namespace Lexer

namespace PP
{
CREATE_ERROR(N_IS_AN_INVALID_PREPROCESSOR_DIRECTIVE, "{} is an invalid preprocessor directive");

CREATE_ERROR(REDEFINITION_OF_MACRO_PARAMETER_N, "Redefinition of macro parameter {}");

CREATE_ERROR(WHITESPACE_REQUIRED_AFTER_OBJECT_MACRO_DEFINITION, "Whitespace required after object macro definition");

CREATE_ERROR(DEFINED_CANNOT_BE_USED_AS_MACRO_NAME, "'defined' cannot be used as macro name");

CREATE_ERROR(REDEFINITION_OF_MACRO_N, "Redefinition of macro {}");

CREATE_ERROR(VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST, "__VA_ARGS__ not allowed in replacement list");

CREATE_ERROR(DEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED, "Defining builtin macro {} is not allowed");

CREATE_ERROR(UNDEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED, "Undefining builtin macro {} is not allowed");

CREATE_ERROR(NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N,
             "Not enough arguments for macro {}. Expected {} got {}");

CREATE_ERROR(NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_AT_LEAST_N_GOT_N,
             "Not enough arguments for macro {}. Expected at least {} got {}");

CREATE_ERROR(TOO_MANY_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N, "Too many arguments for macro {}. Expected {} got {}");

CREATE_ERROR(EXPECTED_AN_ARGUMENT_AFTER_POUND,
             "Expected an argument after '#' in replacement list of function like macro");

CREATE_ERROR(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_BEGINNING_OF_REPLACEMENT_LIST,
             "Operator '##' not allowed at beginning of replacement list");

CREATE_ERROR(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_END_OF_REPLACEMENT_LIST,
             "Operator '##' not allowed at end of replacement list");

CREATE_ERROR(EXPECTED_A_FILENAME_AFTER_INCLUDE, "Expected a <FILENAME> or \"FILENAME\" after #include");

CREATE_ERROR(EXTRA_TOKENS_AFTER_INCLUDE, "Extra tokens after #include");
} // namespace PP
} // namespace Errors

namespace Warnings
{
namespace Semantics
{
CREATE_WARNING(VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N, "overflow", "Value of {} is too large for integer type {}");
} // namespace Semantics
namespace PP
{
CREATE_WARNING(N_REDEFINED, "macro-redefined", "{} redefined");

CREATE_WARNING(TOKEN_CONCATENATION_RESULTING_IN_AN_INVALID_TOKEN_IS_UB, "token-concat",
               "Token concatenation resulting in an invalid token is undefined behaviour");
} // namespace PP
} // namespace Warnings

namespace Notes
{
CREATE_NOTE(TYPEDEF_OVERSHADOWED_BY_DECLARATION, "{} is a typedef but overshadowed by declaration here:");

CREATE_NOTE(IDENTIFIER_IS_TYPEDEF, "{} is a typename and not an identifier due to typedef declaration here:");

CREATE_NOTE(TO_MATCH_N_HERE, "To match {} here:");

CREATE_NOTE(PREVIOUSLY_DECLARED_HERE, "Previously declared here:");

CREATE_NOTE(PREVIOUS_STORAGE_SPECIFIER_HERE, "Previous storage specifier encountered here:");

namespace PP
{
CREATE_NOTE(WHEN_CONCATENATING_N_AND_N, "When concatenating '{}' and '{}'");
} // namespace PP
} // namespace Notes
} // namespace cld

#pragma clang diagnostic pop
#undef CREATE_WARNING
#undef CREATE_ERROR
#undef CREATE_NOTE
