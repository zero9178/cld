#pragma once

#include "Message.hpp"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-const-variable"

namespace cld
{
namespace Errors
{
namespace Parser
{
constexpr auto EXPECTED_N = Format("Expected {}");

constexpr auto EXPECTED_N_BEFORE_N = Format("Expected {} before {}");

constexpr auto EXPECTED_N_AFTER_N = Format("Expected {} after {}");

constexpr auto EXPECTED_N_INSTEAD_OF_N = Format("Expected {} instead of {}");

constexpr auto MISSING_PARAMETER_NAME = "Parameter name omitted in function definition";

constexpr auto N_REQUIRES_AT_LEAST_ONE_N = Format("{} requires at least one {}");

constexpr auto MAXIMUM_N_DEPTH_OF_N_EXCEEDED = Format("Maximum {} depth of {} exceeded");
} // namespace Parser

constexpr auto REDEFINITION_OF_SYMBOL_N = Format("Redefinition of symbol {}");

namespace Semantics
{
constexpr auto ONLY_ONE_STORAGE_SPECIFIER = "Only one storage specifier allowed in declaration";

constexpr auto ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION =
    "Only 'static' or 'extern' are allowed in function definition";

constexpr auto ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DECLARATION =
    "Only 'static' or 'extern' are allowed in function definition";

constexpr auto AT_LEAST_ONE_TYPE_SPECIFIER_REQUIRED = "At least one type specifier required";

constexpr auto EXPECTED_ONLY_PRIMITIVES = Format("Expected only primitives after {}");

constexpr auto N_APPEARING_MORE_THAN_N = Format("{} appearing more than {}");

constexpr auto CANNOT_COMBINE_N_WITH_N = Format("Cannot combine {} with {}");

constexpr auto EXPECTED_NO_FURTHER_N_AFTER_N = Format("Expected no further {} after {}");

constexpr auto ONLY_POINTERS_CAN_BE_RESTRICTED = "Only pointers can be restricted";

constexpr auto ARRAY_SIZE_MUST_BE_AN_INTEGER = "Array size must be an integer";

constexpr auto ONLY_REGISTER_ALLOWED_IN_FUNCTION_ARGUMENT =
    "Only 'register' allowed as storage specifier for function argument";

constexpr auto INLINE_ONLY_ALLOWED_IN_FRONT_OF_FUNCTION = "'inline' only allowed in front of a function";

constexpr auto FUNCTION_PARAMETER_CANNOT_BE_VOID = "Function parameter cannot be 'void'";

constexpr auto PARAMETER_IN_FUNCTION_NOT_ALLOWED_TO_HAVE_INITIALIZER =
    "Parameter in function not allowed to have initializer";

constexpr auto FUNCTION_DECLARATION_NOT_ALLOWED_TO_HAVE_INITIALIZER =
    "Function declaration not allowed to have initializer";

constexpr auto DECLARATION_CANNNOT_BE_VOID = "Declaration cannot have type 'void'";

constexpr auto N_APPEARING_IN_N_BUT_NOT_IN_N = Format("{} appearing in {} but not in {}");

constexpr auto EXPECTED_PARAMETER_LIST_IN_FUNCTION_DEFINITION = "Expected parameter list in function definition";

constexpr auto DECLARATIONS_ONLY_ALLOWED_WITH_IDENTIFIER_LIST = "Declarations only allowed with identifier list";

constexpr auto DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO_OR_REGISTER =
    "Declarations at file scope cannot be 'auto' or 'register'";

constexpr auto STATIC_ONLY_ALLOWED_FOR_FUNCTION_DECLARATION_AT_FILE_SCOPE =
    "'static' only allowed for function declaration at file scope";

constexpr auto IDENTIFIER_LIST_NOT_ALLOWED_IN_FUNCTION_DECLARATION =
    "Identifier list not allowed in function declaration";

constexpr auto INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF = Format("Incomplete type {} in 'alignof'");

constexpr auto INCOMPLETE_TYPE_N_IN_SIZE_OF = Format("Incomplete type {} in 'sizeof'");

constexpr auto INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC = Format("Incomplete type {} used in pointer arithmetic");

constexpr auto FUNCTION_TYPE_NOT_ALLOWED_IN_ALIGNMENT_OF = "Function type not allowed in 'alignof'";

constexpr auto FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF = "Function type not allowed in 'sizeof'";

constexpr auto SIZEOF_VAL_ARRAY_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION =
    "Size of Valarray cannot be determined in constant expression";

constexpr auto N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION = Format("{} not allowed in constant expression");

constexpr auto ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS =
    "Only integers allowed in integer constant expressions";

constexpr auto CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N =
    Format("Cannot apply unary operator '{}' to value of type {}");

constexpr auto CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION =
    "Can only cast to integers in integer constant expression";

constexpr auto CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N =
    Format("Cannot apply binary operator '{}' to values of type {} and {}");

constexpr auto CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_INCOMPATIBLE_TYPES_N_AND_N =
    Format("Cannot apply binary operator '{}' to values of incompatible types {} and {}");

constexpr auto INVALID_CAST_FROM_TYPE_N_TO_TYPE_N = Format("Invalid cast from type {} to type {}");

constexpr auto INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARED_WITH_POINTER =
    "Integer must evaluate to null to be compared with pointer";

constexpr auto UNKNOWN_TYPE_N = Format("Unknown type '{}'");
} // namespace Semantics

namespace Lexer
{
constexpr auto AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED = "At least one hexadecimal digit required";

constexpr auto INVALID_OCTAL_CHARACTER = Format("Invalid octal character '{}'");

constexpr auto INVALID_LITERAL_SUFFIX = Format("Invalid literal suffix '{}'");

constexpr auto INVALID_ESCAPE_SEQUENCE_N = Format("Invalid escape sequence '{}'");

constexpr auto INVALID_UNIVERSAL_CHARACTER_EXPECTED_N_MORE_DIGITS =
    Format("Invalid universal character. Expected {} more hex digits");

constexpr auto INVALID_UNIVERSAL_CHARACTER_VALUE_ILLEGAL_VALUE_N =
    Format("Invalid universal character. Illegal value of {} ({})");

constexpr auto VALUE_MUSTNT_BE_LESS_THAN_A0 = "Value mustn't be less than 0x00A0";

constexpr auto VALUE_MUSTNT_BE_IN_RANGE = "Value mustn't be in range of 0xD800 to 0xDFFF";

constexpr auto INVALID_HEX_ESCAPE_SEQUENCE_N = Format("Invalid hex escape sequence {}");

constexpr auto INVALID_OCTAL_ESCAPE_SEQUENCE_N = Format("Invalid hex escape sequence {}");

constexpr auto VALUE_MUST_FIT_IN_UTF32 = "Value must fit in UTF32";

constexpr auto EXPECTED_CHARACTER_AFTER_BACKSLASH = "Expected character after \\";

constexpr auto BINARY_FLOATING_POINT_MUST_CONTAIN_EXPONENT = "Binary floating point literal must contain an exponent";

constexpr auto EXPECTED_DIGITS_AFTER_EXPONENT = "Expected digits after exponent";

constexpr auto UNEXPECTED_CHARACTER = Format("Unexpected character '{}'");

constexpr auto NON_PRINTABLE_CHARACTER_N = Format("Non printable character '{}'");

constexpr auto NEWLINE_IN_N_USE_BACKLASH_N = Format("Newline in {} use \\n instead");

constexpr auto CHARACTER_LITERAL = "Character literal";

constexpr auto STRING_LITERAL = "String literal";

constexpr auto UNTERMINATED_N = Format("Unterminated {}");

constexpr auto BLOCK_COMMENT = "Block comment";

constexpr auto INCLUDE_DIRECTIVE = "Include directive";

constexpr auto INVALID_UTF8_SEQUENCE = "Invalid UTF-8 Sequence";

constexpr auto CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE = "Character too large for literal type";

constexpr auto CHARACTER_LITERAL_CANNOT_BE_EMPTY = "Character literal cannot be empty";

constexpr auto DISCARDING_ALL_BUT_FIRST_CHARACTER = "Discarding all but first character";

constexpr auto STRAY_N_IN_PROGRAM = Format("Stray '{}' in program");

constexpr auto NO_WHITESPACE_ALLOWED_BETWEEN_BACKSLASH_AND_NEWLINE =
    "No whitespace allowed between backslash and newline";

constexpr auto INTEGER_VALUE_TOO_BIG_TO_BE_REPRESENTABLE = "Integer value too big to be representable";
} // namespace Lexer

namespace PP
{
constexpr auto N_IS_AN_INVALID_PREPROCESSOR_DIRECTIVE = Format("{} is an invalid preprocessor directive");

constexpr auto REDEFINITION_OF_MACRO_PARAMETER_N = Format("Redefinition of macro parameter {}");

constexpr auto WHITESPACE_REQUIRED_AFTER_OBJECT_MACRO_DEFINITION = "Whitespace required after object macro definition";

constexpr auto DEFINED_CANNOT_BE_USED_AS_MACRO_NAME = "'defined' cannot be used as macro name";

constexpr auto REDEFINITION_OF_MACRO_N = Format("Redefinition of macro {}");

constexpr auto VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST = "__VA_ARGS__ not allowed in replacement list";

constexpr auto DEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED = Format("Defining builtin macro {} is not allowed");

constexpr auto UNDEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED = Format("Undefining builtin macro {} is not allowed");

constexpr auto NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N = Format("Not enough arguments for macro {}");

constexpr auto TOO_MANY_ARGUMENTS_FOR_MACRO_N = Format("Too many arguments for macro {}");
} // namespace PP
} // namespace ErrorMessages

namespace Warnings
{
namespace Semantics
{
constexpr auto VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N = Format("Value of {} is too large for integer type {}");
}
namespace PP
{
constexpr auto N_REDEFINED = Format("{} redefined");
}
} // namespace Warnings

namespace Notes
{
constexpr auto TYPEDEF_OVERSHADOWED_BY_DECLARATION = Format("{} is a typedef but overshadowed by declaration here:");

constexpr auto IDENTIFIER_IS_TYPEDEF =
    Format("{} is a typename and not an identifier due to typedef declaration here:");

constexpr auto TO_MATCH_N_HERE = Format("To match {} here:");

constexpr auto PREVIOUSLY_DECLARED_HERE = "Previously declared here:";

constexpr auto PREVIOUS_STORAGE_SPECIFIER_HERE = "Previous storage specifier encountered here:";

namespace Lexer
{
constexpr auto UNIVERSAL_CHARACTER_REQUIRES_N_MORE_DIGITS = Format("Universal character requires {} more digits");
}
} // namespace Notes
} // namespace cld

#pragma clang diagnostic pop
