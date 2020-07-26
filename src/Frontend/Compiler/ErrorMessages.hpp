#pragma once

#include "Diagnostic.hpp"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wgnu-zero-variadic-macro-arguments"

// TODO: Use __VA_OPT__(,) in C++20

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
CREATE_ERROR(EXPECTED_N, "Expected %0", InsertAfter<1, 0>);

CREATE_ERROR(EXPECTED_N_OR_N, "Expected %0 or %1", InsertAfter<2>);

CREATE_ERROR(EXPECTED_N_AFTER_N, "Expected %0 after %1", InsertAfter<1, 0>);

CREATE_ERROR(EXPECTED_N_OR_N_AFTER_N, "Expected %0 or %1 after %2", InsertAfter<2>);

CREATE_ERROR(EXPECTED_N_INSTEAD_OF_N, "Expected %0 instead of %1", PointAt<1>);

CREATE_ERROR(EXPECTED_N_OR_N_INSTEAD_OF_N, "Expected %0 or %1 instead of %2", PointAt<2>);

CREATE_ERROR(EXPECTED_LITERAL_N_OR_N, "Expected literal, %0 or %1", InsertAfter<2>);

CREATE_ERROR(EXPECTED_LITERAL_N_OR_N_INSTEAD_OF_N, "Expected literal, %0 or %1 instead of %2", PointAt<2>);

CREATE_ERROR(EXPECTED_TYPENAME, "Expected typename", InsertAfter<0>);

CREATE_ERROR(EXPECTED_TYPENAME_INSTEAD_OF_N, "Expected typename instead of %0", PointAt<0>);

CREATE_ERROR(EXPECTED_TYPENAME_BEFORE_N, "Expected typename before %0", PointAt<0>);

CREATE_ERROR(EXPECTED_N_INSTEAD_OF_TYPENAME, "Expected %0 instead of typename", Underline<1>);

CREATE_ERROR(EXPECTED_STORAGE_SPECIFIER_OR_TYPENAME, "Expected storage specifier or typename", InsertAfter<0>);

CREATE_ERROR(EXPECTED_STORAGE_SPECIFIER_OR_TYPENAME_BEFORE_N, "Expected storage specifier or typename before %0",
             PointAt<0>);

CREATE_ERROR(EXPECTED_EXPRESSION, "Expected expression", InsertAfter<0>);

CREATE_ERROR(EXPECTED_EXPRESSION_OR_DECLARATION, "Expected expression or declaration", InsertAfter<0>);

CREATE_ERROR(EXPECTED_TOKENS_AFTER_N, "Expected tokens after %0", InsertAfter<0>);

CREATE_ERROR(EXPECTED_PARAMETER_AFTER_N, "Expected parameter after %0", InsertAfter<0>);

CREATE_ERROR(EXPECTED_ENDIF, "Expected '#endif'", InsertAfter<0, 1>);

CREATE_ERROR(EXPECTED_ENDIF_INSTEAD_OF_N, "Expected 'endif' instead of %0", PointAt<0>);

CREATE_ERROR(MISSING_PARAMETER_NAME, "Parameter name omitted in function definition", Underline<0>);

CREATE_ERROR(UNION_REQUIRES_AT_LEAST_ONE_FIELD, "Union requires at least one field", Underline<0>);

CREATE_ERROR(STRUCT_REQUIRES_AT_LEAST_ONE_FIELD, "Struct requires at least one field", Underline<0>);

CREATE_ERROR(ENUM_REQUIRES_AT_LEAST_ONE_VALUE, "Enum requires at least one value", Underline<0>);

CREATE_ERROR(PARAMETER_LIST_REQUIRES_AT_LEAST_ONE_PARAMETER, "Parameter list requires at least one parameter",
             PointAt<0>);

CREATE_ERROR(MAXIMUM_BRACKET_DEPTH_OF_N_EXCEEDED, "Maximum bracket depth of %0 exceeded", PointAt<1>);
} // namespace Parser

CREATE_ERROR(REDEFINITION_OF_SYMBOL_N, "Redefinition of symbol %0", Underline<0>);

namespace Semantics
{
CREATE_ERROR(ONLY_ONE_STORAGE_SPECIFIER, "Only one storage specifier allowed in declaration", Underline<0>);

CREATE_ERROR(ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION,
             "Only 'static' or 'extern' are allowed in function definition", Underline<0>);

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

CREATE_ERROR(EXPECTED_PARAMETER_LIST_IN_FUNCTION_DEFINITION, "Expected parameter list in function definition",
             Underline<0>);

CREATE_ERROR(DECLARATIONS_ONLY_ALLOWED_WITH_IDENTIFIER_LIST, "Declarations only allowed with identifier list",
             Underline<0>);

CREATE_ERROR(DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO_OR_REGISTER,
             "Declarations at file scope cannot be 'auto' or 'register'");

CREATE_ERROR(STATIC_ONLY_ALLOWED_FOR_FUNCTION_DECLARATION_AT_FILE_SCOPE,
             "'static' only allowed for function declaration at file scope");

CREATE_ERROR(IDENTIFIER_LIST_NOT_ALLOWED_IN_FUNCTION_DECLARATION,
             "Identifier list not allowed in function declaration");

CREATE_ERROR(INCOMPLETE_TYPE_N_IN_ALIGNMENT_OF, "Incomplete type %full0 in 'alignof'", Underline<1>);

CREATE_ERROR(INCOMPLETE_TYPE_N_IN_SIZE_OF, "Incomplete type %full0 in 'sizeof'", Underline<1>);

CREATE_ERROR(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "Incomplete type %full0 used in pointer arithmetic",
             Annotate<1, 2>);

CREATE_ERROR(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC_2, "Incomplete type %full0 used in pointer arithmetic",
             Annotate<1, 2>, Annotate<3, 4>);

CREATE_ERROR(FUNCTION_TYPE_NOT_ALLOWED_IN_ALIGNMENT_OF, "Function type not allowed in 'alignof'", Underline<0>);

CREATE_ERROR(FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF, "Function type not allowed in 'sizeof'", Underline<0>);

CREATE_ERROR(SIZEOF_VAL_ARRAY_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION,
             "Size of variable length array cannot be determined in constant expression", Underline<0>);

CREATE_ERROR(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "%tokenType0 not allowed in constant expression", PointAt<0>);

CREATE_ERROR(STRING_LITERALS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "String literals not allowed in constant expression",
             Underline<0>);

CREATE_ERROR(VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "Variable access not allowed in constant expression",
             Underline<0>);

CREATE_ERROR(FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "Function call not allowed in constant expression",
             Underline<0>);

CREATE_ERROR(INITIALIZER_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "Initializer not allowed in constant expression",
             Underline<0>);

CREATE_ERROR(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
             "Only integers allowed in integer constant expressions", Annotate<0, 1>);

CREATE_ERROR(CANNOT_APPLY_UNARY_OPERATOR_N_TO_VALUE_OF_TYPE_N,
             "Cannot apply unary operator %tokenType0 to value of type %full1", PointAt<0>, Annotate<2, 1>);

CREATE_ERROR(CAN_ONLY_CAST_TO_INTEGERS_IN_INTEGER_CONSTANT_EXPRESSION,
             "Can only cast to integers in integer constant expression", Underline<0>);

CREATE_ERROR(CANNOT_APPLY_BINARY_OPERATOR_N_TO_VALUES_OF_TYPE_N_AND_N,
             "Cannot apply binary operator %tokenType0 to values of type %full1 and %full2", PointAt<0>, Annotate<3, 1>,
             Annotate<4, 2>);

CREATE_ERROR(INVALID_CAST_FROM_TYPE_N_TO_TYPE_N, "Invalid cast from type %full0 to type %full1", Underline<2>,
             Annotate<3, 0>);

CREATE_ERROR(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARABLE_WITH_POINTER,
             "Integer must evaluate to null to be comparable with pointer", Underline<0>);

CREATE_ERROR(UNKNOWN_TYPE_N, "Unknown type '{}'");
} // namespace Semantics

namespace Lexer
{
CREATE_ERROR(AT_LEAST_ONE_HEXADECIMAL_DIGIT_REQUIRED, "At least one hexadecimal digit required", PointAt<0>);

CREATE_ERROR(INVALID_OCTAL_CHARACTER, "Invalid octal character '%0'", PointAt<1>);

CREATE_ERROR(INVALID_LITERAL_SUFFIX, "Invalid literal suffix '%0'", Underline<1>);

CREATE_ERROR(INVALID_ESCAPE_SEQUENCE_N, "Invalid escape sequence '%0'", PointAt<1>);

CREATE_ERROR(INVALID_UC_EXPECTED_N_MORE_DIGITS, "Invalid universal character. Expected %0 more hex digit%s0",
             Underline<1>);

CREATE_ERROR(INVALID_UC_VALUE_MUSTNT_BE_LESS_THAN_A0, "Invalid universal character. Value mustn't be less than 0x00A0",
             Underline<0>);

CREATE_ERROR(INVALID_UC_VALUE_MUSTNT_BE_IN_RANGE,
             "Invalid universal character. Value mustn't be in range of 0xD800 to 0xDFFF", Underline<0>);

CREATE_ERROR(INVALID_UC_VALUE_MUST_FIT_IN_UTF32, "Invalid universal character. Value must fit in UTF32", Underline<0>);

CREATE_ERROR(INVALID_HEX_VALUE_MUSTNT_BE_LESS_THAN_A0, "Invalid hex escape sequence. Value mustn't be less than 0x00A0",
             Underline<0>);

CREATE_ERROR(INVALID_HEX_VALUE_MUSTNT_BE_IN_RANGE,
             "Invalid hex escape sequence. Value mustn't be in range of 0xD800 to 0xDFFF", Underline<0>);

CREATE_ERROR(INVALID_HEX_VALUE_MUST_FIT_IN_UTF32, "Invalid hex escape sequence. Value must fit in UTF32", Underline<0>);

CREATE_ERROR(EXPECTED_CHARACTER_AFTER_BACKSLASH, "Expected character after '\\'", PointAt<0>);

CREATE_ERROR(BINARY_FLOATING_POINT_MUST_CONTAIN_EXPONENT, "Binary floating point literal must contain an exponent",
             InsertAfter<0>, Underline<0>);

CREATE_ERROR(EXPECTED_DIGITS_AFTER_EXPONENT, "Expected digits after exponent", Underline<0>, InsertAfter<1>);

CREATE_ERROR(UNEXPECTED_CHARACTER, "Unexpected character", PointAt<0>);

CREATE_ERROR(NON_PRINTABLE_CHARACTER_N, "Non printable character '%0'", PointAt<1>);

CREATE_ERROR(NEWLINE_IN_CHARACTER_LITERAL_USE_BACKLASH_N, "Newline in character literal. Use \\n instead", PointAt<0>);

CREATE_ERROR(NEWLINE_IN_STRING_LITERAL_USE_BACKLASH_N, "Newline in string literal. Use \\n instead", PointAt<0>);

CREATE_ERROR(INVALID_UTF8_SEQUENCE, "Invalid UTF-8 Sequence", PointAt<0>);

CREATE_ERROR(CHARACTER_TOO_LARGE_FOR_LITERAL_TYPE, "Character too large for literal type", Underline<0>);

CREATE_ERROR(CHARACTER_LITERAL_CANNOT_BE_EMPTY, "Character literal cannot be empty", Underline<0>);

CREATE_ERROR(STRAY_N_IN_PROGRAM, "Stray %tokenType0 in program", PointAt<0>);

CREATE_ERROR(INTEGER_VALUE_TOO_BIG_TO_BE_REPRESENTABLE, "Integer value too big to be representable", Underline<0>);

CREATE_ERROR(UNTERMINATED_INCLUDE_DIRECTIVE, "Unterminated include directive", Underline<0>);

CREATE_ERROR(UNTERMINATED_BLOCK_COMMENT, "Unterminated block comment", Underline<0>);

CREATE_ERROR(UNTERMINATED_CHARACTER_LITERAL, "Unterminated character literal", Underline<0>);

CREATE_ERROR(UNTERMINATED_STRING_LITERAL, "Unterminated string literal", Underline<0>);
} // namespace Lexer

namespace PP
{
CREATE_ERROR(N_IS_AN_INVALID_PREPROCESSOR_DIRECTIVE, "%0 is an invalid preprocessor directive", Underline<0>);

CREATE_ERROR(REDEFINITION_OF_MACRO_PARAMETER_N, "Redefinition of macro parameter %0", Underline<0>);

CREATE_ERROR(WHITESPACE_REQUIRED_AFTER_OBJECT_MACRO_DEFINITION, "Whitespace required after object macro definition",
             Underline<0>);

CREATE_ERROR(DEFINED_CANNOT_BE_USED_AS_MACRO_NAME, "'defined' cannot be used as macro name", Underline<0>);

CREATE_ERROR(REDEFINITION_OF_MACRO_N, "Redefinition of macro %0", Underline<0>);

CREATE_ERROR(VA_ARGS_NOT_ALLOWED_IN_REPLACEMENT_LIST, "__VA_ARGS__ not allowed in replacement list", Underline<0>);

CREATE_ERROR(DEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED, "Defining builtin macro %0 is not allowed", Underline<0>);

CREATE_ERROR(UNDEFINING_BUILTIN_MACRO_N_IS_NOT_ALLOWED, "Undefining builtin macro %0 is not allowed", Underline<0>);

CREATE_ERROR(NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N, "Not enough arguments for macro %0. Expected %1 got %2",
             Underline<0>);

CREATE_ERROR(NOT_ENOUGH_ARGUMENTS_FOR_MACRO_N_EXPECTED_AT_LEAST_N_GOT_N,
             "Not enough arguments for macro %0. Expected at least %1 got %2", Underline<0>);

CREATE_ERROR(TOO_MANY_ARGUMENTS_FOR_MACRO_N_EXPECTED_N_GOT_N, "Too many arguments for macro %0. Expected %1 got %2",
             Underline<0>, PointAt<3>);

CREATE_ERROR(EXPECTED_AN_ARGUMENT_AFTER_POUND,
             "Expected an argument after '#' in replacement list of function like macro", PointAt<0>, Underline<1>);

CREATE_ERROR(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_BEGINNING_OF_REPLACEMENT_LIST,
             "Operator '##' not allowed at beginning of replacement list", Underline<0>);

CREATE_ERROR(OPERATOR_DOUBLE_POUND_NOT_ALLOWED_AT_END_OF_REPLACEMENT_LIST,
             "Operator '##' not allowed at end of replacement list", Underline<0>);

CREATE_ERROR(EXPECTED_A_FILENAME_AFTER_INCLUDE, "Expected a <FILENAME> or \"FILENAME\" after #include", InsertAfter<0>);

CREATE_ERROR(EXPECTED_A_FILENAME_AFTER_INCLUDE_2, "Expected a <FILENAME> or \"FILENAME\" after #include", Underline<0>);

CREATE_ERROR(EXTRA_TOKENS_AFTER_INCLUDE, "Extra tokens after '#include'", Underline<0>);

CREATE_ERROR(FILE_NOT_FOUND, "'%0' file not found", Underline<1>);

CREATE_ERROR(COULD_NOT_OPEN_FILE, "Could not open '%0'", Underline<1>);

CREATE_ERROR(EXPECTED_A_NUMBER_AFTER_LINE, "Expected a number after '#line'", InsertAfter<0>);

CREATE_ERROR(EXPECTED_END_OF_LINE_OR_STRING_AFTER_NUMBER_IN_LINE,
             "Expected end of line or string after number in '#line' directive", Underline<0>);

CREATE_ERROR(EXTRA_TOKENS_AFTER_LINE, "Extra tokens after '#line'", Underline<0>);

CREATE_ERROR(STRING_MUST_BE_NORMAL_IN_LINE_DIRECTIVE, "String must be normal in '#line' directive", Underline<0>);

CREATE_ERROR(NUMBER_MUST_BE_IN_DECIMAL_IN_LINE_DIRECTIVE, "Number must be in decimal in '#line' directive",
             Underline<0>);

CREATE_ERROR(NUMBER_MUST_NOT_BE_ZERO_IN_LINE_DIRECTIVE, "Number must not be zero in '#line' directive", Underline<0>);

CREATE_ERROR(NUMBER_MUST_NOT_BE_GREATER_THAN_X_IN_LINE_DIRECTIVE,
             "Number must not be greater than 2147483647 in '#line' directive", Underline<0>);

CREATE_ERROR(ERROR_ENCOUNTERED, "'#error' encountered", Underline<0>);
} // namespace PP
} // namespace Errors

namespace Warnings
{
namespace Semantics
{
CREATE_WARNING(VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N, "overflow",
               "Value of '%0' is too large for integer type %full1", Underline<2>);
} // namespace Semantics
namespace Lexer
{
CREATE_WARNING(DISCARDING_ALL_BUT_FIRST_CHARACTER, "multichar", "Discarding all but first character", Underline<0>);
} // namespace Lexer
namespace PP
{
CREATE_WARNING(N_REDEFINED, "macro-redefined", "%0 redefined", Underline<0>);

CREATE_WARNING(TOKEN_CONCATENATION_RESULTING_IN_AN_INVALID_TOKEN_IS_UB, "token-concat",
               "Token concatenation resulting in an invalid token is undefined behaviour", Underline<0>, PointAt<1>,
               Underline<2>);

CREATE_WARNING(MACRO_EXPANSION_PRODUCING_DEFINED_IS_NOT_PORTABLE, "expansion-to-defined",
               "Macro expansion producing 'defined' is not portable", Underline<0>);
} // namespace PP
} // namespace Warnings

namespace Notes
{
CREATE_NOTE(TYPEDEF_OVERSHADOWED_BY_DECLARATION, "%0 is a typedef but overshadowed by declaration here:", PointAt<0>);

CREATE_NOTE(IDENTIFIER_IS_TYPEDEF,
            "%0 is a typename and not an identifier due to typedef declaration here:", Underline<0>);

CREATE_NOTE(TO_MATCH_N_HERE, "To match %0 here:", PointAt<0>);

CREATE_NOTE(PREVIOUSLY_DECLARED_HERE, "Previously declared here:", Underline<0>);

CREATE_NOTE(PREVIOUS_STORAGE_SPECIFIER_HERE, "Previous storage specifier encountered here:", Underline<0>);

namespace PP
{
CREATE_NOTE(WHEN_CONCATENATING_N_AND_N, "When concatenating %0 and %2", Underline<0>, PointAt<1>, Underline<2>);
} // namespace PP
} // namespace Notes
} // namespace cld

#pragma clang diagnostic pop
#undef CREATE_WARNING
#undef CREATE_ERROR
#undef CREATE_NOTE
