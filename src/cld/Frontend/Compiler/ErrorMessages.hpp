#pragma once

#include "Diagnostic.hpp"

#define CREATE_NOTE_2(variableName, format)                           \
    namespace detail                                                  \
    {                                                                 \
    constexpr auto variableName##Text = ::ctll::fixed_string{format}; \
    }                                                                 \
    constexpr auto variableName = ::cld::makeDiagnostic<detail::variableName##Text>(::cld::Severity::Note, "")

#define CREATE_NOTE_3(...) CREATE_NOTE_VAARG(__VA_ARGS__)
#define CREATE_NOTE_4(...) CREATE_NOTE_VAARG(__VA_ARGS__)
#define CREATE_NOTE_5(...) CREATE_NOTE_VAARG(__VA_ARGS__)
#define CREATE_NOTE_6(...) CREATE_NOTE_VAARG(__VA_ARGS__)
#define CREATE_NOTE_7(...) CREATE_NOTE_VAARG(__VA_ARGS__)
#define CREATE_NOTE_8(...) CREATE_NOTE_VAARG(__VA_ARGS__)

#define CREATE_NOTE(...) P99_PASTE2(CREATE_NOTE_, CLD_MACRO_COUNT_ARGUMENTS(__VA_ARGS__))(__VA_ARGS__)

#define CREATE_NOTE_VAARG(variableName, format, ...)                  \
    namespace detail                                                  \
    {                                                                 \
    constexpr auto variableName##Text = ::ctll::fixed_string{format}; \
    }                                                                 \
    constexpr auto variableName =                                     \
        ::cld::makeDiagnostic<detail::variableName##Text, __VA_ARGS__>(::cld::Severity::Note, "")

#define CREATE_WARNING_3(variableName, cliName, format)                                        \
    namespace detail                                                                           \
    {                                                                                          \
    constexpr auto variableName##Text = ::ctll::fixed_string{format};                          \
    inline auto variableName##Register = ::cld::detail::Diagnostic::WarningRegistrar(cliName); \
    }                                                                                          \
    constexpr auto variableName = ::cld::makeDiagnostic<detail::variableName##Text>(::cld::Severity::Warning, cliName)

#define CREATE_WARNING_4(...) CREATE_WARNING_VAARG(__VA_ARGS__)
#define CREATE_WARNING_5(...) CREATE_WARNING_VAARG(__VA_ARGS__)
#define CREATE_WARNING_6(...) CREATE_WARNING_VAARG(__VA_ARGS__)
#define CREATE_WARNING_7(...) CREATE_WARNING_VAARG(__VA_ARGS__)
#define CREATE_WARNING_8(...) CREATE_WARNING_VAARG(__VA_ARGS__)
#define CREATE_WARNING_9(...) CREATE_WARNING_VAARG(__VA_ARGS__)

#define CREATE_WARNING(...) P99_PASTE2(CREATE_WARNING_, CLD_MACRO_COUNT_ARGUMENTS(__VA_ARGS__))(__VA_ARGS__)

#define CREATE_WARNING_VAARG(variableName, cliName, format, ...)                               \
    namespace detail                                                                           \
    {                                                                                          \
    constexpr auto variableName##Text = ::ctll::fixed_string{format};                          \
    inline auto variableName##Register = ::cld::detail::Diagnostic::WarningRegistrar(cliName); \
    }                                                                                          \
    constexpr auto variableName =                                                              \
        ::cld::makeDiagnostic<detail::variableName##Text, __VA_ARGS__>(::cld::Severity::Warning, cliName)

#define CREATE_ERROR_2(variableName, format)                          \
    namespace detail                                                  \
    {                                                                 \
    constexpr auto variableName##Text = ::ctll::fixed_string{format}; \
    }                                                                 \
    constexpr auto variableName = ::cld::makeDiagnostic<detail::variableName##Text>(::cld::Severity::Error, "")

#define CREATE_ERROR_3(...) CREATE_ERROR_VAARG(__VA_ARGS__)
#define CREATE_ERROR_4(...) CREATE_ERROR_VAARG(__VA_ARGS__)
#define CREATE_ERROR_5(...) CREATE_ERROR_VAARG(__VA_ARGS__)
#define CREATE_ERROR_6(...) CREATE_ERROR_VAARG(__VA_ARGS__)
#define CREATE_ERROR_7(...) CREATE_ERROR_VAARG(__VA_ARGS__)
#define CREATE_ERROR_8(...) CREATE_ERROR_VAARG(__VA_ARGS__)

#define CREATE_ERROR(...) P99_PASTE2(CREATE_ERROR_, CLD_MACRO_COUNT_ARGUMENTS(__VA_ARGS__))(__VA_ARGS__)

#define CREATE_ERROR_VAARG(variableName, format, ...)                 \
    namespace detail                                                  \
    {                                                                 \
    constexpr auto variableName##Text = ::ctll::fixed_string{format}; \
    }                                                                 \
    constexpr auto variableName =                                     \
        ::cld::makeDiagnostic<detail::variableName##Text, __VA_ARGS__>(::cld::Severity::Error, "")

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

// GNU

CREATE_ERROR(EXPECTED_ATTRIBUTE_NAME_INSTEAD_OF_N, "Expected attribute name instead of %0", PointAt<0>);

CREATE_ERROR(EXPECTED_NORMAL_STRING_LITERAL_INSIDE_OF_ASM, "Expected normal string literal inside of 'asm'",
             Underline<0>);
} // namespace Parser

CREATE_ERROR(REDEFINITION_OF_SYMBOL_N, "Redefinition of symbol %0", Underline<0>);

namespace Semantics
{
// Type spcifiers and qualifiers

CREATE_ERROR(ONLY_ONE_STORAGE_SPECIFIER, "Only one storage specifier allowed in declaration", Underline<0>);

CREATE_ERROR(AT_LEAST_ONE_TYPE_SPECIFIER_REQUIRED, "At least one type specifier required", Underline<0>);

CREATE_ERROR(CANNOT_COMBINE_N_WITH_N, "Cannot combine %0 with %1", Underline<2>);

CREATE_ERROR(CANNOT_COMBINE_N_WITH_TYPENAME, "Cannot combine %0 with typename", Underline<1>);

CREATE_ERROR(EXPECTED_NO_FURTHER_TYPE_SPECIFIERS_AFTER_N, "Expected no further type specifiers after %0", Underline<1>);

CREATE_ERROR(EXPECTED_NO_FURTHER_TYPE_SPECIFIERS_AFTER_TYPENAME, "Expected no further type specifiers after typename",
             Underline<0>);

CREATE_ERROR(RESTRICT_CAN_ONLY_BE_APPLIED_TO_POINTERS, "'restrict' can only be applied to pointers", Underline<0>);

// Function definitions

CREATE_ERROR(EXPECTED_PARAMETER_LIST_IN_FUNCTION_DEFINITION, "Expected parameter list in function definition",
             Underline<0>);

CREATE_ERROR(DECLARATIONS_ONLY_ALLOWED_WITH_IDENTIFIER_LIST, "Declarations only allowed with identifier list",
             Underline<0>);

CREATE_ERROR(DEFINING_FUNCTIONS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR,
             "Defining functions with the name __func__ is undefined behaviour", PointAt<0>);

CREATE_ERROR(DECLARING_PARAMETERS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR,
             "Declaring parameters with the name __func__ is undefined behaviour", PointAt<0>);

CREATE_ERROR(INLINE_MAIN_IS_NOT_ALLOWED_IN_A_HOSTED_ENVIRONMENT, "Inline main is not allowed in a hosted environment",
             PointAt<0>);

CREATE_ERROR(INLINE_FUNCTION_N_WITH_EXTERNAL_LINKAGE_IS_NOT_ALLOWED_TO_CONTAIN_OR_ACCESS_THE_INTERNAL_IDENTIFIER_N,
             "Inline function %0 with external linkage is not allowed to contain or access the internal identifier %1",
             PointAt<1>);

CREATE_ERROR(NO_DEFINITION_FOR_INLINE_FUNCTION_N_FOUND, "No definition for inline function %0 found", PointAt<0>);

// Declarations

CREATE_ERROR(DECLARATION_DOES_NOT_DECLARE_ANYTHING, "Declaration does not declare anything", Underline<0>);

CREATE_ERROR(DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_AUTO, "Declarations at file scope cannot be 'auto'", Underline<0>);

CREATE_ERROR(DECLARATIONS_AT_FILE_SCOPE_CANNOT_BE_REGISTER, "Declarations at file scope cannot be 'register'",
             Underline<0>);

CREATE_ERROR(DECLARATION_MUST_HAVE_A_COMPLETE_TYPE, "Declaration must have a complete type", Annotate<0, 1>);

CREATE_ERROR(DECLARATION_MUST_NOT_BE_VOID, "Declaration must not be void", Underline<0>);

CREATE_ERROR(INLINE_ONLY_ALLOWED_FOR_FUNCTIONS, "'inline' only allowed for functions", PointAt<0>);

CREATE_ERROR(DECLARING_FUNCTIONS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR,
             "Declaring functions with the name __func__ is undefined behaviour", PointAt<0>);

CREATE_ERROR(DECLARING_VARIABLES_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR,
             "Declaring variables with the name __func__ is undefined behaviour", PointAt<0>);

CREATE_ERROR(DECLARING_TYPEDEFS_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR,
             "Declaring typedefs with the name __func__ is undefined behaviour", PointAt<0>);

CREATE_ERROR(TYPEDEF_DECLARATION_DOES_NOT_HAVE_A_NAME, "Typedef declaration does not have a name", Underline<0>);

CREATE_ERROR(STATIC_VARIABLE_N_REDEFINED_WITHOUT_STATIC, "Static variable %0 redefined without static", Underline<0>);

// Arrays

CREATE_ERROR(ARRAY_ELEMENT_TYPE_MUST_NOT_BE_A_FUNCTION, "Array element type must not be a function type",
             Annotate<0, 1>);

CREATE_ERROR(ARRAY_ELEMENT_TYPE_MUST_BE_A_COMPLETE_TYPE, "Array element type must be a complete type", Annotate<0, 1>);

CREATE_ERROR(ARRAY_ELEMENT_TYPE_MUST_NOT_CONTAIN_A_FLEXIBLE_ARRAY_MEMBER,
             "Array element type must not contain a flexible array member", Underline<0>);

CREATE_ERROR(ARRAY_SIZE_MUST_BE_AN_INTEGER_TYPE, "Array size must be an integer type", AnnotateExpr<0>);

CREATE_ERROR(ARRAY_SIZE_MUST_BE_GREATER_THAN_ZERO, "Array size must be greater than 0", Annotate<0, 1>);

CREATE_ERROR(ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_STATIC,
             "Array outside of function parameter may not be 'static'", PointAt<0>);

CREATE_ERROR(ARRAY_OUTSIDE_OF_FUNCTION_PARAMETER_MAY_NOT_BE_QUALIFIED,
             "Array outside of function parameter may not be qualified", PointAt<0>);

CREATE_ERROR(ONLY_PARAMETER_OF_ARRAY_TYPE_MAY_BE_QUALIFIED, "Only parameter of array type may be qualified",
             Annotate<0, 1>);

CREATE_ERROR(ONLY_PARAMETER_OF_ARRAY_TYPE_MAY_BE_STATIC, "Only parameter of array type may be static", Annotate<0, 1>);

// VLA

CREATE_ERROR(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_AT_FILE_SCOPE, "Variably modified type not allowed at file scope",
             Underline<0>);

CREATE_ERROR(VARIABLY_MODIFIED_TYPEDEF_NOT_ALLOWED_AT_FILE_SCOPE, "Variably modified typedef not allowed at file scope",
             Underline<0>);

CREATE_ERROR(VARIABLY_MODIFIED_TYPE_MUST_NOT_HAVE_ANY_LINKAGE, "Variably modified type must not have any linkage",
             PointAt<0>);

CREATE_ERROR(VARIABLE_LENGTH_ARRAY_MUST_NOT_HAVE_STATIC_LIFETIME, "Variable length array must not have static lifetime",
             PointAt<0>);

CREATE_ERROR(STAR_IN_ARRAY_DECLARATOR_ONLY_ALLOWED_IN_FUNCTION_PROTOTYPES,
             "Star in array declarator only allowed in function prototypes", PointAt<0>);

// Function type

CREATE_ERROR(FUNCTION_RETURN_TYPE_MUST_NOT_BE_A_FUNCTION, "Function return type must not be a function type",
             Annotate<0, 1>);

CREATE_ERROR(FUNCTION_RETURN_TYPE_MUST_NOT_BE_AN_ARRAY, "Function return type must not be an array type",
             Annotate<0, 1>);

CREATE_ERROR(FUNCTION_PROTOTYPE_MUST_NOT_HAVE_AN_INITIALIZER, "Function prototype must not have an initializer",
             Underline<0>);

CREATE_ERROR(FUNCTION_PROTOTYPE_AT_BLOCK_SCOPE_MAY_ONLY_BE_EXTERN,
             "Function prototype at block scope may only be extern", Underline<0>);

CREATE_ERROR(STATIC_ONLY_ALLOWED_IN_OUTERMOST_ARRAY, "'static' only allowed in outermost array", Underline<0>);

CREATE_ERROR(ARRAY_QUALIFIERS_ONLY_ALLOWED_IN_OUTERMOST_ARRAY, "Array qualifiers only allowed in outermost array",
             Underline<0>);

CREATE_ERROR(VOID_TYPE_NOT_ALLOWED_AS_FUNCTION_PARAMETER, "Void type not allowed as function parameter", Underline<0>);

// Struct & Union

CREATE_ERROR(INCOMPLETE_TYPE_NOT_ALLOWED_IN_STRUCT, "Incomplete type %full0 not allowed in struct", Underline<1>,
             Annotate<2, 0>);

CREATE_ERROR(VOID_TYPE_NOT_ALLOWED_IN_STRUCT, "Void type not allowed in struct", Underline<0>, Underline<1>);

CREATE_ERROR(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_IN_STRUCT, "Variably modified type not allowed in struct", Underline<0>,
             Underline<1>);

CREATE_ERROR(FUNCTION_TYPE_NOT_ALLOWED_IN_STRUCT, "Function type not allowed in struct", Underline<0>, Annotate<1, 2>);

CREATE_ERROR(INCOMPLETE_TYPE_NOT_ALLOWED_IN_UNION, "Incomplete type %full0 not allowed in union", Underline<1>,
             Annotate<2, 0>);

CREATE_ERROR(VARIABLY_MODIFIED_TYPE_NOT_ALLOWED_IN_UNION, "Variably modified type not allowed in union", Underline<0>,
             Underline<1>);

CREATE_ERROR(STRUCT_WITH_FLEXIBLE_ARRAY_MEMBER_NOT_ALLOWED_IN_STRUCT,
             "Struct with flexible array member not allowed in struct", Underline<0>);

CREATE_ERROR(UNION_WITH_STRUCT_OR_UNION_CONTAINING_A_FLEXIBLE_ARRAY_MEMBER_IS_NOT_ALLOWED_IN_STRUCT,
             "Union with struct or union containing a flexible array member is not allowed in struct", Underline<0>);

CREATE_ERROR(VOID_TYPE_NOT_ALLOWED_IN_UNION, "Incomplete type not allowed in union", Underline<0>, Underline<1>);

CREATE_ERROR(FUNCTION_TYPE_NOT_ALLOWED_IN_UNION, "Function type not allowed in union", Underline<0>, Annotate<1, 2>);

CREATE_ERROR(FIELD_WITHOUT_A_NAME_IS_NOT_ALLOWED, "Field without a name is not allowed", Underline<0>);

CREATE_ERROR(REDEFINITION_OF_FIELD_N, "Redefinition of field %0", Underline<0>);
// Bitfield

CREATE_ERROR(BITFIELD_MAY_ONLY_BE_OF_TYPE_INT_OR_BOOL, "Bit-field may only be of type (unsigned) int or _Bool",
             Underline<0>);

CREATE_ERROR(BITFIELD_MUST_BE_OF_SIZE_ZERO_OR_GREATER, "Bit-field must be of size 0 or greater", Annotate<0, 1>);

CREATE_ERROR(BITFIELD_MUST_NOT_HAVE_A_GREATER_WIDTH_THAN_THE_TYPE,
             "Bit-field must not have a greater width than the type", Annotate<0, 1>, Annotate<2, 3>);

CREATE_ERROR(BITFIELD_WITH_SIZE_ZERO_MAY_NOT_HAVE_A_NAME, "Bit-field with size 0 may not have a name", Underline<0>);

// Identifier list

CREATE_ERROR(IDENTIFIER_LIST_ONLY_ALLOWED_AS_PART_OF_A_FUNCTION_DEFINITION,
             "Identifier list only allowed as part of a function definition", Underline<0>);

CREATE_ERROR(DECLARATION_OF_IDENTIFIER_LIST_NOT_ALLOWED_TO_HAVE_AN_INITIALIZER,
             "Declaration of identifier list not allowed to have an initializer", Underline<0>);

CREATE_ERROR(DECLARATION_OF_IDENTIFIER_LIST_MUST_DECLARE_AT_LEAST_ONE_IDENTIFIER,
             "Declaration of identifier list must declare at least one identifier", Underline<0>);

CREATE_ERROR(DECLARATION_OF_IDENTIFIER_LIST_NOT_BELONGING_TO_ANY_PARAMETER,
             "Declaration of identifier list not belonging to any parameter", Underline<0>, Underline<1>);

CREATE_ERROR(PARAMETER_N_IN_IDENTIFIER_LIST_DOES_NOT_HAVE_A_MATCHING_DECLARATION,
             "Identifier %0 in identifier list does not have a matching declaration", Underline<0>);

CREATE_ERROR(NO_STORAGE_CLASS_SPECIFIER_ALLOWED_IN_PARAMETER_BESIDES_REGISTER,
             "No storage class specifier allowed in parameter besides 'register'", Underline<0>);

CREATE_ERROR(ELEMENT_TYPE_OF_POINTER_WITH_RESTRICT_QUALIFIER_MUST_NOT_BE_A_FUNCTION_TYPE,
             "Element type of pointer with restrict qualifier must not be a function type", Underline<0>, PointAt<1>);

CREATE_ERROR(UNDECLARED_IDENTIFIER_N, "Undeclared identifier %0", Underline<0>);

// Subscript operator

CREATE_ERROR(EXPECTED_ONE_OPERAND_TO_BE_OF_POINTER_TYPE, "Expected one operand to be of pointer type", AnnotateExpr<0>,
             AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_OTHER_OPERAND_TO_BE_OF_INTEGER_TYPE, "Expected other operand to be of integer type",
             AnnotateExpr<0>);

CREATE_ERROR(POINTER_TO_INCOMPLETE_TYPE_N_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR,
             "Pointer to incomplete type %full0 not allowed in subscript operator", AnnotateExpr<1>);

CREATE_ERROR(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_SUBSCRIPT_OPERATOR,
             "Pointer to function type not allowed subscript operator", AnnotateExpr<0>);

// Member access

CREATE_ERROR(EXPECTED_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_THE_DOT_OPERATOR,
             "Expected struct or union type on the left side of the '.' operator", AnnotateExpr<0>);

CREATE_ERROR(EXPECTED_POINTER_TO_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_THE_ARROW_OPERATOR,
             "Expected pointer to struct or union type on the left side of the '->' operator", AnnotateExpr<0>);

CREATE_ERROR(STRUCT_N_IS_AN_INCOMPLETE_TYPE, "Struct %0 is an incomplete type", Underline<1>);

CREATE_ERROR(UNION_N_IS_AN_INCOMPLETE_TYPE, "Union %0 is an incomplete type", Underline<1>);

CREATE_ERROR(NO_MEMBER_CALLED_N_FOUND_IN_STRUCT_N, "No member called %0 found in struct %1", Underline<0>);

CREATE_ERROR(NO_MEMBER_CALLED_N_FOUND_IN_UNION_N, "No member called %0 found in union %1", Underline<0>);

CREATE_ERROR(NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_STRUCT, "No member called %0 found in anonymous struct",
             Underline<0>);

CREATE_ERROR(NO_MEMBER_CALLED_N_FOUND_IN_ANONYMOUS_UNION, "No member called %0 found in anonymous union", Underline<0>);

// Address of

CREATE_ERROR(CANNOT_TAKE_ADDRESS_OF_TEMPORARY, "Cannot take address of temporary", PointAt<0>, Underline<1>);

CREATE_ERROR(CANNOT_TAKE_ADDRESS_OF_DECLARATION_ANNOTATED_WITH_REGISTER,
             "Cannot take address of declaration annotated with register", PointAt<0>, Underline<1>);

CREATE_ERROR(CANNOT_TAKE_ADDRESS_OF_BITFIELD, "Cannot take address of bit-field", PointAt<0>, Underline<1>);

// Dereference

CREATE_ERROR(CANNOT_DEREFERENCE_NON_POINTER_TYPE_N, "Cannot dereference non pointer type %fullType1", PointAt<0>,
             AnnotateExpr<1>);

// Expressions in general

CREATE_ERROR(OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE, "Operand of operator %0 must be an arithmetic type",
             PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "Operand of operator %0 must be an integer type",
             PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "Operand of operator %0 must be an arithmetic or pointer type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE,
             "Left operand of operator %0 must be an arithmetic type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE, "Left operand of operator %0 must be an integer type",
             PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(LEFT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "Left operand of operator %0 must be an arithmetic or pointer type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_LEFT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE,
             "Expected left operand of operator %0 to be an arithmetic type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_LEFT_OPERAND_OF_OPERATOR_N_TO_BE_AN_INTEGER_TYPE,
             "Expected left operand of operator %0 to be an integer type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_LEFT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "Expected left operand of operator %0 to be an arithmetic or pointer type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(LEFT_OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST,
             "Left operand of operator %0 must not be a temporary or const", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_TYPE,
             "Right operand of operator %0 must be an arithmetic type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_INTEGER_TYPE,
             "Right operand of operator %0 must be an integer type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "Right operand of operator %0 must be an arithmetic or pointer type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(RIGHT_OPERAND_OF_OPERATOR_N_MUST_BE_A_POINTER_TYPE, "Right operand of operator %0 must be a pointer type",
             PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_TYPE,
             "Expected right operand of operator %0 to be an arithmetic type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_INTEGER_TYPE,
             "Expected right operand of operator %0 to be an integer type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "Expected right operand of operator %0 to be an arithmetic or pointer type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_A_POINTER_TYPE,
             "Expected right operand of operator %0 to be a pointer type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL, "Expected right operand of operator %0 to be null",
             PointAt<0>, Annotate<1, 2>);

CREATE_ERROR(EXPECTED_RIGHT_OPERAND_OF_OPERATOR_N_TO_BE_NULL_2, "Expected right operand of operator %0 to be null",
             PointAt<0>, Underline<1>);

CREATE_ERROR(EXPECTED_OTHER_OPERAND_OF_OPERATOR_N_TO_BE_OF_INTEGER_TYPE,
             "Expected other operand of operator %0 to be of integer type", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(CANNOT_SUBTRACT_POINTER_FROM_ARITHMETIC_TYPE, "Cannot subtract pointer from arithmetic type",
             AnnotateExpr<0>, PointAt<1>, AnnotateExpr<2>);

CREATE_ERROR(CANNOT_SUBTRACT_POINTERS_OF_INCOMPATIBLE_TYPES, "Cannot subtract pointers of incompatible types",
             AnnotateExpr<0>, PointAt<1>, AnnotateExpr<2>);

CREATE_ERROR(CANNOT_COMPARE_POINTERS_OF_INCOMPATIBLE_TYPES, "Cannot compare pointers of incompatible types",
             AnnotateExpr<0>, PointAt<1>, AnnotateExpr<2>);

CREATE_ERROR(OPERAND_OF_OPERATOR_N_MUST_NOT_BE_A_TEMPORARY_OR_CONST,
             "Operand of operator %0 must not be a temporary or const", PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(OPERAND_OF_N_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "Operand of %0 must be an arithmetic or pointer types",
             PointAt<0>, AnnotateExpr<1>);

CREATE_ERROR(POINTER_TO_FUNCTION_TYPE_NOT_ALLOWED_IN_POINTER_ARITHMETIC,
             "Pointer to function type not allowed in pointer arithmetic", AnnotateExpr<0>);

CREATE_ERROR(TYPE_OF_VECTOR_OPERANDS_OF_BINARY_OPERATOR_N_MUST_MATCH,
             "Type of vector operands of binary operator %0 must match", PointAt<0>, AnnotateExpr<1>, AnnotateExpr<2>);

CREATE_ERROR(CONVERSION_OF_SCALAR_IN_VECTOR_OPERATION_COULD_CAUSE_TRUNCATION,
             "Conversion of scalar in vector operation could cause truncation", AnnotateExpr<0>, AnnotateExpr<1>);

// Conditional expression

CREATE_ERROR(FIRST_OPERAND_OF_CONDITIONAL_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "First operand of conditional expression must be an arithmetic or pointer type", AnnotateExpr<0>,
             PointAt<1>, PointAt<2>);

CREATE_ERROR(EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_AN_ARITHMETIC_TYPE,
             "Expected third operand of conditional expression to be an arithmetic type", AnnotateExpr<0>, PointAt<1>,
             PointAt<2>);

CREATE_ERROR(EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_NULL,
             "Expected third operand of conditional expression to be null", Annotate<0, 1>, PointAt<2>, PointAt<3>);

CREATE_ERROR(EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_NULL_2,
             "Expected third operand of conditional expression to be null", Underline<0>, PointAt<1>, PointAt<2>);

CREATE_ERROR(EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_VOID,
             "Expected third operand of conditional expression to be void", AnnotateExpr<0>, PointAt<1>, PointAt<2>);

CREATE_ERROR(EXPECTED_THIRD_OPERAND_OF_CONDITIONAL_EXPRESSION_TO_BE_A_POINTER_TYPE,
             "Expected third operand of conditional expression to be a pointer type", AnnotateExpr<0>, PointAt<1>,
             PointAt<2>);

CREATE_ERROR(POINTER_TYPES_IN_CONDITIONAL_EXPRESSION_MUST_BE_OF_COMPATIBLE_TYPES,
             "Pointer types in conditional expression must be of compatible types", PointAt<0>, AnnotateExpr<1>,
             PointAt<2>, AnnotateExpr<3>);

CREATE_ERROR(TYPES_IN_CONDITIONAL_EXPRESSION_MUST_BE_OF_COMPATIBLE_TYPES,
             "Types in conditional expression must be of compatible types", PointAt<0>, AnnotateExpr<1>, PointAt<2>,
             AnnotateExpr<3>);

// Simple assignment

CREATE_ERROR(CANNOT_ASSIGN_TO_ARRAY_TYPE_N, "Cannot assign to array type %fullType0", AnnotateExpr<0>, PointAt<1>);

CREATE_ERROR(CANNOT_ASSIGN_TO_INCOMPLETE_TYPE_N, "Cannot assign to incomplete type %fullType0", AnnotateExpr<0>,
             PointAt<1>);

CREATE_ERROR(CANNOT_ASSIGN_INCOMPATIBLE_TYPES, "Cannot assign incompatible types", AnnotateExpr<0>, PointAt<1>,
             AnnotateExpr<2>);

CREATE_ERROR(CANNOT_ASSIGN_VOID_POINTER_TO_FUNCTION_POINTER, "Cannot assign void pointer to function pointer",
             AnnotateExpr<0>, PointAt<1>, AnnotateExpr<2>);

CREATE_ERROR(CANNOT_ASSIGN_FUNCTION_POINTER_TO_VOID_POINTER, "Cannot assign function pointer to void pointer",
             AnnotateExpr<0>, PointAt<1>, AnnotateExpr<2>);

// Function calls

CREATE_ERROR(CANNOT_CALL_NON_FUNCTION_TYPE, "Cannot call non function type", AnnotateExpr<0>, PointAt<1>, PointAt<2>);

CREATE_ERROR(NOT_ENOUGH_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_N_GOT_N,
             "Not enough arguments for function call. Expected %0 got %1", AnnotateExpr<2>);

CREATE_ERROR(NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N,
             "Not enough arguments for calling function %0. Expected %1 got %2", Underline<0>);

CREATE_ERROR(NOT_ENOUGH_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_AT_LEAST_N_GOT_N,
             "Not enough arguments for function call. Expected at least %0 got %1", AnnotateExpr<2>);

CREATE_ERROR(NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_AT_LEAST_N_GOT_N,
             "Not enough arguments for calling function %0. Expected at least %1 got %2", Underline<0>);

CREATE_ERROR(TOO_MANY_ARGUMENTS_FOR_FUNCTION_CALL_EXPECTED_N_GOT_N,
             "Too many arguments for function call. Expected %0 got %1", AnnotateExpr<2>, Underline<3>);

CREATE_ERROR(TOO_MANY_ARGUMENTS_FOR_CALLING_FUNCTION_N_EXPECTED_N_GOT_N,
             "Too many arguments for calling function %0. Expected %1 got %2", Underline<0>, Underline<3>);

CREATE_ERROR(EXPECTED_ARGUMENT_N_TO_BE_AN_ARITHMETIC_TYPE, "Expected argument %0 to be an arithmetic type",
             AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_ARGUMENT_N_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "Expected argument %0  to be an arithmetic or pointer type", AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_ARGUMENT_N_TO_BE_A_POINTER_TYPE, "Expected argument %0 to be a pointer type", AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_ARGUMENT_N_TO_BE_NULL, "Expected argument %0 to be null", Annotate<1, 2>);

CREATE_ERROR(EXPECTED_ARGUMENT_N_TO_BE_NULL_2, "Expected argument %0 to be null", Underline<1>);

CREATE_ERROR(CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_N,
             "Cannot pass incompatible type to parameter %0 of type %full1", AnnotateExpr<2>);

CREATE_ERROR(CANNOT_PASS_ARGUMENT_TO_INCOMPLETE_TYPE_N_OF_PARAMETER_N,
             "Cannot pass argument to incomplete type %full0 of parameter %1", AnnotateExpr<2>);

CREATE_ERROR(CANNOT_PASS_VOID_POINTER_TO_FUNCTION_POINTER_PARAMETER,
             "Cannot pass void pointer to function pointer parameter", AnnotateExpr<0>);

CREATE_ERROR(CANNOT_PASS_FUNCTION_POINTER_TO_VOID_POINTER_PARAMETER,
             "Cannot assign function pointer to void pointer parameter", AnnotateExpr<0>);

CREATE_ERROR(BUILTIN_FUNCTION_MAY_ONLY_BE_CALLED_DIRECTLY, "Builtin function may only be called directly",
             Underline<0>);

// VA Arg

CREATE_ERROR(CANNOT_USE_VA_START_OUTSIDE_OF_A_FUNCTION, "Cannot use 'va_start' outside of a function", Underline<0>);

CREATE_ERROR(CANNOT_USE_VA_START_IN_A_FUNCTION_WITH_FIXED_ARGUMENT_COUNT,
             "Cannot use 'va_start' in a function with fixed argument count", Underline<0>);

CREATE_ERROR(NOT_ENOUGH_ARGUMENTS_FOR_CALLING_FUNCTION_VA_START_EXPECTED_N_GOT_N,
             "Not enough arguments for calling function 'va_start'. Expected %0 got %1", Underline<2>);

CREATE_ERROR(TOO_MANY_ARGUMENTS_FOR_CALLING_FUNCTION_VA_START_EXPECTED_N_GOT_N,
             "Too many arguments for calling function 'va_start'. Expected %0 got %1", Underline<2>, Underline<3>);

CREATE_ERROR(INCOMPLETE_TYPE_N_IN_VA_ARG, "Incomplete type %full0 in 'va_arg'", Underline<1>);

CREATE_ERROR(CANNOT_PASS_INCOMPATIBLE_TYPE_TO_PARAMETER_N_OF_TYPE_VA_LIST,
             "Cannot pass incompatible type to parameter %0 of type 'va_list'", AnnotateExpr<1>);

// Offset of

CREATE_ERROR(TYPE_N_IN_OFFSETOF_MUST_BE_A_STRUCT_OR_UNION_TYPE, "Type %0 in 'offsetof' must be a struct or union type",
             Annotate<1, 0>);

CREATE_ERROR(EXPECTED_STRUCT_OR_UNION_ON_THE_LEFT_SIDE_OF_THE_DOT_OPERATOR_2,
             "Expected struct or union type on the left side of the '.' operator", Annotate<0, 1>);

CREATE_ERROR(EXPECTED_ARRAY_TYPE_ON_THE_LEFT_SIDE_OF_THE_SUBSCRIPT_OPERATOR,
             "Expected array type on the left side of the subscript operator", Annotate<0, 1>);

CREATE_ERROR(BITFIELD_NOT_ALLOWED_IN_OFFSET_OF, "Bitfield not allowed in 'offsetof'", Underline<0>);

// __builtin_prefetch

CREATE_ERROR(EXPECTED_INTEGER_CONSTANT_EXPRESSION_AS_SECOND_ARGUMENT_TO_BUILTIN_PREFETCH,
             "Expected integer constant expression as second argument to __builtin_prefetch", Underline<0>);

CREATE_ERROR(EXPECTED_INTEGER_CONSTANT_EXPRESSION_AS_THIRD_ARGUMENT_TO_BUILTIN_PREFETCH,
             "Expected integer constant expression as third argument to __builtin_prefetch", Underline<0>);

CREATE_ERROR(EXPECTED_A_VALUE_OF_0_OR_1_AS_SECOND_ARGUMENT_TO_BUILTIN_PREFETCH,
             "Expected a value of 0 or 1 as second argument to __builtin_prefetch", Annotate<0, 1>);

CREATE_ERROR(EXPECTED_A_VALUE_OF_0_TO_3_AS_THIRD_ARGUMENT_TO_BUILTIN_PREFETCH,
             "Expected a value of 0 to 3 as third argument to __builtin_prefetch", Annotate<0, 1>);

// __builtin_expect_with_probability

CREATE_ERROR(EXPECTED_ARITHMETIC_CONSTANT_EXPRESSION_AS_THIRD_ARGUMENT_TO_BUILTIN_EXPECT_WITH_PROBABILITY,
             "Expected arithmetic constant expression as third argument to __builtin_expect_with_probability",
             Underline<0>);

CREATE_ERROR(EXPECTED_A_VALUE_OF_0_TO_1_AS_THIRD_ARGUMENT_TO_BUILTIN_EXPECT_WITH_PROBABILITY,
             "Expected a value of 0 to 1 as third argument to __builtin_expect_with_probability", Annotate<0, 1>);

// __sync_*

CREATE_ERROR(EXPECTED_POINTER_TYPE_AS_FIRST_ARGUMENT_TO_N, "Expected pointer type as first argument to %0",
             Underline<0>, AnnotateExpr<1>);

CREATE_ERROR(POINTER_ELEMENT_TYPE_IN_N_MAY_NOT_BE_CONST_QUALIFIED,
             "Pointer element type in %0 may not be const qualified", Underline<0>, AnnotateExpr<1>);

CREATE_ERROR(POINTER_ELEMENT_TYPE_IN_N_MUST_BE_AN_INTEGER_OR_POINTER_TYPe,
             "Pointer element type in %0 must be an integer or pointer type", Underline<0>, AnnotateExpr<1>);

CREATE_ERROR(POINTER_ELEMENT_TYPE_IN_N_MUST_NOT_BE_BOOL, "Pointer element type in %0 must not be bool", Underline<0>,
             AnnotateExpr<1>);

CREATE_ERROR(POINTER_ELEMENT_TYPE_IN_N_MUST_NOT_HAVE_A_SIZE_GREATER_THAN_8,
             "Pointer element type in %0 must not have a size greater than 8", Underline<0>, AnnotateExpr<1>);

// Size of

CREATE_ERROR(INCOMPLETE_TYPE_N_IN_SIZE_OF, "Incomplete type %full0 in 'sizeof'", Underline<1>);

CREATE_ERROR(FUNCTION_TYPE_NOT_ALLOWED_IN_SIZE_OF, "Function type not allowed in 'sizeof'", Annotate<0, 1>);

CREATE_ERROR(BITFIELD_NOT_ALLOWED_IN_SIZE_OF, "Bitfield not allowed in 'sizeof'", Underline<0>);

// Cast expression

CREATE_ERROR(TYPE_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE, "Type in cast must be an arithmetic or pointer type",
             Annotate<0, 1>);

CREATE_ERROR(EXPRESSION_IN_CAST_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "Expression in cast must be an arithmetic or pointer type", AnnotateExpr<0>);

CREATE_ERROR(CANNOT_CAST_NON_INTEGER_AND_POINTER_TYPE_N_TO_POINTER_TYPE,
             "Cannot cast non integer and pointer type %fullType0 to pointer type", AnnotateExpr<0>);

CREATE_ERROR(CANNOT_CAST_POINTER_TYPE_TO_NON_INTEGER_AND_POINTER_TYPE,
             "Cannot cast pointer type to non integer and pointer type", Underline<0>, AnnotateExpr<1>);

CREATE_ERROR(INCOMPLETE_TYPE_N_USED_IN_POINTER_ARITHMETIC, "Incomplete type %full0 used in pointer arithmetic",
             Annotate<1, 2>);

CREATE_ERROR(CANNOT_CAST_TO_VECTOR_TYPE_FROM_TYPE_OF_DIFFERING_SIZE,
             "Cannot cast to vector type from type of differing size", Annotate<0, 1>, Annotate<2, 3>);

CREATE_ERROR(CANNOT_CAST_FROM_VECTOR_TYPE_TO_TYPE_OF_DIFFERING_SIZE,
             "Cannot cast from vector type to type of differing size", Annotate<0, 1>, Annotate<2, 3>);

// Initializer

CREATE_ERROR(EXPECTED_INITIALIZER_TO_BE_AN_ARITHMETIC_TYPE, "Expected initializer to be an arithmetic type",
             AnnotateExpr<0>);

CREATE_ERROR(EXPECTED_INITIALIZER_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "Expected initializer to be an arithmetic or pointer type", AnnotateExpr<0>);

CREATE_ERROR(EXPECTED_INITIALIZER_TO_BE_A_POINTER_TYPE, "Expected initializer to be a pointer type", AnnotateExpr<0>);

CREATE_ERROR(EXPECTED_INITIALIZER_TO_BE_NULL, "Expected initializer to be null", Annotate<0, 1>);

CREATE_ERROR(EXPECTED_INITIALIZER_TO_BE_NULL_2, "Expected initializer to be null", Underline<0>);

CREATE_ERROR(CANNOT_INITIALIZE_VARIABLE_OF_TYPE_N_WITH_INCOMPATIBLE_TYPE_N,
             "Cannot initialize variable of type %full0 with incompatible type %fullType1", AnnotateExpr<1>);

CREATE_ERROR(CANNOT_INITIALIZE_VOID_POINTER_WITH_FUNCTION_POINTER,
             "Cannot initialize void pointer with function pointer", AnnotateExpr<0>);

CREATE_ERROR(CANNOT_INITIALIZE_FUNCTION_POINTER_WITH_VOID_POINTER_PARAMETER,
             "Cannot initialize function pointer with void pointer", AnnotateExpr<0>);

CREATE_ERROR(ARRAY_MUST_BE_INITIALIZED_WITH_INITIALIZER_LIST, "Array must be initialized with initializer list",
             Underline<0>);

CREATE_ERROR(ARRAY_MUST_BE_INITIALIZED_WITH_STRING_OR_INITIALIZER_LIST,
             "Array must be initialized with string or initializer list", Underline<0>);

CREATE_ERROR(ARRAY_MUST_BE_INITIALIZED_WITH_WIDE_STRING_OR_INITIALIZER_LIST,
             "Array must be initialized with wide string or initializer list", Underline<0>);

CREATE_ERROR(CANNOT_INITIALIZE_CHAR_ARRAY_WITH_WIDE_STRING_LITERAL,
             "Cannot initialize char array with wide string literal", Underline<0>);

CREATE_ERROR(CANNOT_INITIALIZE_WCHART_ARRAY_WITH_STRING_LITERAL, "Cannot initialize wchar_t array with string literal",
             Underline<0>);

CREATE_ERROR(CANNOT_INITIALIZE_ARITHMETIC_OR_POINTER_TYPE_WITH_INITIALIZER_LIST,
             "Cannot initialize arithmetic or pointer type with initializer list", Underline<0>);

CREATE_ERROR(CANNOT_INITIALIZE_FUNCTION_TYPE, "Cannot initialize function type", Annotate<0, 1>);

CREATE_ERROR(CANNOT_INITIALIZE_VARIABLE_LENGTH_ARRAY_TYPE, "Cannot initialize variable length array type",
             Annotate<0, 1>);

CREATE_ERROR(CANNOT_INITIALIZE_FLEXIBLE_ARRAY_MEMBER, "Cannot initialize flexible array member", Underline<0>);

CREATE_ERROR(CANNOT_INITIALIZE_STATIC_OR_EXTERN_VARIABLE_AT_BLOCK_SCOPE,
             "Cannot initialize static or extern variable at block scope", Underline<0>);

CREATE_ERROR(NO_MORE_SUB_OBJECTS_TO_INITIALIZE, "No more sub objects to initialize", Underline<0>);

CREATE_ERROR(EXPECTED_INDEX_DESIGNATOR_FOR_ARRAY_TYPE, "Expected index designator for array type", Underline<0>);

CREATE_ERROR(EXPECTED_MEMBER_DESIGNATOR_FOR_STRUCT_TYPE, "Expected member designator for struct type", Underline<0>);

CREATE_ERROR(EXPECTED_MEMBER_DESIGNATOR_FOR_UNION_TYPE, "Expected member designator for union type", Underline<0>);

CREATE_ERROR(DESIGNATOR_INDEX_MUST_NOT_BE_NEGATIVE, "Designator index must not be negative", Annotate<0, 1>);

CREATE_ERROR(DESIGNATOR_INDEX_OUT_OF_RANGE_FOR_ARRAY_TYPE_N, "Designator index out of range for array type %full0",
             Annotate<1, 2>);

CREATE_ERROR(CANNOT_INDEX_INTO_NON_ARRAY_TYPE_N, "Cannot index into non array type %full0", Underline<1>);

CREATE_ERROR(CANNOT_ACCESS_MEMBERS_OF_NON_STRUCT_OR_UNION_TYPE_N,
             "Cannot access members of non struct or union type %full0", Underline<1>);

// Enum

CREATE_ERROR(FORWARD_DECLARING_AN_ENUM_IS_NOT_ALLOWED, "Forward declaring an enum is not allowed", Underline<0>);

CREATE_ERROR(VALUE_OF_ENUMERATION_CONSTANT_MUST_FIT_IN_TYPE_INT, "Value of enumeration constant must fit in type 'int'",
             Underline<0>, Annotate<1, 2>);

// Function definition

CREATE_ERROR(ONLY_STATIC_OR_EXTERN_ALLOWED_IN_FUNCTION_DEFINITION,
             "Only 'static' or 'extern' are allowed in function definition", Underline<0>);

CREATE_ERROR(FUNCTION_DEFINITION_MUST_HAVE_FUNCTION_TYPE, "Function definition must have function type",
             Annotate<0, 1>);

CREATE_ERROR(RETURN_TYPE_OF_FUNCTION_DEFINITION_MUST_BE_A_COMPLETE_TYPE,
             "Return type of function definition must be a complete type", Underline<0>);

CREATE_ERROR(FUNCTION_DEFINITION_MUST_HAVE_A_PARAMETER_LIST, "Function definition must have a parameter list",
             Underline<0>);

CREATE_ERROR(FUNCTION_DEFINITION_WITH_A_PARAMETER_LIST_MUST_NOT_HAVE_DECLARATIONS_FOLLOWING_IT,
             "Function definition with a parameter list must not have declarations following it", Underline<0>);

// Constant evaluator

CREATE_ERROR(SIZEOF_VAL_MODIFIED_TYPE_CANNOT_BE_DETERMINED_IN_CONSTANT_EXPRESSION,
             "Size of variably modified type cannot be determined in constant expression", Underline<0>);

CREATE_ERROR(N_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "%tokenType0 not allowed in constant expression", PointAt<0>);

CREATE_ERROR(STRING_LITERALS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "String literals not allowed in constant expression",
             Underline<0>);

CREATE_ERROR(VARIABLE_ACCESS_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "Variable access not allowed in constant expression",
             Underline<0>);

CREATE_ERROR(FUNCTION_CALL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "Function call not allowed in constant expression",
             Underline<0>);

CREATE_ERROR(COMPOUND_LITERAL_NOT_ALLOWED_IN_CONSTANT_EXPRESSION, "Initializer not allowed in constant expression",
             Underline<0>);

CREATE_ERROR(INTEGER_DIVISION_BY_ZERO_NOT_ALLOWED_IN_CONSTANT_EXPRESSION,
             "Integer division by zero not allowed in constant expression", PointAt<0>, Annotate<1, 2>);

CREATE_ERROR(ONLY_INTEGERS_ALLOWED_IN_INTEGER_CONSTANT_EXPRESSIONS,
             "Only integers allowed in integer constant expressions", Underline<0>);

CREATE_ERROR(CANNOT_CAST_TO_NON_INTEGER_TYPE_IN_INTEGER_CONSTANT_EXPRESSION,
             "Cannot cast to non integer type in integer constant expression", Underline<0>);

CREATE_ERROR(CANNOT_CAST_TO_NON_ARITHMETIC_TYPE_IN_ARITHMETIC_CONSTANT_EXPRESSION,
             "Cannot cast to non arithmetic type in arithmetic constant expression", Underline<0>);

CREATE_ERROR(INTEGER_MUST_EVALUATE_TO_NULL_TO_BE_COMPARABLE_WITH_POINTER,
             "Integer must evaluate to null to be comparable with pointer", Underline<0>);

// Return statement

CREATE_ERROR(CANNOT_RETURN_NO_VALUE_FROM_FUNCTION_N_WITH_RETURN_TYPE_N,
             "Cannot return no value from function %0 with return type %full1", Underline<2>);

CREATE_ERROR(CANNOT_RETURN_VALUE_FROM_FUNCTION_N_WITH_VOID_RETURN_TYPE,
             "Cannot return value from function %0 with void return type", Underline<1>);

CREATE_ERROR(EXPECTED_RETURN_VALUE_TO_BE_AN_ARITHMETIC_TYPE, "Expected return value to be an arithmetic type",
             Underline<0>, AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_RETURN_VALUE_TO_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "Expected return value to be an arithmetic or pointer type", Underline<0>, AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_RETURN_VALUE_TO_BE_A_POINTER_TYPE, "Expected return value to be a pointer type", Underline<0>,
             AnnotateExpr<1>);

CREATE_ERROR(EXPECTED_RETURN_VALUE_TO_BE_NULL, "Expected return value to be null", Underline<0>, Annotate<1, 2>);

CREATE_ERROR(EXPECTED_RETURN_VALUE_TO_BE_NULL_2, "Expected return value to be null", Underline<0>, Underline<1>);

CREATE_ERROR(CANNOT_RETURN_VARIABLE_OF_TYPE_N_TO_INCOMPATIBLE_RETURN_TYPE_N,
             "Cannot return variable of type %fullType0 to incompatible return type %full1", AnnotateExpr<0>,
             Underline<2>);

CREATE_ERROR(CANNOT_RETURN_VOID_POINTER_WITH_FUNCTION_POINTER_RETURN_TYPE,
             "Cannot return void pointer with function pointer return type", Underline<0>, AnnotateExpr<1>);

CREATE_ERROR(CANNOT_RETURN_FUNCTION_POINTER_WITH_VOID_POINTER_RETURN_TYPE,
             "Cannot return function pointer with void pointer return type", Underline<0>, AnnotateExpr<1>);

// Statements in general

CREATE_ERROR(CONTROLLING_EXPRESSION_MUST_BE_AN_ARITHMETIC_OR_POINTER_TYPE,
             "Controlling expression must be an arithmetic or pointer type", AnnotateExpr<0>);

// For Statement

CREATE_ERROR(ONLY_AUTO_OR_REGISTER_ALLOWED_IN_FOR_STATEMENTS_DECLARATION,
             "Only 'auto' or 'register' allowed in for-statements declaration", Underline<0>);

// Labels

CREATE_ERROR(DECLARING_LABEL_WITH_THE_NAME_FUNC_IS_UNDEFINED_BEHAVIOUR,
             "Declaring label with the name __func__ is undefined behaviour", PointAt<0>);

CREATE_ERROR(REDEFINITION_OF_LABEL_N, "Redefinition of label %0", Underline<0>);

// Goto

CREATE_ERROR(NO_LABEL_CALLED_N_FOUND_IN_FUNCTION_N, "No label called %0 found in function %1", Underline<0>);

CREATE_ERROR(GOTO_TO_LABEL_N_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N,
             "Goto to label %0 skips initialization of variably modified variable %1", Underline<0>);

CREATE_ERROR(GOTO_TO_LABEL_N_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_TYPEDEF_N,
             "Goto to label %0 skips initialization of variably modified typedef %1", Underline<0>);

// Switch

CREATE_ERROR(CONTROLLING_EXPRESSION_MUST_BE_AN_INTEGER_TYPE, "Controlling expression must be an integer type",
             AnnotateExpr<0>);

// Case

CREATE_ERROR(CASE_MUST_BE_WITHIN_A_SWITCH_STATEMENT, "Case must be within a switch statement", Underline<0>);

CREATE_ERROR(REDEFINITION_OF_CASE_WITH_VALUE_N, "Redefinition of case with value %0", Annotate<1, 0>);

CREATE_ERROR(CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N,
             "Case of switch skips initialization of variably modified variable %0", Underline<1>);

CREATE_ERROR(CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_TYPEDEF_N,
             "Case of switch skips initialization of variably modified typedef %0", Underline<1>);

// Default

CREATE_ERROR(DEFAULT_MUST_BE_WITHIN_A_SWITCH_STATEMENT, "Default must be within a switch statement", Underline<0>);

CREATE_ERROR(REDEFINITION_OF_DEFAULT, "Redefinition of default", Underline<0>);

CREATE_ERROR(DEFAULT_CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_VARIABLE_N,
             "Default case of switch skips initialization of variably modified variable %0", Underline<1>);

CREATE_ERROR(DEFAULT_CASE_OF_SWITCH_SKIPS_INITIALIZATION_OF_VARIABLY_MODIFIED_TYPEDEF_N,
             "Default case of switch skips initialization of variably modified typedef %0", Underline<1>);

// Break

CREATE_ERROR(BREAK_MUST_BE_WITHIN_A_SWITCH_OR_LOOP_STATEMENT, "Break must be within a switch or loop statement",
             Underline<0>);

// Continue

CREATE_ERROR(CONTINUE_MUST_BE_WITHIN_A_LOOP_STATEMENT, "Continue must be within a loop statement", Underline<0>);

// __attribute__

CREATE_ERROR(INVALID_NUMBER_OF_ARGUMENTS_FOR_ATTRIBUTE_N_EXPECTED_N_GOT_N,
             "Invalid number of arguments for attribute %0. Expected %1 got %2", Underline<0>);

// __attribute__((vector_size(n)))

CREATE_ERROR(EXPECTED_INTEGER_CONSTANT_EXPRESSION_AS_ARGUMENT_TO_VECTOR_SIZE,
             "Expected integer constant expression as argument to 'vector_size'", Underline<0>);

CREATE_ERROR(ARGUMENT_TO_VECTOR_SIZE_MUST_BE_A_POSITIVE_NUMBER, "Argument to 'vector_size' must be a positive number",
             Annotate<0, 1>);

CREATE_ERROR(VECTOR_SIZE_CAN_ONLY_BE_APPLIED_TO_VARIABLES_OF_ARITHMETIC_TYPES,
             "'vector_size' can only be applied to variables of arithmetic types", Annotate<0, 1>);

CREATE_ERROR(VECTOR_SIZE_CAN_NOT_BE_APPLIED_TO_LONG_DOUBLE, "'vector_size' can not be applied to long double",
             Annotate<0, 1>);

CREATE_ERROR(VECTOR_SIZE_CAN_ONLY_BE_APPLIED_TO_ARITHMETIC_TYPES,
             "'vector_size' can only be applied to arithmetic types", Annotate<0, 1>);

CREATE_ERROR(ARGUMENT_OF_VECTOR_SIZE_MUST_BE_A_MULTIPLE_OF_THE_SIZE_OF_THE_BASE_TYPE,
             "Argument of 'vector_size' must be a multiple of the size of the base type", Annotate<0, 1>);

CREATE_ERROR(ARGUMENT_OF_VECTOR_SIZE_SHOULD_BE_A_POWER_OF_2_MULTIPLE_OF_THE_SIZE_OF_THE_BASE_TYPE,
             "Argument of 'vector_size' must be a power of 2 multiple of the size of the base type", Annotate<0, 1>);

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

CREATE_ERROR(EXPECTED_AN_EXPRESSION_AFTER_IF, "Expected an expression after #if", Underline<0>);

CREATE_ERROR(EXPECTED_AN_EXPRESSION_AFTER_ELIF, "Expected an expression after #elif", InsertAfter<0>);

CREATE_ERROR(EXPECTED_AN_EXPRESSION_AFTER_ELIF_2, "Expected an expression after #elif", Underline<0>);

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

namespace CLI
{
CREATE_ERROR(FAILED_TO_OPEN_C_SOURCE_FILE_N, "Failed to open C source file '%0'");

CREATE_ERROR(FAILED_TO_OPEN_FILE_N_FOR_OUTPUT, "Failed to open file '%0' for output");

CREATE_ERROR(UNKNOWN_LANGUAGE_STANDARD_N, "Unknown language standard '%0'");

CREATE_ERROR(NO_SOURCE_FILES_SPECIFIED, "No source files specified");

CREATE_ERROR(CANNOT_COMPILE_TO_OBJECT_FILE_AND_ASSEMBLY_AT_THE_SAME_TIME,
             "Cannot compile to object file and assembly at the same time");

CREATE_ERROR(CANNOT_COMPILE_TO_OBJECT_FILE_AND_PREPROCESS_AT_THE_SAME_TIME,
             "Cannot compile to object file and preprocess at the same time");

CREATE_ERROR(CANNOT_COMPILE_TO_ASSEMBLY_AND_PREPROCESS_AT_THE_SAME_TIME,
             "Cannot compile to assembly and preprocess at the same time");

CREATE_ERROR(EXPECTED_WHITESPACE_AFTER_N, "Expected whitespace after '%0'");

CREATE_ERROR(EXPECTED_N_AFTER_N, "Expected '%0' after '%1'");

CREATE_ERROR(EXPECTED_ARGUMENT_AFTER_N, "Expected argument after '%0'");

CREATE_ERROR(EXPECTED_ARGUMENT_IMMEDIATELY_AFTER_N, "Expected argument immediately after '%0'");

CREATE_ERROR(ERRORS_PARSING_INTEGER_ARGUMENT_IN_N, "Errors parsing integer argument in '%0'");

CREATE_ERROR(ERRORS_PARSING_INVALID_UTF8_IN_N, "Errors parsing invalid utf8 in '%0'");
} // namespace CLI
} // namespace Errors

namespace Warnings
{
namespace Semantics
{
CREATE_WARNING(VALUE_OF_N_IS_TO_LARGE_FOR_INTEGER_TYPE_N, "overflow",
               "Value of %0 is too large for integer type %full1", Underline<2>);

CREATE_WARNING(SECOND_ARGUMENT_OF_VA_START_SHOULD_BE_THE_LAST_PARAMETER, "varargs",
               "Second argument of 'va_start' should be the last parameter", Underline<0>);

CREATE_WARNING(UNUSED_VARIABLE_N, "unused-variable", "Unused variable %0", PointAt<0>);

CREATE_WARNING(UNUSED_FUNCTION_N, "unused-function", "Unused function %0", PointAt<0>);

// __attribute__

CREATE_WARNING(ATTRIBUTE_N_DOES_NOT_APPLY_TO_TYPES, "ignored-attributes", "Attribute %0 does not apply to types",
               Underline<0>, Underline<1>);

CREATE_WARNING(ATTRIBUTE_N_DOES_NOT_APPLY_TO_VARIABLES, "ignored-attributes",
               "Attribute %0 does not apply to variables", Underline<0>, Underline<1>);

CREATE_WARNING(ATTRIBUTE_N_DOES_NOT_APPLY_TO_FUNCTIONS, "ignored-attributes",
               "Attribute %0 does not apply to functions", Underline<0>, Underline<1>);

CREATE_WARNING(UNKNOWN_ATTRIBUTE_N_IGNORED, "unknown-attributes", "Unknown attribute %0 ignored", Underline<0>);

// __attribute__((used))

CREATE_WARNING(ATTRIBUTE_USED_ONLY_APPLIES_TO_FUNCTIONS_WITH_INTERNAL_LINKAGE, "ignored-attributes",
               "Attribute 'used' only applies to functions with internal linkage", Underline<0>, Underline<1>);

CREATE_WARNING(ATTRIBUTE_USED_ONLY_APPLIES_TO_GLOBAL_VARIABLES_WITH_INTERNAL_LINKAGE, "ignored-attributes",
               "Attribute 'used' only applies to global variables with internal linkage", Underline<0>, Underline<1>);

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

namespace Semantics
{
CREATE_NOTE(PREVIOUS_STORAGE_SPECIFIER_HERE, "Previous storage specifier encountered here:", Underline<0>);

CREATE_NOTE(VARIABLY_MODIFIED_VARIABLE_N_WITH_TYPE_N_HERE,
            "Variably modified variable %0 with type %full1 here:", Annotate<0, 1>);

CREATE_NOTE(VARIABLY_MODIFIED_TYPEDEF_N_HERE, "Variably modified typedef %0 here:", Underline<0>);

CREATE_NOTE(LABEL_N_HERE, "Label %0 here:", Underline<0>);

CREATE_NOTE(PREVIOUS_CASE_HERE, "Previous case here:", Annotate<0, 1>);

CREATE_NOTE(PREVIOUS_DEFAULT_HERE, "Previous default here:", Underline<0>);

} // namespace Semantics

namespace PP
{
CREATE_NOTE(WHEN_CONCATENATING_N_AND_N, "When concatenating %0 and %2", Underline<0>, PointAt<1>, Underline<2>);
} // namespace PP
} // namespace Notes
} // namespace cld

#undef CREATE_WARNING
#undef CREATE_WARNING_3
#undef CREATE_WARNING_4
#undef CREATE_WARNING_5
#undef CREATE_WARNING_6
#undef CREATE_WARNING_7
#undef CREATE_WARNING_8
#undef CREATE_WARNING_9
#undef CREATE_WARNING_VAARG

#undef CREATE_ERROR
#undef CREATE_ERROR_2
#undef CREATE_ERROR_3
#undef CREATE_ERROR_4
#undef CREATE_ERROR_5
#undef CREATE_ERROR_6
#undef CREATE_ERROR_7
#undef CREATE_ERROR_8
#undef CREATE_ERROR_VAARG

#undef CREATE_NOTE
#undef CREATE_NOTE_2
#undef CREATE_NOTE_3
#undef CREATE_NOTE_4
#undef CREATE_NOTE_5
#undef CREATE_NOTE_6
#undef CREATE_NOTE_7
#undef CREATE_NOTE_8
#undef CREATE_NOTE_VAARG
