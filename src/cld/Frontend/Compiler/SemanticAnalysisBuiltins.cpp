#include "SemanticAnalysis.hpp"

namespace
{
enum class Types
{
    Void,
    Bool,
    Int,
    UnsignedInt,
    Long,
    LongLong,
    Float,
    Double,
    LongDouble,
    VAList,
    VoidStar,
    ConstVoidStar,
    ConstCharStar,
    Placeholder,
    PlaceholderPointer
};

template <std::size_t n>
struct Builtin
{
    Types returnType;
    std::string_view name;
    std::array<Types, n> parameterTypes;
    bool vararg;
    cld::Semantics::BuiltinFunction::Kind kind;

    constexpr static std::size_t size = n;
};

template <>
struct Builtin<0>
{
    Types returnType;
    std::string_view name;
    bool vararg;
    cld::Semantics::BuiltinFunction::Kind kind;

    constexpr static std::size_t size = 0;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunneeded-internal-declaration"
    inline static std::array<Types, 0> parameterTypes;
#pragma clang diagnostic pop
};

constexpr void skipWhitespace(std::string_view& text)
{
    std::size_t i = 0;
    for (; i < text.size() && text[i] == ' '; i++)
        ;
    text.remove_prefix(i);
}

constexpr Types extractType(std::string_view& text)
{
    skipWhitespace(text);
    std::optional<Types> result = std::nullopt;
    do
    {
        if (text.substr(0, 5) == "void*")
        {
            CLD_ASSERT(!result);
            result = Types::VoidStar;
            text.remove_prefix(5);
        }
        else if (text.substr(0, 4) == "void")
        {
            CLD_ASSERT(!result);
            result = Types::Void;
            text.remove_prefix(4);
        }
        else if (text.substr(0, 4) == "bool")
        {
            CLD_ASSERT(!result);
            result = Types::Bool;
            text.remove_prefix(4);
        }
        else if (text.substr(0, 5) == "type*")
        {
            CLD_ASSERT(!result);
            result = Types::PlaceholderPointer;
            text.remove_prefix(5);
        }
        else if (text.substr(0, 4) == "type")
        {
            CLD_ASSERT(!result);
            result = Types::Placeholder;
            text.remove_prefix(4);
        }
        else if (text.substr(0, 7) == "va_list")
        {
            CLD_ASSERT(!result);
            result = Types::VAList;
            text.remove_prefix(7);
        }
        else if (text.substr(0, 11) == "const void*")
        {
            CLD_ASSERT(!result);
            result = Types::ConstVoidStar;
            text.remove_prefix(11);
        }
        else if (text.substr(0, 11) == "const char*")
        {
            CLD_ASSERT(!result);
            result = Types::ConstCharStar;
            text.remove_prefix(11);
        }
        else if (text.substr(0, 5) == "float")
        {
            CLD_ASSERT(!result);
            result = Types::Float;
            text.remove_prefix(5);
        }
        else if (text.substr(0, 3) == "int")
        {
            CLD_ASSERT(!result);
            result = Types::Int;
            text.remove_prefix(3);
        }
        else if (text.substr(0, 12) == "unsigned int")
        {
            CLD_ASSERT(!result);
            result = Types::UnsignedInt;
            text.remove_prefix(12);
        }
        else if (text.substr(0, 6) == "double")
        {
            if (!result)
            {
                result = Types::Double;
            }
            else
            {
                CLD_ASSERT(*result == Types::Long);
                result = Types::LongDouble;
            }
            text.remove_prefix(6);
        }
        else if (text.substr(0, 4) == "long")
        {
            if (!result)
            {
                result = Types::Long;
            }
            else
            {
                CLD_ASSERT(*result == Types::Long || *result == Types::Double);
                if (*result == Types::Long)
                {
                    result = Types::LongLong;
                }
                else
                {
                    result = Types::LongDouble;
                }
            }
            text.remove_prefix(4);
        }
        else
        {
            break;
        }
        skipWhitespace(text);
    } while (true);
    CLD_ASSERT(result);
    return *result;
}

constexpr std::size_t figureOutParameterCount(std::string_view text)
{
    std::size_t i = 0;
    for (; i < text.size() && text[i] != '('; i++)
        ;
    text.remove_prefix(i + 1);
    skipWhitespace(text);
    std::size_t count = 0;
    while (text[0] != ')')
    {
        i = 0;
        for (; i < text.size() && text[i] != ')' && text[i] != ','; i++)
            ;
        if (text.substr(0, i) != "...")
        {
            count++;
        }
        text.remove_prefix(i);
        skipWhitespace(text);
        if (text[0] == ',')
        {
            text.remove_prefix(1);
            skipWhitespace(text);
        }
    }
    return count;
}

template <auto& str>
constexpr auto createBuiltin(cld::Semantics::BuiltinFunction::Kind kind)
{
    std::string_view text = str.view();
    auto returnType = extractType(text);
    std::size_t i = 0;
    for (; i < text.size() && text[i] != '(' && text[i] != ' '; i++)
        ;
    auto name = text.substr(0, i);
    text.remove_prefix(name.size());
    skipWhitespace(text);
    CLD_ASSERT(text[0] == '(');
    text.remove_prefix(1);
    constexpr auto size = figureOutParameterCount(str.view());
    if constexpr (size == 0)
    {
        return Builtin<0>{returnType, name, false, kind};
    }
    else
    {
        std::array<Types, size> parameterTypes{};
        for (i = 0; i < size; i++)
        {
            skipWhitespace(text);
            parameterTypes[i] = extractType(text);
            skipWhitespace(text);
            text.remove_prefix(1);
        }
        bool vaArg = text.substr(0, 3) == "...";
        return Builtin<size>{returnType, name, parameterTypes, vaArg, kind};
    }
}

} // namespace

#define DECL_BUILTIN(string, value)                                            \
    constexpr auto value##Text = ::cld::Constexpr::basic_fixed_string{string}; \
    constexpr auto value##Builtin = createBuiltin<value##Text>(::cld::Semantics::BuiltinFunction::Kind::value)

#define DEF_BUILTIN(value)                                                                                             \
    do                                                                                                                 \
    {                                                                                                                  \
        if (name == value##Builtin.name)                                                                               \
        {                                                                                                              \
            std::vector<FunctionType::Parameter> parameters;                                                           \
            if constexpr (value##Builtin.size != 0)                                                                    \
            {                                                                                                          \
                for (auto& iter : value##Builtin.parameterTypes)                                                       \
                {                                                                                                      \
                    parameters.push_back({typeAlloc(*adjustParameterType(*typeEnumToType(iter))), ""});                \
                }                                                                                                      \
            }                                                                                                          \
            auto result = m_usedBuiltins                                                                               \
                              .insert({value##Builtin.name,                                                            \
                                       {typeAlloc<FunctionType>(typeAlloc(*typeEnumToType(value##Builtin.returnType)), \
                                                                std::move(parameters), value##Builtin.vararg, false),  \
                                        ::cld::Semantics::BuiltinFunction::Kind::value}})                              \
                              .first;                                                                                  \
            auto iter = m_scopes[0]                                                                                    \
                            .declarations.insert({value##Builtin.name, DeclarationInScope{nullptr, &result->second}})  \
                            .first;                                                                                    \
            return &iter->second.declared;                                                                             \
        }                                                                                                              \
    } while (0)

#define HANDLE_BUILTIN(a, b) DECL_BUILTIN(a, b)
#include "Builtins.def"

const cld::Semantics::ProgramInterface::DeclarationInScope::Variant* CLD_NULLABLE
    cld::Semantics::SemanticAnalysis::getBuiltinFuncDecl(std::string_view name)
{
    auto typeEnumToType = [&](Types types) -> cld::IntrVarValue<Type>
    {
        switch (types)
        {
            case Types::Void: return PrimitiveType::createVoid(false, false);
            case Types::Int: return PrimitiveType::createInt(false, false, m_sourceInterface.getLanguageOptions());
            case Types::UnsignedInt:
                return PrimitiveType::createUnsignedInt(false, false, m_sourceInterface.getLanguageOptions());
            case Types::Long: return PrimitiveType::createLong(false, false, m_sourceInterface.getLanguageOptions());
            case Types::LongLong:
                return PrimitiveType::createLongLong(false, false, m_sourceInterface.getLanguageOptions());
            case Types::Float: return PrimitiveType::createFloat(false, false);
            case Types::Double:
                return PrimitiveType::createDouble(false, false, m_sourceInterface.getLanguageOptions());
            case Types::LongDouble:
                return PrimitiveType::createLongDouble(false, false, m_sourceInterface.getLanguageOptions());
            case Types::VAList: return *getTypedef("__builtin_va_list");
            case Types::VoidStar:
                return PointerType(false, false, false, typeAlloc(PrimitiveType::createVoid(false, false)));
            case Types::ConstVoidStar:
                return PointerType(false, false, false, typeAlloc(PrimitiveType::createVoid(true, false)));
            case Types::Placeholder: return ErrorType();
            case Types::PlaceholderPointer: return PointerType(false, false, false, typeAlloc<ErrorType>());
            case Types::Bool: return PrimitiveType::createUnderlineBool(false, false);
            case Types::ConstCharStar:
                return PointerType(
                    false, false, false,
                    typeAlloc(PrimitiveType::createChar(true, false, m_sourceInterface.getLanguageOptions())));
        }
        CLD_UNREACHABLE;
    };

#define HANDLE_BUILTIN(a, b) DEF_BUILTIN(b)
#define HANDLE_X86(signature, name, feature)                                                      \
    if (m_sourceInterface.getLanguageOptions().targetFeatures->hasFeature(TargetFeatures::IsX86)) \
    {                                                                                             \
        std::initializer_list<TargetFeatures::Features> featuresRequired = feature;               \
        if (std::any_of(featuresRequired.begin(), featuresRequired.end(), [&](auto value) {       \
                return m_sourceInterface.getLanguageOptions().targetFeatures->hasFeature(value);  \
            }))                                                                                   \
        {                                                                                         \
            DEF_BUILTIN(name);                                                                    \
        }                                                                                         \
    }

#include "Builtins.def"
    return nullptr;
}

#undef DEF_BUILTIN
#undef DECL_BUILTIN

void cld::Semantics::SemanticAnalysis::createBuiltinTypes()
{
    switch (m_sourceInterface.getLanguageOptions().vaListKind)
    {
        case LanguageOptions::BuiltInVaList::CharPtr:
        {
            auto type =
                PointerType(false, false, false,
                            typeAlloc(PrimitiveType::createChar(false, false, m_sourceInterface.getLanguageOptions())));
            type.setName("__builtin_va_list");
            getCurrentScope().declarations.emplace("__builtin_va_list", DeclarationInScope{nullptr, std::move(type)});
            break;
        }
        case LanguageOptions::BuiltInVaList::VoidPtr:
        {
            auto type = PointerType(false, false, false, typeAlloc(PrimitiveType::createVoid(false, false)));
            type.setName("__builtin_va_list");
            getCurrentScope().declarations.emplace("__builtin_va_list", DeclarationInScope{nullptr, std::move(type)});
            break;
        }
        case LanguageOptions::BuiltInVaList::x86_64ABI:
        {
            FieldMap fields;
            auto unsignedInt =
                typeAlloc(PrimitiveType::createUnsignedInt(false, false, m_sourceInterface.getLanguageOptions()));
            fields.insert({"gp_offset", {unsignedInt, "gp_offset", nullptr, {0}, {}, {}}});
            fields.insert({"fp_offset", {unsignedInt, "fp_offset", nullptr, {1}, {}, {}}});
            auto voidStar =
                typeAlloc<PointerType>(false, false, false, typeAlloc(PrimitiveType::createVoid(false, false)));
            fields.insert({"overflow_arg_area", {voidStar, "overflow_arg_area", nullptr, {2}, {}, {}}});
            fields.insert({"reg_save_area", {voidStar, "reg_save_area", nullptr, {3}, {}, {}}});
            auto fieldLayout = std::vector<FieldInLayout>{
                {unsignedInt, 0, {}}, {unsignedInt, 1, {}}, {voidStar, 2, {}}, {voidStar, 3, {}}};
            auto memLayout =
                std::vector<MemoryLayout>{{unsignedInt, 0}, {unsignedInt, 4}, {voidStar, 8}, {voidStar, 16}};
            m_structDefinitions.push_back({m_structDefinitions.size(),
                                           StructDefinition("__va_list_tag", std::move(fields), std::move(fieldLayout),
                                                            std::move(memLayout), 24, 8),
                                           0, nullptr});
            IntrVarValue<Type> elementType = StructType(false, false, "__va_list_tag", m_structDefinitions.back());
            getCurrentScope().types.emplace("__va_list_tag", TagTypeInScope{nullptr, &m_structDefinitions.back()});
            elementType = ArrayType(false, false, false, false, typeAlloc(*elementType), 1);
            elementType->setName("__builtin_va_list");
            getCurrentScope().declarations.emplace("__builtin_va_list",
                                                   DeclarationInScope{nullptr, std::move(elementType)});
            break;
        }
        default: CLD_UNREACHABLE;
    }
    if (m_sourceInterface.getLanguageOptions().int128Enabled)
    {
        auto type = PrimitiveType::createInt128(false, false);
        type.setName("__int128_t");
        getCurrentScope().declarations.emplace("__int128_t", DeclarationInScope{nullptr, std::move(type)});
        type = PrimitiveType::createUnsignedInt128(false, false);
        type.setName("__uint128_t");
        getCurrentScope().declarations.emplace("__uint128_t", DeclarationInScope{nullptr, std::move(type)});
    }
}
