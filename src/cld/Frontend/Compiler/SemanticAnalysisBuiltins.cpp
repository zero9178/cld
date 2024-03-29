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
    SizeT,
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
        else if (text.substr(0, 6) == "size_t")
        {
            CLD_ASSERT(!result);
            result = Types::SizeT;
            text.remove_prefix(6);
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

#define DEF_BUILTIN(value)                                                                                            \
    do                                                                                                                \
    {                                                                                                                 \
        if (name == value##Builtin.name)                                                                              \
        {                                                                                                             \
            std::vector<FunctionType::Parameter> parameters;                                                          \
            if constexpr (value##Builtin.size != 0)                                                                   \
            {                                                                                                         \
                for (auto& iter : value##Builtin.parameterTypes)                                                      \
                {                                                                                                     \
                    parameters.push_back({typeAlloc(*adjustParameterType(*typeEnumToType(iter))), ""});               \
                }                                                                                                     \
            }                                                                                                         \
            auto result = m_usedBuiltins                                                                              \
                              .insert({value##Builtin.name,                                                           \
                                       {FunctionType(typeAlloc(*typeEnumToType(value##Builtin.returnType)),           \
                                                     std::move(parameters), flag::isVARArg = value##Builtin.vararg),  \
                                        ::cld::Semantics::BuiltinFunction::Kind::value}})                             \
                              .first;                                                                                 \
            auto iter = m_scopes[0]                                                                                   \
                            .declarations.insert({value##Builtin.name, DeclarationInScope{nullptr, &result->second}}) \
                            .first;                                                                                   \
            return &iter->second.declared;                                                                            \
        }                                                                                                             \
    } while (0)

#define HANDLE_BUILTIN(a, b) DECL_BUILTIN(a, b);
#include "Builtins.def"

const cld::Semantics::DeclarationInScope::Variant* CLD_NULLABLE
    cld::Semantics::SemanticAnalysis::getBuiltinFuncDecl(std::string_view name)
{
    auto typeEnumToType = [&](Types types) -> IntrVarValue<Type>
    {
        const auto& options = getLanguageOptions();
        switch (types)
        {
            case Types::Void: return PrimitiveType(PrimitiveType::Void, options);
            case Types::Int: return PrimitiveType(PrimitiveType::Int, options);
            case Types::UnsignedInt: return PrimitiveType(PrimitiveType::UnsignedInt, options);
            case Types::Long: return PrimitiveType(PrimitiveType::Long, options);
            case Types::LongLong: return PrimitiveType(PrimitiveType::LongLong, options);
            case Types::Float: return PrimitiveType(PrimitiveType::Float, options);
            case Types::Double: return PrimitiveType(PrimitiveType::Double, options);
            case Types::LongDouble: return PrimitiveType(PrimitiveType::LongDouble, options);
            case Types::VAList: return getTypedef("__builtin_va_list")->type;
            case Types::SizeT: return PrimitiveType(options.sizeTType, options);
            case Types::VoidStar: return PointerType(typeAlloc<PrimitiveType>(PrimitiveType::Void, options));
            case Types::ConstVoidStar:
                return PointerType(typeAlloc<PrimitiveType>(PrimitiveType::Void, options, flag::isConst = true));
            case Types::Placeholder: return ErrorType();
            case Types::PlaceholderPointer: return PointerType(typeAlloc<ErrorType>());
            case Types::Bool: return PrimitiveType(PrimitiveType::Bool, options);
            case Types::ConstCharStar:
                return PointerType(typeAlloc<PrimitiveType>(PrimitiveType::Char, options, flag::isConst = true));
        }
        CLD_UNREACHABLE;
    };

#define HANDLE_BUILTIN(a, b) DEF_BUILTIN(b);
#define HANDLE_X86(signature, name, feature)                                                                 \
    if (getLanguageOptions().targetFeatures->hasFeature(TargetFeatures::IsX86))                              \
    {                                                                                                        \
        std::initializer_list<TargetFeatures::Features> featuresRequired = feature;                          \
        if (std::any_of(featuresRequired.begin(), featuresRequired.end(),                                    \
                        [&](auto value) { return getLanguageOptions().targetFeatures->hasFeature(value); })) \
        {                                                                                                    \
            DEF_BUILTIN(name);                                                                               \
        }                                                                                                    \
    }

#include "Builtins.def"
    return nullptr;
}

#undef DEF_BUILTIN
#undef DECL_BUILTIN

void cld::Semantics::SemanticAnalysis::createBuiltinTypes()
{
    const auto& options = getLanguageOptions();
    switch (options.vaListKind)
    {
        case LanguageOptions::BuiltInVaList::CharPtr:
        {
            auto type = PointerType(typeAlloc<PrimitiveType>(PrimitiveType::Char, options));
            insertTypedef({"__builtin_va_list", std::move(type), GLOBAL_SCOPE});
            break;
        }
        case LanguageOptions::BuiltInVaList::VoidPtr:
        {
            auto type = PointerType(typeAlloc<PrimitiveType>(PrimitiveType::Void, options));
            insertTypedef({"__builtin_va_list", std::move(type), GLOBAL_SCOPE});
            break;
        }
        case LanguageOptions::BuiltInVaList::x86_64ABI:
        {
            FieldMap fields;
            auto unsignedInt = typeAlloc<PrimitiveType>(PrimitiveType::UnsignedInt, options);
            fields.insert({"gp_offset", {unsignedInt, "gp_offset", nullptr, {0}, {}, {}}});
            fields.insert({"fp_offset", {unsignedInt, "fp_offset", nullptr, {1}, {}, {}}});
            auto voidStar = typeAlloc<PointerType>(typeAlloc<PrimitiveType>(PrimitiveType::Void, options));
            fields.insert({"overflow_arg_area", {voidStar, "overflow_arg_area", nullptr, {2}, {}, {}}});
            fields.insert({"reg_save_area", {voidStar, "reg_save_area", nullptr, {3}, {}, {}}});
            auto fieldLayout = std::vector<FieldInLayout>{
                {unsignedInt, 0, {}}, {unsignedInt, 1, {}}, {voidStar, 2, {}}, {voidStar, 3, {}}};
            auto memLayout =
                std::vector<MemoryLayout>{{unsignedInt, 0}, {unsignedInt, 4}, {voidStar, 8}, {voidStar, 16}};
            m_structDefinitions.push_back({m_structDefinitions.size(),
                                           StructDefinition("__va_list_tag", std::move(fields), std::move(fieldLayout),
                                                            std::move(memLayout), 24, 8),
                                           0, nullptr, "__va_list_tag"});
            IntrVarValue elementType = StructType(m_structDefinitions.back());
            getCurrentScope().types.emplace("__va_list_tag", TagTypeInScope{nullptr, &m_structDefinitions.back()});
            elementType = ArrayType(typeAlloc(std::move(*elementType)), 1);
            insertTypedef({"__builtin_va_list", std::move(elementType), GLOBAL_SCOPE});
            break;
        }
        default: CLD_UNREACHABLE;
    }
    if (options.int128Enabled)
    {
        auto type = PrimitiveType(PrimitiveType::Int128, options);
        insertTypedef({"__builtin_va_list", std::move(type), GLOBAL_SCOPE});
        type = PrimitiveType(PrimitiveType::UnsignedInt128, options);
        insertTypedef({"__builtin_va_list", std::move(type), GLOBAL_SCOPE});
    }
}
