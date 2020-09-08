#include "Codegen.hpp"

#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include <cld/Common/Filesystem.hpp>
#include <cld/Frontend/Compiler/Program.hpp>

namespace
{
class CodeGenerator final
{
    llvm::Module& m_module;
    const cld::Semantics::ProgramInterface& m_programInterface;
    const cld::SourceInterface& m_sourceInterface;
    cld::Triple m_triple;
    // void* for now although strictly speaking the pointers can only be const Declaration* or const FunctionDefinition*
    // but hashing the variant seems overkill? I am not sure
    std::unordered_map<const void*, llvm::Value * CLD_NON_NULL> m_lvalues;

    using TypeVariantKey = std::variant<cld::Semantics::StructType, cld::Semantics::UnionType,
                                        cld::Semantics::AnonymousUnionType, cld::Semantics::AnonymousStructType>;

    struct TypeHasher
    {
        std::size_t operator()(const TypeVariantKey& variant) const noexcept
        {
            return cld::rawHashCombine(std::hash<std::size_t>{}(variant.index()),
                                       cld::match(
                                           variant,
                                           [](const cld::Semantics::StructType& structType) -> std::size_t {
                                               return cld::hashCombine(structType.getScopeOrId(), structType.getName());
                                           },
                                           [](const cld::Semantics::UnionType& unionType) -> std::size_t {
                                               return cld::hashCombine(unionType.getScopeOrId(), unionType.getName());
                                           },
                                           [](const cld::Semantics::AnonymousStructType& structType) -> std::size_t {
                                               return std::hash<std::uint64_t>{}(structType.getId());
                                           },
                                           [](const cld::Semantics::AnonymousUnionType& unionType) -> std::size_t {
                                               return std::hash<std::uint64_t>{}(unionType.getId());
                                           }));
        }
    };

    struct TypeEqual
    {
        const cld::Semantics::ProgramInterface& programInterface;

        bool operator()(const TypeVariantKey& lhs, const TypeVariantKey& rhs) const noexcept
        {
            if (lhs.index() != rhs.index())
            {
                return false;
            }
            return cld::match(lhs, [this, &rhs](const auto& value) -> bool {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<cld::Semantics::StructType, T>)
                {
                    auto& other = cld::get<T>(rhs);
                    return programInterface.getStructDefinition(value.getName(), value.getScopeOrId())
                           == programInterface.getStructDefinition(other.getName(), other.getScopeOrId());
                }
                else if constexpr (std::is_same_v<cld::Semantics::UnionType, T>)
                {
                    auto& other = cld::get<T>(rhs);
                    return programInterface.getUnionDefinition(value.getName(), value.getScopeOrId())
                           == programInterface.getUnionDefinition(other.getName(), other.getScopeOrId());
                }
                else
                {
                    return value == cld::get<T>(rhs);
                }
            });
        }
    };

    std::unordered_map<TypeVariantKey, llvm::Type*, TypeHasher, TypeEqual> m_types{0, {}, {m_programInterface}};

    struct ABITransformations
    {
        enum Change
        {
            Unchanged,
            IntegerRegister,
            PointerToTemporary,
            OnStack,
            Flattened
        };
        struct MultipleArgs
        {
            std::size_t size;
        };
        using Variant = std::variant<Change, MultipleArgs>;
        Change returnType;              // Never OnStack
        std::vector<Variant> arguments; // Never Flattened
    };

    std::unordered_map<const llvm::FunctionType*, ABITransformations> m_functionABITransformations;
    std::unordered_map<cld::Semantics::LoopStatements, llvm::BasicBlock*> m_continueTargets;
    std::unordered_map<cld::Semantics::BreakableStatements, llvm::BasicBlock*> m_breakTargets;
    std::unordered_map<const cld::Semantics::LabelStatement*, llvm::BasicBlock*> m_labels;
    struct Switch
    {
        std::unordered_map<const cld::Semantics::CaseStatement*, llvm::BasicBlock*> cases;
        llvm::BasicBlock* defaultBlock;
    };
    std::unordered_map<const cld::Semantics::SwitchStatement*, Switch> m_switches;

    llvm::IRBuilder<> m_builder{m_module.getContext()};
    llvm::Value* m_returnSlot = nullptr;
    llvm::DIBuilder m_debugInfo{m_module};

    llvm::Value* toBool(llvm::Value* value)
    {
        if (value->getType()->isIntegerTy())
        {
            return m_builder.CreateICmpNE(value, llvm::ConstantInt::get(value->getType(), 0));
        }
        else if (value->getType()->isPointerTy())
        {
            return m_builder.CreateICmpNE(
                value, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(value->getType())));
        }
        else
        {
            return m_builder.CreateFCmpUNE(value, llvm::ConstantFP::get(value->getType(), 0));
        }
    }

    std::vector<llvm::Type*> flatten(llvm::Type* type)
    {
        std::vector<llvm::Type*> result;
        if (type->isStructTy())
        {
            for (std::size_t i = 0; i < type->getStructNumElements(); i++)
            {
                auto* element = type->getStructElementType(i);
                auto temp = flatten(element);
                result.insert(result.end(), temp.begin(), temp.end());
            }
        }
        else if (type->isArrayTy())
        {
            auto temp = flatten(type->getArrayElementType());
            for (std::size_t i = 0; i < type->getArrayNumElements(); i++)
            {
                result.insert(result.end(), temp.begin(), temp.end());
            }
        }
        else
        {
            result.emplace_back(type);
        }
        return result;
    }

    std::tuple<ABITransformations::Variant, llvm::Type * CLD_NON_NULL, llvm::Type * CLD_NULLABLE>
        flattenSingleArg(llvm::Type* type, std::uint8_t& takenIntegers, std::uint8_t& takenFloats)
    {
        constexpr std::uint8_t availableIntegerRegisters = 6;
        constexpr std::uint8_t availableFloatingPointRegisters = 8;
        ABITransformations::Variant dest;
        std::size_t retIndex = 0;
        std::array<llvm::Type*, 2> ret = {};

        std::uint8_t takenIntegerRegisters = takenIntegers;
        std::uint8_t takenFloatingPointRegisters = takenFloats;
        const auto flat = flatten(type);
        auto iter = flat.begin();
        while (iter != flat.end())
        {
            const auto begin = iter;
            bool encounteredInteger = false;
            std::size_t size = 0;
            std::size_t currentAlignment = 0;
            while (size < 8 && iter != flat.end())
            {
                const auto alignment = m_module.getDataLayout().getABITypeAlign(*iter).value();
                const auto rest = size % alignment;
                if (rest != 0)
                {
                    if (size + alignment - rest >= 8)
                    {
                        break;
                    }
                    size += alignment - rest;
                }
                if ((*iter)->isIntOrPtrTy())
                {
                    encounteredInteger = true;
                }
                currentAlignment = std::max(currentAlignment, alignment);
                const auto typeSize = m_module.getDataLayout().getTypeAllocSize(*iter).getKnownMinSize();
                size += typeSize;
                iter++;
            }
            const auto rest = size % currentAlignment;
            if (rest != 0)
            {
                size += currentAlignment - rest;
            }
            if (encounteredInteger)
            {
                // We encountered at least one integer therefore even if a floating point type was in there
                // it's gotta go into a integer register
                if (takenIntegerRegisters >= availableIntegerRegisters)
                {
                    return {type->isStructTy() ? ABITransformations::OnStack : ABITransformations::Unchanged, type,
                            nullptr};
                }
                else
                {
                    takenIntegerRegisters++;
                    if (type->isStructTy() && !std::holds_alternative<ABITransformations::MultipleArgs>(dest))
                    {
                        dest = ABITransformations::MultipleArgs{};
                    }
                    if (type->isStructTy())
                    {
                        ret[retIndex++] = m_builder.getIntNTy(size * 8);
                    }
                    else
                    {
                        ret[retIndex++] = type;
                    }
                    if (std::holds_alternative<ABITransformations::MultipleArgs>(dest))
                    {
                        cld::get<ABITransformations::MultipleArgs>(dest).size++;
                    }
                }
                continue;
            }
            else if (std::distance(begin, iter) == 2 && (*begin)->isFloatTy() && ((*(begin + 1))->isFloatTy()))
            {
                // Two floats can be packed as a single 64 bit value into a xmm register. This is represented as
                // a vector in LLVM IR
                if (takenFloatingPointRegisters >= availableFloatingPointRegisters)
                {
                    return {type->isStructTy() ? ABITransformations::OnStack : ABITransformations::Unchanged, type,
                            nullptr};
                }
                takenFloatingPointRegisters++;
                if (!std::holds_alternative<ABITransformations::MultipleArgs>(dest))
                {
                    dest = ABITransformations::MultipleArgs{};
                }
                ret[retIndex++] = llvm::FixedVectorType::get(m_builder.getFloatTy(), 2);
                cld::get<ABITransformations::MultipleArgs>(dest).size++;
                continue;
            }

            CLD_ASSERT(std::distance(begin, iter) == 1);
            // Must be a floating point type because if it were integer it would have taken the
            // encounteredInteger branch above
            if (takenFloatingPointRegisters >= availableFloatingPointRegisters)
            {
                return {type->isStructTy() ? ABITransformations::OnStack : ABITransformations::Unchanged, type,
                        nullptr};
            }
            takenFloatingPointRegisters++;
            if (type->isStructTy() && !std::holds_alternative<ABITransformations::MultipleArgs>(dest))
            {
                dest = ABITransformations::MultipleArgs{};
            }
            ret[retIndex++] = *begin;
            if (std::holds_alternative<ABITransformations::MultipleArgs>(dest))
            {
                cld::get<ABITransformations::MultipleArgs>(dest).size++;
            }
        }
        takenFloats = takenFloatingPointRegisters;
        takenIntegers = takenIntegerRegisters;
        return {dest, ret[0], ret[1]};
    }

    ABITransformations applyPlatformABI(llvm::Type*& returnType, std::vector<llvm::Type*>& arguments)
    {
        ABITransformations transformations;
        transformations.returnType = ABITransformations::Unchanged;
        transformations.arguments.resize(arguments.size(), ABITransformations::Unchanged);
        if (m_triple.getPlatform() == cld::Platform::Windows && m_triple.getArchitecture() == cld::Architecture::x86_64)
        {
            for (auto iter = arguments.begin(); iter != arguments.end(); iter++)
            {
                if (!(*iter)->isStructTy() && !(*iter)->isX86_FP80Ty())
                {
                    continue;
                }
                std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(*iter);
                if (m_module.getDataLayout().isLegalInteger(size))
                {
                    transformations.arguments[iter - arguments.begin()] = ABITransformations::IntegerRegister;
                    *iter = m_builder.getIntNTy(size);
                }
                else
                {
                    transformations.arguments[iter - arguments.begin()] = ABITransformations::PointerToTemporary;
                    *iter = llvm::PointerType::getUnqual(*iter);
                }
            }
            if (returnType->isVoidTy())
            {
                return transformations;
            }
            std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(returnType);
            if (m_module.getDataLayout().isLegalInteger(size) && returnType->isStructTy())
            {
                transformations.returnType = ABITransformations::IntegerRegister;
                returnType = m_builder.getIntNTy(size);
            }
            else if (!m_module.getDataLayout().isLegalInteger(size))
            {
                transformations.returnType = ABITransformations::PointerToTemporary;
                arguments.insert(arguments.begin(), llvm::PointerType::getUnqual(returnType));
                returnType = m_builder.getVoidTy();
            }
        }
        else if (m_triple.getArchitecture() == cld::Architecture::x86_64)
        {
            std::uint8_t takenIntegerRegisters = 0;
            std::uint8_t takenFloatingPointRegisters = 0;
            std::size_t transFormIndex = 0;
            for (auto arg = arguments.begin(); arg != arguments.end(); transFormIndex++, arg++)
            {
                auto& dest = transformations.arguments[transFormIndex];
                if (m_module.getDataLayout().getTypeAllocSizeInBits(*arg) > 128)
                {
                    dest = ABITransformations::OnStack;
                    *arg = llvm::PointerType::getUnqual(*arg);
                    continue;
                }
                std::pair<llvm::Type*, llvm::Type*> types;
                std::tie(dest, types.first, types.second) =
                    flattenSingleArg(*arg, takenIntegerRegisters, takenFloatingPointRegisters);
                if (std::holds_alternative<ABITransformations::Change>(dest)
                    && cld::get<ABITransformations::Change>(dest) == ABITransformations::OnStack)
                {
                    *arg = llvm::PointerType::getUnqual(*arg);
                }
                else if (std::holds_alternative<ABITransformations::MultipleArgs>(dest))
                {
                    *arg = types.first;
                    if (types.second)
                    {
                        arg++;
                        arg = arguments.insert(arg, types.second);
                    }
                }
            }
            takenIntegerRegisters = 0;
            takenFloatingPointRegisters = 0;
            if (!returnType->isVoidTy())
            {
                std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(returnType);
                if (size > 128)
                {
                    transformations.returnType = ABITransformations::PointerToTemporary;
                    arguments.insert(arguments.begin(), llvm::PointerType::getUnqual(returnType));
                    returnType = m_builder.getVoidTy();
                }
                else
                {
                    auto* prevReturnType = returnType;
                    auto [temp, firstType, secondType] =
                        flattenSingleArg(returnType, takenIntegerRegisters, takenFloatingPointRegisters);
                    if (secondType)
                    {
                        returnType = llvm::StructType::get(firstType, secondType);
                    }
                    else
                    {
                        returnType = firstType;
                    }
                    if (prevReturnType->isStructTy())
                    {
                        transformations.returnType = ABITransformations::Flattened;
                    }
                }
            }
        }
        return transformations;
    }

    template <class T>
    void applyFunctionAttributes(T& attributeApply, llvm::FunctionType* CLD_NON_NULL functionType,
                                 const cld::Semantics::FunctionType& ft,
                                 const std::vector<std::unique_ptr<cld::Semantics::Declaration>>* paramDecls = nullptr)
    {
        auto transformations = m_functionABITransformations.find(functionType);
        CLD_ASSERT(transformations != m_functionABITransformations.end());
        std::size_t argStart = 0;
        if (transformations->second.returnType == ABITransformations::PointerToTemporary)
        {
            attributeApply.addAttribute(1, llvm::Attribute::StructRet);
            attributeApply.addAttribute(1, llvm::Attribute::NoAlias);
            argStart = 1;
        }
        else if (transformations->second.returnType == ABITransformations::Unchanged
                 && cld::Semantics::isInteger(ft.getReturnType())
                 && cld::get<cld::Semantics::PrimitiveType>(ft.getReturnType().get()).isSigned())
        {
            attributeApply.addAttribute(0, llvm::Attribute::SExt);
        }
        std::size_t origArgI = 0;
        for (std::size_t i = argStart; i < functionType->getNumParams(); origArgI++)
        {
            auto& argument = transformations->second.arguments[origArgI];
            if (std::holds_alternative<ABITransformations::Change>(argument))
            {
                auto change = cld::get<ABITransformations::Change>(argument);
                if (change == ABITransformations::Unchanged)
                {
                    auto& arg = ft.getArguments()[origArgI].first;
                    if (cld::Semantics::isInteger(arg) && cld::get<cld::Semantics::PrimitiveType>(arg.get()).isSigned())
                    {
                        attributeApply.addParamAttr(i, llvm::Attribute::SExt);
                    }
                }
                else if (change == ABITransformations::OnStack)
                {
                    attributeApply.addParamAttr(
                        i, llvm::Attribute::getWithByValType(m_builder.getContext(),
                                                             functionType->getParamType(i)->getPointerElementType()));
                    attributeApply.addParamAttr(
                        i,
                        llvm::Attribute::getWithAlignment(
                            m_builder.getContext(), llvm::Align(m_module.getDataLayout().getPointerABIAlignment(0))));
                    i++;
                    continue;
                }
                if (change == ABITransformations::PointerToTemporary || !paramDecls)
                {
                    i++;
                    continue;
                }
                auto& paramDecl = (*paramDecls)[origArgI];
                auto* operand = functionType->getParamType(i);
                i++;
                if (change == ABITransformations::Unchanged)
                {
                    auto* var = m_builder.CreateAlloca(operand);
                    var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
                    m_lvalues.emplace(paramDecl.get(), var);
                    continue;
                }
                auto* var = m_builder.CreateAlloca(visit(paramDecl->getType()));
                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
                m_lvalues.emplace(paramDecl.get(), var);
            }
            else if (std::holds_alternative<ABITransformations::MultipleArgs>(argument))
            {
                auto& multiArgs = cld::get<ABITransformations::MultipleArgs>(argument);
                i += multiArgs.size;
                if (!paramDecls)
                {
                    continue;
                }
                auto& paramDecl = (*paramDecls)[origArgI];
                auto* var = m_builder.CreateAlloca(visit(paramDecl->getType()));
                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
                m_lvalues.emplace(paramDecl.get(), var);
            }
        }
    }

public:
    explicit CodeGenerator(llvm::Module& module, const cld::Semantics::ProgramInterface& programInterface,
                           const cld::SourceInterface& sourceInterface, cld::Triple triple)
        : m_module(module), m_programInterface(programInterface), m_sourceInterface(sourceInterface), m_triple(triple)
    {
        auto fullPath = cld::fs::u8path(m_sourceInterface.getFiles()[1].path);
        module.setSourceFileName(fullPath.filename().u8string());
        llvm::Triple llvmTriple;
        switch (triple.getArchitecture())
        {
            case cld::Architecture::x86: llvmTriple.setArch(llvm::Triple::ArchType::x86); break;
            case cld::Architecture::x86_64: llvmTriple.setArch(llvm::Triple::ArchType::x86_64); break;
            case cld::Architecture::Unknown: llvmTriple.setArch(llvm::Triple::ArchType::UnknownArch); break;
        }
        switch (triple.getPlatform())
        {
            case cld::Platform::Windows: llvmTriple.setOS(llvm::Triple::OSType::Win32); break;
            case cld::Platform::Linux: llvmTriple.setOS(llvm::Triple::OSType::Linux); break;
            case cld::Platform::Unknown: llvmTriple.setOS(llvm::Triple::OSType::UnknownOS); break;
        }
        switch (triple.getEnvironment())
        {
            case cld::Environment::GNU: llvmTriple.setEnvironment(llvm::Triple::GNU); break;
            case cld::Environment::MSVC: llvmTriple.setEnvironment(llvm::Triple::MSVC); break;
            case cld::Environment::Unknown: llvmTriple.setEnvironment(llvm::Triple::UnknownEnvironment); break;
        }
        module.setTargetTriple(llvmTriple.normalize());
        std::string error;
        auto* targetM = llvm::TargetRegistry::lookupTarget(module.getTargetTriple(), error);
        if (!targetM)
        {
            llvm::errs() << "Target lookup failed with error: " << error;
            return;
        }
        auto machine = std::unique_ptr<llvm::TargetMachine>(
            targetM->createTargetMachine(module.getTargetTriple(), "generic", "", {}, {}));
        module.setDataLayout(machine->createDataLayout());
    }

    llvm::Type* visit(const cld::Semantics::Type& type)
    {
        return cld::match(
            type.get(),
            [&](const cld::Semantics::PrimitiveType& primitiveType) -> llvm::Type* {
                switch (primitiveType.getKind())
                {
                    case cld::Semantics::PrimitiveType::Char:
                    case cld::Semantics::PrimitiveType::SignedChar:
                    case cld::Semantics::PrimitiveType::UnsignedChar: return m_builder.getInt8Ty();
                    case cld::Semantics::PrimitiveType::Bool:
                        return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfUnderlineBool * 8);
                    case cld::Semantics::PrimitiveType::UnsignedShort:
                    case cld::Semantics::PrimitiveType::Short:
                        return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfShort * 8);
                    case cld::Semantics::PrimitiveType::Int:
                    case cld::Semantics::PrimitiveType::UnsignedInt:
                        return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfInt * 8);
                    case cld::Semantics::PrimitiveType::Long:
                    case cld::Semantics::PrimitiveType::UnsignedLong:
                        return m_builder.getIntNTy(m_sourceInterface.getLanguageOptions().sizeOfLong * 8);
                    case cld::Semantics::PrimitiveType::UnsignedLongLong:
                    case cld::Semantics::PrimitiveType::LongLong: return m_builder.getInt64Ty();
                    case cld::Semantics::PrimitiveType::Float: return m_builder.getFloatTy();
                    case cld::Semantics::PrimitiveType::Double: return m_builder.getDoubleTy();
                    case cld::Semantics::PrimitiveType::LongDouble:
                        switch (m_sourceInterface.getLanguageOptions().sizeOfLongDoubleBits)
                        {
                            case 64: return m_builder.getDoubleTy();
                            case 80: return llvm::Type::getX86_FP80Ty(m_module.getContext());
                            case 128: return llvm::Type::getFP128Ty(m_module.getContext());
                        }
                        CLD_UNREACHABLE;
                    case cld::Semantics::PrimitiveType::Void: return m_builder.getVoidTy();
                }
                CLD_UNREACHABLE;
            },
            [&](const cld::Semantics::ArrayType& arrayType) -> llvm::Type* {
                auto* elementType = visit(arrayType.getType());
                return llvm::ArrayType::get(elementType, arrayType.getSize());
            },
            [&](const cld::Semantics::FunctionType& functionType) -> llvm::Type* {
                auto* returnType = visit(functionType.getReturnType());
                std::vector<llvm::Type*> args;
                for (auto& [type, name] : functionType.getArguments())
                {
                    (void)name;
                    args.push_back(visit(cld::Semantics::adjustParameterType(type)));
                }
                auto transformation = applyPlatformABI(returnType, args);
                auto* ft = llvm::FunctionType::get(returnType, args, functionType.isLastVararg());
                m_functionABITransformations.emplace(ft, transformation);
                return ft;
            },
            [&](const cld::Semantics::PointerType& pointerType) -> llvm::Type* {
                if (cld::Semantics::isVoid(pointerType.getElementType()))
                {
                    return m_builder.getInt8PtrTy();
                }
                auto* elementType = visit(pointerType.getElementType());
                return llvm::PointerType::getUnqual(elementType);
            },
            [&](const cld::Semantics::StructType& structType) -> llvm::Type* {
                auto result = m_types.find(structType);
                if (result != m_types.end())
                {
                    return result->second;
                }
                auto* structDef =
                    m_programInterface.getStructDefinition(structType.getName(), structType.getScopeOrId());
                if (!structDef)
                {
                    auto* type = llvm::StructType::create(m_module.getContext(), structType.getName().data());
                    m_types.insert({structType, type});
                    return type;
                }
                std::vector<llvm::Type*> fields;
                for (auto& iter : structDef->getLayout())
                {
                    fields.push_back(visit(iter));
                }
                auto* type = llvm::StructType::create(m_module.getContext(), fields, structType.getName().data());
                m_types.insert({structType, type});
                return type;
            },
            [&](const cld::Semantics::UnionType& unionType) -> llvm::Type* {
                auto result = m_types.find(unionType);
                if (result != m_types.end())
                {
                    return result->second;
                }
                auto* unionDef = m_programInterface.getUnionDefinition(unionType.getName(), unionType.getScopeOrId());
                if (!unionDef)
                {
                    auto* type = llvm::StructType::get(m_module.getContext());
                    m_types.insert({unionType, type});
                    return type;
                }
                auto largestField = std::max_element(
                    unionDef->getFields().begin(), unionDef->getFields().end(),
                    [&](const cld::Semantics::Field& lhs, const cld::Semantics::Field& rhs) {
                        return lhs.type->getSizeOf(m_programInterface) < rhs.type->getSizeOf(m_programInterface);
                    });
                auto* type = llvm::StructType::get(m_module.getContext(), llvm::ArrayRef(visit(*largestField->type)));
                m_types.insert({unionType, type});
                return type;
            },
            [&](const cld::Semantics::AnonymousStructType& structType) -> llvm::Type* {
                auto result = m_types.find(structType);
                if (result != m_types.end())
                {
                    return result->second;
                }
                std::vector<llvm::Type*> fields;
                for (auto& iter : structType.getLayout())
                {
                    fields.push_back(visit(iter));
                }
                auto* type = llvm::StructType::get(m_module.getContext(), fields);
                m_types.insert({structType, type});
                return type;
            },
            [&](const cld::Semantics::AnonymousUnionType& unionType) -> llvm::Type* {
                auto result = m_types.find(unionType);
                if (result != m_types.end())
                {
                    return result->second;
                }
                auto largestField = std::max_element(
                    unionType.getFields().begin(), unionType.getFields().end(),
                    [&](const cld::Semantics::Field& lhs, const cld::Semantics::Field& rhs) {
                        return lhs.type->getSizeOf(m_programInterface) < rhs.type->getSizeOf(m_programInterface);
                    });
                auto* type = llvm::StructType::get(m_module.getContext(), llvm::ArrayRef(visit(*largestField->type)));
                m_types.insert({unionType, type});
                return type;
            },
            [&](const cld::Semantics::AbstractArrayType&) -> llvm::Type* { CLD_UNREACHABLE; },
            [&](const std::monostate&) -> llvm::Type* { CLD_UNREACHABLE; },
            [&](const cld::Semantics::EnumType& enumType) -> llvm::Type* {
                auto* enumDef = m_programInterface.getEnumDefinition(enumType.getName(), enumType.getScopeOrId());
                CLD_ASSERT(enumDef);
                return visit(enumDef->getType());
            },
            [&](const cld::Semantics::AnonymousEnumType& enumType) -> llvm::Type* { return visit(enumType.getType()); },
            [&](const cld::Semantics::ValArrayType& valArrayType) -> llvm::Type* {
                // TODO:
                (void)valArrayType;
                CLD_UNREACHABLE;
            });
    }

    void visit(const cld::Semantics::TranslationUnit&)
    {
        for (auto& [id, iter] : m_programInterface.getScopes()[0].declarations)
        {
            (void)id;
            cld::match(
                iter.declared,
                [&](const cld::Semantics::FunctionDefinition* CLD_NON_NULL functionDefinition) {
                    visit(*functionDefinition);
                },
                [&](const cld::Semantics::Declaration* CLD_NON_NULL declaration) { visit(*declaration); },
                [](const auto&) {});
        }
    }

    llvm::Value* visit(const cld::Semantics::Declaration& declaration)
    {
        llvm::Function::LinkageTypes linkageType;
        switch (declaration.getLinkage())
        {
            case cld::Semantics::Linkage::Internal: linkageType = llvm::GlobalValue::InternalLinkage; break;
            case cld::Semantics::Linkage::External: linkageType = llvm::GlobalValue::ExternalLinkage; break;
            case cld::Semantics::Linkage::None: break;
        }
        if (std::holds_alternative<cld::Semantics::FunctionType>(declaration.getType().get()))
        {
            auto* ft = llvm::cast<llvm::FunctionType>(visit(declaration.getType()));
            auto* function =
                llvm::Function::Create(ft, linkageType, -1, declaration.getNameToken()->getText().data(), &m_module);
            applyFunctionAttributes(*function, ft, cld::get<cld::Semantics::FunctionType>(declaration.getType().get()));
            m_lvalues.emplace(&declaration, function);
            return function;
        }
        auto* type = visit(declaration.getType());
        if (declaration.getLifetime() == cld::Semantics::Lifetime::Static)
        {
            llvm::Constant* constant = nullptr;
            if (declaration.getInitializer() && declaration.getKind() != cld::Semantics::Declaration::DeclarationOnly)
            {
                // TODO:
                constant = llvm::Constant::getAllOnesValue(type);
            }
            else if (declaration.getKind() != cld::Semantics::Declaration::DeclarationOnly)
            {
                constant = llvm::Constant::getNullValue(type);
            }
            if (declaration.getLinkage() != cld::Semantics::Linkage::Internal
                && declaration.getKind() == cld::Semantics::Declaration::TentativeDefinition)
            {
                linkageType = llvm::GlobalValue::CommonLinkage;
            }

            auto* global = new llvm::GlobalVariable(
                m_module, type, declaration.getType().isConst() && linkageType != llvm::GlobalValue::CommonLinkage,
                linkageType, constant, declaration.getNameToken()->getText().data());
            global->setAlignment(llvm::MaybeAlign(declaration.getType().getAlignOf(m_programInterface)));
            m_lvalues.emplace(&declaration, global);
            return global;
        }
        // Place all allocas up top
        // TODO: Except VAL Arrays
        llvm::IRBuilder<> temp(&m_builder.GetInsertBlock()->getParent()->getEntryBlock(),
                               m_builder.GetInsertBlock()->getParent()->getEntryBlock().begin());
        auto* var = temp.CreateAlloca(type);
        var->setAlignment(llvm::Align(declaration.getType().getAlignOf(m_programInterface)));
        m_lvalues.emplace(&declaration, var);
        if (cld::Semantics::isVariableLengthArray(declaration.getType()))
        {
            m_builder.CreateLifetimeStart(var);
        }
        else
        {
            visit(cld::Semantics::PrimitiveType::createSizeT(false, false, m_programInterface.getLanguageOptions()));
            auto* size = m_builder.getInt64(declaration.getType().getSizeOf(m_programInterface));
            m_builder.CreateLifetimeStart(var, llvm::cast<llvm::ConstantInt>(size));
        }
        return var;
    }

    void visit(const cld::Semantics::FunctionDefinition& functionDefinition)
    {
        auto* function = m_module.getFunction(functionDefinition.getNameToken()->getText().data());
        if (!function)
        {
            llvm::Function::LinkageTypes linkageType;
            switch (functionDefinition.getLinkage())
            {
                case cld::Semantics::Linkage::Internal: linkageType = llvm::GlobalValue::InternalLinkage; break;
                case cld::Semantics::Linkage::External: linkageType = llvm::GlobalValue::ExternalLinkage; break;
                case cld::Semantics::Linkage::None: CLD_UNREACHABLE;
            }
            auto* ft = llvm::cast<llvm::FunctionType>(visit(functionDefinition.getType()));
            function = llvm::Function::Create(ft, linkageType, -1, functionDefinition.getNameToken()->getText().data(),
                                              &m_module);
            m_lvalues.emplace(&functionDefinition, function);
        }
        auto* bb = llvm::BasicBlock::Create(m_module.getContext(), "entry", function);
        m_builder.SetInsertPoint(bb);

        auto& ft = cld::get<cld::Semantics::FunctionType>(functionDefinition.getType().get());
        applyFunctionAttributes(*function, function->getFunctionType(), ft,
                                &functionDefinition.getParameterDeclarations());
        auto transformations = m_functionABITransformations.find(function->getFunctionType());
        CLD_ASSERT(transformations != m_functionABITransformations.end());
        if (transformations->second.returnType == ABITransformations::Flattened
            || transformations->second.returnType == ABITransformations::IntegerRegister)
        {
            m_returnSlot = m_builder.CreateAlloca(function->getReturnType());
        }
        else
        {
            m_returnSlot = nullptr;
        }

        std::size_t argStart = 0;
        if (transformations->second.returnType == ABITransformations::PointerToTemporary)
        {
            argStart = 1;
        }
        std::size_t origArgI = 0;
        for (std::size_t i = argStart; i < function->arg_size(); origArgI++)
        {
            auto& paramDecl = functionDefinition.getParameterDeclarations()[origArgI];
            if (std::holds_alternative<ABITransformations::Change>(transformations->second.arguments[origArgI]))
            {
                auto* operand = function->getArg(i);
                i++;
                auto change = cld::get<ABITransformations::Change>(transformations->second.arguments[origArgI]);
                if (change == ABITransformations::PointerToTemporary || change == ABITransformations::OnStack)
                {
                    m_lvalues[paramDecl.get()] = operand;
                    continue;
                }
                auto* alloc = m_lvalues[paramDecl.get()];
                if (change == ABITransformations::Unchanged)
                {
                    m_builder.CreateStore(operand, alloc, paramDecl->getType().isVolatile());
                    continue;
                }
                auto* cast = m_builder.CreateBitCast(alloc, llvm::PointerType::getUnqual(operand->getType()));
                m_builder.CreateAlignedStore(operand, cast, llvm::cast<llvm::AllocaInst>(alloc)->getAlign(),
                                             paramDecl->getType().isVolatile());
            }
            else
            {
                auto& multiArg =
                    cld::get<ABITransformations::MultipleArgs>(transformations->second.arguments[origArgI]);
                CLD_ASSERT(multiArg.size <= 2 && multiArg.size > 0);
                auto* alloc = m_lvalues[paramDecl.get()];
                std::vector<llvm::Type*> elements;
                elements.push_back(function->getArg(i)->getType());
                if (multiArg.size == 2)
                {
                    elements.push_back(function->getArg(i + 1)->getType());
                }
                auto* structType = m_builder.CreateBitCast(
                    alloc, llvm::PointerType::getUnqual(llvm::StructType::get(m_builder.getContext(), elements)));
                auto* firstElement =
                    m_builder.CreateInBoundsGEP(structType, {m_builder.getInt32(0), m_builder.getInt32(0)});
                m_builder.CreateStore(function->getArg(i), firstElement, paramDecl->getType().isVolatile());
                if (multiArg.size == 2)
                {
                    auto* secondElement =
                        m_builder.CreateInBoundsGEP(structType, {m_builder.getInt32(0), m_builder.getInt32(1)});
                    m_builder.CreateStore(function->getArg(i + 1), secondElement, paramDecl->getType().isVolatile());
                }
                i += multiArg.size;
            }
        }

        visit(functionDefinition.getCompoundStatement());
        m_builder.ClearInsertionPoint();
    }

    void visit(const cld::Semantics::CompoundStatement& compoundStatement)
    {
        for (auto& iter : compoundStatement.getCompoundItems())
        {
            if (std::holds_alternative<std::shared_ptr<const cld::Semantics::Expression>>(iter))
            {
                // TODO: Evaluate val array expression
            }
            else if (std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(iter))
            {
                visit(*cld::get<std::unique_ptr<cld::Semantics::Declaration>>(iter));
            }
            else if (std::holds_alternative<cld::Semantics::Statement>(iter))
            {
                visit(cld::get<cld::Semantics::Statement>(iter));
            }
        }
    }

    void visit(const cld::Semantics::Statement& statement)
    {
        cld::match(
            statement,
            [&](const auto& statement) {
                using T = std::decay_t<decltype(statement)>;
                if constexpr (cld::IsUniquePtr<T>{})
                {
                    visit(*statement);
                }
                else
                {
                    visit(statement);
                }
            },
            [&](const cld::Semantics::ExpressionStatement& expressionStatement) {
                if (!expressionStatement.getExpression())
                {
                    return;
                }
                auto* instr = visit(*expressionStatement.getExpression());
                if (llvm::isa<llvm::Instruction>(instr) && instr->getNumUses() == 0
                    && !llvm::cast<llvm::Instruction>(instr)->mayHaveSideEffects())
                {
                    llvm::cast<llvm::Instruction>(instr)->eraseFromParent();
                }
            });
    }

    void visit(const cld::Semantics::ReturnStatement& returnStatement)
    {
        if (!returnStatement.getExpression())
        {
            m_builder.CreateRetVoid();
            return;
        }

        auto* function = m_builder.GetInsertBlock()->getParent();
        auto transformation = m_functionABITransformations.find(function->getFunctionType());
        CLD_ASSERT(transformation != m_functionABITransformations.end());
        auto* value = visit(*returnStatement.getExpression());
        if (transformation->second.returnType == ABITransformations::PointerToTemporary)
        {
            m_builder.CreateStore(value, function->getArg(0));
            m_builder.CreateRetVoid();
        }
        else if (transformation->second.returnType == ABITransformations::Flattened
                 || transformation->second.returnType == ABITransformations::IntegerRegister)
        {
            auto* bitCast = m_builder.CreateBitCast(m_returnSlot, llvm::PointerType::getUnqual(value->getType()));
            m_builder.CreateStore(value, bitCast);
            auto* ret = m_builder.CreateLoad(m_returnSlot);
            m_builder.CreateRet(ret);
        }
        else
        {
            m_builder.CreateRet(value);
        }
    }

    void visit(const cld::Semantics::ForStatement& forStatement)
    {
        cld::match(
            forStatement.getInitial(), [](std::monostate) {},
            [&](const std::vector<std::unique_ptr<cld::Semantics::Declaration>>& declaration) {
                for (auto& iter : declaration)
                {
                    visit(*iter);
                }
            },
            [&](const cld::Semantics::Expression& expression) { visit(expression); });
        auto* controlling =
            llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        m_builder.SetInsertPoint(controlling);
        auto* body = llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        llvm::BasicBlock* contBlock =
            llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        m_breakTargets[&forStatement] = contBlock;
        if (forStatement.getControlling())
        {
            auto* value = visit(*forStatement.getControlling());
            value = m_builder.CreateTrunc(value, m_builder.getInt1Ty());
            m_builder.CreateCondBr(value, body, contBlock);
        }
        m_builder.SetInsertPoint(body);
        auto* iteration =
            forStatement.getIteration() ?
                llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent()) :
                controlling;
        m_continueTargets[&forStatement] = iteration;
        visit(forStatement.getStatement());
        m_builder.CreateBr(iteration);
        if (forStatement.getIteration())
        {
            m_builder.SetInsertPoint(iteration);
            visit(*forStatement.getIteration());
            m_builder.CreateBr(controlling);
        }
        if (contBlock)
        {
            m_builder.SetInsertPoint(contBlock);
        }
    }

    void visit(const cld::Semantics::IfStatement& ifStatement)
    {
        // TODO: CFG analysis
        auto* expression = visit(ifStatement.getExpression());
        expression = m_builder.CreateTrunc(expression, m_builder.getInt1Ty());
        auto* trueBranch = llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        if (!ifStatement.getFalseBranch())
        {
            auto* contBranch =
                llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
            m_builder.CreateCondBr(expression, trueBranch, contBranch);
            m_builder.SetInsertPoint(trueBranch);
            visit(ifStatement.getTrueBranch());
            m_builder.CreateBr(contBranch);
            m_builder.SetInsertPoint(contBranch);
            return;
        }
        auto* falseBranch =
            llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        auto* contBranch = llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        m_builder.CreateCondBr(expression, trueBranch, falseBranch);
        m_builder.SetInsertPoint(trueBranch);
        visit(ifStatement.getTrueBranch());
        m_builder.CreateBr(contBranch);
        m_builder.SetInsertPoint(falseBranch);
        visit(*ifStatement.getFalseBranch());
        m_builder.CreateBr(contBranch);
        m_builder.SetInsertPoint(contBranch);
    }

    void visit(const cld::Semantics::HeadWhileStatement& headWhileStatement)
    {
        auto* controlling =
            llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        m_continueTargets[&headWhileStatement] = controlling;
        m_builder.SetInsertPoint(controlling);
        auto* expression = visit(headWhileStatement.getExpression());
        expression = m_builder.CreateTrunc(expression, m_builder.getInt1Ty());
        auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        auto* body = llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        m_builder.CreateCondBr(expression, body, contBlock);
        m_builder.SetInsertPoint(body);
        m_breakTargets[&headWhileStatement] = contBlock;
        visit(headWhileStatement.getStatement());
        m_builder.CreateBr(controlling);
        m_builder.SetInsertPoint(contBlock);
    }

    void visit(const cld::Semantics::FootWhileStatement& footWhileStatement)
    {
        auto* controlling =
            llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        auto* body = llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        m_continueTargets[&footWhileStatement] = controlling;
        m_breakTargets[&footWhileStatement] = contBlock;
        m_builder.SetInsertPoint(body);
        visit(footWhileStatement.getStatement());
        m_builder.CreateBr(controlling);
        m_builder.SetInsertPoint(controlling);
        auto* expression = visit(footWhileStatement.getExpression());
        expression = m_builder.CreateTrunc(expression, m_builder.getInt1Ty());
        m_builder.CreateCondBr(expression, body, contBlock);
        m_builder.SetInsertPoint(contBlock);
    }

    void visit(const cld::Semantics::BreakStatement& breakStatement)
    {
        m_builder.CreateBr(m_breakTargets[breakStatement.getBreakableStatement()]);
    }

    void visit(const cld::Semantics::ContinueStatement& continueStatement)
    {
        m_builder.CreateBr(m_continueTargets[continueStatement.getLoopStatement()]);
    }

    void visit(const cld::Semantics::SwitchStatement& switchStatement)
    {
        auto* expression = visit(switchStatement.getExpression());
        auto& switchData = m_switches[&switchStatement];
        auto* contBlock = llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        if (switchStatement.getDefaultStatement())
        {
            switchData.defaultBlock =
                llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        }
        auto* switchStmt = m_builder.CreateSwitch(
            expression, switchData.defaultBlock ? switchData.defaultBlock : contBlock, switchData.cases.size());
        for (auto& [value, theCase] : switchStatement.getCases())
        {
            auto iter = switchData.cases.emplace(
                theCase, llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent()));
            switchStmt->addCase(llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(expression->getType(), value)),
                                iter.first->second);
        }
        visit(switchStatement.getStatement());
        m_builder.CreateBr(contBlock);
        m_builder.SetInsertPoint(contBlock);
    }

    void visit(const cld::Semantics::DefaultStatement& defaultStatement)
    {
        auto& switchData = m_switches[&defaultStatement.getSwitchStatement()];
        auto* bb = switchData.defaultBlock;
        m_builder.CreateBr(bb);
        m_builder.SetInsertPoint(bb);
        visit(defaultStatement.getStatement());
    }

    void visit(const cld::Semantics::CaseStatement& caseStatement)
    {
        auto& switchData = m_switches[&caseStatement.getSwitchStatement()];
        auto* bb = switchData.cases[&caseStatement];
        m_builder.CreateBr(bb);
        m_builder.SetInsertPoint(bb);
        visit(caseStatement.getStatement());
    }

    void visit(const cld::Semantics::GotoStatement& gotoStatement)
    {
        auto* bb = m_labels[gotoStatement.getLabel()];
        if (!bb)
        {
            bb = m_labels[gotoStatement.getLabel()] =
                llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        }
        m_builder.CreateBr(bb);
    }

    void visit(const cld::Semantics::LabelStatement& labelStatement)
    {
        auto* bb = m_labels[&labelStatement];
        if (!bb)
        {
            bb = m_labels[&labelStatement] =
                llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
        }
        m_builder.CreateBr(bb);
        m_builder.SetInsertPoint(bb);
        visit(labelStatement.getStatement());
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression)
    {
        return cld::match(
            expression.get(),
            [](const std::pair<cld::Lexer::CTokenIterator, cld::Lexer::CTokenIterator>&) -> llvm::Value* {
                CLD_UNREACHABLE;
            },
            [&](const auto& value) -> llvm::Value* { return visit(expression, value); });
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::Constant& constant)
    {
        auto* type = visit(expression.getType());
        if (std::holds_alternative<llvm::APSInt>(constant.getValue()))
        {
            return llvm::Constant::getIntegerValue(type, cld::get<llvm::APSInt>(constant.getValue()));
        }
        else if (std::holds_alternative<llvm::APFloat>(constant.getValue()))
        {
            return llvm::ConstantFP::get(type, cld::get<llvm::APFloat>(constant.getValue()));
        }
        else if (std::holds_alternative<std::string>(constant.getValue()))
        {
            return llvm::ConstantDataArray::getString(m_module.getContext(),
                                                      cld::get<std::string>(constant.getValue()));
        }
        else
        {
            auto& str = cld::get<cld::Lexer::NonCharString>(constant.getValue());
            // TODO: Wide strings
            (void)str;
            CLD_UNREACHABLE;
        }
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::DeclarationRead& declarationRead)
    {
        auto result = cld::match(
            declarationRead.getDeclRead(),
            [&](const cld::Semantics::Declaration* declaration) { return m_lvalues.find(declaration); },
            [&](const cld::Semantics::FunctionDefinition* functionDefinition) {
                return m_lvalues.find(functionDefinition);
            });
        CLD_ASSERT(result != m_lvalues.end());
        return result->second;
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::Conversion& conversion)
    {
        auto* value = visit(conversion.getExpression());
        switch (conversion.getKind())
        {
            case cld::Semantics::Conversion::LValue:
            {
                if (cld::Semantics::isArray(conversion.getExpression().getType()))
                {
                    auto* zero = llvm::ConstantInt::get(m_builder.getIntPtrTy(m_module.getDataLayout()), 0);
                    return m_builder.CreateInBoundsGEP(value, {zero, zero});
                }
                else if (std::holds_alternative<cld::Semantics::FunctionType>(
                             conversion.getExpression().getType().get())
                         || m_programInterface.isBitfieldAccess(conversion.getExpression()))
                {
                    return value;
                }
                else
                {
                    return m_builder.CreateLoad(value->getType()->getPointerElementType(), value,
                                                conversion.getExpression().getType().isVolatile());
                }
            }
            case cld::Semantics::Conversion::IntegerPromotion:
            {
                auto& prevType = conversion.getExpression().getType();
                return m_builder.CreateIntCast(value, visit(expression.getType()),
                                               cld::get<cld::Semantics::PrimitiveType>(prevType.get()).isSigned());
            }
            case cld::Semantics::Conversion::Implicit:
            {
                auto& prevType = conversion.getExpression().getType();
                auto& newType = expression.getType();
                if (cld::Semantics::isBool(newType))
                {
                    return m_builder.CreateIntCast(toBool(value), visit(newType), false);
                }
                if (std::holds_alternative<cld::Semantics::PointerType>(newType.get()))
                {
                    if (cld::Semantics::isInteger(prevType))
                    {
                        return m_builder.CreateIntToPtr(value, visit(newType));
                    }
                    return m_builder.CreatePointerCast(value, visit(newType));
                }
                [[fallthrough]];
            }
            case cld::Semantics::Conversion::ArithmeticConversion:
            {
                auto& prevType = conversion.getExpression().getType();
                auto& newType = expression.getType();
                if (cld::Semantics::isInteger(prevType) && cld::Semantics::isInteger(newType))
                {
                    return m_builder.CreateIntCast(value, visit(newType),
                                                   cld::get<cld::Semantics::PrimitiveType>(prevType.get()).isSigned());
                }
                if (cld::Semantics::isInteger(prevType))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(prevType.get()).isSigned())
                    {
                        return m_builder.CreateSIToFP(value, visit(newType));
                    }
                    else
                    {
                        return m_builder.CreateUIToFP(value, visit(newType));
                    }
                }
                if (cld::Semantics::isInteger(newType))
                {
                    if (std::holds_alternative<cld::Semantics::PointerType>(prevType.get()))
                    {
                        return m_builder.CreatePtrToInt(value, visit(newType));
                    }
                    else if (cld::get<cld::Semantics::PrimitiveType>(newType.get()).isSigned())
                    {
                        return m_builder.CreateFPToSI(value, visit(newType));
                    }
                    else
                    {
                        return m_builder.CreateFPToUI(value, visit(newType));
                    }
                }
                return m_builder.CreateFPCast(value, visit(newType));
            }
            case cld::Semantics::Conversion::DefaultArgumentPromotion:
            {
                auto& prevType = conversion.getExpression().getType();
                if (cld::Semantics::isInteger(prevType))
                {
                    return m_builder.CreateIntCast(value, visit(expression.getType()),
                                                   cld::get<cld::Semantics::PrimitiveType>(prevType.get()).isSigned());
                }
                return m_builder.CreateFPCast(value, visit(expression.getType()));
            }
        }
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::MemberAccess& memberAccess)
    {
        auto* value = visit(memberAccess.getRecordExpression());
        auto& type =
            std::holds_alternative<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().get()) ?
                cld::get<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().get())
                    .getElementType() :
                memberAccess.getRecordExpression().getType();
        if (std::holds_alternative<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().get()))
        {
            value = m_builder.CreateLoad(value, memberAccess.getRecordExpression().getType().isVolatile());
        }
        else if (std::holds_alternative<cld::Semantics::CallExpression>(memberAccess.getRecordExpression().get()))
        {
            // Struct access is only ever allowed on pointers or lvalue except if it's the return value of a function
            // then it's also allowed to be an rvalue
            auto* load = llvm::cast<llvm::LoadInst>(value);
            value = load->getPointerOperand();
            load->eraseFromParent();
        }
        auto index = memberAccess.getMemberIndex();
        llvm::ArrayRef<cld::Semantics::Field> fields = m_programInterface.getFields(type);

        llvm::Value* field = nullptr;
        if (cld::Semantics::isStruct(type))
        {
            auto* zero = m_builder.getInt64(0);
            auto* member = m_builder.getInt32(fields[index].layoutIndex);
            field = m_builder.CreateInBoundsGEP(value, {zero, member});
        }
        else
        {
            auto* destTy = visit(*fields[index].type);
            field = m_builder.CreateBitCast(value, llvm::PointerType::getUnqual(destTy));
        }
        if (!fields[index].bitFieldBounds)
        {
            // If the record expression is the return value of a function and this is a dot access not arrow access
            // we must load because an rvalue is returned and no lvalue conversion will load for us
            if (!std::holds_alternative<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().get())
                && std::holds_alternative<cld::Semantics::CallExpression>(memberAccess.getRecordExpression().get()))
            {
                return m_builder.CreateLoad(field, type.isVolatile());
            }
            return field;
        }

        auto* loaded = m_builder.CreateLoad(field, expression.getType().isVolatile());
        auto upLeft = loaded->getType()->getPrimitiveSizeInBits() - fields[index].bitFieldBounds->second;
        auto* shl = m_builder.CreateShl(loaded, llvm::ConstantInt::get(loaded->getType(), upLeft));
        auto* shrConstant = llvm::ConstantInt::get(loaded->getType(), upLeft + fields[index].bitFieldBounds->first);
        if (cld::get<cld::Semantics::PrimitiveType>(expression.getType().get()).isSigned())
        {
            return m_builder.CreateAShr(shl, shrConstant);
        }
        else
        {
            return m_builder.CreateLShr(shl, shrConstant);
        }
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::BinaryOperator& binaryExpression)
    {
        auto* lhs = visit(binaryExpression.getLeftExpression());
        switch (binaryExpression.getKind())
        {
            case cld::Semantics::BinaryOperator::Addition:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                if (cld::Semantics::isArithmetic(binaryExpression.getLeftExpression().getType())
                    && cld::Semantics::isArithmetic(binaryExpression.getLeftExpression().getType()))
                {
                    if (cld::Semantics::isInteger(binaryExpression.getLeftExpression().getType()))
                    {
                        if (!cld::get<cld::Semantics::PrimitiveType>(
                                 binaryExpression.getLeftExpression().getType().get())
                                 .isSigned())
                        {
                            return m_builder.CreateNSWAdd(lhs, rhs);
                        }
                        else
                        {
                            return m_builder.CreateAdd(lhs, rhs);
                        }
                    }
                    else
                    {
                        return m_builder.CreateFAdd(lhs, rhs);
                    }
                }
                else
                {
                    if (lhs->getType()->isPointerTy())
                    {
                        rhs = m_builder.CreateIntCast(rhs, m_builder.getInt64Ty(),
                                                      cld::get<cld::Semantics::PrimitiveType>(
                                                          binaryExpression.getRightExpression().getType().get())
                                                          .isSigned());
                        return m_builder.CreateGEP(lhs, rhs);
                    }
                    else
                    {
                        lhs = m_builder.CreateIntCast(lhs, m_builder.getInt64Ty(),
                                                      cld::get<cld::Semantics::PrimitiveType>(
                                                          binaryExpression.getLeftExpression().getType().get())
                                                          .isSigned());
                        return m_builder.CreateGEP(rhs, lhs);
                    }
                }
            }
            case cld::Semantics::BinaryOperator::Subtraction:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                if (cld::Semantics::isArithmetic(binaryExpression.getLeftExpression().getType())
                    && cld::Semantics::isArithmetic(binaryExpression.getLeftExpression().getType()))
                {
                    if (cld::Semantics::isInteger(binaryExpression.getLeftExpression().getType()))
                    {
                        if (!cld::get<cld::Semantics::PrimitiveType>(
                                 binaryExpression.getLeftExpression().getType().get())
                                 .isSigned())
                        {
                            return m_builder.CreateNSWSub(lhs, rhs);
                        }
                        else
                        {
                            return m_builder.CreateSub(lhs, rhs);
                        }
                    }
                    else
                    {
                        return m_builder.CreateFSub(lhs, rhs);
                    }
                }
                else
                {
                    if (lhs->getType()->isPointerTy())
                    {
                        if (rhs->getType()->isIntegerTy())
                        {
                            rhs = m_builder.CreateNeg(rhs);
                            rhs = m_builder.CreateIntCast(rhs, m_builder.getInt64Ty(),
                                                          cld::get<cld::Semantics::PrimitiveType>(
                                                              binaryExpression.getRightExpression().getType().get())
                                                              .isSigned());
                            return m_builder.CreateGEP(lhs, rhs);
                        }
                        else
                        {
                            return m_builder.CreatePtrDiff(lhs, rhs);
                        }
                    }
                    else
                    {
                        if (rhs->getType()->isIntegerTy())
                        {
                            lhs = m_builder.CreateNeg(lhs);
                            lhs = m_builder.CreateIntCast(lhs, m_builder.getInt64Ty(),
                                                          cld::get<cld::Semantics::PrimitiveType>(
                                                              binaryExpression.getLeftExpression().getType().get())
                                                              .isSigned());
                            return m_builder.CreateGEP(rhs, lhs);
                        }
                        else
                        {
                            return m_builder.CreatePtrDiff(lhs, rhs);
                        }
                    }
                }
            }
            case cld::Semantics::BinaryOperator::Multiply:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                if (cld::Semantics::isInteger(binaryExpression.getLeftExpression().getType()))
                {
                    return m_builder.CreateNSWMul(lhs, rhs);
                }
                else
                {
                    return m_builder.CreateFMul(lhs, rhs);
                }
            }
            case cld::Semantics::BinaryOperator::Divide:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                if (cld::Semantics::isInteger(binaryExpression.getLeftExpression().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(binaryExpression.getLeftExpression().getType().get())
                            .isSigned())
                    {
                        return m_builder.CreateSDiv(lhs, rhs);
                    }
                    else
                    {
                        return m_builder.CreateUDiv(lhs, rhs);
                    }
                }
                else
                {
                    return m_builder.CreateFDiv(lhs, rhs);
                }
            }
            case cld::Semantics::BinaryOperator::Modulo:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                if (cld::get<cld::Semantics::PrimitiveType>(binaryExpression.getLeftExpression().getType().get())
                        .isSigned())
                {
                    return m_builder.CreateSRem(lhs, rhs);
                }
                else
                {
                    return m_builder.CreateURem(lhs, rhs);
                }
            }
            case cld::Semantics::BinaryOperator::LeftShift:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                if (lhs->getType() != rhs->getType())
                {
                    rhs = m_builder.CreateIntCast(
                        rhs, lhs->getType(),
                        cld::get<cld::Semantics::PrimitiveType>(binaryExpression.getRightExpression().getType().get())
                            .isSigned());
                }
                return m_builder.CreateShl(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::RightShift:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                if (lhs->getType() != rhs->getType())
                {
                    rhs = m_builder.CreateIntCast(
                        rhs, lhs->getType(),
                        cld::get<cld::Semantics::PrimitiveType>(binaryExpression.getRightExpression().getType().get())
                            .isSigned());
                }
                return m_builder.CreateAShr(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::GreaterThan:
            case cld::Semantics::BinaryOperator::LessOrEqual:
            case cld::Semantics::BinaryOperator::GreaterOrEqual:
            case cld::Semantics::BinaryOperator::Equal:
            case cld::Semantics::BinaryOperator::NotEqual:
            case cld::Semantics::BinaryOperator::LessThan:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                llvm::CmpInst::Predicate predicate;
                bool fp = cld::Semantics::isArithmetic(binaryExpression.getLeftExpression().getType())
                          && !cld::Semantics::isInteger(binaryExpression.getLeftExpression().getType());
                bool isSigned =
                    cld::Semantics::isInteger(binaryExpression.getLeftExpression().getType())
                    && cld::get<cld::Semantics::PrimitiveType>(binaryExpression.getLeftExpression().getType().get())
                           .isSigned();
                switch (binaryExpression.getKind())
                {
                    case cld::Semantics::BinaryOperator::GreaterThan:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_UGT;
                        }
                        else if (isSigned)
                        {
                            predicate = llvm::CmpInst::ICMP_SGT;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_UGT;
                        }
                        break;
                    case cld::Semantics::BinaryOperator::LessOrEqual:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_ULE;
                        }
                        else if (isSigned)
                        {
                            predicate = llvm::CmpInst::ICMP_SLE;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_ULE;
                        }
                        break;
                    case cld::Semantics::BinaryOperator::GreaterOrEqual:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_UGE;
                        }
                        else if (isSigned)
                        {
                            predicate = llvm::CmpInst::ICMP_SGE;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_UGE;
                        }
                        break;
                    case cld::Semantics::BinaryOperator::Equal:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_UEQ;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_EQ;
                        }
                        break;
                    case cld::Semantics::BinaryOperator::NotEqual:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_UGT;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_NE;
                        }
                        break;
                    case cld::Semantics::BinaryOperator::LessThan:
                        if (fp)
                        {
                            predicate = llvm::CmpInst::FCMP_ULT;
                        }
                        else if (isSigned)
                        {
                            predicate = llvm::CmpInst::ICMP_SLT;
                        }
                        else
                        {
                            predicate = llvm::CmpInst::ICMP_ULT;
                        }
                        break;
                    default: CLD_UNREACHABLE;
                }
                auto* value = m_builder.CreateCmp(predicate, lhs, rhs);
                return m_builder.CreateZExt(value, visit(cld::Semantics::PrimitiveType::createInt(
                                                       false, false, m_sourceInterface.getLanguageOptions())));
            }
            case cld::Semantics::BinaryOperator::BitOr:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return m_builder.CreateOr(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::BitAnd:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return m_builder.CreateAnd(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::BitXor:
            {
                auto* rhs = visit(binaryExpression.getRightExpression());
                return m_builder.CreateXor(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::LogicAnd:
            {
                lhs = m_builder.CreateTrunc(lhs, m_builder.getInt1Ty());
                auto* falseBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
                auto* trueBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
                auto* continueBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
                m_builder.CreateCondBr(lhs, trueBranch, falseBranch);
                m_builder.SetInsertPoint(trueBranch);
                auto* rhs = visit(binaryExpression.getRightExpression());
                rhs = m_builder.CreateTrunc(rhs, m_builder.getInt1Ty());
                m_builder.CreateBr(continueBranch);
                m_builder.SetInsertPoint(falseBranch);
                m_builder.CreateBr(continueBranch);
                m_builder.SetInsertPoint(continueBranch);
                auto* phi = m_builder.CreatePHI(m_builder.getInt1Ty(), 2);
                phi->addIncoming(lhs, falseBranch);
                phi->addIncoming(rhs, trueBranch);
                return m_builder.CreateZExt(phi, m_builder.getInt32Ty());
            }
            case cld::Semantics::BinaryOperator::LogicOr:
            {
                lhs = m_builder.CreateTrunc(lhs, m_builder.getInt1Ty());
                auto* falseBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
                auto* trueBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
                auto* continueBranch =
                    llvm::BasicBlock::Create(m_module.getContext(), "", m_builder.GetInsertBlock()->getParent());
                m_builder.CreateCondBr(lhs, trueBranch, falseBranch);
                m_builder.SetInsertPoint(falseBranch);
                auto* rhs = visit(binaryExpression.getRightExpression());
                rhs = m_builder.CreateTrunc(rhs, m_builder.getInt1Ty());
                m_builder.CreateBr(continueBranch);
                m_builder.SetInsertPoint(trueBranch);
                m_builder.CreateBr(continueBranch);
                m_builder.SetInsertPoint(continueBranch);
                auto* phi = m_builder.CreatePHI(m_builder.getInt1Ty(), 2);
                phi->addIncoming(lhs, trueBranch);
                phi->addIncoming(rhs, falseBranch);
                return m_builder.CreateZExt(phi, m_builder.getInt32Ty());
            }
        }
        CLD_UNREACHABLE;
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::Cast& cast)
    {
        auto* value = visit(cast.getExpression());
        auto& prevType = cast.getExpression().getType();
        auto& newType = expression.getType();
        if (std::holds_alternative<cld::Semantics::PointerType>(newType.get()))
        {
            if (cld::Semantics::isInteger(prevType))
            {
                return m_builder.CreateIntToPtr(value, visit(newType));
            }
            return m_builder.CreatePointerCast(value, visit(newType));
        }
        if (cld::Semantics::isBool(newType))
        {
            return m_builder.CreateIntCast(toBool(value), visit(newType), false);
        }
        if (cld::Semantics::isInteger(prevType) && cld::Semantics::isInteger(newType))
        {
            return m_builder.CreateIntCast(value, visit(newType),
                                           cld::get<cld::Semantics::PrimitiveType>(prevType.get()).isSigned());
        }
        if (cld::Semantics::isInteger(prevType))
        {
            if (cld::get<cld::Semantics::PrimitiveType>(prevType.get()).isSigned())
            {
                return m_builder.CreateSIToFP(value, visit(newType));
            }
            else
            {
                return m_builder.CreateUIToFP(value, visit(newType));
            }
        }
        if (cld::Semantics::isInteger(newType))
        {
            if (std::holds_alternative<cld::Semantics::PointerType>(prevType.get()))
            {
                return m_builder.CreatePtrToInt(value, visit(newType));
            }
            else if (cld::get<cld::Semantics::PrimitiveType>(newType.get()).isSigned())
            {
                return m_builder.CreateFPToSI(value, visit(newType));
            }
            else
            {
                return m_builder.CreateFPToUI(value, visit(newType));
            }
        }
        return m_builder.CreateFPCast(value, visit(newType));
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::UnaryOperator& unaryOperator)
    {
        auto* value = visit(unaryOperator.getOperand());
        switch (unaryOperator.getKind())
        {
            case cld::Semantics::UnaryOperator::AddressOf:
            case cld::Semantics::UnaryOperator::Dereference:
                // The difference between address of and dereference is that an lvalue conversion follows a dereference
                return value;
            case cld::Semantics::UnaryOperator::PostIncrement:
            {
                auto* prev = m_builder.CreateLoad(value, unaryOperator.getOperand().getType().isVolatile());
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().get()).isSigned())
                    {
                        auto* result = m_builder.CreateNSWAdd(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                    else
                    {
                        auto* result = m_builder.CreateAdd(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().get()))
                {
                    auto* result = m_builder.CreateFAdd(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(result, value);
                }
                else
                {
                    auto* result = m_builder.CreateGEP(prev, m_builder.getInt32(1));
                    m_builder.CreateStore(result, value);
                }
                return prev;
            }
            case cld::Semantics::UnaryOperator::PostDecrement:
            {
                auto* prev = m_builder.CreateLoad(value, unaryOperator.getOperand().getType().isVolatile());
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().get()).isSigned())
                    {
                        auto* result = m_builder.CreateNSWSub(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                    else
                    {
                        auto* result = m_builder.CreateSub(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().get()))
                {
                    auto* result = m_builder.CreateFSub(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(result, value);
                }
                else
                {
                    auto* result = m_builder.CreateGEP(prev, m_builder.getInt32(-1));
                    m_builder.CreateStore(result, value);
                }
                return prev;
            }
            case cld::Semantics::UnaryOperator::PreIncrement:
            {
                llvm::Value* result = nullptr;
                auto* prev = m_builder.CreateLoad(value, unaryOperator.getOperand().getType().isVolatile());
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().get()).isSigned())
                    {
                        result = m_builder.CreateNSWAdd(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                    else
                    {
                        result = m_builder.CreateAdd(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().get()))
                {
                    result = m_builder.CreateFAdd(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(result, value);
                }
                else
                {
                    result = m_builder.CreateGEP(prev, m_builder.getInt32(1));
                    m_builder.CreateStore(result, value);
                }
                return result;
            }
            case cld::Semantics::UnaryOperator::PreDecrement:
            {
                llvm::Value* result = nullptr;
                auto* prev = m_builder.CreateLoad(value, unaryOperator.getOperand().getType().isVolatile());
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().get()).isSigned())
                    {
                        result = m_builder.CreateNSWSub(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                    else
                    {
                        result = m_builder.CreateSub(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(result, value);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().get()))
                {
                    result = m_builder.CreateFSub(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(result, value);
                }
                else
                {
                    result = m_builder.CreateGEP(prev, m_builder.getInt32(-1));
                    m_builder.CreateStore(result, value);
                }
                return result;
            }
            case cld::Semantics::UnaryOperator::Plus: return value;
            case cld::Semantics::UnaryOperator::Minus:
            {
                if (cld::Semantics::isInteger(unaryOperator.getOperand().getType()))
                {
                    if (cld::get<cld::Semantics::PrimitiveType>(unaryOperator.getOperand().getType().get()).isSigned())
                    {
                        return m_builder.CreateNSWNeg(value);
                    }
                    else
                    {
                        return m_builder.CreateNeg(value);
                    }
                }
                else
                {
                    return m_builder.CreateFNeg(value);
                }
            }
            case cld::Semantics::UnaryOperator::BooleanNegate:
            {
                value = m_builder.CreateNot(m_builder.CreateTrunc(value, m_builder.getInt1Ty()));
                return m_builder.CreateZExt(value, visit(cld::Semantics::PrimitiveType::createInt(
                                                       false, false, m_sourceInterface.getLanguageOptions())));
            }
            case cld::Semantics::UnaryOperator::BitwiseNegate: return m_builder.CreateNot(value);
        }
        CLD_UNREACHABLE;
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::SizeofOperator& sizeofOperator)
    {
        if (sizeofOperator.getSize())
        {
            auto* type = visit(
                cld::Semantics::PrimitiveType::createSizeT(false, false, m_programInterface.getLanguageOptions()));
            return llvm::ConstantInt::get(type, *sizeofOperator.getSize());
        }
        // TODO:
        CLD_UNREACHABLE;
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::SubscriptOperator& subscriptOperator)
    {
        auto* lhs = visit(subscriptOperator.getLeftExpression());
        auto* rhs = visit(subscriptOperator.getRightExpression());
        if (rhs->getType()->isIntegerTy())
        {
            rhs = m_builder.CreateIntCast(
                rhs, m_builder.getInt64Ty(),
                cld::get<cld::Semantics::PrimitiveType>(subscriptOperator.getRightExpression().getType().get())
                    .isSigned());
            return m_builder.CreateGEP(lhs, rhs);
        }
        else
        {
            lhs = m_builder.CreateIntCast(
                lhs, m_builder.getInt64Ty(),
                cld::get<cld::Semantics::PrimitiveType>(subscriptOperator.getLeftExpression().getType().get())
                    .isSigned());
            return m_builder.CreateGEP(rhs, lhs);
        }
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::Conditional& conditional)
    {
        auto* boolean = visit(conditional.getBoolExpression());
        boolean = m_builder.CreateTrunc(boolean, m_builder.getInt1Ty());
        auto* trueBranch =
            llvm::BasicBlock::Create(m_builder.getContext(), "", m_builder.GetInsertBlock()->getParent());
        auto* falseBranch =
            llvm::BasicBlock::Create(m_builder.getContext(), "", m_builder.GetInsertBlock()->getParent());
        auto* contBr = llvm::BasicBlock::Create(m_builder.getContext(), "", m_builder.GetInsertBlock()->getParent());
        m_builder.CreateCondBr(boolean, trueBranch, falseBranch);
        m_builder.SetInsertPoint(trueBranch);
        auto* trueValue = visit(conditional.getTrueExpression());
        m_builder.CreateBr(contBr);
        m_builder.SetInsertPoint(falseBranch);
        auto* falseValue = visit(conditional.getFalseExpression());
        m_builder.CreateBr(contBr);
        m_builder.SetInsertPoint(contBr);
        auto* phi = m_builder.CreatePHI(trueValue->getType(), 2);
        phi->addIncoming(trueValue, trueBranch);
        phi->addIncoming(falseValue, falseBranch);
        return phi;
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::Assignment& assignment)
    {
        if (!m_programInterface.isBitfieldAccess(assignment.getLeftExpression()))
        {
            auto* lhs = visit(assignment.getLeftExpression());
            auto* rhs = visit(assignment.getRightExpression());
            m_builder.CreateStore(rhs, lhs, assignment.getLeftExpression().getType().isVolatile());
            return m_builder.CreateLoad(lhs->getType()->getPointerElementType(), lhs,
                                        assignment.getLeftExpression().getType().isVolatile());
        }
        auto& memberAccess = cld::get<cld::Semantics::MemberAccess>(assignment.getLeftExpression().get());
        auto* lhsRecord = visit(memberAccess.getRecordExpression());
        auto& type =
            std::holds_alternative<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().get()) ?
                cld::get<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().get())
                    .getElementType() :
                memberAccess.getRecordExpression().getType();
        if (std::holds_alternative<cld::Semantics::PointerType>(memberAccess.getRecordExpression().getType().get()))
        {
            lhsRecord = m_builder.CreateLoad(lhsRecord, memberAccess.getRecordExpression().getType().isVolatile());
        }
        auto* rhsValue = visit(assignment.getRightExpression());

        auto& field = m_programInterface.getFields(type)[memberAccess.getMemberIndex()];
        llvm::Value* fieldPtr = nullptr;

        if (cld::Semantics::isStruct(type))
        {
            auto* zero = m_builder.getInt64(0);
            auto* member = m_builder.getInt32(field.layoutIndex);
            fieldPtr = m_builder.CreateInBoundsGEP(lhsRecord, {zero, member});
        }
        else
        {
            fieldPtr = m_builder.CreateBitCast(lhsRecord, llvm::PointerType::getUnqual(visit(*field.type)));
        }

        auto* loaded = m_builder.CreateLoad(fieldPtr, type.isVolatile());
        auto size = field.bitFieldBounds->second - field.bitFieldBounds->first;
        rhsValue = m_builder.CreateAnd(rhsValue, llvm::ConstantInt::get(rhsValue->getType(), (1u << size) - 1));
        rhsValue =
            m_builder.CreateShl(rhsValue, llvm::ConstantInt::get(rhsValue->getType(), field.bitFieldBounds->first));
        auto* result = m_builder.CreateOr(loaded, rhsValue);
        m_builder.CreateStore(result, fieldPtr, type.isVolatile());
        return m_builder.CreateLoad(fieldPtr, type.isVolatile());
    }

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::CommaExpression& commaExpression)
    {
        for (auto& iter : commaExpression.getCommaExpressions())
        {
            auto* instr = visit(iter.first);
            if (llvm::isa<llvm::Instruction>(instr) && instr->getNumUses() == 0
                && !llvm::cast<llvm::Instruction>(instr)->mayHaveSideEffects())
            {
                llvm::cast<llvm::Instruction>(instr)->eraseFromParent();
            }
        }
        return visit(commaExpression.getLastExpression());
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::CallExpression& call)
    {
        auto* function = visit(call.getFunctionExpression());
        auto cldFt = cld::get<cld::Semantics::FunctionType>(
            cld::get<cld::Semantics::PointerType>(call.getFunctionExpression().getType().get()).getElementType().get());
        auto* ft = llvm::cast<llvm::FunctionType>(function->getType()->getPointerElementType());
        auto transformation = m_functionABITransformations.find(ft);
        CLD_ASSERT(transformation != m_functionABITransformations.end());
        bool isKandR = cldFt.isKandR();
        if (isKandR || cldFt.isLastVararg())
        {
            std::vector<std::pair<cld::Semantics::Type, std::string_view>> arguments;
            for (auto& iter : call.getArgumentExpressions())
            {
                arguments.emplace_back(iter.getType(), "");
            }
            auto callerFt =
                cld::Semantics::FunctionType::create(cldFt.getReturnType(), std::move(arguments), false, false);
            ft = llvm::cast<llvm::FunctionType>(visit(callerFt));
            transformation = m_functionABITransformations.find(ft);
            CLD_ASSERT(transformation != m_functionABITransformations.end());
            cldFt = cld::get<cld::Semantics::FunctionType>(callerFt.get());
        }

        std::size_t llvmFnI = 0;
        std::vector<llvm::Value*> arguments;
        llvm::AllocaInst* returnSlot = nullptr;
        if (transformation->second.returnType == ABITransformations::PointerToTemporary)
        {
            llvmFnI = 1;
            llvm::IRBuilder<> temp(&m_builder.GetInsertBlock()->getParent()->getEntryBlock(),
                                   m_builder.GetInsertBlock()->getParent()->getEntryBlock().begin());
            returnSlot = temp.CreateAlloca(ft->getParamType(0)->getPointerElementType());
            returnSlot->setAlignment(llvm::Align(expression.getType().getAlignOf(m_programInterface)));
            m_builder.CreateLifetimeStart(returnSlot,
                                          m_builder.getInt64(expression.getType().getSizeOf(m_programInterface)));
            arguments.emplace_back(returnSlot);
        }
        else if (transformation->second.returnType == ABITransformations::Flattened
                 || transformation->second.returnType == ABITransformations::IntegerRegister)
        {
            llvm::IRBuilder<> temp(&m_builder.GetInsertBlock()->getParent()->getEntryBlock(),
                                   m_builder.GetInsertBlock()->getParent()->getEntryBlock().begin());
            returnSlot = temp.CreateAlloca(visit(expression.getType()));
            returnSlot->setAlignment(llvm::Align(expression.getType().getAlignOf(m_programInterface)));
            m_builder.CreateLifetimeStart(returnSlot,
                                          m_builder.getInt64(expression.getType().getSizeOf(m_programInterface)));
        }

        for (auto iter = call.getArgumentExpressions().begin(); iter != call.getArgumentExpressions().end(); iter++)
        {
            const std::size_t currentIndex = iter - call.getArgumentExpressions().begin();
            auto* value = visit(*iter);
            if (std::holds_alternative<ABITransformations::Change>(transformation->second.arguments[currentIndex]))
            {
                llvmFnI++;
                auto change = cld::get<ABITransformations::Change>(transformation->second.arguments[currentIndex]);
                if (change == ABITransformations::Unchanged)
                {
                    arguments.emplace_back(value);
                    continue;
                }
                if (change == ABITransformations::OnStack)
                {
                    // structs rvalues don't exist in LLVM IR so this should be sound?
                    auto* load = llvm::cast<llvm::LoadInst>(value);
                    arguments.emplace_back(load->getPointerOperand());
                    load->eraseFromParent();
                    continue;
                }
                if (change == ABITransformations::PointerToTemporary)
                {
                    llvm::IRBuilder<> temp(&m_builder.GetInsertBlock()->getParent()->getEntryBlock(),
                                           m_builder.GetInsertBlock()->getParent()->getEntryBlock().begin());
                    auto* ret = temp.CreateAlloca(value->getType());
                    ret->setAlignment(llvm::Align(iter->getType().getAlignOf(m_programInterface)));
                    m_builder.CreateLifetimeStart(ret,
                                                  m_builder.getInt64(iter->getType().getSizeOf(m_programInterface)));
                    if (value->getType()->isX86_FP80Ty())
                    {
                        m_builder.CreateStore(value, ret);
                    }
                    else
                    {
                        auto* load = llvm::cast<llvm::LoadInst>(value);
                        m_builder.CreateMemCpy(ret, llvm::MaybeAlign(), load->getPointerOperand(), llvm::MaybeAlign(),
                                               iter->getType().getSizeOf(m_programInterface));
                        load->eraseFromParent();
                    }
                    arguments.emplace_back(ret);
                    continue;
                }
                // Integer register
                auto* load = llvm::cast<llvm::LoadInst>(value);
                auto* integer = m_builder.CreateBitCast(
                    load->getPointerOperand(), llvm::PointerType::getUnqual(m_builder.getIntNTy(
                                                   m_module.getDataLayout().getTypeAllocSizeInBits(load->getType()))));
                arguments.emplace_back(m_builder.CreateLoad(integer, load->isVolatile()));
                load->eraseFromParent();
                continue;
            }
            auto* load = llvm::cast<llvm::LoadInst>(value);
            auto& multiIndex =
                cld::get<ABITransformations::MultipleArgs>(transformation->second.arguments[currentIndex]);
            if (multiIndex.size == 1)
            {
                auto* paramType = ft->getParamType(llvmFnI);
                auto* cast =
                    m_builder.CreateBitCast(load->getPointerOperand(), llvm::PointerType::getUnqual(paramType));
                arguments.emplace_back(m_builder.CreateLoad(cast, load->isVolatile()));
            }
            else
            {
                CLD_ASSERT(multiIndex.size == 2);
                auto* firstType = ft->getParamType(llvmFnI);
                auto* secondType = ft->getParamType(llvmFnI + 1);
                auto* cast =
                    m_builder.CreateBitCast(load->getPointerOperand(),
                                            llvm::PointerType::getUnqual(llvm::StructType::get(firstType, secondType)));
                auto* firstValue = m_builder.CreateInBoundsGEP(cast, {m_builder.getInt64(0), m_builder.getInt32(0)});
                auto* secondValue = m_builder.CreateInBoundsGEP(cast, {m_builder.getInt64(0), m_builder.getInt32(1)});
                arguments.emplace_back(m_builder.CreateLoad(firstValue, load->isVolatile()));
                arguments.emplace_back(m_builder.CreateLoad(secondValue, load->isVolatile()));
            }
            load->eraseFromParent();
            llvmFnI += multiIndex.size;
        }
        if (isKandR)
        {
            function = m_builder.CreateBitCast(function, llvm::PointerType::getUnqual(ft));
        }
        auto* result = m_builder.CreateCall(
            isKandR ? ft : llvm::cast<llvm::FunctionType>(function->getType()->getPointerElementType()), function,
            arguments);
        applyFunctionAttributes(*result, ft, cldFt);
        switch (transformation->second.returnType)
        {
            case ABITransformations::Unchanged: return result;
            case ABITransformations::PointerToTemporary:
            {
                return m_builder.CreateLoad(returnSlot);
            }
            case ABITransformations::IntegerRegister:
            case ABITransformations::Flattened:
            {
                auto* cast = m_builder.CreateBitCast(returnSlot, llvm::PointerType::getUnqual(ft->getReturnType()));
                m_builder.CreateStore(result, cast);
                return m_builder.CreateLoad(returnSlot);
            }
            default: CLD_UNREACHABLE;
        }
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression,
                       const cld::Semantics::CompoundLiteral& compoundLiteral)
    {
    }
};
} // namespace

void cld::CGLLVM::generateLLVM(llvm::Module& module, const Semantics::Program& program, Triple triple)
{
    CodeGenerator codeGenerator(module, program, program.getSourceObject(), triple);
    codeGenerator.visit(program.getTranslationUnit());
}
