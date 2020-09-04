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
        };
        Change returnType;
        std::vector<Change> arguments;
    };

    std::unordered_map<const llvm::FunctionType*, ABITransformations> m_functionABITransformations;
    llvm::IRBuilder<> m_builder{m_module.getContext()};
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
            constexpr std::uint8_t availableIntegerRegisters = 4;
            constexpr std::uint8_t availableFloatingPointRegisters = 8;
            std::uint8_t takenIntegerRegisters = 0;
            std::uint8_t takenFloatingPointRegisters = 0;
            for (auto iter = arguments.begin(); iter != arguments.end(); iter++)
            {
                std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(returnType);
                if (size > 128)
                {
                    transformations.arguments[iter - arguments.begin()] = ABITransformations::OnStack;
                    continue;
                }
                auto flat = flatten(*iter);
            }
            if (returnType->isVoidTy())
            {
                return transformations;
            }
            else if (!returnType->isStructTy())
            {
                return transformations;
            }

            std::uint32_t size = m_module.getDataLayout().getTypeAllocSizeInBits(returnType);
            if (m_module.getDataLayout().getLargestLegalIntTypeSizeInBits() < size)
            {
                transformations.returnType = ABITransformations::PointerToTemporary;
                arguments.insert(arguments.begin(), llvm::PointerType::getUnqual(returnType));
                returnType = m_builder.getVoidTy();
            }
        }
        return transformations;
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

    void visit(const cld::Semantics::Declaration& declaration)
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
            m_lvalues.emplace(&declaration, function);
            return;
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
            return;
        }
        auto* var = m_builder.CreateAlloca(type);
        var->setAlignment(llvm::Align(declaration.getType().getAlignOf(m_programInterface)));
        m_lvalues.emplace(&declaration, var);
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
        auto transformations = m_functionABITransformations.find(function->getFunctionType());
        CLD_ASSERT(transformations != m_functionABITransformations.end());
        std::size_t argStart = 0;
        if (transformations->second.returnType == ABITransformations::PointerToTemporary)
        {
            function->addAttribute(1, llvm::Attribute::StructRet);
            function->addAttribute(1, llvm::Attribute::NoAlias);
            argStart = 1;
        }
        for (std::size_t i = argStart; i < function->arg_size(); i++)
        {
            if (transformations->second.arguments[i - argStart] == ABITransformations::PointerToTemporary)
            {
                continue;
            }
            auto& paramDecl = functionDefinition.getParameterDeclarations()[i - argStart];
            auto* operand = function->getArg(i);
            if (transformations->second.arguments[i - argStart] == ABITransformations::Unchanged)
            {
                auto* var = m_builder.CreateAlloca(operand->getType());
                var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
                m_lvalues.emplace(paramDecl.get(), var);
                continue;
            }
            auto* var = m_builder.CreateAlloca(visit(paramDecl->getType()));
            var->setAlignment(llvm::Align(paramDecl->getType().getAlignOf(m_programInterface)));
            m_lvalues.emplace(paramDecl.get(), var);
        }

        cld::YComb{[&](auto&& self, std::int64_t scope) -> void {
            auto& decls = m_programInterface.getScopes()[scope];
            for (auto& [name, decl] : decls.declarations)
            {
                if (std::holds_alternative<const cld::Semantics::Declaration*>(decl.declared))
                {
                    auto& var = *cld::get<const cld::Semantics::Declaration*>(decl.declared);
                    if (cld::Semantics::isVariablyModified(var.getType()))
                    {
                        continue;
                    }
                    visit(var);
                }
            }
            for (auto& subScope : decls.subScopes)
            {
                self(subScope);
            }
        }}(functionDefinition.getCompoundStatement().getScope());

        for (std::size_t i = argStart; i < function->arg_size(); i++)
        {
            if (transformations->second.arguments[i - argStart] == ABITransformations::PointerToTemporary)
            {
                continue;
            }
            auto& paramDecl = functionDefinition.getParameterDeclarations()[i - argStart];
            auto* operand = function->getArg(i);
            auto* alloc = m_lvalues[paramDecl.get()];
            if (transformations->second.arguments[i - argStart] == ABITransformations::Unchanged)
            {
                m_builder.CreateStore(operand, alloc, paramDecl->getType().isVolatile());
                continue;
            }
            auto* cast = m_builder.CreateBitCast(alloc, llvm::PointerType::getUnqual(operand->getType()));
            m_builder.CreateAlignedStore(operand, cast, llvm::cast<llvm::AllocaInst>(alloc)->getAlign(),
                                         paramDecl->getType().isVolatile());
        }

        visit(functionDefinition.getCompoundStatement());
        m_builder.ClearInsertionPoint();
    }

    void visit(const cld::Semantics::CompoundStatement& compoundStatement)
    {
        for (auto& [name, decl] : m_programInterface.getScopes()[compoundStatement.getScope()].declarations)
        {
            if (std::holds_alternative<const cld::Semantics::Declaration*>(decl.declared))
            {
                auto& var = *cld::get<const cld::Semantics::Declaration*>(decl.declared);
                if (!cld::Semantics::isVariablyModified(var.getType()))
                {
                    continue;
                }
                // TODO: Evaluate val array expression
                // visit(var);
            }
        }
        for (auto& iter : compoundStatement.getCompoundItems())
        {
            if (std::holds_alternative<std::shared_ptr<const cld::Semantics::Expression>>(iter))
            {
                // TODO: Evaluate val array expression
            }
            else if (std::holds_alternative<std::unique_ptr<cld::Semantics::Declaration>>(iter))
            {
                auto& decl = cld::get<std::unique_ptr<cld::Semantics::Declaration>>(iter);
                auto result = m_lvalues.find(decl.get());
                CLD_ASSERT(result != m_lvalues.end());
                if (cld::Semantics::isVariableLengthArray(decl->getType()))
                {
                    m_builder.CreateLifetimeStart(result->second);
                }
                else
                {
                    visit(cld::Semantics::PrimitiveType::createSizeT(false, false,
                                                                     m_programInterface.getLanguageOptions()));
                    auto* size =
                        llvm::ConstantInt::get(m_builder.getInt64Ty(), decl->getType().getSizeOf(m_programInterface));
                    m_builder.CreateLifetimeStart(result->second, llvm::cast<llvm::ConstantInt>(size));
                }
                // TODO: Initializer
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
                visit(*expressionStatement.getExpression());
            });
    }

    void visit(const cld::Semantics::ReturnStatement& returnStatement)
    {
        if (!returnStatement.getExpression())
        {
            m_builder.CreateRetVoid();
        }
        else
        {
            auto* function = m_builder.GetInsertBlock()->getParent();
            auto transformation = m_functionABITransformations.find(function->getFunctionType());
            CLD_ASSERT(transformation != m_functionABITransformations.end());
            if (transformation->second.returnType == ABITransformations::PointerToTemporary)
            {
                auto* value = visit(*returnStatement.getExpression());
                m_builder.CreateStore(value, function->getArg(0));
                m_builder.CreateRetVoid();
            }
            else
            {
                m_builder.CreateRet(visit(*returnStatement.getExpression()));
            }
        }
    }

    void visit(const cld::Semantics::ForStatement& forStatement) {}

    void visit(const cld::Semantics::IfStatement& ifStatement) {}

    void visit(const cld::Semantics::HeadWhileStatement& headWhileStatement) {}

    void visit(const cld::Semantics::FootWhileStatement& footWhileStatement) {}

    void visit(const cld::Semantics::BreakStatement& breakStatement) {}

    void visit(const cld::Semantics::ContinueStatement& continueStatement) {}

    void visit(const cld::Semantics::SwitchStatement& switchStatement) {}

    void visit(const cld::Semantics::DefaultStatement& defaultStatement) {}

    void visit(const cld::Semantics::CaseStatement& caseStatement) {}

    void visit(const cld::Semantics::GotoStatement& gotoStatement) {}

    void visit(const cld::Semantics::LabelStatement& labelStatement) {}

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
                    if (cld::get<cld::Semantics::PrimitiveType>(newType.get()).isSigned())
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
        auto* rhs = visit(binaryExpression.getRightExpression());
        switch (binaryExpression.getKind())
        {
            case cld::Semantics::BinaryOperator::Addition:
            {
                if (cld::Semantics::isArithmetic(binaryExpression.getLeftExpression().getType())
                    && cld::Semantics::isArithmetic(binaryExpression.getLeftExpression().getType()))
                {
                    if (cld::Semantics::isInteger(binaryExpression.getLeftExpression().getType()))
                    {
                        return m_builder.CreateNSWAdd(lhs, rhs);
                    }
                    else
                    {
                        return m_builder.CreateFAdd(lhs, rhs);
                    }
                }
                // TODO: Pointer arithmetic
            }
            case cld::Semantics::BinaryOperator::Subtraction:
            {
                if (cld::Semantics::isArithmetic(binaryExpression.getLeftExpression().getType())
                    && cld::Semantics::isArithmetic(binaryExpression.getLeftExpression().getType()))
                {
                    if (cld::Semantics::isInteger(binaryExpression.getLeftExpression().getType()))
                    {
                        return m_builder.CreateNSWSub(lhs, rhs);
                    }
                    else
                    {
                        return m_builder.CreateFSub(lhs, rhs);
                    }
                }
                // TODO: Pointer arithmetic
            }
            case cld::Semantics::BinaryOperator::Multiply:
            {
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
                    return m_builder.CreateFMul(lhs, rhs);
                }
            }
            case cld::Semantics::BinaryOperator::Modulo:
            {
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
                if (lhs->getType() != rhs->getType())
                {
                    rhs = m_builder.CreateIntCast(
                        rhs, lhs->getType(),
                        cld::get<cld::Semantics::PrimitiveType>(binaryExpression.getRightExpression().getType().get())
                            .isSigned());
                }
                return m_builder.CreateShl(lhs, rhs, "", false, true);
            }
            case cld::Semantics::BinaryOperator::RightShift:
            {
                if (lhs->getType() != rhs->getType())
                {
                    rhs = m_builder.CreateIntCast(
                        rhs, lhs->getType(),
                        cld::get<cld::Semantics::PrimitiveType>(binaryExpression.getRightExpression().getType().get())
                            .isSigned());
                }
                return m_builder.CreateAShr(lhs, rhs);
            }
            case cld::Semantics::BinaryOperator::LessThan: break;
            case cld::Semantics::BinaryOperator::GreaterThan: break;
            case cld::Semantics::BinaryOperator::LessOrEqual: break;
            case cld::Semantics::BinaryOperator::GreaterOrEqual: break;
            case cld::Semantics::BinaryOperator::Equal: break;
            case cld::Semantics::BinaryOperator::NotEqual: break;
            case cld::Semantics::BinaryOperator::BitOr: return m_builder.CreateOr(lhs, rhs);
            case cld::Semantics::BinaryOperator::BitAnd: return m_builder.CreateAnd(lhs, rhs);
            case cld::Semantics::BinaryOperator::BitXor: return m_builder.CreateXor(lhs, rhs);
            case cld::Semantics::BinaryOperator::LogicAnd: break;
            case cld::Semantics::BinaryOperator::LogicOr: break;
        }
        CLD_UNREACHABLE;
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::Cast& constant) {}

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
                    // TODO:
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
                    // TODO:
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
                    // TODO:
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
                    // TODO:
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
                auto* boolean = toBool(value);
                boolean = m_builder.CreateNot(boolean);
                return m_builder.CreateZExt(boolean, visit(cld::Semantics::PrimitiveType::createInt(
                                                         false, false, m_sourceInterface.getLanguageOptions())));
            }
            case cld::Semantics::UnaryOperator::BitwiseNegate: return m_builder.CreateNot(value);
        }
        CLD_UNREACHABLE;
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::SizeofOperator& constant) {}

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

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::Conditional& constant) {}

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::Assignment& assignment)
    {
        // TODO: Special cases
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
            visit(iter.first);
        }
        return visit(commaExpression.getLastExpression());
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::CallExpression& constant) {}

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::CompoundLiteral& constant) {}
};
} // namespace

void cld::CGLLVM::generateLLVM(llvm::Module& module, const Semantics::Program& program, Triple triple)
{
    CodeGenerator codeGenerator(module, program, program.getSourceObject(), triple);
    codeGenerator.visit(program.getTranslationUnit());
}
