#include "Codegen.hpp"

#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>

#include <Frontend/Common/Filesystem.hpp>
#include <Frontend/Compiler/Program.hpp>

namespace std
{
template <>
struct hash<const cld::Semantics::StructType*>
{
    std::size_t operator()(const cld::Semantics::StructType* structType)
    {
        return cld::hashCombine(structType->getScopeOrId(), structType->getName());
    }
};

template <>
struct hash<const cld::Semantics::UnionType*>
{
    std::size_t operator()(const cld::Semantics::UnionType* unionType)
    {
        return cld::hashCombine(unionType->getScopeOrId(), unionType->getName());
    }
};

template <>
struct hash<const cld::Semantics::AnonymousStructType*>
{
    std::size_t operator()(const cld::Semantics::AnonymousStructType* structType)
    {
        return std::hash<std::uint64_t>{}(structType->getId());
    }
};

template <>
struct hash<const cld::Semantics::AnonymousUnionType*>
{
    std::size_t operator()(const cld::Semantics::AnonymousUnionType* unionType)
    {
        return std::hash<std::uint64_t>{}(unionType->getId());
    }
};
} // namespace std

namespace
{
class CodeGenerator final
{
    llvm::Module& m_module;
    const cld::Semantics::ProgramInterface& m_programInterface;
    const cld::SourceInterface& m_sourceInterface;
    // void* for now although strictly speaking the pointers can only be const Declaration* or const FunctionDefinition*
    // but hashing the variant seems overkill? I am not sure
    std::unordered_map<const void*, llvm::Value * CLD_NON_NULL> m_lvalues;
    std::unordered_map<
        std::variant<const cld::Semantics::StructType * CLD_NON_NULL, const cld::Semantics::UnionType * CLD_NON_NULL,
                     const cld::Semantics::AnonymousUnionType * CLD_NON_NULL,
                     const cld::Semantics::AnonymousStructType * CLD_NON_NULL>,
        llvm::Type*>
        m_types;
    llvm::IRBuilder<> m_builder{m_module.getContext()};
    llvm::DIBuilder m_debugInfo{m_module};

public:
    explicit CodeGenerator(llvm::Module& module, const cld::Semantics::ProgramInterface& programInterface,
                           const cld::SourceInterface& sourceInterface)
        : m_module(module), m_programInterface(programInterface), m_sourceInterface(sourceInterface)
    {
        auto fullPath = cld::fs::u8path(m_sourceInterface.getFiles()[1].path);
        module.setSourceFileName(fullPath.filename().u8string());
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
                // TODO: Calling convention
                auto* returnType = visit(functionType.getReturnType());
                std::vector<llvm::Type*> args;
                for (auto& [type, name] : functionType.getArguments())
                {
                    (void)name;
                    args.push_back(visit(cld::Semantics::adjustParameterType(type)));
                }
                return llvm::FunctionType::get(returnType, args, functionType.isLastVararg());
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
                auto result = m_types.find(&structType);
                if (result != m_types.end())
                {
                    return result->second;
                }
                auto* structDef =
                    m_programInterface.getStructDefinition(structType.getName(), structType.getScopeOrId());
                if (!structDef)
                {
                    auto* type = llvm::StructType::get(m_module.getContext());
                    m_types.insert({&structType, type});
                    return type;
                }
                std::vector<llvm::Type*> fields;
                for (auto& iter : structDef->getLayout())
                {
                    fields.push_back(visit(iter));
                }
                auto* type = llvm::StructType::get(m_module.getContext(), fields);
                m_types.insert({&structType, type});
                return type;
            },
            [&](const cld::Semantics::UnionType& unionType) -> llvm::Type* {
                auto result = m_types.find(&unionType);
                if (result != m_types.end())
                {
                    return result->second;
                }
                auto* unionDef = m_programInterface.getUnionDefinition(unionType.getName(), unionType.getScopeOrId());
                if (!unionDef)
                {
                    auto* type = llvm::StructType::get(m_module.getContext());
                    m_types.insert({&unionType, type});
                    return type;
                }
                auto largestField = std::max_element(
                    unionDef->getFields().begin(), unionDef->getFields().end(),
                    [&](const cld::Semantics::Field& lhs, const cld::Semantics::Field& rhs) {
                        return lhs.type->getSizeOf(m_programInterface) < rhs.type->getSizeOf(m_programInterface);
                    });
                auto* type = llvm::StructType::get(m_module.getContext(), llvm::ArrayRef(visit(*largestField->type)));
                m_types.insert({&unionType, type});
                return type;
            },
            [&](const cld::Semantics::AnonymousStructType& structType) -> llvm::Type* {
                auto result = m_types.find(&structType);
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
                m_types.insert({&structType, type});
                return type;
            },
            [&](const cld::Semantics::AnonymousUnionType& unionType) -> llvm::Type* {
                auto result = m_types.find(&unionType);
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
                m_types.insert({&unionType, type});
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
        // TODO: params

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

        visit(functionDefinition.getCompoundStatement());
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
            statement, [](const auto&) { CLD_UNREACHABLE; },
            [&](const cld::Semantics::ExpressionStatement& expressionStatement) {
                if (!expressionStatement.getExpression())
                {
                    return;
                }
                visit(*expressionStatement.getExpression());
            },
            [&](const cld::Semantics::ReturnStatement& returnStatement) {
                if (!returnStatement.getExpression())
                {
                    m_builder.CreateRetVoid();
                }
                else
                {
                    m_builder.CreateRet(visit(*returnStatement.getExpression()));
                }
            });
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
                             conversion.getExpression().getType().get()))
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

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::MemberAccess& constant) {}

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
                        m_builder.CreateStore(value, result);
                    }
                    else
                    {
                        auto* result = m_builder.CreateAdd(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(value, result);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().get()))
                {
                    auto* result = m_builder.CreateFAdd(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(value, result);
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
                        m_builder.CreateStore(value, result);
                    }
                    else
                    {
                        auto* result = m_builder.CreateSub(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(value, result);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().get()))
                {
                    auto* result = m_builder.CreateFSub(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(value, result);
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
                        m_builder.CreateStore(value, result);
                    }
                    else
                    {
                        result = m_builder.CreateAdd(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(value, result);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().get()))
                {
                    result = m_builder.CreateFAdd(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(value, result);
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
                        m_builder.CreateStore(value, result);
                    }
                    else
                    {
                        result = m_builder.CreateSub(prev, llvm::ConstantInt::get(prev->getType(), 1));
                        m_builder.CreateStore(value, result);
                    }
                }
                else if (!std::holds_alternative<cld::Semantics::PointerType>(
                             unaryOperator.getOperand().getType().get()))
                {
                    result = m_builder.CreateFSub(prev, llvm::ConstantFP::get(prev->getType(), 1));
                    m_builder.CreateStore(value, result);
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
            case cld::Semantics::UnaryOperator::BooleanNegate: break;
            case cld::Semantics::UnaryOperator::BitwiseNegate: return m_builder.CreateNot(value);
        }
        CLD_UNREACHABLE;
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::SizeofOperator& constant) {}

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::SubscriptOperator& constant)
    {
    }

    llvm::Value* visit(const cld::Semantics::Expression& expression, const cld::Semantics::Conditional& constant) {}

    llvm::Value* visit(const cld::Semantics::Expression&, const cld::Semantics::Assignment& assignment)
    {
        auto* lhs = visit(assignment.getLeftExpression());
        auto* rhs = visit(assignment.getRightExpression());
        // TODO: Special cases
        m_builder.CreateStore(rhs, lhs, assignment.getLeftExpression().getType().isVolatile());
        return m_builder.CreateLoad(lhs->getType()->getPointerElementType(), lhs,
                                    assignment.getLeftExpression().getType().isVolatile());
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

void cld::CGLLVM::generateLLVM(llvm::Module& module, const Semantics::Program& program)
{
    CodeGenerator codeGenerator(module, program, program.getSourceObject());
    codeGenerator.visit(program.getTranslationUnit());
}
