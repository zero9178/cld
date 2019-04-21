#ifndef OPENCLPARSER_CONSTANTEVALUATOR_HPP
#define OPENCLPARSER_CONSTANTEVALUATOR_HPP

#include <map>
#include "Syntax.hpp"
#include "Expected.hpp"
#include "FailureReason.hpp"

namespace OpenCL::Codegen
{
    using ConstRetType = OpenCL::Expected<std::variant<std::int32_t,
                                      std::uint32_t,
                                      std::int64_t,
                                      std::uint64_t,
                                      float,
                                      double,
                                      void*>,OpenCL::FailureReason>;

    class ConstantEvaluator final : public OpenCL::Syntax::NodeVisitor<ConstRetType>
    {
        const std::map<std::string,const OpenCL::Syntax::StructOrUnionSpecifier*>& m_structOrUnions;

    public:

        using OpenCL::Syntax::NodeVisitor<ConstRetType>::visit;

        explicit ConstantEvaluator(const std::map<std::string, const Syntax::StructOrUnionSpecifier*>& structOrUnions = {});

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::Expression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::AssignmentExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::PrimaryExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::PrimaryExpressionConstant& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::PrimaryExpressionParenthese& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::PostFixExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::PostFixExpressionPrimaryExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::PostFixExpressionSubscript& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::PostFixExpressionDot& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::PostFixExpressionArrow& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::UnaryExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::UnaryExpressionPostFixExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::UnaryExpressionUnaryOperator& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::UnaryExpressionSizeOf& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::CastExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::Term& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::AdditiveExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::ShiftExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::RelationalExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::EqualityExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::BitAndExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::BitXorExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::BitOrExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::LogicalAndExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::LogicalOrExpression& node) override;

        Syntax::StrongTypedef<ConstRetType>& visit(const Syntax::ConditionalExpression& node) override;
    };
}

#endif //OPENCLPARSER_CONSTANTEVALUATOR_HPP
