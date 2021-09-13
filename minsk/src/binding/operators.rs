use crate::plumbing::ObjectKind;
use crate::syntax::SyntaxKind;

use super::BoundBinaryOperatorKind;
use super::BoundUnaryOperatorKind;

#[derive(Debug, Clone)]
pub(crate) struct BoundBinaryOperator {
    syntax_kind: SyntaxKind,
    pub(crate) kind: BoundBinaryOperatorKind,
    left_type: ObjectKind,
    right_type: ObjectKind,
    pub(super) result_type: ObjectKind,
}

const BINARY_OPERATORS: &[BoundBinaryOperator] = &[
    BoundBinaryOperator::new(
        SyntaxKind::PlusToken,
        BoundBinaryOperatorKind::Addition,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::MinusToken,
        BoundBinaryOperatorKind::Subtraction,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::StarToken,
        BoundBinaryOperatorKind::Multiplication,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::SlashToken,
        BoundBinaryOperatorKind::Division,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::AmpersandAmpersandToken,
        BoundBinaryOperatorKind::LogicalAnd,
        ObjectKind::Boolean,
        ObjectKind::Boolean,
        ObjectKind::Boolean,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::PipePipeToken,
        BoundBinaryOperatorKind::LogicalOr,
        ObjectKind::Boolean,
        ObjectKind::Boolean,
        ObjectKind::Boolean,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::EqualsEqualsToken,
        BoundBinaryOperatorKind::Equality,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Boolean,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::EqualsEqualsToken,
        BoundBinaryOperatorKind::Equality,
        ObjectKind::Boolean,
        ObjectKind::Boolean,
        ObjectKind::Boolean,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::BangEqualsToken,
        BoundBinaryOperatorKind::Inequality,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Boolean,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::BangEqualsToken,
        BoundBinaryOperatorKind::Inequality,
        ObjectKind::Boolean,
        ObjectKind::Boolean,
        ObjectKind::Boolean,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::LessToken,
        BoundBinaryOperatorKind::Less,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Boolean,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::LessOrEqualsToken,
        BoundBinaryOperatorKind::LessOrEquals,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Boolean,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::GreaterToken,
        BoundBinaryOperatorKind::Greater,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Boolean,
    ),
    BoundBinaryOperator::new(
        SyntaxKind::GreaterOrEqualsToken,
        BoundBinaryOperatorKind::GreaterOrEquals,
        ObjectKind::Number,
        ObjectKind::Number,
        ObjectKind::Boolean,
    ),
];

impl BoundBinaryOperator {
    const fn new(
        syntax_kind: SyntaxKind,
        kind: BoundBinaryOperatorKind,
        left_type: ObjectKind,
        right_type: ObjectKind,
        result_type: ObjectKind,
    ) -> Self {
        Self {
            syntax_kind,
            kind,
            left_type,
            right_type,
            result_type,
        }
    }

    pub(super) fn bind(
        syntax_kind: SyntaxKind,
        left_type: ObjectKind,
        right_type: ObjectKind,
    ) -> Option<&'static BoundBinaryOperator> {
        for op in BINARY_OPERATORS {
            if op.syntax_kind == syntax_kind
                && op.left_type == left_type
                && op.right_type == right_type
            {
                return Some(op);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BoundUnaryOperator {
    syntax_kind: SyntaxKind,
    pub(crate) kind: BoundUnaryOperatorKind,
    operand_type: ObjectKind,
    pub(super) result_type: ObjectKind,
}

const UNARY_OPERATORS: &[BoundUnaryOperator] = &[
    BoundUnaryOperator::new(
        SyntaxKind::PlusToken,
        BoundUnaryOperatorKind::Identity,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundUnaryOperator::new(
        SyntaxKind::MinusToken,
        BoundUnaryOperatorKind::Negation,
        ObjectKind::Number,
        ObjectKind::Number,
    ),
    BoundUnaryOperator::new(
        SyntaxKind::BangToken,
        BoundUnaryOperatorKind::LogicalNegation,
        ObjectKind::Boolean,
        ObjectKind::Boolean,
    ),
];

impl BoundUnaryOperator {
    const fn new(
        syntax_kind: SyntaxKind,
        kind: BoundUnaryOperatorKind,
        operand_type: ObjectKind,
        result_type: ObjectKind,
    ) -> Self {
        Self {
            syntax_kind,
            kind,
            operand_type,
            result_type,
        }
    }

    pub(super) fn bind(
        syntax_kind: SyntaxKind,
        operand_type: ObjectKind,
    ) -> Option<&'static BoundUnaryOperator> {
        for operator in UNARY_OPERATORS {
            if operator.syntax_kind == syntax_kind && operator.operand_type == operand_type {
                return Some(operator);
            }
        }
        None
    }
}
