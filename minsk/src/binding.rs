use crate::plumbing::Object;
use crate::plumbing::ObjectKind;
use crate::syntax::BinaryExpressionSyntax;
use crate::syntax::ExpressionSyntaxRef;
use crate::syntax::UnaryExpressionSyntax;

use self::operators::BoundBinaryOperator;
use self::operators::BoundUnaryOperator;

mod operators;

pub(crate) enum BoundNodeKind {
    BinaryExpression,
    UnaryExpression,
    LiteralExpression,
}

pub(crate) enum BoundNode {
    Expression(BoundExpression),
}

impl BoundNode {
    pub(crate) fn kind(&self) -> BoundNodeKind {
        match self {
            BoundNode::Expression(e) => e.kind(),
        }
    }
}

pub(crate) enum BoundExpression {
    Binary(BoundBinaryExpression),
    Unary(BoundUnaryExpression),
    Literal(BoundLiteralExpression),
}

impl BoundExpression {
    pub(crate) fn kind(&self) -> BoundNodeKind {
        match self {
            BoundExpression::Binary(_) => BoundNodeKind::BinaryExpression,
            BoundExpression::Unary(_) => BoundNodeKind::UnaryExpression,
            BoundExpression::Literal(_) => BoundNodeKind::LiteralExpression,
        }
    }

    pub(crate) fn get_type(&self) -> ObjectKind {
        match self {
            BoundExpression::Binary(e) => e.operator.result_type,
            BoundExpression::Unary(e) => e.operator.result_type,
            BoundExpression::Literal(e) => e.value.kind(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundBinaryOperatorKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LogicalAnd,
    LogicalOr,
    Equality,
    Inequality,
}

pub struct BoundBinaryExpression {
    pub(crate) left: Box<BoundExpression>,
    pub(crate) operator: &'static BoundBinaryOperator,
    pub(crate) right: Box<BoundExpression>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundUnaryOperatorKind {
    Identity,
    Negation,
    LogicalNegation,
}

pub(crate) struct BoundUnaryExpression {
    pub(crate) operator: &'static BoundUnaryOperator,
    pub(crate) operand: Box<BoundExpression>,
}

pub(crate) struct BoundLiteralExpression {
    pub(crate) value: Object,
}

pub(crate) struct Binder {
    pub(crate) diagnostics: Vec<String>,
}

impl Binder {
    pub(crate) fn bind_expression(&mut self, expression: ExpressionSyntaxRef) -> Box<BoundExpression> {
        match expression {
            ExpressionSyntaxRef::Binary(e) => self.bind_binary_expression(e),
            ExpressionSyntaxRef::Unary(e) => self.bind_unary_expression(e),
            ExpressionSyntaxRef::Literal(e) => self.bind_literal_expression(e),
            ExpressionSyntaxRef::Parenthesized(e) => self.bind_parenthesized_expression(e),
        }
    }

    fn bind_binary_expression(&mut self, e: &BinaryExpressionSyntax) -> Box<BoundExpression> {
        let left = self.bind_expression(e.left.create_ref());
        let right = self.bind_expression(e.right.create_ref());
        let operator =
            BoundBinaryOperator::bind(e.operator_token.kind, left.get_type(), right.get_type());
        if let Some(operator) = operator {
            Box::new(BoundExpression::Binary(BoundBinaryExpression {
                left,
                operator,
                right,
            }))
        } else {
            self.diagnostics.push(format!(
                "Binary operator '{}' is not defined for types {:?} and {:?}.",
                e.operator_token.text,
                left.get_type(),
                right.get_type()
            ));
            left
        }
    }

    fn bind_unary_expression(&mut self, e: &UnaryExpressionSyntax) -> Box<BoundExpression> {
        let operand = self.bind_expression(e.operand.create_ref());
        let operator = BoundUnaryOperator::bind(e.operator_token.kind, operand.get_type());
        if let Some(operator) = operator {
            Box::new(BoundExpression::Unary(BoundUnaryExpression {
                operator,
                operand,
            }))
        } else {
            self.diagnostics.push(format!(
                "Unary operator '{}' is not defined for type {:?}",
                e.operator_token.text,
                operand.get_type()
            ));
            operand
        }
    }

    fn bind_literal_expression(
        &self,
        e: &crate::syntax::LiteralExpressionSyntax,
    ) -> Box<BoundExpression> {
        Box::new(BoundExpression::Literal(BoundLiteralExpression {
            value: e.value.clone(),
        }))
    }

    fn bind_parenthesized_expression(
        &mut self,
        e: &crate::syntax::ParenthesizedExpressionSyntax,
    ) -> Box<BoundExpression> {
        self.bind_expression(e.expression.create_ref())
    }

    pub(crate) fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }
}

impl Default for Binder {
    fn default() -> Self {
        Self::new()
    }
}
