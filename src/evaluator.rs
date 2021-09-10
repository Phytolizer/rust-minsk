use crate::binding::BoundBinaryExpression;
use crate::binding::BoundBinaryOperatorKind;
use crate::binding::BoundExpression;
use crate::binding::BoundLiteralExpression;
use crate::binding::BoundUnaryExpression;
use crate::binding::BoundUnaryOperatorKind;
use crate::plumbing::Object;

pub(crate) struct Evaluator {
    root: Box<BoundExpression>,
}

impl Evaluator {
    pub(crate) fn new(root: Box<BoundExpression>) -> Self {
        Self { root }
    }

    pub(crate) fn evaluate(&self) -> i64 {
        self.evaluate_expression(&self.root)
    }

    fn evaluate_expression(&self, expr: &BoundExpression) -> i64 {
        match expr {
            BoundExpression::Binary(e) => self.evaluate_binary_expression(e),
            BoundExpression::Unary(e) => self.evaluate_unary_expression(e),
            BoundExpression::Literal(e) => self.evaluate_literal_expression(e),
        }
    }

    fn evaluate_binary_expression(&self, e: &BoundBinaryExpression) -> i64 {
        let left = self.evaluate_expression(&e.left);
        let right = self.evaluate_expression(&e.right);
        match e.operator_kind {
            BoundBinaryOperatorKind::Addition => left + right,
            BoundBinaryOperatorKind::Subtraction => left - right,
            BoundBinaryOperatorKind::Multiplication => left * right,
            BoundBinaryOperatorKind::Division => left / right,
        }
    }

    fn evaluate_unary_expression(&self, e: &BoundUnaryExpression) -> i64 {
        let operand = self.evaluate_expression(&e.operand);
        match e.operator_kind {
            BoundUnaryOperatorKind::Identity => operand,
            BoundUnaryOperatorKind::Negation => -operand,
        }
    }

    fn evaluate_literal_expression(&self, e: &BoundLiteralExpression) -> i64 {
        match e.value {
            Object::Number(n) => n,
            _ => panic!("No value for literal token"),
        }
    }
}
