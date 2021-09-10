use crate::binding::BoundBinaryExpression;
use crate::binding::BoundBinaryOperatorKind;
use crate::binding::BoundExpression;
use crate::binding::BoundLiteralExpression;
use crate::binding::BoundUnaryExpression;
use crate::binding::BoundUnaryOperatorKind;
use crate::plumbing::Object;

pub struct Evaluator {
    root: Box<BoundExpression>,
}

impl Evaluator {
    pub fn new(root: Box<BoundExpression>) -> Self {
        Self { root }
    }

    pub fn evaluate(&self) -> Object {
        self.evaluate_expression(&self.root)
    }

    fn evaluate_expression(&self, expr: &BoundExpression) -> Object {
        match expr {
            BoundExpression::Binary(e) => self.evaluate_binary_expression(e),
            BoundExpression::Unary(e) => self.evaluate_unary_expression(e),
            BoundExpression::Literal(e) => self.evaluate_literal_expression(e),
        }
    }

    fn evaluate_binary_expression(&self, e: &BoundBinaryExpression) -> Object {
        let left = self.evaluate_expression(&e.left);
        let right = self.evaluate_expression(&e.right);
        match e.operator.kind {
            BoundBinaryOperatorKind::Addition => {
                Object::Number(left.as_number() + right.as_number())
            }
            BoundBinaryOperatorKind::Subtraction => {
                Object::Number(left.as_number() - right.as_number())
            }
            BoundBinaryOperatorKind::Multiplication => {
                Object::Number(left.as_number() * right.as_number())
            }
            BoundBinaryOperatorKind::Division => {
                Object::Number(left.as_number() / right.as_number())
            }
            BoundBinaryOperatorKind::LogicalAnd => {
                Object::Boolean(left.as_boolean() && right.as_boolean())
            }
            BoundBinaryOperatorKind::LogicalOr => {
                Object::Boolean(left.as_boolean() || right.as_boolean())
            }
            BoundBinaryOperatorKind::Equality => Object::Boolean(left == right),
            BoundBinaryOperatorKind::Inequality => Object::Boolean(left != right),
        }
    }

    fn evaluate_unary_expression(&self, e: &BoundUnaryExpression) -> Object {
        let operand = self.evaluate_expression(&e.operand);
        match e.operator.kind {
            BoundUnaryOperatorKind::Identity => operand,
            BoundUnaryOperatorKind::Negation => Object::Number(-operand.as_number()),
            BoundUnaryOperatorKind::LogicalNegation => Object::Boolean(!operand.as_boolean()),
        }
    }

    fn evaluate_literal_expression(&self, e: &BoundLiteralExpression) -> Object {
        e.value.clone()
    }
}
