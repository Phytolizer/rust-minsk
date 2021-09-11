use std::collections::HashMap;

use crate::binding::BoundBinaryExpression;
use crate::binding::BoundBinaryOperatorKind;
use crate::binding::BoundExpression;
use crate::binding::BoundLiteralExpression;
use crate::binding::BoundUnaryExpression;
use crate::binding::BoundUnaryOperatorKind;
use crate::plumbing::Object;

pub(crate) struct Evaluator<'v> {
    variables: &'v mut HashMap<String, Object>,
}

impl<'v> Evaluator<'v> {
    pub(crate) fn new(variables: &'v mut HashMap<String, Object>) -> Self {
        Self { variables }
    }

    pub(crate) fn evaluate(&mut self, root: &BoundExpression) -> Object {
        self.evaluate_expression(root)
    }

    fn evaluate_expression(&mut self, expr: &BoundExpression) -> Object {
        match expr {
            BoundExpression::Binary(e) => self.evaluate_binary_expression(e),
            BoundExpression::Unary(e) => self.evaluate_unary_expression(e),
            BoundExpression::Literal(e) => self.evaluate_literal_expression(e),
            BoundExpression::Variable(e) => self.evaluate_variable_expression(e),
            BoundExpression::Assignment(e) => self.evaluate_assignment_expression(e),
        }
    }

    fn evaluate_binary_expression(&mut self, e: &BoundBinaryExpression) -> Object {
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

    fn evaluate_unary_expression(&mut self, e: &BoundUnaryExpression) -> Object {
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

    fn evaluate_variable_expression(&self, e: &crate::binding::BoundVariableExpression) -> Object {
        let value = self.variables.get(&e.name).unwrap();
        value.clone()
    }

    fn evaluate_assignment_expression(
        &mut self,
        e: &crate::binding::BoundAssignmentExpression,
    ) -> Object {
        let value = self.evaluate_expression(&e.expression);
        self.variables.insert(e.name.clone(), value.clone());
        value
    }
}
