use std::collections::HashMap;

use crate::binding::BoundBinaryExpression;
use crate::binding::BoundBinaryOperatorKind;
use crate::binding::BoundBlockStatement;
use crate::binding::BoundExpression;
use crate::binding::BoundExpressionStatement;
use crate::binding::BoundLiteralExpression;
use crate::binding::BoundStatement;
use crate::binding::BoundUnaryExpression;
use crate::binding::BoundUnaryOperatorKind;
use crate::binding::BoundVariableDeclarationStatement;
use crate::plumbing::Object;
use crate::text::VariableSymbol;

pub(crate) struct Evaluator<'v> {
    variables: &'v mut HashMap<VariableSymbol, Object>,
    last_value: Object,
}

impl<'v> Evaluator<'v> {
    pub(crate) fn evaluate(&mut self, root: &BoundStatement) -> Object {
        self.evaluate_statement(root);
        self.last_value.clone()
    }

    fn evaluate_assignment_expression(
        &mut self,
        e: &crate::binding::BoundAssignmentExpression,
    ) -> Object {
        let value = self.evaluate_expression(&e.expression);
        self.variables.insert(e.variable.clone(), value.clone());
        value
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

    fn evaluate_expression(&mut self, expr: &BoundExpression) -> Object {
        match expr {
            BoundExpression::Binary(e) => self.evaluate_binary_expression(e),
            BoundExpression::Unary(e) => self.evaluate_unary_expression(e),
            BoundExpression::Literal(e) => self.evaluate_literal_expression(e),
            BoundExpression::Variable(e) => self.evaluate_variable_expression(e),
            BoundExpression::Assignment(e) => self.evaluate_assignment_expression(e),
        }
    }

    fn evaluate_literal_expression(&self, e: &BoundLiteralExpression) -> Object {
        e.value.clone()
    }

    fn evaluate_statement(&mut self, root: &BoundStatement) {
        match root {
            BoundStatement::Block(s) => self.evaluate_block_statement(s),
            BoundStatement::Expression(s) => self.evaluate_expression_statement(s),
            BoundStatement::VariableDeclaration(s) => {
                self.evaluate_variable_declaration_statement(s)
            }
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

    fn evaluate_variable_expression(&self, e: &crate::binding::BoundVariableExpression) -> Object {
        let value = self.variables.get(&e.variable).unwrap();
        value.clone()
    }

    pub(crate) fn new(variables: &'v mut HashMap<VariableSymbol, Object>) -> Self {
        Self {
            variables,
            last_value: Object::Null,
        }
    }

    fn evaluate_block_statement(&mut self, s: &BoundBlockStatement) {
        for statement in s.statements.iter() {
            self.evaluate_statement(statement);
        }
    }

    fn evaluate_expression_statement(&mut self, s: &BoundExpressionStatement) {
        self.last_value = self.evaluate_expression(&s.expression);
    }

    fn evaluate_variable_declaration_statement(&mut self, s: &BoundVariableDeclarationStatement) {
        let value = self.evaluate_expression(&s.initializer);
        self.variables.insert(s.variable.clone(), value.clone());
        self.last_value = value;
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::compilation::Compilation;
    use crate::plumbing::Object;
    use crate::syntax::SyntaxTree;

    fn get_value_tests() -> Vec<(&'static str, Object)> {
        vec![
            ("1", Object::Number(1)),
            ("-1", Object::Number(-1)),
            ("+1", Object::Number(1)),
            ("1 + 2", Object::Number(3)),
            ("1 - 2", Object::Number(-1)),
            ("1 * 2", Object::Number(2)),
            ("9 / 3", Object::Number(3)),
            ("(10)", Object::Number(10)),
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
            ("true && false", Object::Boolean(false)),
            ("true && true", Object::Boolean(true)),
            ("true || false", Object::Boolean(true)),
            ("true || true", Object::Boolean(true)),
            ("false || false", Object::Boolean(false)),
            ("true == false", Object::Boolean(false)),
            ("1 == 1", Object::Boolean(true)),
            ("2 == 3", Object::Boolean(false)),
            ("true != false", Object::Boolean(true)),
            ("true != true", Object::Boolean(false)),
            ("1 != 2", Object::Boolean(true)),
            ("1 != 1", Object::Boolean(false)),
            ("(a = 10) * a", Object::Number(100)),
        ]
    }

    #[test]
    fn computes_correct_value() {
        for (text, value) in get_value_tests() {
            let syntax_tree = SyntaxTree::parse(text);
            let mut compilation = Compilation::new(syntax_tree);
            let mut variables = HashMap::new();
            let actual_result = compilation.evaluate(&mut variables);

            if actual_result != Ok(value.clone()) {
                panic!(
                    "input: {}, expected result: {:?}, actual result: {:?}",
                    text, value, actual_result
                );
            }
        }
    }
}
