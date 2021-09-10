use crate::plumbing::Object;
use crate::syntax::BinaryExpressionSyntax;
use crate::syntax::ExpressionSyntax;
use crate::syntax::LiteralExpressionSyntax;
use crate::syntax::ParenthesizedExpressionSyntax;
use crate::syntax::SyntaxKind;
use crate::syntax::UnaryExpressionSyntax;

pub(crate) struct Evaluator {
    root: Box<ExpressionSyntax>,
}

impl Evaluator {
    pub(crate) fn new(root: Box<ExpressionSyntax>) -> Self {
        Self { root }
    }

    pub(crate) fn evaluate(&self) -> i64 {
        self.evaluate_expression(&self.root)
    }

    fn evaluate_expression(&self, expr: &ExpressionSyntax) -> i64 {
        match expr {
            ExpressionSyntax::Binary(e) => self.evaluate_binary_expression(e),
            ExpressionSyntax::Unary(e) => self.evaluate_unary_expression(e),
            ExpressionSyntax::Literal(e) => self.evaluate_literal_expression(e),
            ExpressionSyntax::Parenthesized(e) => self.evaluate_parenthesized_expression(e),
        }
    }

    fn evaluate_binary_expression(&self, e: &BinaryExpressionSyntax) -> i64 {
        let left = self.evaluate_expression(&e.left);
        let right = self.evaluate_expression(&e.right);
        match e.operator_token.kind {
            SyntaxKind::PlusToken => left + right,
            SyntaxKind::MinusToken => left - right,
            SyntaxKind::StarToken => left * right,
            SyntaxKind::SlashToken => left / right,
            _ => panic!("Unexpected operator {:?}", e.operator_token.kind),
        }
    }

    fn evaluate_unary_expression(&self, e: &UnaryExpressionSyntax) -> i64 {
        let operand = self.evaluate_expression(&e.operand);
        match e.operator_token.kind {
            SyntaxKind::PlusToken => operand,
            SyntaxKind::MinusToken => -operand,
            _ => panic!("Unexpected operator {:?}", e.operator_token.kind),
        }
    }

    fn evaluate_literal_expression(&self, e: &LiteralExpressionSyntax) -> i64 {
        match e.literal_token.value {
            Object::Number(n) => n,
            _ => panic!("No value for literal token"),
        }
    }

    fn evaluate_parenthesized_expression(&self, e: &ParenthesizedExpressionSyntax) -> i64 {
        self.evaluate_expression(&e.expression)
    }
}
