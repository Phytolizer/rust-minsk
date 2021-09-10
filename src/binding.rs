use crate::plumbing::Object;
use crate::plumbing::ObjectKind;
use crate::syntax::BinaryExpressionSyntax;
use crate::syntax::ExpressionSyntaxRef;
use crate::syntax::SyntaxKind;
use crate::syntax::UnaryExpressionSyntax;

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
            BoundExpression::Binary(e) => e.left.get_type(),
            BoundExpression::Unary(e) => e.operand.get_type(),
            BoundExpression::Literal(e) => e.value.kind(),
        }
    }
}

pub(crate) enum BoundBinaryOperatorKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

pub(crate) struct BoundBinaryExpression {
    pub(crate) left: Box<BoundExpression>,
    pub(crate) operator_kind: BoundBinaryOperatorKind,
    pub(crate) right: Box<BoundExpression>,
}

pub(crate) enum BoundUnaryOperatorKind {
    Identity,
    Negation,
}

pub(crate) struct BoundUnaryExpression {
    pub(crate) operator_kind: BoundUnaryOperatorKind,
    pub(crate) operand: Box<BoundExpression>,
}

pub(crate) struct BoundLiteralExpression {
    pub(crate) value: Object,
}

pub(crate) struct Binder {
    pub(crate) diagnostics: Vec<String>,
}

impl Binder {
    pub(crate) fn bind_expression(
        &mut self,
        expression: ExpressionSyntaxRef,
    ) -> Box<BoundExpression> {
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
        let operator_kind = self.bind_binary_operator_kind(
            e.operator_token.kind,
            left.get_type(),
            right.get_type(),
        );
        if let Some(operator_kind) = operator_kind {
            Box::new(BoundExpression::Binary(BoundBinaryExpression {
                left,
                operator_kind,
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

    fn bind_binary_operator_kind(
        &self,
        kind: SyntaxKind,
        left_type: ObjectKind,
        right_type: ObjectKind,
    ) -> Option<BoundBinaryOperatorKind> {
        if left_type == ObjectKind::Number && right_type == ObjectKind::Number {
            Some(match kind {
                SyntaxKind::PlusToken => BoundBinaryOperatorKind::Addition,
                SyntaxKind::MinusToken => BoundBinaryOperatorKind::Subtraction,
                SyntaxKind::StarToken => BoundBinaryOperatorKind::Multiplication,
                SyntaxKind::SlashToken => BoundBinaryOperatorKind::Division,
                _ => panic!("unexpected binary operator kind {:?}", kind),
            })
        } else {
            None
        }
    }

    fn bind_unary_expression(&mut self, e: &UnaryExpressionSyntax) -> Box<BoundExpression> {
        let operand = self.bind_expression(e.operand.create_ref());
        let operator_kind =
            self.bind_unary_operator_kind(e.operator_token.kind, operand.get_type());
        if let Some(operator_kind) = operator_kind {
            Box::new(BoundExpression::Unary(BoundUnaryExpression {
                operator_kind,
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

    fn bind_unary_operator_kind(
        &self,
        kind: SyntaxKind,
        operand_type: ObjectKind,
    ) -> Option<BoundUnaryOperatorKind> {
        match operand_type {
            ObjectKind::Number => Some(match kind {
                SyntaxKind::PlusToken => BoundUnaryOperatorKind::Identity,
                SyntaxKind::MinusToken => BoundUnaryOperatorKind::Negation,
                _ => panic!("unexpected unary operator kind {:?}", kind),
            }),
            _ => None,
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
