use crate::plumbing::Object;
use crate::syntax::SyntaxNodeRef;

use super::SyntaxKind;
use super::SyntaxToken;

#[derive(Debug, Clone)]
pub enum ExpressionSyntax {
    Binary(BinaryExpressionSyntax),
    Unary(UnaryExpressionSyntax),
    Literal(LiteralExpressionSyntax),
    Parenthesized(ParenthesizedExpressionSyntax),
    Name(NameExpressionSyntax),
    Assignment(AssignmentExpressionSyntax),
}

#[derive(Debug, Clone, Copy)]
pub enum ExpressionSyntaxRef<'a> {
    Binary(&'a BinaryExpressionSyntax),
    Unary(&'a UnaryExpressionSyntax),
    Literal(&'a LiteralExpressionSyntax),
    Parenthesized(&'a ParenthesizedExpressionSyntax),
    Name(&'a NameExpressionSyntax),
    Assignment(&'a AssignmentExpressionSyntax),
}

impl<'a> ExpressionSyntaxRef<'a> {
    pub(crate) fn kind(&self) -> SyntaxKind {
        match self {
            ExpressionSyntaxRef::Binary(_) => SyntaxKind::BinaryExpression,
            ExpressionSyntaxRef::Unary(_) => SyntaxKind::UnaryExpression,
            ExpressionSyntaxRef::Literal(_) => SyntaxKind::LiteralExpression,
            ExpressionSyntaxRef::Parenthesized(_) => SyntaxKind::ParenthesizedExpression,
            ExpressionSyntaxRef::Name(_) => SyntaxKind::NameExpression,
            ExpressionSyntaxRef::Assignment(_) => SyntaxKind::AssignmentExpression,
        }
    }

    pub(super) fn children(self) -> Vec<SyntaxNodeRef<'a>> {
        match self {
            ExpressionSyntaxRef::Binary(e) => vec![
                SyntaxNodeRef::Expression(e.left.create_ref()),
                SyntaxNodeRef::Token(&e.operator_token),
                SyntaxNodeRef::Expression(e.right.create_ref()),
            ],
            ExpressionSyntaxRef::Unary(e) => vec![
                SyntaxNodeRef::Token(&e.operator_token),
                SyntaxNodeRef::Expression(e.operand.create_ref()),
            ],
            ExpressionSyntaxRef::Literal(e) => vec![SyntaxNodeRef::Token(&e.literal_token)],
            ExpressionSyntaxRef::Parenthesized(e) => vec![
                SyntaxNodeRef::Token(&e.open_parenthesis_token),
                SyntaxNodeRef::Expression(e.expression.create_ref()),
                SyntaxNodeRef::Token(&e.close_parenthesis_token),
            ],
            ExpressionSyntaxRef::Name(e) => vec![SyntaxNodeRef::Token(&e.identifier_token)],
            ExpressionSyntaxRef::Assignment(e) => vec![
                SyntaxNodeRef::Token(&e.identifier_token),
                SyntaxNodeRef::Token(&e.equals_token),
                SyntaxNodeRef::Expression(e.expression.create_ref()),
            ],
        }
    }
}

impl ExpressionSyntax {
    pub fn create_ref(&self) -> ExpressionSyntaxRef {
        match self {
            ExpressionSyntax::Binary(e) => ExpressionSyntaxRef::Binary(e),
            ExpressionSyntax::Unary(e) => ExpressionSyntaxRef::Unary(e),
            ExpressionSyntax::Literal(e) => ExpressionSyntaxRef::Literal(e),
            ExpressionSyntax::Parenthesized(e) => ExpressionSyntaxRef::Parenthesized(e),
            ExpressionSyntax::Name(e) => ExpressionSyntaxRef::Name(e),
            ExpressionSyntax::Assignment(e) => ExpressionSyntaxRef::Assignment(e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpressionSyntax {
    pub(crate) left: Box<ExpressionSyntax>,
    pub(crate) operator_token: SyntaxToken,
    pub(crate) right: Box<ExpressionSyntax>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpressionSyntax {
    pub(crate) operator_token: SyntaxToken,
    pub(crate) operand: Box<ExpressionSyntax>,
}

#[derive(Debug, Clone)]
pub struct LiteralExpressionSyntax {
    pub(crate) literal_token: SyntaxToken,
    pub(crate) value: Object,
}

#[derive(Debug, Clone)]
pub struct ParenthesizedExpressionSyntax {
    pub(crate) open_parenthesis_token: SyntaxToken,
    pub(crate) expression: Box<ExpressionSyntax>,
    pub(crate) close_parenthesis_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct NameExpressionSyntax {
    pub(crate) identifier_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpressionSyntax {
    pub(crate) identifier_token: SyntaxToken,
    pub(crate) equals_token: SyntaxToken,
    pub(crate) expression: Box<ExpressionSyntax>,
}
