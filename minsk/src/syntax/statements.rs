use super::expressions::ExpressionSyntax;
use super::SyntaxKind;
use super::SyntaxNodeRef;
use super::SyntaxToken;

#[derive(Debug, Clone)]
pub enum StatementSyntax {
    Block(BlockStatementSyntax),
    Expression(ExpressionStatementSyntax),
}

impl StatementSyntax {
    pub fn create_ref(&self) -> StatementSyntaxRef {
        match self {
            StatementSyntax::Block(s) => StatementSyntaxRef::Block(s),
            StatementSyntax::Expression(s) => StatementSyntaxRef::Expression(s),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum StatementSyntaxRef<'a> {
    Block(&'a BlockStatementSyntax),
    Expression(&'a ExpressionStatementSyntax),
}

impl<'a> StatementSyntaxRef<'a> {
    pub(crate) fn kind(&self) -> SyntaxKind {
        match self {
            StatementSyntaxRef::Block(_) => SyntaxKind::BlockStatement,
            StatementSyntaxRef::Expression(_) => SyntaxKind::ExpressionStatement,
        }
    }

    pub(super) fn children(self) -> Vec<SyntaxNodeRef<'a>> {
        match self {
            StatementSyntaxRef::Block(s) => {
                let mut result = vec![SyntaxNodeRef::Token(&s.open_brace_token)];
                result.append(
                    &mut s
                        .statements
                        .iter()
                        .map(|s| SyntaxNodeRef::Statement(s.create_ref()))
                        .collect(),
                );
                result.push(SyntaxNodeRef::Token(&s.close_brace_token));
                result
            }
            StatementSyntaxRef::Expression(s) => {
                vec![SyntaxNodeRef::Expression(s.expression.create_ref())]
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatementSyntax {
    pub open_brace_token: SyntaxToken,
    pub statements: Vec<StatementSyntax>,
    pub close_brace_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatementSyntax {
    pub expression: ExpressionSyntax,
}
