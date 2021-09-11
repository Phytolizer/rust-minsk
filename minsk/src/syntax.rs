use crate::diagnostic::DiagnosticBag;
use crate::parser::Parser;
use crate::plumbing::Object;
use crate::text::TextSpan;
#[derive(Debug, Clone, Copy, PartialEq)]

pub(crate) enum SyntaxKind {
    NumberToken,
    WhitespaceToken,
    IdentifierToken,
    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,
    BangToken,
    AmpersandAmpersandToken,
    PipePipeToken,
    EqualsEqualsToken,
    BangEqualsToken,
    OpenParenthesisToken,
    CloseParenthesisToken,
    EndOfFileToken,
    BadToken,

    TrueKeyword,
    FalseKeyword,

    BinaryExpression,
    UnaryExpression,
    LiteralExpression,
    ParenthesizedExpression,
}

impl SyntaxKind {
    pub(crate) fn get_binary_operator_precedence(&self) -> usize {
        match self {
            SyntaxKind::StarToken | SyntaxKind::SlashToken => 5,
            SyntaxKind::PlusToken | SyntaxKind::MinusToken => 4,
            SyntaxKind::EqualsEqualsToken | SyntaxKind::BangEqualsToken => 3,
            SyntaxKind::AmpersandAmpersandToken => 2,
            SyntaxKind::PipePipeToken => 1,
            _ => 0,
        }
    }

    pub(crate) fn get_unary_operator_precedence(&self) -> usize {
        match self {
            SyntaxKind::PlusToken | SyntaxKind::MinusToken | SyntaxKind::BangToken => 6,
            _ => 0,
        }
    }
}

pub(crate) fn keyword_kind(text: &str) -> SyntaxKind {
    match text {
        "true" => SyntaxKind::TrueKeyword,
        "false" => SyntaxKind::FalseKeyword,
        _ => SyntaxKind::IdentifierToken,
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxToken {
    pub(crate) kind: SyntaxKind,
    pub(crate) position: usize,
    pub(crate) text: String,
    pub(crate) value: Object,
}

impl SyntaxToken {
    pub(crate) fn new(kind: SyntaxKind, position: usize, text: String, value: Object) -> Self {
        Self {
            kind,
            position,
            text,
            value,
        }
    }

    pub(crate) fn span(&self) -> TextSpan {
        TextSpan::new(self.position, self.text.len())
    }
}

impl Default for SyntaxToken {
    fn default() -> Self {
        Self {
            kind: SyntaxKind::BadToken,
            position: 0,
            text: String::default(),
            value: Object::Null,
        }
    }
}

pub struct SyntaxTree {
    pub root: Box<ExpressionSyntax>,
    pub end_of_file_token: SyntaxToken,
    pub diagnostics: DiagnosticBag,
}

impl SyntaxTree {
    pub fn parse(input: &str) -> Self {
        let mut parser = Parser::new(input);
        parser.parse()
    }
}

#[derive(Debug)]
pub enum SyntaxNode {
    Expression(ExpressionSyntax),
    Token(SyntaxToken),
}

pub enum SyntaxNodeRef<'a> {
    Expression(ExpressionSyntaxRef<'a>),
    Token(&'a SyntaxToken),
}

impl<'a> SyntaxNodeRef<'a> {
    pub(crate) fn kind(&self) -> SyntaxKind {
        match self {
            SyntaxNodeRef::Expression(e) => e.kind(),
            SyntaxNodeRef::Token(t) => t.kind,
        }
    }

    fn children(&self) -> Vec<SyntaxNodeRef> {
        match self {
            Self::Expression(e) => e.children(),
            Self::Token(_) => vec![],
        }
    }
    pub fn pretty_print<W: std::io::Write>(&self, writer: &mut W) {
        self.pretty_print_node(writer, String::new(), true);
    }
    fn pretty_print_node<W: std::io::Write>(
        &self,
        writer: &mut W,
        mut indent: String,
        is_last: bool,
    ) {
        write!(writer, "{}", indent).unwrap();
        let marker = if is_last {
            "└───"
        } else {
            "├───"
        };
        write!(writer, "{}", marker).unwrap();
        write!(writer, "{:?}", self.kind()).unwrap();
        if let Self::Token(t) = self {
            if t.value != Object::Null {
                write!(writer, " {}", t.value).unwrap();
            }
        }

        indent += if is_last { "    " } else { "│   " };
        writeln!(writer).unwrap();
        let children = self.children();
        for i in 0..children.len() {
            children[i].pretty_print_node(writer, indent.clone(), i == children.len() - 1);
        }
    }
}

impl SyntaxNode {
    pub fn create_ref(&self) -> SyntaxNodeRef {
        match self {
            SyntaxNode::Expression(e) => SyntaxNodeRef::Expression(e.create_ref()),
            SyntaxNode::Token(t) => SyntaxNodeRef::Token(t),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionSyntax {
    Binary(BinaryExpressionSyntax),
    Unary(UnaryExpressionSyntax),
    Literal(LiteralExpressionSyntax),
    Parenthesized(ParenthesizedExpressionSyntax),
}

pub enum ExpressionSyntaxRef<'a> {
    Binary(&'a BinaryExpressionSyntax),
    Unary(&'a UnaryExpressionSyntax),
    Literal(&'a LiteralExpressionSyntax),
    Parenthesized(&'a ParenthesizedExpressionSyntax),
}

impl<'a> ExpressionSyntaxRef<'a> {
    pub(crate) fn kind(&self) -> SyntaxKind {
        match self {
            ExpressionSyntaxRef::Binary(_) => SyntaxKind::BinaryExpression,
            ExpressionSyntaxRef::Unary(_) => SyntaxKind::UnaryExpression,
            ExpressionSyntaxRef::Literal(_) => SyntaxKind::LiteralExpression,
            ExpressionSyntaxRef::Parenthesized(_) => SyntaxKind::ParenthesizedExpression,
        }
    }

    fn children(&self) -> Vec<SyntaxNodeRef> {
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
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpressionSyntax {
    pub(crate) left: Box<ExpressionSyntax>,
    pub(crate) operator_token: SyntaxToken,
    pub(crate) right: Box<ExpressionSyntax>,
}

#[derive(Debug)]
pub struct UnaryExpressionSyntax {
    pub(crate) operator_token: SyntaxToken,
    pub(crate) operand: Box<ExpressionSyntax>,
}

#[derive(Debug)]
pub struct LiteralExpressionSyntax {
    pub(crate) literal_token: SyntaxToken,
    pub(crate) value: Object,
}

#[derive(Debug)]
pub struct ParenthesizedExpressionSyntax {
    pub(crate) open_parenthesis_token: SyntaxToken,
    pub(crate) expression: Box<ExpressionSyntax>,
    pub(crate) close_parenthesis_token: SyntaxToken,
}
