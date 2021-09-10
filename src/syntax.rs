use crate::plumbing::Object;
#[derive(Debug, Clone, Copy, PartialEq)]

pub(crate) enum SyntaxKind {
    NumberToken,
    WhitespaceToken,
    PlusToken,
    MinusToken,
    StarToken,
    SlashToken,
    OpenParenthesisToken,
    CloseParenthesisToken,
    EndOfFileToken,
    BadToken,

    BinaryExpression,
    UnaryExpression,
    LiteralExpression,
}

#[derive(Debug, Clone)]
pub(crate) struct SyntaxToken {
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

#[derive(Debug)]
pub(crate) enum SyntaxNode {
    Expression(ExpressionSyntax),
    Token(SyntaxToken),
}

enum SyntaxNodeRef<'a> {
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
            Self::Token(t) => vec![],
        }
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
    fn create_ref(&self) -> SyntaxNodeRef {
        match self {
            SyntaxNode::Expression(e) => SyntaxNodeRef::Expression(e.create_ref()),
            SyntaxNode::Token(t) => SyntaxNodeRef::Token(t),
        }
    }
    pub(crate) fn pretty_print<W: std::io::Write>(&self, writer: &mut W) {
        self.create_ref()
            .pretty_print_node(writer, String::new(), true);
    }
}

#[derive(Debug)]
pub(crate) enum ExpressionSyntax {
    Binary(BinaryExpressionSyntax),
    Unary(UnaryExpressionSyntax),
    Literal(LiteralExpressionSyntax),
}

pub(crate) enum ExpressionSyntaxRef<'a> {
    Binary(&'a BinaryExpressionSyntax),
    Unary(&'a UnaryExpressionSyntax),
    Literal(&'a LiteralExpressionSyntax),
}

impl<'a> ExpressionSyntaxRef<'a> {
    pub(crate) fn kind(&self) -> SyntaxKind {
        match self {
            ExpressionSyntaxRef::Binary(_) => SyntaxKind::BinaryExpression,
            ExpressionSyntaxRef::Unary(_) => SyntaxKind::UnaryExpression,
            ExpressionSyntaxRef::Literal(_) => SyntaxKind::LiteralExpression,
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
        }
    }
}

impl ExpressionSyntax {
    fn create_ref(&self) -> ExpressionSyntaxRef {
        match self {
            ExpressionSyntax::Binary(e) => ExpressionSyntaxRef::Binary(e),
            ExpressionSyntax::Unary(e) => ExpressionSyntaxRef::Unary(e),
            ExpressionSyntax::Literal(e) => ExpressionSyntaxRef::Literal(e),
        }
    }
}

#[derive(Debug)]
pub(crate) struct BinaryExpressionSyntax {
    pub(crate) left: Box<ExpressionSyntax>,
    pub(crate) operator_token: SyntaxToken,
    pub(crate) right: Box<ExpressionSyntax>,
}

#[derive(Debug)]
pub(crate) struct UnaryExpressionSyntax {
    pub(crate) operator_token: SyntaxToken,
    pub(crate) operand: Box<ExpressionSyntax>,
}

#[derive(Debug)]
pub(crate) struct LiteralExpressionSyntax {
    pub(crate) literal_token: SyntaxToken,
}
