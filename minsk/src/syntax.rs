use std::fmt::Display;
use std::io::stdout;

use crate::diagnostic::DiagnosticBag;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::plumbing::Object;
use crate::text::SourceText;
use crate::text::TextSpan;

use crossterm::style::Color;
use crossterm::style::ResetColor;
use crossterm::style::SetForegroundColor;
use crossterm::ExecutableCommand;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, strum_macros::EnumIter)]
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
    EqualsToken,
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
    NameExpression,
    AssignmentExpression,
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

    pub(crate) fn get_text(&self) -> Option<&'static str> {
        match self {
            SyntaxKind::PlusToken => Some("+"),
            SyntaxKind::MinusToken => Some("-"),
            SyntaxKind::StarToken => Some("*"),
            SyntaxKind::SlashToken => Some("/"),
            SyntaxKind::BangToken => Some("!"),
            SyntaxKind::EqualsToken => Some("="),
            SyntaxKind::AmpersandAmpersandToken => Some("&&"),
            SyntaxKind::PipePipeToken => Some("||"),
            SyntaxKind::EqualsEqualsToken => Some("=="),
            SyntaxKind::BangEqualsToken => Some("!="),
            SyntaxKind::OpenParenthesisToken => Some("("),
            SyntaxKind::CloseParenthesisToken => Some(")"),
            SyntaxKind::FalseKeyword => Some("false"),
            SyntaxKind::TrueKeyword => Some("true"),
            _ => None,
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
    pub source_text: SourceText,
    pub root: Box<ExpressionSyntax>,
    pub end_of_file_token: SyntaxToken,
    pub diagnostics: DiagnosticBag,
}

impl SyntaxTree {
    pub fn parse(input: &str) -> Self {
        Self::parse_text(SourceText::from(input.chars().collect()))
    }

    pub fn parse_text(text: SourceText) -> Self {
        let parser = Parser::new(text);
        parser.parse()
    }

    pub fn parse_tokens(input: &str) -> Vec<SyntaxToken> {
        Self::parse_tokens_text(&SourceText::from(input.chars().collect()))
    }

    pub fn parse_tokens_text(input: &SourceText) -> Vec<SyntaxToken> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            if token.kind == SyntaxKind::EndOfFileToken {
                break;
            }

            tokens.push(token);
        }
        tokens
    }
}

#[derive(Debug)]
pub enum SyntaxNode {
    Expression(ExpressionSyntax),
    Token(SyntaxToken),
}

#[derive(Debug, Clone, Copy)]
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

    pub(crate) fn children(self) -> Vec<SyntaxNodeRef<'a>> {
        match self {
            Self::Expression(e) => e.children(),
            Self::Token(_) => vec![],
        }
    }

    pub fn span(&self) -> TextSpan {
        let children = self.children();
        let first = children.first().unwrap().span();
        let last = children.last().unwrap().span();
        TextSpan::from_bounds(first.start, last.end())
    }

    pub fn pretty_print(&self) {
        self.pretty_print_node(&mut stdout(), String::new(), true, true);
    }
    pub fn pretty_print_to<W: std::io::Write>(&self, writer: &mut W) {
        self.pretty_print_node(writer, String::new(), true, false);
    }
    fn pretty_print_node<W: std::io::Write>(
        &self,
        writer: &mut W,
        mut indent: String,
        is_last: bool,
        colors: bool,
    ) {
        if colors {
            writer.execute(SetForegroundColor(Color::DarkGrey)).unwrap();
        }
        write!(writer, "{}", indent).unwrap();
        let marker = if is_last {
            "└───"
        } else {
            "├───"
        };
        write!(writer, "{}", marker).unwrap();
        if colors {
            writer.execute(ResetColor).unwrap();
        }
        if colors {
            if let Self::Token(_) = self {
                writer.execute(SetForegroundColor(Color::Blue)).unwrap();
            } else {
                writer.execute(SetForegroundColor(Color::Cyan)).unwrap();
            }
        }
        write!(writer, "{:?}", self.kind()).unwrap();
        if colors {
            writer.execute(ResetColor).unwrap();
        }
        if let Self::Token(t) = self {
            if t.value != Object::Null {
                if colors {
                    writer.execute(SetForegroundColor(Color::Magenta)).unwrap();
                }
                write!(writer, " {}", t.value).unwrap();
                if colors {
                    writer.execute(ResetColor).unwrap();
                }
            }
        }

        indent += if is_last { "    " } else { "│   " };
        writeln!(writer).unwrap();
        let children = self.children();
        for i in 0..children.len() {
            children[i].pretty_print_node(writer, indent.clone(), i == children.len() - 1, colors);
        }
        if colors {
            writer.execute(ResetColor).unwrap();
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

impl Display for SyntaxNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut text = Vec::<u8>::new();
        self.create_ref().pretty_print_to(&mut text);
        write!(f, "{}", String::from_utf8(text).unwrap())
    }
}

#[derive(Debug)]
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

    fn children(self) -> Vec<SyntaxNodeRef<'a>> {
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

#[derive(Debug)]
pub struct NameExpressionSyntax {
    pub(crate) identifier_token: SyntaxToken,
}

#[derive(Debug)]
pub struct AssignmentExpressionSyntax {
    pub(crate) identifier_token: SyntaxToken,
    pub(crate) equals_token: SyntaxToken,
    pub(crate) expression: Box<ExpressionSyntax>,
}

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use super::SyntaxKind;
    use super::SyntaxTree;

    #[test]
    fn get_text_round_trips() {
        for (kind, text) in SyntaxKind::iter().filter_map(|k| k.get_text().map(|t| (k, t))) {
            let tokens = SyntaxTree::parse_tokens(text);
            assert_eq!(1, tokens.len());
            assert_eq!(kind, tokens[0].kind);
            assert_eq!(text, tokens[0].text);
        }
    }
}
