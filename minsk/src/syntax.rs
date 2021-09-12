use std::fmt::Display;
use std::io::stdout;

use crate::diagnostic::Diagnostic;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::plumbing::Object;
use crate::text::SourceText;
use crate::text::TextSpan;

use crossterm::style::Color;
use crossterm::style::ResetColor;
use crossterm::style::SetForegroundColor;
use crossterm::ExecutableCommand;

use self::expressions::ExpressionSyntax;
use self::expressions::ExpressionSyntaxRef;
use self::statements::StatementSyntax;
use self::statements::StatementSyntaxRef;

pub mod expressions;
pub mod statements;

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
    OpenBraceToken,
    CloseBraceToken,
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

    BlockStatement,
    ExpressionStatement,

    CompilationUnit,
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
            SyntaxKind::OpenBraceToken => Some("{"),
            SyntaxKind::CloseBraceToken => Some("}"),
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

#[derive(Clone)]
pub struct SyntaxTree {
    pub source_text: SourceText,
    pub root: CompilationUnitSyntax,
    pub diagnostics: Vec<Diagnostic>,
}

impl SyntaxTree {
    fn new(text: SourceText) -> Self {
        let mut parser = Parser::new(text.clone());
        let root = parser.parse_compilation_unit();

        Self {
            source_text: text,
            root,
            diagnostics: parser.diagnostics.into_iter().collect(),
        }
    }

    pub fn parse(input: &str) -> Self {
        Self::parse_text(SourceText::from(input.chars().collect()))
    }

    pub fn parse_text(text: SourceText) -> Self {
        Self::new(text)
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
    Statement(StatementSyntax),
    CompilationUnit(CompilationUnitSyntax),
    Token(SyntaxToken),
}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxNodeRef<'a> {
    Expression(ExpressionSyntaxRef<'a>),
    Statement(StatementSyntaxRef<'a>),
    CompilationUnit(CompilationUnitSyntaxRef<'a>),
    Token(&'a SyntaxToken),
}

impl<'a> SyntaxNodeRef<'a> {
    pub(crate) fn kind(&self) -> SyntaxKind {
        match self {
            SyntaxNodeRef::Expression(e) => e.kind(),
            &SyntaxNodeRef::Statement(s) => s.kind(),
            SyntaxNodeRef::CompilationUnit(c) => c.kind(),
            SyntaxNodeRef::Token(t) => t.kind,
        }
    }

    pub(crate) fn children(self) -> Vec<SyntaxNodeRef<'a>> {
        match self {
            Self::Expression(e) => e.children(),
            Self::Statement(s) => s.children(),
            Self::CompilationUnit(c) => c.children(),
            Self::Token(_) => vec![],
        }
    }

    pub fn span(&self) -> TextSpan {
        match self {
            Self::Token(t) => t.span(),
            _ => {
                let children = self.children();
                let first = children.first().unwrap().span();
                let last = children.last().unwrap().span();
                TextSpan::from_bounds(first.start, last.end())
            }
        }
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
            SyntaxNode::Statement(s) => SyntaxNodeRef::Statement(s.create_ref()),
            SyntaxNode::CompilationUnit(c) => SyntaxNodeRef::CompilationUnit(c.create_ref()),
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

#[derive(Debug, Clone)]
pub struct CompilationUnitSyntax {
    pub statement: StatementSyntax,
    pub end_of_file_token: SyntaxToken,
}

impl CompilationUnitSyntax {
    pub fn create_ref(&self) -> CompilationUnitSyntaxRef {
        CompilationUnitSyntaxRef {
            statement: self.statement.create_ref(),
            end_of_file_token: &self.end_of_file_token,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CompilationUnitSyntaxRef<'a> {
    pub statement: StatementSyntaxRef<'a>,
    pub end_of_file_token: &'a SyntaxToken,
}

impl<'a> CompilationUnitSyntaxRef<'a> {
    pub(crate) fn kind(&self) -> SyntaxKind {
        SyntaxKind::CompilationUnit
    }

    pub fn children(&self) -> Vec<SyntaxNodeRef<'a>> {
        vec![
            SyntaxNodeRef::Statement(self.statement),
            SyntaxNodeRef::Token(self.end_of_file_token),
        ]
    }
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
