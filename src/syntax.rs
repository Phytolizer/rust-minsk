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
}

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
