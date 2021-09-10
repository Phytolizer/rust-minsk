use crate::plumbing::Object;
use crate::syntax::SyntaxKind;
use crate::syntax::SyntaxToken;
use crate::syntax::keyword_kind;

pub(crate) struct Lexer {
    input: Vec<char>,
    position: usize,
    start: usize,
    kind: SyntaxKind,
    value: Object,
    pub(crate) diagnostics: Vec<String>,
}

impl Lexer {
    pub(crate) fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            start: 0,
            kind: SyntaxKind::BadToken,
            value: Object::Null,
            diagnostics: Vec::new(),
        }
    }

    fn current(&self) -> char {
        self.input.get(self.position).copied().unwrap_or('\0')
    }

    pub(crate) fn next_token(&mut self) -> SyntaxToken {
        self.start = self.position;
        self.value = Object::Null;
        self.kind = match self.current() {
            '\0' => SyntaxKind::EndOfFileToken,
            c if c.is_numeric() => {
                while self.current().is_numeric() {
                    self.position += 1;
                }
                let text = self.input[self.start..self.position]
                    .iter()
                    .collect::<String>();
                let value = match text.parse::<i64>() {
                    Ok(v) => v,
                    Err(_) => {
                        self.diagnostics
                            .push(format!("ERROR: invalid i64: {}", text));
                        0
                    }
                };
                self.value = Object::Number(value);
                SyntaxKind::NumberToken
            }
            c if c.is_alphabetic() => {
                while self.current().is_alphabetic() {
                    self.position += 1;
                }
                let text = self.input[self.start..self.position]
                    .iter()
                    .collect::<String>();
                keyword_kind(&text)
            }
            c if c.is_whitespace() => {
                while self.current().is_whitespace() {
                    self.position += 1;
                }
                SyntaxKind::WhitespaceToken
            }
            '+' => {
                self.position += 1;
                SyntaxKind::PlusToken
            }
            '-' => {
                self.position += 1;
                SyntaxKind::MinusToken
            }
            '*' => {
                self.position += 1;
                SyntaxKind::StarToken
            }
            '/' => {
                self.position += 1;
                SyntaxKind::SlashToken
            }
            '(' => {
                self.position += 1;
                SyntaxKind::OpenParenthesisToken
            }
            ')' => {
                self.position += 1;
                SyntaxKind::CloseParenthesisToken
            }
            _ => {
                self.position += 1;
                self.diagnostics
                    .push(format!("ERROR: bad character input: '{}'", self.current()));
                SyntaxKind::BadToken
            }
        };
        SyntaxToken::new(
            self.kind,
            self.start,
            self.input[self.start..self.position].iter().collect(),
            self.value.clone(),
        )
    }
}
