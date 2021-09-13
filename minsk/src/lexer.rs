use crate::diagnostic::DiagnosticBag;
use crate::plumbing::Object;
use crate::plumbing::ObjectKind;
use crate::syntax::keyword_kind;
use crate::syntax::SyntaxKind;
use crate::syntax::SyntaxToken;
use crate::text::SourceText;
use crate::text::TextSpan;

pub(crate) struct Lexer<'s> {
    input: &'s [char],
    position: usize,
    start: usize,
    kind: SyntaxKind,
    value: Object,
    pub(crate) diagnostics: DiagnosticBag,
}

impl<'s> Lexer<'s> {
    fn current(&self) -> char {
        self.peek(0)
    }

    fn lookahead(&self) -> char {
        self.peek(1)
    }

    pub(crate) fn new(input: &'s SourceText) -> Self {
        Self {
            input: input.as_chars(),
            position: 0,
            start: 0,
            kind: SyntaxKind::BadToken,
            value: Object::Null,
            diagnostics: DiagnosticBag::new(),
        }
    }

    pub(crate) fn next_token(&mut self) -> SyntaxToken {
        self.start = self.position;
        self.value = Object::Null;
        match self.current() {
            '\0' => {
                self.kind = SyntaxKind::EndOfFileToken;
            }
            c if c.is_numeric() => self.read_number(),
            c if c.is_alphabetic() => self.read_identifier_or_keyword(),
            c if c.is_whitespace() => self.read_whitespace(),
            '&' if self.lookahead() == '&' => {
                self.position += 2;
                self.kind = SyntaxKind::AmpersandAmpersandToken;
            }
            '|' if self.lookahead() == '|' => {
                self.position += 2;
                self.kind = SyntaxKind::PipePipeToken;
            }
            '=' if self.lookahead() == '=' => {
                self.position += 2;
                self.kind = SyntaxKind::EqualsEqualsToken;
            }
            '!' if self.lookahead() == '=' => {
                self.position += 2;
                self.kind = SyntaxKind::BangEqualsToken;
            }
            '<' if self.lookahead() == '=' => {
                self.position += 2;
                self.kind = SyntaxKind::LessOrEqualsToken;
            }
            '>' if self.lookahead() == '=' => {
                self.position += 2;
                self.kind = SyntaxKind::GreaterOrEqualsToken;
            }
            '<' => {
                self.position += 1;
                self.kind = SyntaxKind::LessToken;
            }
            '>' => {
                self.position += 1;
                self.kind = SyntaxKind::GreaterToken;
            }
            '+' => {
                self.position += 1;
                self.kind = SyntaxKind::PlusToken;
            }
            '-' => {
                self.position += 1;
                self.kind = SyntaxKind::MinusToken;
            }
            '*' => {
                self.position += 1;
                self.kind = SyntaxKind::StarToken;
            }
            '/' => {
                self.position += 1;
                self.kind = SyntaxKind::SlashToken;
            }
            '=' => {
                self.position += 1;
                self.kind = SyntaxKind::EqualsToken;
            }
            '(' => {
                self.position += 1;
                self.kind = SyntaxKind::OpenParenthesisToken;
            }
            ')' => {
                self.position += 1;
                self.kind = SyntaxKind::CloseParenthesisToken;
            }
            '{' => {
                self.position += 1;
                self.kind = SyntaxKind::OpenBraceToken;
            }
            '}' => {
                self.position += 1;
                self.kind = SyntaxKind::CloseBraceToken;
            }
            '!' => {
                self.position += 1;
                self.kind = SyntaxKind::BangToken;
            }
            _ => {
                self.diagnostics
                    .report_bad_character(self.position, self.current());
                self.position += 1;
                self.kind = SyntaxKind::BadToken;
            }
        };
        SyntaxToken::new(
            self.kind,
            self.start,
            self.input[self.start..self.position].iter().collect(),
            self.value.clone(),
        )
    }

    fn peek(&self, offset: usize) -> char {
        self.input
            .get(self.position + offset)
            .copied()
            .unwrap_or('\0')
    }

    fn read_identifier_or_keyword(&mut self) {
        while self.current().is_alphabetic() {
            self.position += 1;
        }
        let text = self.input[self.start..self.position]
            .iter()
            .collect::<String>();
        self.kind = keyword_kind(&text);
    }

    fn read_number(&mut self) {
        while self.current().is_numeric() {
            self.position += 1;
        }
        let text = self.input[self.start..self.position]
            .iter()
            .collect::<String>();
        let value = match text.parse::<i64>() {
            Ok(v) => v,
            Err(_) => {
                self.diagnostics.report_invalid_number(
                    TextSpan::new(self.start, self.position - self.start),
                    &text,
                    ObjectKind::Number,
                );
                0
            }
        };
        self.value = Object::Number(value);
        self.kind = SyntaxKind::NumberToken;
    }

    fn read_whitespace(&mut self) {
        while self.current().is_whitespace() {
            self.position += 1;
        }
        self.kind = SyntaxKind::WhitespaceToken;
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::syntax::SyntaxKind;
    use crate::syntax::SyntaxTree;

    use strum::IntoEnumIterator;

    #[derive(Debug, Clone, Copy, PartialEq)]
    struct TestToken {
        kind: SyntaxKind,
        text: &'static str,
    }

    fn test_token(kind: SyntaxKind, text: &'static str) -> TestToken {
        TestToken { kind, text }
    }

    fn get_tokens() -> Vec<TestToken> {
        let fixed_tokens =
            SyntaxKind::iter().filter_map(|k| k.get_text().map(|t| test_token(k, t)));
        vec![
            test_token(SyntaxKind::NumberToken, "1"),
            test_token(SyntaxKind::NumberToken, "123"),
            test_token(SyntaxKind::IdentifierToken, "a"),
            test_token(SyntaxKind::IdentifierToken, "abc"),
        ]
        .into_iter()
        .chain(fixed_tokens.into_iter())
        .collect()
    }

    fn get_separators() -> Vec<TestToken> {
        vec![
            test_token(SyntaxKind::WhitespaceToken, " "),
            test_token(SyntaxKind::WhitespaceToken, "  "),
            test_token(SyntaxKind::WhitespaceToken, "\r"),
            test_token(SyntaxKind::WhitespaceToken, "\n"),
            test_token(SyntaxKind::WhitespaceToken, "\r\n"),
        ]
    }

    fn get_token_pairs() -> Vec<(TestToken, TestToken)> {
        let mut pairs = Vec::new();
        for t1 in get_tokens() {
            for t2 in get_tokens() {
                if !requires_separator(t1.kind, t2.kind) {
                    pairs.push((t1, t2));
                }
            }
        }
        pairs
    }

    fn requires_separator(t1kind: SyntaxKind, t2kind: SyntaxKind) -> bool {
        let t1_is_keyword = format!("{:?}", t1kind).ends_with("Keyword");
        let t2_is_keyword = format!("{:?}", t2kind).ends_with("Keyword");
        match (t1kind, t2kind) {
            (SyntaxKind::IdentifierToken, SyntaxKind::IdentifierToken) => true,
            _ if t1_is_keyword && t2_is_keyword => true,
            (SyntaxKind::IdentifierToken, _) if t2_is_keyword => true,
            (_, SyntaxKind::IdentifierToken) if t1_is_keyword => true,
            (SyntaxKind::NumberToken, SyntaxKind::NumberToken) => true,
            (SyntaxKind::BangToken, SyntaxKind::EqualsToken | SyntaxKind::EqualsEqualsToken) => {
                true
            }
            (SyntaxKind::EqualsToken, SyntaxKind::EqualsToken | SyntaxKind::EqualsEqualsToken) => {
                true
            }
            (
                SyntaxKind::LessToken | SyntaxKind::GreaterToken,
                SyntaxKind::EqualsToken | SyntaxKind::EqualsEqualsToken,
            ) => true,
            _ => false,
        }
    }

    fn get_token_pairs_with_separator() -> Vec<(TestToken, TestToken, TestToken)> {
        let mut token_pairs_with_separator = Vec::new();
        for t1 in get_tokens() {
            for t2 in get_tokens() {
                if requires_separator(t1.kind, t2.kind) {
                    for sep in get_separators() {
                        token_pairs_with_separator.push((t1, sep, t2));
                    }
                }
            }
        }
        token_pairs_with_separator
    }

    #[test]
    fn tests_all_tokens() {
        let kinds = SyntaxKind::iter()
            .filter(|k| {
                let str = format!("{:?}", k);
                str.ends_with("Keyword") || str.ends_with("Token")
            })
            .collect::<Vec<_>>();
        let tested_token_kinds = get_tokens()
            .into_iter()
            .chain(get_separators().into_iter())
            .map(|t| t.kind)
            .collect::<Vec<_>>();
        let untested_token_kinds = kinds
            .iter()
            .filter(|k| !tested_token_kinds.contains(k))
            .filter(|k| ![SyntaxKind::BadToken, SyntaxKind::EndOfFileToken].contains(k))
            .copied()
            .collect::<HashSet<SyntaxKind>>();
        assert_eq!(untested_token_kinds, HashSet::new());
    }

    #[test]
    fn lexes_token() {
        for TestToken { kind, text } in get_tokens() {
            let tokens = SyntaxTree::parse_tokens(text);

            assert_eq!(1, tokens.len());
            let token = &tokens[0];
            assert_eq!(kind, token.kind);
            assert_eq!(text, token.text);
        }
    }

    #[test]
    fn lexes_token_pairs() {
        for (t1, t2) in get_token_pairs() {
            let text = format!("{}{}", t1.text, t2.text);
            let tokens = SyntaxTree::parse_tokens(&text);

            if 2 != tokens.len() {
                panic!("\ninputs: {:?}\nresults: {:?}", (t1, t2), tokens);
            }
            assert_eq!(t1.kind, tokens[0].kind);
            assert_eq!(t1.text, tokens[0].text);
            assert_eq!(t2.kind, tokens[1].kind);
            assert_eq!(t2.text, tokens[1].text);
        }
    }

    #[test]
    fn lexes_token_pairs_with_separator() {
        for (t1, sep, t2) in get_token_pairs_with_separator() {
            let text = format!("{}{}{}", t1.text, sep.text, t2.text);
            let tokens = SyntaxTree::parse_tokens(&text);

            if 3 != tokens.len() {
                panic!("\ninputs: {:?}\nresults: {:?}", (t1, sep, t2), tokens);
            }
            assert_eq!(t1.kind, tokens[0].kind);
            assert_eq!(t1.text, tokens[0].text);
            assert_eq!(sep.kind, tokens[1].kind);
            assert_eq!(sep.text, tokens[1].text);
            assert_eq!(t2.kind, tokens[2].kind);
            assert_eq!(t2.text, tokens[2].text);
        }
    }
}
