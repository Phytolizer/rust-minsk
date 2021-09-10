use crate::lexer::Lexer;
use crate::plumbing::Object;
use crate::syntax::BinaryExpressionSyntax;
use crate::syntax::ExpressionSyntax;
use crate::syntax::LiteralExpressionSyntax;
use crate::syntax::ParenthesizedExpressionSyntax;
use crate::syntax::SyntaxKind;
use crate::syntax::SyntaxToken;
use crate::syntax::SyntaxTree;
use crate::syntax::UnaryExpressionSyntax;

pub(crate) struct Parser {
    tokens: Vec<SyntaxToken>,
    position: usize,
    diagnostics: Vec<String>,
}

impl Parser {
    pub(crate) fn new(text: &str) -> Self {
        let mut lexer = Lexer::new(text);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let kind = token.kind;
            if ![SyntaxKind::BadToken, SyntaxKind::WhitespaceToken].contains(&kind) {
                tokens.push(token);
            }
            if kind == SyntaxKind::EndOfFileToken {
                break;
            }
        }
        let mut diagnostics = Vec::new();
        std::mem::swap(&mut diagnostics, &mut lexer.diagnostics);
        Self {
            tokens,
            position: 0,
            diagnostics,
        }
    }

    fn peek(&self, offset: usize) -> &SyntaxToken {
        let index = self.position + offset;
        if index >= self.tokens.len() {
            self.tokens.last().unwrap()
        } else {
            self.tokens.get(index).unwrap()
        }
    }

    fn peek_mut(&mut self, offset: usize) -> &mut SyntaxToken {
        let index = self.position + offset;
        if index >= self.tokens.len() {
            self.tokens.last_mut().unwrap()
        } else {
            self.tokens.get_mut(index).unwrap()
        }
    }

    fn current(&self) -> &SyntaxToken {
        self.peek(0)
    }

    fn current_mut(&mut self) -> &mut SyntaxToken {
        self.peek_mut(0)
    }

    fn next_token(&mut self) -> SyntaxToken {
        let mut old_current = SyntaxToken::default();
        std::mem::swap(&mut old_current, self.current_mut());
        self.position += 1;
        old_current
    }

    fn match_token(&mut self, kind: SyntaxKind) -> SyntaxToken {
        if self.current().kind == kind {
            self.next_token()
        } else {
            self.diagnostics.push(format!(
                "ERROR: unexpected token <{:?}>, expected <{:?}>.",
                self.current().kind,
                kind
            ));
            SyntaxToken {
                kind,
                position: self.current().position,
                text: String::new(),
                value: Object::Null,
            }
        }
    }

    pub(crate) fn parse(&mut self) -> SyntaxTree {
        let root = self.parse_expression(0);
        let end_of_file_token = self.match_token(SyntaxKind::EndOfFileToken);
        let mut diagnostics = Vec::new();
        std::mem::swap(&mut diagnostics, &mut self.diagnostics);
        SyntaxTree {
            root,
            end_of_file_token,
            diagnostics,
        }
    }

    fn parse_expression(&mut self, parent_precedence: usize) -> Box<ExpressionSyntax> {
        let unary_operator_precedence = self.current().kind.get_unary_operator_precedence();
        let mut left =
            if unary_operator_precedence != 0 && unary_operator_precedence >= parent_precedence {
                let operator_token = self.next_token();
                let operand = self.parse_expression(unary_operator_precedence);
                Box::new(ExpressionSyntax::Unary(UnaryExpressionSyntax {
                    operator_token,
                    operand,
                }))
            } else {
                self.parse_primary_expression()
            };

        loop {
            let precedence = self.current().kind.get_binary_operator_precedence();
            if precedence == 0 || precedence <= parent_precedence {
                break;
            }
            let operator_token = self.next_token();
            let right = self.parse_expression(precedence);
            left = Box::new(ExpressionSyntax::Binary(BinaryExpressionSyntax {
                left,
                operator_token,
                right,
            }));
        }

        left
    }

    fn parse_primary_expression(&mut self) -> Box<ExpressionSyntax> {
        match self.current().kind {
            SyntaxKind::OpenParenthesisToken => {
                let open_parenthesis_token = self.next_token();
                let expression = self.parse_expression(0);
                let close_parenthesis_token = self.match_token(SyntaxKind::CloseParenthesisToken);
                Box::new(ExpressionSyntax::Parenthesized(
                    ParenthesizedExpressionSyntax {
                        open_parenthesis_token,
                        expression,
                        close_parenthesis_token,
                    },
                ))
            }
            SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword => {
                let keyword_token = self.next_token();
                let value = keyword_token.kind == SyntaxKind::TrueKeyword;
                Box::new(ExpressionSyntax::Literal(LiteralExpressionSyntax {
                    literal_token: keyword_token,
                    value: Object::Boolean(value),
                }))
            }
            _ => {
                let number_token = self.match_token(SyntaxKind::NumberToken);
                Box::new(ExpressionSyntax::Literal(LiteralExpressionSyntax {
                    value: number_token.value.clone(),
                    literal_token: number_token,
                }))
            }
        }
    }
}
