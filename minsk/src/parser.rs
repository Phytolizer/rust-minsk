use crate::diagnostic::DiagnosticBag;
use crate::lexer::Lexer;
use crate::plumbing::Object;
use crate::syntax::expressions::AssignmentExpressionSyntax;
use crate::syntax::expressions::BinaryExpressionSyntax;
use crate::syntax::expressions::ExpressionSyntax;
use crate::syntax::expressions::LiteralExpressionSyntax;
use crate::syntax::expressions::NameExpressionSyntax;
use crate::syntax::expressions::ParenthesizedExpressionSyntax;
use crate::syntax::expressions::UnaryExpressionSyntax;
use crate::syntax::statements::BlockStatementSyntax;
use crate::syntax::statements::ExpressionStatementSyntax;
use crate::syntax::statements::IfStatementSyntax;
use crate::syntax::statements::StatementSyntax;
use crate::syntax::statements::VariableDeclarationStatementSyntax;
use crate::syntax::CompilationUnitSyntax;
use crate::syntax::ElseClauseSyntax;
use crate::syntax::SyntaxKind;
use crate::syntax::SyntaxToken;
use crate::text::SourceText;

pub(crate) struct Parser {
    tokens: Vec<SyntaxToken>,
    position: usize,
    pub(crate) diagnostics: DiagnosticBag,
    text: SourceText,
}

impl Parser {
    pub(crate) fn new(text: SourceText) -> Self {
        let mut lexer = Lexer::new(&text);
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
        Self {
            tokens,
            position: 0,
            diagnostics: lexer.diagnostics,
            text,
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
            self.diagnostics.report_unexpected_token(
                self.current().span(),
                self.current().kind,
                kind,
            );
            SyntaxToken {
                kind,
                position: self.current().position,
                text: String::new(),
                value: Object::Null,
            }
        }
    }

    pub(crate) fn parse_compilation_unit(&mut self) -> CompilationUnitSyntax {
        let expression = self.parse_statement();
        let end_of_file_token = self.match_token(SyntaxKind::EndOfFileToken);
        CompilationUnitSyntax {
            statement: *expression,
            end_of_file_token,
        }
    }

    fn parse_statement(&mut self) -> Box<StatementSyntax> {
        match self.current().kind {
            SyntaxKind::OpenBraceToken => {
                Box::new(StatementSyntax::Block(self.parse_block_statement()))
            }
            SyntaxKind::LetKeyword | SyntaxKind::VarKeyword => Box::new(
                StatementSyntax::VariableDeclaration(self.parse_variable_declaration_statement()),
            ),
            SyntaxKind::IfKeyword => Box::new(StatementSyntax::If(self.parse_if_statement())),
            _ => Box::new(StatementSyntax::Expression(
                self.parse_expression_statement(),
            )),
        }
    }

    fn parse_block_statement(&mut self) -> BlockStatementSyntax {
        let mut statements = Vec::new();

        let open_brace_token = self.match_token(SyntaxKind::OpenBraceToken);
        while ![SyntaxKind::EndOfFileToken, SyntaxKind::CloseBraceToken]
            .contains(&self.current().kind)
        {
            let statement = self.parse_statement();
            statements.push(*statement);
        }
        let close_brace_token = self.match_token(SyntaxKind::CloseBraceToken);
        BlockStatementSyntax {
            open_brace_token,
            statements,
            close_brace_token,
        }
    }

    fn parse_expression_statement(&mut self) -> ExpressionStatementSyntax {
        let expression = self.parse_expression();
        ExpressionStatementSyntax {
            expression: *expression,
        }
    }

    fn parse_expression(&mut self) -> Box<ExpressionSyntax> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Box<ExpressionSyntax> {
        if self.peek(0).kind == SyntaxKind::IdentifierToken
            && self.peek(1).kind == SyntaxKind::EqualsToken
        {
            let identifier_token = self.next_token();
            let equals_token = self.next_token();
            let expression = self.parse_assignment_expression();
            Box::new(ExpressionSyntax::Assignment(AssignmentExpressionSyntax {
                identifier_token,
                equals_token,
                expression,
            }))
        } else {
            self.parse_binary_expression(0)
        }
    }

    fn parse_binary_expression(&mut self, parent_precedence: usize) -> Box<ExpressionSyntax> {
        let unary_operator_precedence = self.current().kind.get_unary_operator_precedence();
        let mut left =
            if unary_operator_precedence != 0 && unary_operator_precedence >= parent_precedence {
                let operator_token = self.next_token();
                let operand = self.parse_binary_expression(unary_operator_precedence);
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
            let right = self.parse_binary_expression(precedence);
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
            SyntaxKind::OpenParenthesisToken => self.parse_parenthesized_expression(),
            SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword => self.parse_boolean_literal(),
            SyntaxKind::NumberToken => self.parse_number_literal(),
            _ => self.parse_name_expression(),
        }
    }

    fn parse_number_literal(&mut self) -> Box<ExpressionSyntax> {
        let number_token = self.match_token(SyntaxKind::NumberToken);
        Box::new(ExpressionSyntax::Literal(LiteralExpressionSyntax {
            value: number_token.value.clone(),
            literal_token: number_token,
        }))
    }

    fn parse_parenthesized_expression(&mut self) -> Box<ExpressionSyntax> {
        let open_parenthesis_token = self.match_token(SyntaxKind::OpenParenthesisToken);
        let expression = self.parse_expression();
        let close_parenthesis_token = self.match_token(SyntaxKind::CloseParenthesisToken);
        Box::new(ExpressionSyntax::Parenthesized(
            ParenthesizedExpressionSyntax {
                open_parenthesis_token,
                expression,
                close_parenthesis_token,
            },
        ))
    }

    fn parse_boolean_literal(&mut self) -> Box<ExpressionSyntax> {
        let value = self.current().kind == SyntaxKind::TrueKeyword;
        let keyword_token = self.match_token(if value {
            SyntaxKind::TrueKeyword
        } else {
            SyntaxKind::FalseKeyword
        });
        Box::new(ExpressionSyntax::Literal(LiteralExpressionSyntax {
            literal_token: keyword_token,
            value: Object::Boolean(value),
        }))
    }

    fn parse_name_expression(&mut self) -> Box<ExpressionSyntax> {
        let identifier_token = self.match_token(SyntaxKind::IdentifierToken);
        Box::new(ExpressionSyntax::Name(NameExpressionSyntax {
            identifier_token,
        }))
    }

    fn parse_variable_declaration_statement(&mut self) -> VariableDeclarationStatementSyntax {
        let expected = if self.current().kind == SyntaxKind::LetKeyword {
            SyntaxKind::LetKeyword
        } else {
            SyntaxKind::VarKeyword
        };
        let keyword = self.match_token(expected);
        let identifier = self.match_token(SyntaxKind::IdentifierToken);
        let equals_token = self.match_token(SyntaxKind::EqualsToken);
        let initializer = self.parse_expression();
        VariableDeclarationStatementSyntax {
            keyword,
            identifier,
            equals_token,
            initializer: *initializer,
        }
    }

    fn parse_if_statement(&mut self) -> IfStatementSyntax {
        let if_keyword = self.match_token(SyntaxKind::IfKeyword);
        let condition = self.parse_expression();
        let then_statement = self.parse_statement();
        let else_clause = self.parse_else_clause();
        IfStatementSyntax {
            if_keyword,
            condition: *condition,
            then_statement,
            else_clause,
        }
    }

    fn parse_else_clause(&mut self) -> Option<ElseClauseSyntax> {
        if self.current().kind != SyntaxKind::ElseKeyword {
            return None;
        }

        let else_keyword = self.next_token();
        let else_statement = self.parse_statement();
        Some(ElseClauseSyntax {
            else_keyword,
            else_statement,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::syntax::expressions::ExpressionSyntax;
    use crate::syntax::statements::StatementSyntax;
    use crate::syntax::SyntaxKind;
    use crate::syntax::SyntaxNodeRef;
    use crate::syntax::SyntaxTree;
    use strum::IntoEnumIterator;

    struct AssertingIterator<'tree> {
        iterator: vec::IntoIter<SyntaxNodeRef<'tree>>,
        had_error: bool,
    }

    impl<'tree> AssertingIterator<'tree> {
        fn new(node: SyntaxNodeRef<'tree>) -> Self {
            Self {
                iterator: Self::flatten(node).into_iter(),
                had_error: false,
            }
        }

        fn flatten(node: SyntaxNodeRef) -> Vec<SyntaxNodeRef> {
            let mut stack = vec![node];
            let mut result = Vec::new();
            while let Some(n) = stack.pop() {
                result.push(n);
                for child in n.children().into_iter().rev() {
                    stack.push(child);
                }
            }
            result
        }

        fn assert_token(&mut self, kind: SyntaxKind, text: &str) {
            let current = self.iterator.next();
            assert!(current.is_some());
            let current = current.unwrap();
            match current {
                SyntaxNodeRef::Token(t) => {
                    if kind != t.kind || text != t.text {
                        self.had_error = true;
                    }
                    assert_eq!(kind, t.kind);
                    assert_eq!(text, t.text);
                }
                _ => {
                    self.had_error = true;
                    panic!("not a token: {:?}", current);
                }
            }
        }

        fn assert_node(&mut self, kind: SyntaxKind) {
            let current = self.iterator.next();
            assert!(current.is_some());
            let current = current.unwrap();
            match current {
                SyntaxNodeRef::Token(t) => {
                    self.had_error = true;
                    panic!("is a token: {:?}", t);
                }
                _ => {
                    if kind != current.kind() {
                        self.had_error = true;
                    }
                    assert_eq!(kind, current.kind());
                }
            }
        }
    }

    impl<'tree> Drop for AssertingIterator<'tree> {
        fn drop(&mut self) {
            if !self.had_error {
                assert!(self.iterator.next().is_none());
            }
        }
    }

    fn parse_expression(input: &str) -> ExpressionSyntax {
        let statement = SyntaxTree::parse(input).root.statement;
        match statement {
            StatementSyntax::Expression(s) => s.expression,
            _ => panic!("not an expression: {:?}", statement),
        }
    }

    #[test]
    fn binary_expression_honors_precedence() {
        for (op1, op2) in get_binary_operator_pairs() {
            let op1_precedence = op1.get_binary_operator_precedence();
            let op2_precedence = op2.get_binary_operator_precedence();
            let op1_text = op1.get_text().unwrap();
            let op2_text = op2.get_text().unwrap();
            let text = format!("a {} b {} c", op1_text, op2_text);
            let expression = parse_expression(&text);

            let mut e = AssertingIterator::new(SyntaxNodeRef::Expression(expression.create_ref()));
            e.assert_node(SyntaxKind::BinaryExpression);
            if op1_precedence >= op2_precedence {
                e.assert_node(SyntaxKind::BinaryExpression);
                e.assert_node(SyntaxKind::NameExpression);
                e.assert_token(SyntaxKind::IdentifierToken, "a");
                e.assert_token(op1, op1_text);
            } else {
                e.assert_node(SyntaxKind::NameExpression);
                e.assert_token(SyntaxKind::IdentifierToken, "a");
                e.assert_token(op1, op1_text);
                e.assert_node(SyntaxKind::BinaryExpression);
            }
            e.assert_node(SyntaxKind::NameExpression);
            e.assert_token(SyntaxKind::IdentifierToken, "b");
            e.assert_token(op2, op2_text);
            e.assert_node(SyntaxKind::NameExpression);
            e.assert_token(SyntaxKind::IdentifierToken, "c");
        }
    }

    #[test]
    fn unary_operator_honors_precedence() {
        for (unary, binary) in get_unary_operator_pairs() {
            let unary_precedence = unary.get_unary_operator_precedence();
            let binary_precedence = binary.get_binary_operator_precedence();
            let unary_text = unary.get_text().unwrap();
            let binary_text = binary.get_text().unwrap();
            let text = format!("{} a {} b", unary_text, binary_text);
            let expression = parse_expression(&text);

            let mut e = AssertingIterator::new(SyntaxNodeRef::Expression(expression.create_ref()));
            if unary_precedence >= binary_precedence {
                e.assert_node(SyntaxKind::BinaryExpression);
                e.assert_node(SyntaxKind::UnaryExpression);
                e.assert_token(unary, unary_text);
            } else {
                e.assert_node(SyntaxKind::UnaryExpression);
                e.assert_token(unary, unary_text);
                e.assert_node(SyntaxKind::BinaryExpression);
            }
            e.assert_node(SyntaxKind::NameExpression);
            e.assert_token(SyntaxKind::IdentifierToken, "a");
            e.assert_token(binary, binary_text);
            e.assert_node(SyntaxKind::NameExpression);
            e.assert_token(SyntaxKind::IdentifierToken, "b");
        }
    }

    fn get_unary_operator_pairs() -> Vec<(SyntaxKind, SyntaxKind)> {
        let mut result = Vec::new();
        for unary in get_unary_operator_kinds() {
            for binary in get_binary_operator_kinds() {
                result.push((unary, binary));
            }
        }
        result
    }

    fn get_unary_operator_kinds() -> Vec<SyntaxKind> {
        SyntaxKind::iter()
            .filter(|k| k.get_unary_operator_precedence() > 0)
            .collect()
    }

    fn get_binary_operator_kinds() -> Vec<SyntaxKind> {
        SyntaxKind::iter()
            .filter(|k| k.get_binary_operator_precedence() > 0)
            .collect()
    }

    fn get_binary_operator_pairs() -> Vec<(SyntaxKind, SyntaxKind)> {
        let mut result = Vec::new();
        for op1 in get_binary_operator_kinds() {
            for op2 in get_binary_operator_kinds() {
                result.push((op1, op2));
            }
        }
        result
    }
}
