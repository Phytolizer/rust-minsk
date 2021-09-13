use std::collections::HashMap;

use crate::binding::BoundBinaryExpression;
use crate::binding::BoundBinaryOperatorKind;
use crate::binding::BoundBlockStatement;
use crate::binding::BoundExpression;
use crate::binding::BoundExpressionStatement;
use crate::binding::BoundLiteralExpression;
use crate::binding::BoundStatement;
use crate::binding::BoundUnaryExpression;
use crate::binding::BoundUnaryOperatorKind;
use crate::binding::BoundVariableDeclarationStatement;
use crate::plumbing::Object;
use crate::text::VariableSymbol;

pub(crate) struct Evaluator<'v> {
    variables: &'v mut HashMap<VariableSymbol, Object>,
    last_value: Object,
}

impl<'v> Evaluator<'v> {
    pub(crate) fn evaluate(&mut self, root: &BoundStatement) -> Object {
        self.evaluate_statement(root);
        self.last_value.clone()
    }

    fn evaluate_assignment_expression(
        &mut self,
        e: &crate::binding::BoundAssignmentExpression,
    ) -> Object {
        let value = self.evaluate_expression(&e.expression);
        self.variables.insert(e.variable.clone(), value.clone());
        value
    }

    fn evaluate_binary_expression(&mut self, e: &BoundBinaryExpression) -> Object {
        let left = self.evaluate_expression(&e.left);
        let right = self.evaluate_expression(&e.right);
        match e.operator.kind {
            BoundBinaryOperatorKind::Addition => {
                Object::Number(left.as_number() + right.as_number())
            }
            BoundBinaryOperatorKind::Subtraction => {
                Object::Number(left.as_number() - right.as_number())
            }
            BoundBinaryOperatorKind::Multiplication => {
                Object::Number(left.as_number() * right.as_number())
            }
            BoundBinaryOperatorKind::Division => {
                Object::Number(left.as_number() / right.as_number())
            }
            BoundBinaryOperatorKind::LogicalAnd => {
                Object::Boolean(left.as_boolean() && right.as_boolean())
            }
            BoundBinaryOperatorKind::LogicalOr => {
                Object::Boolean(left.as_boolean() || right.as_boolean())
            }
            BoundBinaryOperatorKind::Equality => Object::Boolean(left == right),
            BoundBinaryOperatorKind::Inequality => Object::Boolean(left != right),
        }
    }

    fn evaluate_expression(&mut self, expr: &BoundExpression) -> Object {
        match expr {
            BoundExpression::Binary(e) => self.evaluate_binary_expression(e),
            BoundExpression::Unary(e) => self.evaluate_unary_expression(e),
            BoundExpression::Literal(e) => self.evaluate_literal_expression(e),
            BoundExpression::Variable(e) => self.evaluate_variable_expression(e),
            BoundExpression::Assignment(e) => self.evaluate_assignment_expression(e),
        }
    }

    fn evaluate_literal_expression(&self, e: &BoundLiteralExpression) -> Object {
        e.value.clone()
    }

    fn evaluate_statement(&mut self, root: &BoundStatement) {
        match root {
            BoundStatement::Block(s) => self.evaluate_block_statement(s),
            BoundStatement::Expression(s) => self.evaluate_expression_statement(s),
            BoundStatement::VariableDeclaration(s) => {
                self.evaluate_variable_declaration_statement(s)
            }
        }
    }

    fn evaluate_unary_expression(&mut self, e: &BoundUnaryExpression) -> Object {
        let operand = self.evaluate_expression(&e.operand);
        match e.operator.kind {
            BoundUnaryOperatorKind::Identity => operand,
            BoundUnaryOperatorKind::Negation => Object::Number(-operand.as_number()),
            BoundUnaryOperatorKind::LogicalNegation => Object::Boolean(!operand.as_boolean()),
        }
    }

    fn evaluate_variable_expression(&self, e: &crate::binding::BoundVariableExpression) -> Object {
        let value = self.variables.get(&e.variable).unwrap();
        value.clone()
    }

    pub(crate) fn new(variables: &'v mut HashMap<VariableSymbol, Object>) -> Self {
        Self {
            variables,
            last_value: Object::Null,
        }
    }

    fn evaluate_block_statement(&mut self, s: &BoundBlockStatement) {
        for statement in s.statements.iter() {
            self.evaluate_statement(statement);
        }
    }

    fn evaluate_expression_statement(&mut self, s: &BoundExpressionStatement) {
        self.last_value = self.evaluate_expression(&s.expression);
    }

    fn evaluate_variable_declaration_statement(&mut self, s: &BoundVariableDeclarationStatement) {
        let value = self.evaluate_expression(&s.initializer);
        self.variables.insert(s.variable.clone(), value.clone());
        self.last_value = value;
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::iter::once;

    use crate::compilation::Compilation;
    use crate::plumbing::Object;
    use crate::syntax::SyntaxTree;
    use crate::text::TextSpan;

    struct AnnotatedText {
        text: String,
        spans: Vec<TextSpan>,
    }

    impl AnnotatedText {
        fn parse(text: &str) -> Self {
            let text = Self::unindent(text);

            let mut text_builder = Vec::new();
            let mut spans_builder = Vec::new();
            let mut start_stack = Vec::new();
            let mut position = 0;

            for c in text {
                match c {
                    '[' => {
                        start_stack.push(position);
                    }
                    ']' => {
                        assert!(!start_stack.is_empty());
                        let start = start_stack.pop().unwrap();
                        let end = position;
                        spans_builder.push(TextSpan::from_bounds(start, end));
                    }
                    _ => {
                        position += 1;
                        text_builder.push(c);
                    }
                }
            }

            assert!(start_stack.is_empty());

            Self {
                text: text_builder.into_iter().collect(),
                spans: spans_builder,
            }
        }

        fn unindent(text: &str) -> Vec<char> {
            let mut lines = Vec::new();
            for line in text.lines() {
                lines.push(line.to_string());
            }
            let mut min_indentation = std::usize::MAX;
            for line in &lines {
                let indentation = line.len() - line.trim_start().len();
                if !line.trim().is_empty() && indentation < min_indentation {
                    min_indentation = indentation;
                }
            }

            lines = lines
                .iter()
                .map(|line| {
                    if line.trim().is_empty() { "" } else { line }
                        .chars()
                        .skip(min_indentation)
                        .collect()
                })
                .collect::<Vec<_>>();
            while lines.first().map(|line| line.is_empty()).unwrap_or(false) {
                lines.remove(0);
            }
            while lines.last().map(|line| line.is_empty()).unwrap_or(false) {
                lines.pop();
            }
            lines
                .into_iter()
                .map(|line| {
                    line.chars()
                        .collect::<Vec<_>>()
                        .into_iter()
                        .chain(once('\n'))
                })
                .flatten()
                .collect()
        }

        fn unindent_lines(text: &str) -> Vec<String> {
            let mut lines = Vec::new();
            for line in text.lines() {
                lines.push(line.to_string());
            }
            let mut min_indentation = std::usize::MAX;
            for line in &lines {
                let indentation = line.len() - line.trim_start().len();
                if !line.trim().is_empty() && indentation < min_indentation {
                    min_indentation = indentation;
                }
            }

            lines = lines
                .iter()
                .map(|line| {
                    if line.trim().is_empty() { "" } else { line }
                        .chars()
                        .skip(min_indentation)
                        .collect()
                })
                .collect::<Vec<_>>();
            while lines.first().map(|line| line.is_empty()).unwrap_or(false) {
                lines.remove(0);
            }
            while lines.last().map(|line| line.is_empty()).unwrap_or(false) {
                lines.pop();
            }
            lines
        }
    }

    fn get_value_tests() -> Vec<(&'static str, Object)> {
        vec![
            ("1", Object::Number(1)),
            ("-1", Object::Number(-1)),
            ("+1", Object::Number(1)),
            ("1 + 2", Object::Number(3)),
            ("1 - 2", Object::Number(-1)),
            ("1 * 2", Object::Number(2)),
            ("9 / 3", Object::Number(3)),
            ("(10)", Object::Number(10)),
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
            ("true && false", Object::Boolean(false)),
            ("true && true", Object::Boolean(true)),
            ("true || false", Object::Boolean(true)),
            ("true || true", Object::Boolean(true)),
            ("false || false", Object::Boolean(false)),
            ("true == false", Object::Boolean(false)),
            ("1 == 1", Object::Boolean(true)),
            ("2 == 3", Object::Boolean(false)),
            ("true != false", Object::Boolean(true)),
            ("true != true", Object::Boolean(false)),
            ("1 != 2", Object::Boolean(true)),
            ("1 != 1", Object::Boolean(false)),
            ("{ var a = 0 (a = 10) * a }", Object::Number(100)),
        ]
    }

    #[test]
    fn computes_correct_value() {
        for (text, value) in get_value_tests() {
            let syntax_tree = SyntaxTree::parse(text);
            let mut compilation = Compilation::new(syntax_tree);
            let mut variables = HashMap::new();
            let actual_result = compilation.evaluate(&mut variables);

            if actual_result != Ok(value.clone()) {
                panic!(
                    "input: {}, expected result: {:?}, actual result: {:?}",
                    text, value, actual_result
                );
            }
        }
    }

    fn assert_diagnostics(text: &str, diagnostic_text: &str) {
        let annotated_text = AnnotatedText::parse(text);
        let syntax_tree = SyntaxTree::parse(&annotated_text.text);
        let mut compilation = Compilation::new(syntax_tree);
        let mut variables = HashMap::new();
        let result = compilation.evaluate(&mut variables);
        let diagnostics = AnnotatedText::unindent_lines(diagnostic_text);
        if let Ok(value) = result {
            panic!("not an error: {:?}", value);
        }
        assert_eq!(annotated_text.spans.len(), diagnostics.len());
        assert_eq!(diagnostics.len(), result.as_ref().unwrap_err().len());

        for ((diagnostic, expected_span), actual_diagnostic) in diagnostics
            .iter()
            .zip(annotated_text.spans.iter())
            .zip(result.unwrap_err().into_iter())
        {
            let expected_message = diagnostic;
            let actual_message = &actual_diagnostic.message;
            assert_eq!(expected_message, actual_message);

            let actual_span = &actual_diagnostic.span;
            assert_eq!(expected_span, actual_span);
        }
    }

    #[test]
    fn variable_declaration_reports_redeclaration() {
        let text = "
            {
                var x = 10
                var y = 100
                {
                    var x = 10
                }
                var [x] = 5
            }
        ";

        let diagnostics = "
            Variable 'x' is already declared.
        ";

        assert_diagnostics(text, diagnostics);
    }

    #[test]
    fn name_expression_reports_undefined() {
        let text = "[a]";
        let diagnostics = "
            Undefined name 'a'.
        ";
        assert_diagnostics(text, diagnostics);
    }

    #[test]
    fn assignment_expression_reports_undefined() {
        let text = "[x] = 10";
        let diagnostics = "
            Undefined name 'x'.
        ";
        assert_diagnostics(text, diagnostics);
    }

    #[test]
    fn assignment_expression_reports_cannot_assign() {
        let text = "
            {
                let x = 10
                x [=] 0
            }
        ";
        let diagnostics = "
            Variable 'x' is read-only and cannot be assigned to.
        ";
        assert_diagnostics(text, diagnostics);
    }

    #[test]
    fn assignment_expression_reports_cannot_convert() {
        let text = "
            {
                var x = 10
                x = [true]
            }
        ";
        let diagnostics = "
            Cannot convert Boolean to Number.
        ";
        assert_diagnostics(text, diagnostics);
    }

    #[test]
    fn unary_expression_reports_undefined() {
        let text = "[+]true";
        let diagnostics = "
            Unary operator '+' is not defined for type Boolean.
        ";
        assert_diagnostics(text, diagnostics);
    }

    #[test]
    fn binary_expression_reports_undefined() {
        let text = "10 [*] false";
        let diagnostics = "
            Binary operator '*' is not defined for types Number and Boolean.
        ";
        assert_diagnostics(text, diagnostics);
    }
}
