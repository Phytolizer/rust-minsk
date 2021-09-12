use std::fmt::Display;
use std::iter::FromIterator;

use crate::plumbing::ObjectKind;
use crate::syntax::SyntaxKind;
use crate::text::TextSpan;

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub span: TextSpan,
    pub message: String,
}

impl Diagnostic {
    fn new(span: TextSpan, message: String) -> Self {
        Self { span, message }
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Debug, PartialEq)]
pub struct DiagnosticBag {
    pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBag {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    fn report(&mut self, span: TextSpan, message: String) {
        self.diagnostics.push(Diagnostic::new(span, message));
    }

    pub(crate) fn report_invalid_number(&mut self, span: TextSpan, text: &str, kind: ObjectKind) {
        let message = format!("Invalid {:?}: {}", kind, text);
        self.report(span, message);
    }

    pub(crate) fn report_bad_character(&mut self, position: usize, character: char) {
        let message = format!("Bad character input: '{}'", character);
        let span = TextSpan::new(position, 1);
        self.report(span, message);
    }

    pub(crate) fn report_unexpected_token(
        &mut self,
        span: TextSpan,
        actual_kind: SyntaxKind,
        expected_kind: SyntaxKind,
    ) {
        let message = format!(
            "Unexpected token <{:?}>, expected <{:?}>.",
            actual_kind, expected_kind
        );
        self.report(span, message);
    }

    pub(crate) fn report_undefined_binary_operator(
        &mut self,
        span: TextSpan,
        operator: String,
        left_type: ObjectKind,
        right_type: ObjectKind,
    ) {
        let message = format!(
            "Binary operator '{}' is not defined for types {:?} and {:?}.",
            operator, left_type, right_type
        );
        self.report(span, message);
    }

    pub(crate) fn report_undefined_unary_operator(
        &mut self,
        span: TextSpan,
        operator: String,
        operand_type: ObjectKind,
    ) {
        let message = format!(
            "Unary operator '{}' is not defined for type {:?}.",
            operator, operand_type
        );
        self.report(span, message);
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub(crate) fn report_undefined_name(&mut self, span: TextSpan, name: &str) {
        let message = format!("Undefined name '{}'.", name);
        self.report(span, message);
    }

    pub(crate) fn report_variable_already_declared(&mut self, span: TextSpan, name: &str) {
        let message = format!("Variable '{}' is already declared.", name);
        self.report(span, message);
    }

    pub(crate) fn report_cannot_convert(
        &mut self,
        span: TextSpan,
        from_type: ObjectKind,
        to_type: ObjectKind,
    ) {
        let message = format!("Cannot convert {:?} to {:?}.", from_type, to_type);
        self.report(span, message);
    }
}

impl Default for DiagnosticBag {
    fn default() -> Self {
        Self::new()
    }
}

impl IntoIterator for DiagnosticBag {
    type Item = Diagnostic;

    type IntoIter = std::vec::IntoIter<Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.into_iter()
    }
}

impl FromIterator<Diagnostic> for DiagnosticBag {
    fn from_iter<I: IntoIterator<Item = Diagnostic>>(iter: I) -> Self {
        Self {
            diagnostics: iter.into_iter().collect(),
        }
    }
}
