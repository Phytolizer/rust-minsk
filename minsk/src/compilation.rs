use std::collections::HashMap;

use crate::binding::Binder;
use crate::diagnostic::DiagnosticBag;
use crate::evaluator::Evaluator;
use crate::plumbing::Object;
use crate::syntax::SyntaxTree;

pub type EvaluationResult = Result<Object, DiagnosticBag>;

pub struct Compilation {
    pub syntax: SyntaxTree,
}

impl Compilation {
    pub fn new(syntax: SyntaxTree) -> Self {
        Self { syntax }
    }

    pub fn evaluate(self, variables: &mut HashMap<String, Object>) -> EvaluationResult {
        let mut binder = Binder::new(variables);
        let bound_expression = binder.bind_expression(self.syntax.root.create_ref());
        let diagnostics = self
            .syntax
            .diagnostics
            .into_iter()
            .chain(binder.diagnostics.into_iter())
            .collect::<DiagnosticBag>();
        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        let mut evaluator = Evaluator::new(variables);
        let value = evaluator.evaluate(&bound_expression);
        Ok(value)
    }
}
