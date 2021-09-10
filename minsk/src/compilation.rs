use crate::binding::Binder;
use crate::evaluator::Evaluator;
use crate::plumbing::Object;
use crate::syntax::SyntaxTree;

pub type EvaluationResult = Result<Object, Vec<String>>;

pub struct Compilation {
    pub syntax: SyntaxTree,
}

impl Compilation {
    pub fn new(syntax: SyntaxTree) -> Self {
        Self { syntax }
    }

    pub fn evaluate(self) -> EvaluationResult {
        let mut binder = Binder::new();
        let bound_expression = binder.bind_expression(self.syntax.root.create_ref());
        let diagnostics = self
            .syntax
            .diagnostics
            .into_iter()
            .chain(binder.diagnostics.into_iter())
            .collect::<Vec<_>>();
        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        let evaluator = Evaluator::new(bound_expression);
        let value = evaluator.evaluate();
        Ok(value)
    }
}
