use std::collections::HashMap;

use crate::binding::scope::BoundGlobalScope;
use crate::binding::Binder;
use crate::diagnostic::DiagnosticBag;
use crate::evaluator::Evaluator;
use crate::plumbing::Object;
use crate::syntax::SyntaxTree;
use crate::text::VariableSymbol;

pub type EvaluationResult = Result<Object, DiagnosticBag>;

#[derive(Clone)]
pub struct Compilation {
    previous: Option<Box<Compilation>>,
    pub syntax: SyntaxTree,
    global_scope: Option<BoundGlobalScope>,
}

impl Compilation {
    pub fn new(syntax: SyntaxTree) -> Self {
        Self {
            previous: None,
            syntax,
            global_scope: None,
        }
    }

    fn new_continued(previous: Compilation, syntax: SyntaxTree) -> Self {
        Self {
            previous: Some(Box::new(previous)),
            syntax,
            global_scope: None,
        }
    }

    pub(crate) fn global_scope(&mut self) -> &BoundGlobalScope {
        if self.global_scope.is_none() {
            let mut global_scope = Some(Binder::bind_global_scope(
                self.previous
                    .as_ref()
                    .map(|p| p.global_scope.as_ref().unwrap()),
                self.syntax.root.create_ref(),
            ));
            std::mem::swap(&mut global_scope, &mut self.global_scope);
        }
        self.global_scope.as_ref().unwrap()
    }

    pub fn evaluate(&mut self, variables: &mut HashMap<VariableSymbol, Object>) -> EvaluationResult {
        let global_scope = Binder::bind_global_scope(
            self.previous.as_mut().map(|p| p.global_scope()),
            self.syntax.root.create_ref(),
        );
        let diagnostics = self
            .syntax
            .diagnostics
            .clone()
            .into_iter()
            .chain(global_scope.diagnostics.into_iter())
            .collect::<DiagnosticBag>();
        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        let mut evaluator = Evaluator::new(variables);
        let value = evaluator.evaluate(&global_scope.statement);
        Ok(value)
    }

    pub fn continue_with(self, syntax: SyntaxTree) -> Self {
        Self::new_continued(self, syntax)
    }
}
