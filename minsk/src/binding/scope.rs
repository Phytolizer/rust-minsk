use std::collections::HashMap;

use crate::diagnostic::Diagnostic;
use crate::text::VariableSymbol;

use super::BoundExpression;

pub(crate) struct BoundScope {
    pub(crate) parent: Option<Box<BoundScope>>,
    variables: HashMap<String, VariableSymbol>,
}

impl BoundScope {
    pub(crate) fn new(parent: Option<Box<BoundScope>>) -> Self {
        Self {
            parent,
            variables: HashMap::new(),
        }
    }

    pub(crate) fn try_declare(&mut self, variable: VariableSymbol) -> bool {
        if self.variables.contains_key(&variable.name) {
            false
        } else {
            self.variables.insert(variable.name.clone(), variable);
            true
        }
    }

    pub(crate) fn try_lookup<'a>(&'a self, name: &str) -> Option<&'a VariableSymbol> {
        if let Some(variable) = self.variables.get(name) {
            return Some(variable);
        }

        if let Some(parent) = &self.parent {
            return parent.try_lookup(name);
        }

        None
    }

    pub(crate) fn get_declared_variables(&self) -> Vec<&VariableSymbol> {
        self.variables.values().collect()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BoundGlobalScope {
    pub(crate) previous: Option<Box<BoundGlobalScope>>,
    pub(crate) diagnostics: Vec<Diagnostic>,
    pub(crate) variables: Vec<VariableSymbol>,
    pub(crate) expression: BoundExpression,
}
