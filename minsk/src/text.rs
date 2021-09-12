use crate::plumbing::ObjectKind;

#[derive(Debug, PartialEq)]
pub struct TextSpan {
    pub start: usize,
    pub length: usize,
}

impl TextSpan {
    pub fn new(start: usize, length: usize) -> Self {
        Self { start, length }
    }

    pub fn end(&self) -> usize {
        self.start + self.length
    }

    pub(crate) fn from_bounds(start: usize, end: usize) -> TextSpan {
        TextSpan {
            start,
            length: end - start,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableSymbol {
    pub name: String,
    pub kind: ObjectKind,
}
