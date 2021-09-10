use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Object {
    Null,
    Number(i64),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ObjectKind {
    Null,
    Number,
}

impl Object {
    pub(crate) fn kind(&self) -> ObjectKind {
        match self {
            Object::Null => ObjectKind::Null,
            Object::Number(_) => ObjectKind::Number,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Null => write!(f, "<null>"),
            Object::Number(i) => write!(f, "{}", i),
        }
    }
}
