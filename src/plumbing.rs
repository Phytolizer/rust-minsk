use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Object {
    Null,
    Number(i64),
    Boolean(bool),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ObjectKind {
    Null,
    Number,
    Boolean,
}

impl Object {
    pub(crate) fn kind(&self) -> ObjectKind {
        match self {
            Object::Null => ObjectKind::Null,
            Object::Number(_) => ObjectKind::Number,
            Object::Boolean(_) => ObjectKind::Boolean,
        }
    }

    pub(crate) fn as_number(&self) -> i64 {
        match self {
            Object::Number(n) => *n,
            _ => panic!("not a number: {}", self),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Null => write!(f, "<null>"),
            Object::Number(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
        }
    }
}
