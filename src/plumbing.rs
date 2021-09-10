use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Object {
    Null,
    Number(i64),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Null => write!(f, "<null>"),
            Object::Number(i) => write!(f, "{}", i),
        }
    }
}
