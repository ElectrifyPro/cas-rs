use std::fmt::{Display, Formatter};

/// Represents any value that can be stored in a variable.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A number value.
    Number(f64),

    /// The unit type, analogous to `()` in Rust.
    Unit,
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Number(n)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Unit => write!(f, "()"),
        }
    }
}
