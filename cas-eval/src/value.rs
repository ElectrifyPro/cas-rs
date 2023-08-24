use rug::Float;
use std::fmt::{Display, Formatter};
use super::consts::float;

/// Represents any value that can be stored in a variable.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A number value.
    Number(Float),

    /// The unit type, analogous to `()` in Rust.
    Unit,
}

#[cfg(test)]
impl Value {
    /// Returns true if two values are numbers, and they are approximately equal.
    pub fn approx_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => float(a - b) / float(a) < float(1e-3),
            _ => false,
        }
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Number(float(n))
    }
}

impl From<Float> for Value {
    fn from(n: Float) -> Self {
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
