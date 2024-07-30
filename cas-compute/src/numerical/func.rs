use std::collections::{HashMap, HashSet};
use super::{builtin::Builtin, value::Value};

/// A function.
///
/// Functions are treated as values just like any other value in `cas-rs`; they can be stored
/// in variables, passed as arguments to other functions, and returned from functions.
#[derive(Debug, Clone)]
pub enum Function {
    /// A user-defined function.
    ///
    /// The inner value is a `usize` that represents the index of the function's chunk.
    User(User),

    /// A built-in function.
    Builtin(&'static dyn Builtin),
}

/// A user-defined function.
#[derive(Debug, Clone, PartialEq)]
pub struct User {
    /// The index of the function's chunk.
    ///
    /// TODO: comparing index is not enough to compare two functions
    pub index: usize,

    /// The variables captured by the function from the environment.
    ///
    /// This is determined at compile time.
    pub captures: HashSet<usize>,

    /// The values of the variables in the function's environment at the time of the function's
    /// creation.
    ///
    /// This is determined at runtime.
    pub environment: HashMap<usize, Value>,
}

impl User {
    /// Creates a new user-defined function with the given chunk index an captured variables.
    pub fn new(index: usize, captures: HashSet<usize>) -> Self {
        Self { index, captures, environment: HashMap::new() }
    }
}

/// Manual implementation of [`PartialEq`] to support `dyn Builtin` by comparing pointers.
impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::User(a), Self::User(b)) => a == b,
            (Self::Builtin(a), Self::Builtin(b)) => std::ptr::eq(*a, *b),
            _ => false,
        }
    }
}
