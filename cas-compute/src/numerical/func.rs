use super::builtin::Builtin;

/// A function.
///
/// Functions are treated as values just like any other value in `cas-rs`; they can be stored
/// in variables, passed as arguments to other functions, and returned from functions.
#[derive(Debug, Clone)]
pub enum Function {
    /// A user-defined function.
    ///
    /// The inner value is a `usize` that represents the index of the function's chunk.
    User(usize),

    /// A built-in function.
    Builtin(&'static dyn Builtin),
}

/// Manual implementation of [`PartialEq`] to support `dyn Builtin` by comparing pointers.
///
/// TODO: invalid implementation for `User` variant
impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::User(a), Self::User(b)) => a == b,
            (Self::Builtin(a), Self::Builtin(b)) => std::ptr::eq(*a, *b),
            _ => false,
        }
    }
}
