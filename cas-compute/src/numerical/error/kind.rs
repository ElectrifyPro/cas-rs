use cas_attrs::ErrorKind;

/// An argument to a function call has the wrong type.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!(
        "incorrect type for argument #{} for the `{}` function",
        self.index + 1,
        self.name
    ),
    labels = [
        "this function call".to_string(),
        "".to_string(),
        format!("this argument has type `{}`", self.given),
    ],
    help = format!("should be of type `{}`", self.expected),
)]
pub struct TypeMismatch {
    /// The name of the function that was called.
    pub name: String,

    /// The index of the argument that was mismatched.
    pub index: usize,

    /// The type of the argument that was expected.
    pub expected: &'static str,

    /// The type of the argument that was given.
    pub given: &'static str,
}

/// The stack overflowed while evaluating a function call.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "maximum stack depth exceeded",
    labels = ["this function called itself too many times", ""],
    help = "the maximum stack depth is equal to: `2^11`"
)]
pub struct StackOverflow;
