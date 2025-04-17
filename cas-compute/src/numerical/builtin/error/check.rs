use std::ops::Range;

/// Too many arguments were given to a function call.
#[derive(Debug)]
pub struct TooManyArguments {
    /// The name of the function that was called.
    pub name: &'static str,

    /// The number of arguments that were expected.
    pub expected: usize,

    /// The number of arguments that were given.
    pub given: usize,

    /// The signature of the function.
    pub signature: &'static str,
}

/// An argument to a function call is missing.
#[derive(Debug)]
pub struct MissingArgument {
    /// The name of the function that was called.
    pub name: &'static str,

    /// The indices of the missing arguments.
    pub indices: Range<usize>,

    /// The number of arguments that were expected.
    pub expected: usize,

    /// The number of arguments that were given.
    pub given: usize,

    /// The signature of the function.
    pub signature: &'static str,
}

/// An argument to a function call has the wrong type.
#[derive(Debug)]
pub struct TypeMismatch {
    /// The name of the function that was called.
    pub name: &'static str,

    /// The index of the argument that was mismatched.
    pub index: usize,

    /// The type of the argument that was expected.
    pub expected: &'static str,

    /// The type of the argument that was given.
    pub given: &'static str,

    /// The signature of the function, not including the function name.
    pub signature: &'static str,
}
