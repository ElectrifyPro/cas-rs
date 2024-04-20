/// An argument to a function call has the wrong type.
#[derive(Debug)]
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

// TODO missing argument / too many argument errors?
