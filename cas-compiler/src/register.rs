#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Register {
    /// The arg-counter register, used to keep track of the number of arguments passed to a
    /// function.
    ArgCounter,
}
