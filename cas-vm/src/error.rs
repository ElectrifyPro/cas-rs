use cas_attrs::ErrorKind;
use cas_parser::parser::token::op::{BinOpKind, UnaryOpKind};

/// The given binary operation cannot be applied to the given operands.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("cannot apply the `{:?}` operator to these operands", self.op),
    labels = [
        format!("this operand has type `{}`", self.left),
        format!("this {}operator", if self.implicit { "(implicit) " } else { "" }),
        format!("this operand has type `{}`", self.right),
    ],
)]
pub struct InvalidBinaryOperation {
    /// The operator that was used.
    pub op: BinOpKind,

    /// Whether the operator was implicitly inserted by the parser.
    pub implicit: bool,

    /// The type the left side evaluated to.
    pub left: &'static str,

    /// The type the right side evaluated to.
    pub right: &'static str,
}

/// The given unary operation cannot be applied to the given operand.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("cannot apply the `{:?}` operator to this operand", self.op),
    labels = [
        format!("this operand has type `{}`", self.expr_type),
        "this operator".to_string(),
    ],
)]
pub struct InvalidUnaryOperation {
    /// The operator that was used.
    pub op: UnaryOpKind,

    /// The type the operand evaluated to.
    pub expr_type: &'static str,
}

/// Attempted to bitshift an integer by too many bits.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "maximum bitshift amount exceeded",
    labels = ["this expression", "", "too many bits to shift by"],
    help = "the maximum number of bits you can shift an integer by is equal to: `2^64 - 1`"
)]
pub struct BitshiftOverflow;

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
        format!("this argument has type `{}`", self.given),
    ],
    help = format!("must be of type `{}`", self.expected),
    note = format!("function signature: `{}({})`", self.name, self.signature),
)]
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

impl From<cas_compute::numerical::builtin::error::TypeMismatch> for TypeMismatch {
    fn from(err: cas_compute::numerical::builtin::error::TypeMismatch) -> Self {
        Self {
            name: err.name,
            index: err.index,
            expected: err.expected,
            given: err.given,
            signature: err.signature,
        }
    }
}

/// The stack overflowed while evaluating a function call.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "maximum stack depth exceeded",
    labels = ["stack overflowed when executing this function call", ""],
    help = "stack overflows occur when I have to keep track of too many function calls at once",
    // help = "there might be an error in the function you're calling that's causing it to infinitely recurse",
    note = "the maximum stack depth is equal to: `2^16`",
)]
pub struct StackOverflow;


/// Encountered a non-numeric type while using prime notation to derivate a function call.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "encountered a non-numeric type while differentiating this function",
    labels = [
        format!("during evaluation, this call evaluated to a `{}`", self.expr_type),
        "".to_string(),
    ],
    help = "CalcBot evaluates derivatives numerically using the limit definition of a derivative; this can error when evaluating near undefined points of the function"
)]
pub struct NonNumericDerivative {
    /// The type of the expression that was differentiated.
    pub expr_type: &'static str,
}

/// Cannot index into a non-list type.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "cannot index into this type",
    labels = [format!("this expression evaluated to `{}`", self.expr_type)],
    help = "only lists can be indexed into",
)]
pub struct InvalidIndexTarget {
    /// The type of the expression that was used as the target.
    pub expr_type: &'static str,
}

/// Index must be an integer.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "list index must be an integer",
    labels = [
        "for this list".to_string(),
        format!("this expression evaluated to `{}`", self.expr_type),
    ],
)]
pub struct InvalidIndexType {
    /// The type of the expression that was used as an index.
    pub expr_type: &'static str,
}

/// The index is too large to fit into a `usize`.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "list index is out of valid range",
    labels = ["for this list", "out of range"],
    help = "the index must be positive and less than or equal to: `2^64 - 1`"
)]
pub struct IndexOutOfRange;

/// The index is out of bounds for the given list.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "list index is out of bounds",
    labels = ["for this list".to_string(), format!("out of bounds (tried to index: {})", self.index)],
    help = match self.len {
        0 => "list is empty, so all indexing operations will fail".to_string(),
        1 => "list has length `1`, so the index must be `0`".to_string(),
        n => format!("list has length `{}`, so the index must be between `0-{}` (inclusive)", n, n - 1),
    }
)]
pub struct IndexOutOfBounds {
    /// The length of the list that was indexed into.
    pub len: usize,

    /// The index that was attempted to be accessed.
    pub index: usize,
}

/// Length of a list must be an integer.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "length of list must be an integer",
    labels = [format!("this expression evaluated to `{}`", self.expr_type)],
)]
pub struct InvalidLengthType {
    /// The type of the expression that was used as an index.
    pub expr_type: &'static str,
}

/// The list length is too large to fit into a `usize`.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "length of list is out of valid range",
    labels = ["out of range"],
    help = "the length must be positive and less than or equal to: `2^64 - 1`"
)]
pub struct LengthOutOfRange;

/// An internal error that can't be resolved by the user.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("an internal error occurred at instruction: `{:#?}`", self.instruction),
    labels = spans.iter()
        .enumerate()
        .map(|(idx, span)| format!("{}: `{}..{}`", idx, span.start, span.end)),
    help = "please copy your code and this error message and report them to the developer",
    note = &self.data,
)]
pub struct InternalError {
    /// The [`InstructionKind`] the virtual machine was executing when the error occurred.
    ///
    /// [`InstructionKind`]: cas_compiler::InstructionKind
    pub instruction: String,

    /// Arbitrary data that may help the developer diagnose the issue.
    pub data: String,
}
