use ariadne::Fmt;
use cas_attrs::ErrorKind;
use cas_error::EXPR;
use cas_parser::parser::token::op::{BinOpKind, UnaryOpKind};
use std::ops::Range;

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

/// Cannot differentiate a function using prime notation.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!(
        "cannot differentiate `{}` function using prime notation",
        self.name
    ),
    labels = ["this function"],
    help = format!("only functions with a *single parameter* can be differentiated using prime notation; the `{}` function has {} parameter(s)", (&self.name).fg(EXPR), self.actual),
    note = "consider partially applying the function (i.e. `f(x) = log(x, 2)`) to make it differentiable",
)]
pub struct InvalidDifferentiation {
    /// The name of the function that was called.
    pub name: String,

    /// The number of arguments the function actually takes.
    pub actual: usize,
}

/// Too many arguments were given to a function call.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("too many arguments given to the `{}` function", self.name),
    labels = ["this function call", "", "these argument(s) are extraneous"],
    help = format!(
        "the `{}` function takes {} argument(s); there are {} argument(s) provided here",
        (&self.name).fg(EXPR),
        self.expected,
        self.given
    ),
    note = format!("function signature: `{}`", self.signature),
)]
pub struct TooManyArguments {
    /// The name of the function that was called.
    pub name: String,

    /// The number of arguments that were expected.
    pub expected: usize,

    /// The number of arguments that were given.
    pub given: usize,

    /// The signature of the function.
    pub signature: String,
}

impl From<cas_compute::numerical::builtin::error::TooManyArguments> for TooManyArguments {
    fn from(err: cas_compute::numerical::builtin::error::TooManyArguments) -> Self {
        Self {
            name: err.name.to_string(),
            expected: err.expected,
            given: err.given,
            signature: err.signature.to_string(),
        }
    }
}

/// An argument to a function call is missing.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = if self.indices.start + 1 == self.indices.end { // if range has one index
        format!("missing required argument #{} for the `{}` function", self.indices.start + 1, self.name)
    } else {
        format!(
            "missing required arguments {} for the `{}` function",
            self.indices
                .clone()
                .map(|i| format!("#{}", i + 1))
                .collect::<Vec<_>>()
                .join(", "),
            self.name
        )
    },
    labels = ["this function call", ""],
    help = format!(
        "the `{}` function takes {} argument(s); there are {} argument(s) provided here",
        (&self.name).fg(EXPR),
        self.expected,
        self.given
    ),
    note = format!("function signature: `{}`", self.signature),
)]
pub struct MissingArgument {
    /// The name of the function that was called.
    pub name: String,

    /// The indices of the missing arguments.
    pub indices: Range<usize>,

    /// The number of arguments that were expected.
    pub expected: usize,

    /// The number of arguments that were given.
    pub given: usize,

    /// The signature of the function.
    pub signature: String,
}

impl From<cas_compute::numerical::builtin::error::MissingArgument> for MissingArgument {
    fn from(err: cas_compute::numerical::builtin::error::MissingArgument) -> Self {
        Self {
            name: err.name.to_string(),
            indices: err.indices,
            expected: err.expected,
            given: err.given,
            signature: err.signature.to_string(),
        }
    }
}

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
    help = format!("must be of type `{}`", self.expected),
    note = format!("function signature: `{}`", self.signature),
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

    /// The signature of the function.
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
    help = "derivatives are evaluated numerically using the limit definition of a derivative; this can cause an error when evaluating near undefined points of the function"
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

/// Conditional expression in an `if` or `while` must evaluate to a boolean.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "conditional expression must evaluate to a boolean...",
    labels = [
        "...used to determine whether to run this code".to_string(),
        format!("this expression evaluated to `{}`", self.expr_type),
    ],
    help = "consider using the `bool()` function to convert the expression to a boolean based on \"truthiness\"",
)]
pub struct ConditionalNotBoolean {
    /// The type of the expression that was used as the condition.
    pub expr_type: &'static str,
}

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
