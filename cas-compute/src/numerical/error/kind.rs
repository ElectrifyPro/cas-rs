use ariadne::Fmt;
use cas_attrs::ErrorKind;
use cas_error::EXPR;
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

/// The variable is undefined.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("`{}` is not defined", self.name),
    labels = ["this variable"],
    help = format!("to define it, type: {} = {}", (&self.name).fg(EXPR), "<expression>".fg(EXPR)),
)]
pub struct UndefinedVariable {
    /// The name of the variable that was undefined.
    pub name: String,
}

/// The function is undefined.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("the `{}` function does not exist", self.name),
    labels = ["this function"],
    help = if self.suggestions.is_empty() {
        "see the documentation for a list of available functions".to_string()
    } else if self.suggestions.len() == 1 {
        format!("did you mean the `{}` function?", (&*self.suggestions[0]).fg(EXPR))
    } else {
        format!(
            "did you mean one of these functions? {}",
            self.suggestions
                .iter()
                .map(|s| format!("`{}`", s.fg(EXPR)))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
)]
pub struct UndefinedFunction {
    /// The name of the function that was undefined.
    pub name: String,

    /// A list of similarly named functions, if any.
    pub suggestions: Vec<String>,
}

/// Too many arguments were given to a function call.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("too many arguments were given to the `{}` function", self.name),
    labels = ["this function call", ""],
    help = format!(
        "the `{}` function takes {} argument(s); there are {} argument(s) provided here",
        (&self.name).fg(EXPR),
        self.expected,
        self.given
    ),
    note = format!("function signature: {}", self.signature),
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

/// An argument to a function call is missing.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("missing argument #{} for the `{}` function", self.index + 1, self.name),
    labels = ["this function call", ""],
    help = format!(
        "the `{}` function takes {} argument(s); there are {} argument(s) provided here",
        (&self.name).fg(EXPR),
        self.expected,
        self.given
    ),
    note = format!("function signature: {}", self.signature),
)]
pub struct MissingArgument {
    /// The name of the function that was called.
    pub name: String,

    /// The index of the missing argument.
    pub index: usize,

    /// The number of arguments that were expected.
    pub expected: usize,

    /// The number of arguments that were given.
    pub given: usize,

    /// The signature of the function.
    pub signature: String,
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
    help = format!("should be of type `{}`", self.expected),
    note = format!("function signature: {}", self.signature),
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

    /// The signature of the function.
    pub signature: String,
}

/// The stack overflowed while evaluating a function call.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "maximum stack depth exceeded",
    labels = ["this function called itself too many times", ""],
    help = "the maximum stack depth is equal to: `2^11`"
)]
pub struct StackOverflow;

/// Tried to compute the derivative of a function that does not have a single parameter.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!(
        "the `{}` function does not have a single parameter",
        self.name
    ),
    labels = ["this function call", ""],
    help = "only functions with a single parameter can be differentiated using prime notation"
)]
pub struct InvalidDerivativeArguments {
    /// The name of the function that was called.
    pub name: String,
}

/// Encountered a non-numeric type while using prime notation to derivate a function call
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "encountered a non-numeric type while differentiating this function",
    labels = [
        format!("this function call evaluated to `{}`", self.expr_type),
        "".to_string(),
    ],
    help = "derivatives are evaluated numerically using the limit definition of a derivative"
)]
pub struct NonNumericDerivative {
    /// The type of the expression that was differentiated.
    pub expr_type: &'static str,
}
