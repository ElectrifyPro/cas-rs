use ariadne::Fmt;
use cas_attrs::ErrorKind;
use cas_error::{ErrorKind, EXPR};
use cas_parser::parser::token::op::{BinOpKind, UnaryOpKind};

/// The given binary operation cannot be applied to the given operands.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("cannot apply the `{:?}` operator to these operands", op),
    labels = [
        format!("this operand has type `{}`", left),
        format!("this {}operator", if *implicit { "(implicit) " } else { "" }),
        format!("this operand has type `{}`", right),
    ],
)]
pub struct InvalidBinaryOperation {
    /// The operator that was used.
    pub op: BinOpKind,

    /// Whether the operator was implicitly inserted by the parser.
    pub implicit: bool,

    /// The type the left side evaluated to.
    pub left: String,

    /// The type the right side evaluated to.
    pub right: String,
}

/// The given unary operation cannot be applied to the given operand.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("cannot apply the `{:?}` operator to this operand", op),
    labels = [
        format!("this operand has type `{}`", expr_type),
        "this operator".to_string(),
    ],
)]
pub struct InvalidUnaryOperation {
    /// The operator that was used.
    pub op: UnaryOpKind,

    /// The type the operand evaluated to.
    pub expr_type: String,
}

/// The variable is undefined.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("`{}` is not defined", name),
    labels = ["this variable"],
    help = format!("to define it, type: {} = {}", name.fg(EXPR), "<expression>".fg(EXPR)),
)]
pub struct UndefinedVariable {
    /// The name of the variable that was undefined.
    pub name: String,
}

/// The function is undefined.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("the `{}` function does not exist", name),
    labels = ["this function"],
    help = if suggestions.is_empty() {
        "see the documentation for a list of available functions".to_string()
    } else if suggestions.len() == 1 {
        format!("did you mean the `{}` function?", (&*suggestions[0]).fg(EXPR))
    } else {
        format!(
            "did you mean one of these functions? {}",
            suggestions
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
    message = format!("too many arguments were given to the `{}` function", name),
    labels = ["this function call", ""],
    help = format!(
        "the `{}` function takes {} argument(s); there are {} argument(s) provided here",
        name.fg(EXPR),
        expected,
        given
    )
)]
pub struct TooManyArguments {
    /// The name of the function that was called.
    pub name: String,

    /// The number of arguments that were expected.
    pub expected: usize,

    /// The number of arguments that were given.
    pub given: usize,
}

/// An argument to a function call is missing.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("missing argument #{} for the `{}` function", index + 1, name),
    labels = ["this function call", ""],
    help = format!(
        "the `{}` function takes {} argument(s); there are {} argument(s) provided here",
        name.fg(EXPR),
        expected,
        given
    )
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
}

/// An argument to a function call has the wrong type.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!(
        "incorrect type for argument #{} for the `{}` function",
        index + 1,
        name
    ),
    labels = [
        "this function call".to_string(),
        "".to_string(),
        format!("this argument has type `{}`", given),
    ],
    help = format!("should be of type `{}`", expected),
)]
pub struct TypeMismatch {
    /// The name of the function that was called.
    pub name: String,

    /// The index of the argument that was mismatched.
    pub index: usize,

    /// The type of the argument that was expected.
    pub expected: String,

    /// The type of the argument that was given.
    pub given: String,
}
