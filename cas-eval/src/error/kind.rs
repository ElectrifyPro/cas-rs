use ariadne::Fmt;
use cas_attrs::ErrorKind;
use cas_error::{ErrorKind, EXPR};

/// The given operation cannot be applied to the given operand(s).
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "cannot apply `{}` to `{}`",
    labels = ["while trying to evaluate this"],
    help = "you cannot apply this operation to this operand"
)]
pub struct InvalidOperation; // TODO

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
    labels = ["this function call"],
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
    labels = ["this function call"],
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
