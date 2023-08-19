use ariadne::Fmt;
use cas_attrs::ErrorKind;
use cas_error::{ErrorKind, EXPR};
use crate::tokenizer::TokenKind;

/// An intentionally useless error. This should only be used for non-fatal errors, as it contains
/// no useful information.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "an internal non-fatal error occurred while parsing",
    labels = ["here"],
    help = "you should never see this error; please report this as a bug"
)]
pub struct NonFatal;

/// The end of the source code was reached unexpectedly.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "unexpected end of file",
    labels = [format!("you might need to add another {} here", "expression".fg(EXPR))],
)]
pub struct UnexpectedEof;

/// The end of the source code was expected, but something else was found.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "expected end of file",
    labels = [format!("I could not understand the remaining {} here", "expression".fg(EXPR))],
)]
pub struct ExpectedEof;

/// An unexpected token was encountered.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "unexpected token",
    labels = [format!("expected one of: {}", expected.iter().map(|t| format!("{:?}", t)).collect::<Vec<_>>().join(", "))],
    help = format!("found {:?}", found),
)]
pub struct UnexpectedToken {
    /// The token(s) that were expected.
    pub expected: &'static [TokenKind],

    /// The token that was found.
    pub found: TokenKind,
}

/// The base used in radix notation was out of the allowed range.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "invalid base in radix notation",
    labels = [if *too_large {
        "this value is too large"
    } else {
        "this value is too small"
    }],
    help = format!("the base must be {}", "between 2 and 64, inclusive".fg(EXPR)),
)]
pub struct InvalidRadixBase {
    /// The given base was too large. (Otherwise, it was too small.)
    pub too_large: bool,
}

/// An invalid digit was used in a radix literal.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("invalid digit in radix notation: `{}`", digit),
    labels = ["here"],
    help = format!("base {} uses these digits (from lowest to highest value): {}", radix, allowed.iter().collect::<String>().fg(EXPR)),
)]
pub struct InvalidRadixDigit {
    /// The radix that was expected.
    pub radix: u8,

    /// The set of allowed digits for this radix.
    pub allowed: &'static [char],

    /// The invalid digit that was used.
    pub digit: char,
}

/// A parenthesis was not closed.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "unclosed parenthesis",
    labels = ["this parenthesis is not closed"],
    help = if *opening {
        "add a closing parenthesis `)` somewhere after this"
    } else {
        "add an opening parenthesis `(` somewhere before this"
    },
)]
pub struct UnclosedParenthesis {
    /// Whether the parenthesis was an opening parenthesis `(`. Otherwise, the parenthesis was a
    /// closing parenthesis `)`.
    pub opening: bool,
}

/// There was no expression inside a pair of parentheses.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "missing expression inside parenthesis",
    labels = ["add an expression here"],
)]
pub struct EmptyParenthesis;
