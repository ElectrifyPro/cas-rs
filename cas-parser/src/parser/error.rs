use ariadne::Fmt;
use cas_attrs::ErrorKind;
use cas_error::{ErrorKind, EXPR};
use crate::tokenizer::TokenKind;
use std::{collections::HashSet, ops::Range};

/// An intentionally useless error. This should only be used for non-fatal errors, as it contains
/// no useful information.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "an internal non-fatal error occurred while parsing",
    labels = ["here"],
    help = "you should never see this error; please report this as a bug"
)]
pub struct NonFatal;

/// Expected to see a certain kind of expression here.
///
/// The `expected` field should also contain the word "a" or "an" at the beginning to make the
/// error grammatically correct.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("expected {}", self.expected),
    labels = [format!("I expected to see {} here", self.expected)],
)]
pub struct ExpectedExpr {
    /// The kind of expression that was expected.
    pub expected: &'static str,
}

/// The end of the source code was reached unexpectedly.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "unexpected end of file",
    labels = [format!("you might need to add another {} here", "expression".fg(EXPR))],
)]
pub struct UnexpectedEof;

/// Unexpected end of an expression.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "unexpected end of expression",
    labels = [format!("I expected to see more {} here", "expression".fg(EXPR))],
)]
pub struct UnexpectedEoExpr;

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
    labels = [format!("expected one of: {}", self.expected.iter().map(|t| format!("{:?}", t)).collect::<Vec<_>>().join(", "))],
    help = format!("found {:?}", self.found),
)]
pub struct UnexpectedToken {
    /// The token(s) that were expected.
    pub expected: &'static [TokenKind],

    /// The token that was found.
    pub found: TokenKind,
}

/// Encountered a keyword when a symbol name was expected.
// TODO: this error is not currently reported, see `impl Parse for LitSym`
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "expected symbol name",
    labels = [format!("found keyword `{}`", self.keyword)],
    help = "you cannot use keywords as symbol names"
)]
pub struct ExpectedSymbolName {
    /// The keyword that was found.
    pub keyword: String,
}

/// The base used in radix notation was out of the allowed range.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "invalid base in radix notation",
    labels = [if self.too_large {
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
#[derive(Debug, Clone, PartialEq)]
pub struct InvalidRadixDigit {
    /// The radix that was expected.
    pub radix: u8,

    /// The set of allowed digits for this radix.
    pub allowed: &'static [char],

    /// The invalid digits that were used.
    pub digits: HashSet<char>,

    /// If the last digit in the user's input is a `+` or `/` character, which happens to be a
    /// valid character in base 64, this field contains the span of that character.
    ///
    /// The user may have been trying to add a number in radix notation to another, and mistakenly
    /// placed the `+` or `/` at the end of the radix number instead of spaced apart.
    pub last_op_digit: Option<(char, Range<usize>)>,
}

// manual ErrorKind implementation to support the `last_op_digit` field
impl ErrorKind for InvalidRadixDigit {
    fn build_report<'a>(
        &self,
        src_id: &'a str,
        spans: &[std::ops::Range<usize>],
    ) -> ariadne::Report<(&'a str, Range<usize>)> {
        let labels = spans
            .iter()
            .cloned()
            .map(|span| {
                if let Some((_, last_op_digit)) = self.last_op_digit.as_ref() {
                    // if one of the generated spans points to the last digit, remove that digit
                    // from the generated span
                    if span.end == last_op_digit.end {
                        return span.start..span.end - 1;
                    }
                }

                span
            })
            .filter(|span| span.start < span.end) // ^ that might have made the span empty
            .map(|span| {
                ariadne::Label::new((src_id, span))
                    .with_color(cas_error::EXPR)
            })
            .chain(
                self.last_op_digit.as_ref().map(|(ch, span)| {
                    let operation = match ch {
                        '+' => "add",
                        '/' => "divide",
                        _ => unreachable!(),
                    };
                    ariadne::Label::new((src_id, span.clone()))
                        .with_message(format!(
                            "if you're trying to {} two values, add a space between each value and this operator",
                            operation
                        ))
                        .with_color(cas_error::EXPR)
                }
            ));

        let mut builder =
            ariadne::Report::build(ariadne::ReportKind::Error, src_id, spans[0].start)
                .with_message(format!(
                    "invalid digits in radix notation: `{}`",
                    self.digits
                        .iter()
                        .map(|c| c.to_string())
                        .collect::<Vec<_>>()
                        .join("`, `"),
                ))
                .with_labels(labels);
        builder.set_help(format!(
            "base {} uses these digits (from lowest to highest value): {}",
            self.radix,
            self.allowed.iter().collect::<String>().fg(EXPR)
        ));
        builder.finish()
    }
}

/// No number was provided in a radix literal.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "missing value in radix notation",
    labels = [format!("I was expecting to see a number in base {}, directly after this quote", self.radix)],
    help = format!("base {} uses these digits (from lowest to highest value): {}", self.radix, self.allowed.iter().collect::<String>().fg(EXPR)),
)]
pub struct EmptyRadixLiteral {
    /// The radix that was expected.
    pub radix: u8,

    /// The set of allowed digits for this radix.
    pub allowed: &'static [char],
}

/// A parenthesis was not closed.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "unclosed parenthesis",
    labels = ["this parenthesis is not closed"],
    help = if self.opening {
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

/// The left-hand-side of an assignment was not a valid symbol or function header.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "invalid left-hand-side of assignment operator",
    labels = ["(1) this expression should be a symbol or function header...", "(2) ...to work with this assignment operator"],
    help = if self.is_call {
        "(1) looks like a function *call*, not a function *header*"
    } else {
        "maybe you meant to compare expressions with `==`?"
    }
)]
pub struct InvalidAssignmentLhs {
    /// Whether the expression span is pointing towards a function call.
    pub is_call: bool,
}

/// Default arguments must be placed at the end of the argument list.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "default arguments must be placed at the end of the function argument list",
    labels = spans.iter()
        .enumerate()
        .map(|(i, _)| format!("default argument #{}", i + 1)),
    help = "move the default argument(s) to the end",
)]
pub struct DefaultArgumentNotLast;

/// The left-hand-side of a compound assignment operator cannot be a function header.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "invalid left-hand-side of compound assignment operator",
    labels = ["(1) this expression must be a symbol...", "(2) ...to work with this compound assignment operator"],
    help = "use the standard assignment operator (`=`) instead",
)]
pub struct InvalidCompoundAssignmentLhs;

/// A compound assignment operator was used within a function header.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "cannot use compound assignment operator here",
    labels = ["this operator"],
    help = "only the standard assignment operator (`=`) is allowed in function headers",
)]
pub struct CompoundAssignmentInHeader;

/// There were too many derivatives in prime notation.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "too many derivatives in prime notation",
    labels = ["you can only take at most 255 derivatives of a function"],
    help = format!("I counted {} derivatives here", self.derivatives),
)]
pub struct TooManyDerivatives {
    /// The number of derivatives that were found.
    pub derivatives: usize,
}

/// Cannot use `return` outside of a function.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "cannot use `return` keyword here",
    labels = [""],
    help = "`return` can only be used within a function definition"
)]
pub struct ReturnOutsideFunction;

/// Missing `then` or `else` keyword in an `if` expression.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("missing `{}` in `if` expression", self.keyword),
    labels = ["this `if` expression".to_string(), format!("I expected to see `{}` here", self.keyword)],
)]
pub struct MissingIfKeyword {
    /// The keyword that was expected.
    pub keyword: &'static str,
}

/// Missing `then` or `else` branch in an `if` expression.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("missing `{}` branch in `if` expression", self.keyword),
    labels = ["this `if` expression".to_string(), format!("I expected to see an `{}` branch here", self.keyword)],
)]
pub struct MissingIfBranch {
    /// The keyword that was expected.
    pub keyword: &'static str,
}

/// Cannot use `then` keyword outside of an `if`, `while` or `for` expression.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "cannot use `then` keyword here",
    labels = [""],
    help = "`then` can only be used directly after the condition in an `if` or `while` expression, or after the range in a `for` expression",
)]
pub struct ThenOutsideIfWhileFor;

/// Cannot use `of` keyword outside of a `sum` or `product` expression.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "cannot use `of` keyword here",
    labels = [""],
    help = "`of` can only be used directly after the range in a `sum` or `product` expression",
)]
pub struct OfOutsideSumProduct;

/// Cannot use `break` outside of a loop.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "cannot use `break` keyword here",
    labels = [""],
    help = "`break` or `continue` can only be used within a `loop`, `while`, or `for` expression",
)]
pub struct BreakOutsideLoop;

/// Cannot use `continue` outside of a loop.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = "cannot use `continue` keyword here",
    labels = [""],
    help = "`break` or `continue` can only be used within a `loop`, `while`, or `for` expression",
)]
pub struct ContinueOutsideLoop;
