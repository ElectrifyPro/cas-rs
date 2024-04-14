use ariadne::Fmt;
use cas_attrs::ErrorKind;
use cas_error::EXPR;

/// Tried to override a builtin constant.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("cannot override builtin constant: `{}`", self.name),
    labels = ["this variable"],
    help = "choose a different name for this variable",
    // TODO: add this note when merging with `dev`
    // note = "builtin constants include: `i`, `e`, `phi`, `pi`, or `tau`",
)]
pub struct OverrideBuiltinConstant {
    /// The name of the variable that was attempted to be overridden.
    pub name: String,
}

/// Tried to override a builtin function.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("cannot override builtin function: `{}`", self.name),
    labels = ["this function"],
    help = "choose a different name for this function",
)]
pub struct OverrideBuiltinFunction {
    /// The name of the function that was attempted to be overridden.
    pub name: String,
}

/// The variable is undefined.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("unknown variable: `{}`", self.name),
    labels = ["this variable"],
    help = format!("to define it, type: {} = {}", (&self.name).fg(EXPR), "<expression>".fg(EXPR)),
)]
pub struct UnknownVariable {
    /// The name of the variable that was undefined.
    pub name: String,
}

/// The function is undefined.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("unknown function: `{}`", self.name),
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
pub struct UnknownFunction {
    /// The name of the function that was undefined.
    pub name: String,

    /// A list of similarly named functions, if any.
    pub suggestions: Vec<String>,
}
