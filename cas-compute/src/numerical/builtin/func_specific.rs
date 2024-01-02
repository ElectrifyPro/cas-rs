use cas_attrs::ErrorKind;
use cas_error::ErrorKind;
use cas_parser::parser::ast::call::Call;
use crate::numerical::builtin::BuiltinError;
use std::ops::Range;

/// Represents an error specific to a builtin function.
#[derive(Debug)]
pub enum FunctionSpecific {
    /// Errors for the `ncr` and `npr` builtin function.
    Ncpr(NcprError),
}

impl FunctionSpecific {
    /// Get the spans for the error.
    pub fn spans(&self, call: &Call) -> Vec<Range<usize>> {
        match self {
            FunctionSpecific::Ncpr(e) => e.spans(call),
        }
    }

    /// Convert the [`FunctionSpecific`] into an [`ErrorKind`].
    pub fn into_kind(self) -> Box<dyn ErrorKind> {
        match self {
            FunctionSpecific::Ncpr(e) => Box::new(e),
        }
    }
}

/// Errors for the `ncr` and `npr` builtin function.
#[derive(Debug, Clone, Copy, ErrorKind, PartialEq, Eq)]
#[error(
    message = format!("incorrect arguments for the `{}` function", self.function_name),
    labels = if self.error == NcprErrorKind::NLessThanK {
        [
            "this function call",
            "",
            "(1) argument `n`...",
            "(2) ...must be less than or equal to argument `k`",
        ].iter()
    } else {
        [
            "this function call",
            "",
            "argument `k` must be positive",
        ].iter()
    }
)]
pub struct NcprError {
    /// The specific function name.
    pub function_name: &'static str,

    /// The error that occurred.
    pub error: NcprErrorKind,
}

impl NcprError {
    pub fn new(function_name: &'static str, error: NcprErrorKind) -> Self {
        Self { function_name, error }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NcprErrorKind {
    /// The first argument is less than the second.
    NLessThanK,

    /// One or both of the arguments is negative.
    NegativeArgs,
}

impl NcprError {
    fn spans(&self, call: &Call) -> Vec<Range<usize>> {
        let mut this_function_call = call.outer_span().to_vec();
        match self.error {
            NcprErrorKind::NLessThanK => {
                this_function_call.extend(call.args.iter().map(|arg| arg.span()));
            },
            NcprErrorKind::NegativeArgs => {
                this_function_call.push(call.args[1].span());
            },
        };
        this_function_call
    }
}

impl From<NcprError> for BuiltinError {
    fn from(e: NcprError) -> Self {
        BuiltinError::FunctionSpecific(FunctionSpecific::Ncpr(e))
    }
}
