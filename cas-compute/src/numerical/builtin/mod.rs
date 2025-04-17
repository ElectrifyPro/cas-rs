pub mod error;

use error::BuiltinError;
use super::{trig_mode::TrigMode, value::Value};

/// A function parameter to a builtin function.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BuiltinParam {
    /// The name of the parameter.
    pub name: &'static str,

    /// Whether the parameter is required or optional.
    pub kind: ParamKind,

    /// The typename of the parameter.
    pub typename: Option<&'static str>,
}

/// The kind of the function parameter; either required or optional.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParamKind {
    Required,
    Optional,
}

/// A trait implemented by all builtin functions.
pub trait Builtin: std::fmt::Debug + Send + Sync {
    /// Returns the name of the function.
    // NOTE: this is a `&self` method and not an associated constant to make the trait object-safe
    fn name(&self) -> &'static str;

    /// The function's signature, indicating all parameters, whether they are required or optional,
    /// and the expected typenames.
    fn sig(&self) -> &'static [BuiltinParam];

    /// The function's signature as a string, used for error messages.
    fn sig_str(&self) -> &'static str;

    /// Evaluates the function.
    fn eval(&self, trig_mode: TrigMode, args: Vec<Value>) -> Result<Value, BuiltinError>;
}

impl Builtin for &'static dyn Builtin {
    fn name(&self) -> &'static str {
        (**self).name()
    }

    fn sig(&self) -> &'static [BuiltinParam] {
        (**self).sig()
    }

    fn sig_str(&self) -> &'static str {
        (**self).sig_str()
    }

    fn eval(&self, trig_mode: TrigMode, args: Vec<Value>) -> Result<Value, BuiltinError> {
        (**self).eval(trig_mode, args)
    }
}
