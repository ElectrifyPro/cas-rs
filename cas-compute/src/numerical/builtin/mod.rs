pub mod error;

use error::BuiltinError;
use super::{trig_mode::TrigMode, value::Value};

/// Whether a function parameter in a function signature is marked required or optional.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Param {
    Required,
    Optional,
}

/// A trait implemented by all builtin functions.
pub trait Builtin: std::fmt::Debug + Send + Sync {
    /// Returns the name of the function.
    // NOTE: this is a `&self` method and not an associated constant to make the trait object-safe
    fn name(&self) -> &'static str;

    /// Simplified function signature, indicating which arguments are required or optional.
    fn sig(&self) -> &'static [Param];

    /// Evaluates the function.
    fn eval(
        &self,
        trig_mode: TrigMode,
        args: &mut dyn Iterator<Item = Value>,
    ) -> Result<Value, BuiltinError>;
}
