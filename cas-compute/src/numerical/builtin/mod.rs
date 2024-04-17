pub mod error;
pub mod func_specific;

use error::BuiltinError;
use super::{trig_mode::TrigMode, value::Value};

type Result = std::result::Result<Value, BuiltinError>;

/// A trait implemented by all builtin functions.
pub trait Builtin: std::fmt::Debug + Send + Sync {
    /// Returns the name of the function.
    // NOTE: this is a `&self` method and not an associated constant to make the trait object-safe
    fn name(&self) -> &'static str;

    /// Number of args the function takes.
    fn num_args(&self) -> usize;

    /// Evaluates the function.
    fn eval(&self, trig_mode: TrigMode, args: &mut dyn Iterator<Item = Value>) -> Result;
}
