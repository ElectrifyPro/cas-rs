pub mod error;
pub mod func_specific;

use error::BuiltinError;
use super::{ctxt::Ctxt, value::Value};

type Result = std::result::Result<Value, BuiltinError>;

/// A trait implemented by all builtin functions.
pub trait Builtin: std::fmt::Debug {
    /// Number of args the function takes.
    // NOTE: this is a `&self` method and not an associated constant to make the trait object-safe
    fn num_args(&self) -> usize;

    /// Evaluates the function.
    fn eval(&self, ctxt: &Ctxt, args: &mut dyn Iterator<Item = Value>) -> Result;
}
