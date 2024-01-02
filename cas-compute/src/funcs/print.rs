use cas_attrs::builtin;
use crate::numerical::value::Value;

/// Prints the given value to stdout with a trailing newline.
#[derive(Debug)]
pub struct Print;

#[cfg_attr(feature = "numerical", builtin)]
impl Print {
    pub fn eval_static(v: &Value) {
        println!("{}", v);
    }
}
