use std::collections::HashMap;
use cas_compute::numerical::value::Value;

/// A stack frame in a call stack.
#[derive(Default)]
pub struct Frame {
    /// Variables available in this stack frame.
    vars: HashMap<String, Value>,

    /// The stack frames this particular function call is allowed to access.
    ///
    /// This ensures that scoping rules are followed.
    parent: Option<Box<Frame>>,
}

impl Frame {
    /// Add a variable to the stack frame.
    pub fn add_var(&mut self, var: String, value: Value) {
        self.vars.insert(var, value);
    }

    /// Searches for a variable in this stack frame.
    pub fn get_var(&self, var: &str) -> Option<Value> {
        self.vars.get(var).cloned()
    }
}
