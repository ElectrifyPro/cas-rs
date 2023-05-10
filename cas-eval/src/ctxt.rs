use std::{collections::HashMap, f64};

/// A context to use when evaluating an expression, containing variables and functions that can be
/// used within the expression.
pub struct Ctxt {
    vars: HashMap<String, f64>,
}

impl Default for Ctxt {
    fn default() -> Self {
        Self::with_defaults()
    }
}

impl Ctxt {
    /// Creates a new empty context.
    pub fn new() -> Ctxt {
        Ctxt {
            vars: HashMap::new(),
        }
    }

    /// Creates a new context with the default variables: `e`, `phi`, `pi`, and `tau`.
    pub fn with_defaults() -> Ctxt {
        Ctxt {
            vars: HashMap::from([
                ("e".to_string(), f64::consts::E),
                ("phi".to_string(), super::consts::PHI),
                ("pi".to_string(), f64::consts::PI),
                ("tau".to_string(), f64::consts::TAU),
            ]),
        }
    }

    /// Add a variable to the context.
    pub fn add_var(&mut self, name: &str, value: f64) {
        self.vars.insert(name.to_string(), value);
    }

    /// Get the value of a variable in the context.
    pub fn get_var(&self, name: &str) -> Option<f64> {
        self.vars.get(name).copied()
    }
}
