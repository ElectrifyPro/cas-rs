use cas_parser::parser::{assign::FuncHeader, expr::Expr};
use levenshtein::levenshtein;
use std::{collections::HashMap, f64};
use super::value::Value;

/// A context to use when evaluating an expression, containing variables and functions that can be
/// used within the expression.
#[derive(Debug, Clone)]
pub struct Ctxt {
    /// The variables in the context.
    vars: HashMap<String, Value>,

    /// The functions in the context.
    funcs: HashMap<String, (FuncHeader, Expr)>,
}

impl Default for Ctxt {
    fn default() -> Self {
        Self {
            vars: HashMap::from([
                ("e".to_string(), f64::consts::E.into()),
                ("phi".to_string(), super::consts::PHI.into()),
                ("pi".to_string(), f64::consts::PI.into()),
                ("tau".to_string(), f64::consts::TAU.into()),
            ]),
            funcs: HashMap::new(),
        }
    }
}

impl Ctxt {
    /// Creates a new empty context.
    ///
    /// The empty context is probably not very useful, as it does not contain any variables or
    /// functions. Consider using the [`Default`] implementation instead.
    pub fn new() -> Ctxt {
        Ctxt {
            vars: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    /// Add a variable to the context.
    pub fn add_var(&mut self, name: &str, value: Value) {
        self.vars.insert(name.to_string(), value);
    }

    /// Get the value of a variable in the context.
    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.vars.get(name).cloned()
    }

    /// Add a function to the context.
    pub fn add_func(&mut self, header: FuncHeader, body: Expr) {
        self.funcs.insert(header.name.name.clone(), (header, body));
    }

    /// Get the header and body of a function in the context.
    pub fn get_func(&self, name: &str) -> Option<&(FuncHeader, Expr)> {
        self.funcs.get(name)
    }

    /// Returns all functions in the context with a name similar to the given name.
    pub fn get_similar_funcs(&self, name: &str) -> Vec<&(FuncHeader, Expr)> {
        self.funcs
            .iter()
            .filter(|(n, _)| levenshtein(n, name) < 2)
            .map(|(_, f)| f)
            .collect()
    }
}
