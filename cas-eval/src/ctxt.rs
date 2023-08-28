use cas_parser::parser::{assign::FuncHeader, expr::Expr};
use levenshtein::levenshtein;
use std::collections::HashMap;
use super::value::Value;

/// The trigonometric mode of a context. This will affect the evaluation of input to trigonometric
/// functions, and output from trigonometric functions.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum TrigMode {
    /// Use radians.
    #[default]
    Radians,

    /// Use degrees.
    Degrees,
}

/// A context to use when evaluating an expression, containing variables and functions that can be
/// used within the expression.
#[derive(Debug, Clone)]
pub struct Ctxt {
    /// The variables in the context.
    vars: HashMap<String, Value>,

    /// The functions in the context.
    funcs: HashMap<String, (FuncHeader, Expr)>,

    /// The trigonometric mode of the context.
    pub trig_mode: TrigMode,
}

impl Default for Ctxt {
    fn default() -> Self {
        Self {
            vars: HashMap::from([
                ("i".to_string(), super::consts::I.clone().into()),
                ("e".to_string(), super::consts::E.clone().into()),
                ("phi".to_string(), super::consts::PHI.clone().into()),
                ("pi".to_string(), super::consts::PI.clone().into()),
                ("tau".to_string(), super::consts::TAU.clone().into()),
                ("true".to_string(), true.into()),
                ("false".to_string(), false.into()),
            ]),
            funcs: HashMap::new(),
            trig_mode: TrigMode::default(),
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
            trig_mode: TrigMode::default(),
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
