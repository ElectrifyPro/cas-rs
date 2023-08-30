use cas_parser::parser::{assign::FuncHeader, expr::Expr};
use levenshtein::levenshtein;
use std::collections::HashMap;
use super::value::Value;

/// The maximum recursion depth of a context. This is used to detect stack overflows.
pub const MAX_RECURSION_DEPTH: usize = 1 << 11;

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

/// A function definition in a context.
#[derive(Debug, Clone)]
pub struct Func {
    /// The header of the function.
    pub header: FuncHeader,

    /// The body of the function.
    pub body: Expr,

    /// Whether the function is recursive, used to report better errors if the stack overflows
    /// while evaluating the function.
    pub recursive: bool,
}

/// A context to use when evaluating an expression, containing variables and functions that can be
/// used within the expression.
#[derive(Debug, Clone)]
pub struct Ctxt {
    /// The variables in the context.
    vars: HashMap<String, Value>,

    /// The functions in the context.
    funcs: HashMap<String, Func>,

    /// The trigonometric mode of the context.
    pub trig_mode: TrigMode,

    /// The current depth of the stack. This is used to detect stack overflows.
    pub stack_depth: usize,

    /// TODO: Whether the maximum recursion depth was reached while evaluating an expression.
    pub max_depth_reached: bool,
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
            stack_depth: 0,
            max_depth_reached: false,
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
            ..Default::default()
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
    pub fn add_func(&mut self, header: FuncHeader, body: Expr, recursive: bool) {
        self.funcs.insert(header.name.name.clone(), Func { header, body, recursive });
    }

    /// Get the header and body of a function in the context.
    pub fn get_func(&self, name: &str) -> Option<&Func> {
        self.funcs.get(name)
    }

    /// Returns all functions in the context with a name similar to the given name.
    pub fn get_similar_funcs(&self, name: &str) -> Vec<&Func> {
        self.funcs
            .iter()
            .filter(|(n, _)| levenshtein(n, name) < 2)
            .map(|(_, f)| f)
            .collect()
    }
}
