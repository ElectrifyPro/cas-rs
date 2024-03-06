use cas_parser::parser::ast::{assign::FuncHeader, expr::Expr};
use crate::consts;
use levenshtein::levenshtein;
use std::{collections::HashMap, sync::Arc};
use super::{builtin::Builtin, value::Value};

#[cfg(feature = "mysql")]
use mysql_common::prelude::FromValue;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
#[cfg(feature = "serde")]
use serde_repr::{Deserialize_repr, Serialize_repr};

/// The maximum recursion depth of a context. This is used to detect stack overflows.
pub const MAX_RECURSION_DEPTH: usize = 1 << 11;

/// The trigonometric mode of a context. This will affect the evaluation of input to trigonometric
/// functions, and output from trigonometric functions.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[cfg_attr(feature = "mysql", derive(FromValue))]
#[cfg_attr(feature = "mysql", mysql(is_integer))]
#[cfg_attr(feature = "serde", derive(Serialize_repr, Deserialize_repr))]
#[repr(u8)]
pub enum TrigMode {
    /// Use radians.
    #[default]
    Radians,

    /// Use degrees.
    Degrees,
}

impl std::fmt::Display for TrigMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TrigMode::Radians => write!(f, "radians"),
            TrigMode::Degrees => write!(f, "degrees"),
        }
    }
}

/// A function available for use in a context.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Func {
    /// A builtin function.
    Builtin(Arc<dyn Builtin>),

    /// A user-defined function.
    UserDefined {
        /// The header of the function.
        header: FuncHeader,

        /// The body of the function.
        body: Expr,

        /// Whether the function is recursive, used to report better errors if the stack overflows
        /// while evaluating the function.
        recursive: bool,
    },
}

impl From<Box<dyn Builtin>> for Func {
    fn from(builtin: Box<dyn Builtin>) -> Self {
        Func::Builtin(builtin.into())
    }
}

/// A context to use when evaluating an expression, containing variables and functions that can be
/// used within the expression.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Ctxt {
    /// The variables in the context.
    vars: HashMap<String, Value>,

    /// The functions in the context.
    funcs: HashMap<String, Func>,

    /// The trigonometric mode of the context.
    pub trig_mode: TrigMode,

    /// When true, a `break` expression was evaluated in the current loop. The evaluator should
    /// stop and propogate the value of the `break` expression.
    #[cfg_attr(feature = "serde", serde(skip))]
    pub(crate) break_loop: bool,

    /// The current depth of the stack. This is used to detect stack overflows.
    #[cfg_attr(feature = "serde", serde(skip))]
    pub(crate) stack_depth: usize,

    /// TODO: Whether the maximum recursion depth was reached while evaluating an expression.
    #[cfg_attr(feature = "serde", serde(skip))]
    pub(crate) max_depth_reached: bool,
}

impl Default for Ctxt {
    fn default() -> Self {
        Self {
            vars: HashMap::from([
                ("i".to_string(), consts::I.clone().into()),
                ("e".to_string(), consts::E.clone().into()),
                ("phi".to_string(), consts::PHI.clone().into()),
                ("pi".to_string(), consts::PI.clone().into()),
                ("tau".to_string(), consts::TAU.clone().into()),
                ("true".to_string(), true.into()),
                ("false".to_string(), false.into()),
            ]),
            funcs: crate::funcs::all()
                .into_iter()
                .map(|(name, func)| (name.to_string(), func.into()))
                .collect(),
            trig_mode: TrigMode::default(),
            break_loop: false,
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

    /// Returns the variables in the context.
    pub fn get_vars(&self) -> &HashMap<String, Value> {
        &self.vars
    }

    /// Add a function to the context.
    pub fn add_func(&mut self, header: FuncHeader, body: Expr, recursive: bool) {
        self.funcs.insert(header.name.name.clone(), Func::UserDefined { header, body, recursive });
    }

    /// Get the header and body of a function in the context.
    pub fn get_func(&self, name: &str) -> Option<&Func> {
        self.funcs.get(name)
    }

    /// Returns the functions in the context.
    pub fn get_funcs(&self) -> &HashMap<String, Func> {
        &self.funcs
    }

    /// Returns all functions in the context with a name similar to the given name.
    pub fn get_similar_funcs(&self, name: &str) -> Vec<&str> {
        self.funcs
            .iter()
            .filter(|(n, _)| levenshtein(n, name) < 2)
            .map(|(n, _)| n.as_str())
            .collect()
    }
}
