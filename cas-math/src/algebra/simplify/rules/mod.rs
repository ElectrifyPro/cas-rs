//! Implementation of many simplification rules.
//!
//! Each rule in this module is a function that takes the expression to simplify as an argument,
//! and returns `Some(expr)` with the simplified expression if the rule applies, or `None` if the
//! rule does not apply.

pub mod power;

use super::Expr;

/// Applies all rules.
pub fn all(expr: &Expr) -> Option<Expr> {
    power::all(expr)
}
