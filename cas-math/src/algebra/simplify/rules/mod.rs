//! Implementation of many simplification rules.
//!
//! Each rule in this module is a function that takes the expression to simplify as an argument,
//! and returns `Some(expr)` with the simplified expression if the rule applies, or `None` if the
//! rule does not apply.

pub mod power;

use crate::step::StepCollector;
use super::{Expr, step::Step};

/// Applies all rules.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    power::all(expr, step_collector)
}
