//! Simplification rules for trigonometric functions.

mod consts;
mod table;

use cas_eval::consts::int;
use crate::{
    algebra::{
        expr::{Expr, Primary},
        simplify::{
            fraction::{extract_explicit_frac, make_fraction},
            rules::do_call,
            step::Step,
            self,
        },
    },
    step::StepCollector,
};
use std::collections::HashMap;

/// Apply normalization and simplify the given trigonometric expression using the provided lookup
/// table.
fn simplify_trig(arg: Expr, table: &HashMap<&Expr, table::TrigOut>) -> Option<Expr> {
    // example: compute sin(pi/6)
    // compute normalized fraction: (pi/6) / (2pi) = 1/12
    let mut expr = {
        let two_pi = Expr::Primary(Primary::Integer(int(2))) * Expr::Primary(Primary::Symbol("pi".to_string()));
        let raw = make_fraction(arg, two_pi);
        simplify::simplify(&raw)
    };

    // expect the result to be a fraction
    let (numerator, denominator) = extract_explicit_frac(&mut expr)?;

    // turn the fraction into a normalized `Expr`
    let fraction = {
        if numerator.is_zero() {
            Expr::Primary(Primary::Integer(int(0)))
        } else if denominator == 1 {
            Expr::Primary(Primary::Integer(numerator))
        } else {
            // the fraction is the normalized angle from 0 to 1, but can be outside that range
            // get the fraction in the range 0 to 1 by computing `numerator % denominator`

            // positive modulo (to handle negative numerators)
            let numerator = (numerator % &denominator + &denominator) % &denominator;
            make_fraction(
                Expr::Primary(Primary::Integer(numerator)),
                Expr::Primary(Primary::Integer(denominator)),
            )
        }
    };

    // use the provided table to get the output
    table.get(&fraction)
        .map(|out| {
            if out.neg {
                -out.output.clone()
            } else {
                out.output.clone()
            }
        })
}

/// `sin(x)`
pub fn sin(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_call(expr, "sin", |args| {
        simplify_trig(args.get(0).cloned()?, &table::SIN_TABLE)
    })?;

    // keep the step collection logic outside of the closure to make it implement `Fn`
    step_collector.push(Step::Sin);
    Some(opt)
}

/// `cos(x)`
pub fn cos(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_call(expr, "cos", |args| {
        simplify_trig(args.get(0).cloned()?, &table::COS_TABLE)
    })?;

    step_collector.push(Step::Cos);
    Some(opt)
}

/// `tan(x)`
pub fn tan(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_call(expr, "tan", |args| {
        simplify_trig(args.get(0).cloned()?, &table::TAN_TABLE)
    })?;

    step_collector.push(Step::Tan);
    Some(opt)
}

/// Applies all trigonometric rules.
///
/// All trigonometric rules will reduce the complexity of the expression.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    sin(expr, step_collector)
        .or_else(|| cos(expr, step_collector))
        .or_else(|| tan(expr, step_collector))
}
