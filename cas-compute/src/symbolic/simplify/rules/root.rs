//! Simplification rules for square, cube, and higher roots.

use crate::primitive::int;
use crate::symbolic::{
    expr::{Expr, Primary},
    simplify::{rules::do_call, step::Step},
    step_collector::StepCollector,
};
use rug::Integer;
use std::collections::HashMap;

/// Returns the prime factorization of the given integer.
fn prime_factorization(mut n: Integer) -> HashMap<Integer, usize> {
    let mut factors = HashMap::new();
    if n < 0 {
        factors.insert(int(-1), 1);
        n = -n;
    }

    let mut i = Integer::from(2);
    while i <= n {
        while int(&n % &i) == 0 {
            *factors.entry(i.clone()).or_insert(0) += 1;
            n /= &i;
        }
        i += 1;
    }

    factors
}

/// General simplification function for roots.
///
/// Returns a 2-tuple of the factors that have been moved outside of the root and the factors that
/// remain inside the root.
fn do_root(expr: &Expr, root: usize) -> Option<Expr> {
    let factors = if let Expr::Mul(factors) = expr {
        factors.clone()
    } else {
        vec![expr.clone()]
    };

    // count the number of times each factor appears
    let counts = factors.into_iter().fold(
        std::collections::HashMap::new(),
        |mut counts, factor| {
            match factor {
                // for each integer factor, replace with its prime factorization
                Expr::Primary(Primary::Integer(n)) => {
                    let factorization = prime_factorization(n);
                    for (factor, count) in factorization {
                        *counts.entry(Expr::Primary(Primary::Integer(factor))).or_insert(0) += count;
                    }
                },

                // extract numerical exponent
                Expr::Exp(left, right) if right.is_integer() => {
                    *counts.entry(*left).or_insert(0) += right.as_integer().unwrap().to_usize().unwrap();
                },

                _ => {
                    *counts.entry(factor).or_insert(0) += 1;
                }
            }
            counts
        }
    );

    // create power nodes for each node with factors that appear a multiple of `root` times
    let outside_factors = counts
        .iter()
        .filter_map(|(factor, count)| {
            if count / root == 0 {
                None
            } else {
                Some(Expr::Exp(
                    Box::new(factor.clone()),
                    Box::new(Expr::Primary(Primary::Integer(Integer::from(count / root))))
                ))
            }
        })
        .collect::<Vec<_>>();
    let inside_factors = counts
        .into_iter()
        .filter_map(|(factor, count)| {
            if count % root == 0 {
                None
            } else {
                Some(Expr::Exp(
                    Box::new(factor),
                    Box::new(Expr::Primary(Primary::Integer(Integer::from(count % root))))
                ))
            }
        })
        .collect::<Vec<_>>();

    if outside_factors.is_empty() {
        // nothing was pulled out of the root; no simplification was performed
        None
    } else if inside_factors.is_empty() {
        // everything was pulled out of the root; the root / call is gone
        Some(Expr::Mul(outside_factors))
    } else {
        // call needs to be rebuilt with the new arguments
        let call = match root {
            2 => Primary::Call("sqrt".to_string(), vec![Expr::Mul(inside_factors)]),
            3 => Primary::Call("cbrt".to_string(), vec![Expr::Mul(inside_factors)]),
            n => Primary::Call(
                "root".to_string(),
                vec![Expr::Mul(inside_factors), Expr::Primary(Primary::Integer(Integer::from(n)))],
            ),
        };
        Some(Expr::Mul(outside_factors) * Expr::Primary(call))
    }
}

/// `sqrt(x^2) = x`, `x >= 0`
fn sqrt(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_call(expr, "sqrt", |args| {
        do_root(args.get(0)?, 2)
    })?;

    // keep the step collection logic outside of the closure to make it implement `Fn`
    step_collector.push(Step::Root);
    Some(opt)
}

/// `cbrt(x^3) = x`
fn cbrt(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_call(expr, "cbrt", |args| {
        do_root(args.get(0)?, 3)
    })?;

    step_collector.push(Step::Root);
    Some(opt)
}

/// `root(x^y, y) = x`
fn root(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_call(expr, "root", |args| {
        let y = args.get(1)?.as_integer()?;
        do_root(args.get(0)?, y.to_usize()?)
    })?;

    step_collector.push(Step::Root);
    Some(opt)
}

// TODO: `(x^y)^(1/y) = x`

/// Applies all root rules.
///
/// Root simplification may or may not reduce the complexity of the expression, since it can
/// introduce additional operations. However, it may be necessary for future rules to apply.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    sqrt(expr, step_collector)
        .or_else(|| cbrt(expr, step_collector))
        .or_else(|| root(expr, step_collector))
}
