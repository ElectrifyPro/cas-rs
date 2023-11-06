//! Simplification rules for trigonometric functions.

mod consts;

use cas_eval::consts::int;
use consts::{input::*, output::*, ONE, ONE_HALF, ZERO};
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
use once_cell::sync::Lazy;
use std::collections::HashMap;

/// The output of the trigonometric functions for certain angles, and whether to negate the output.
#[derive(PartialEq, Eq, Hash)]
struct TrigOut {
    /// The output of the trigonometric function.
    output: &'static Expr,

    /// Whether to negate the output.
    neg: bool,
}

impl From<&'static Expr> for TrigOut {
    fn from(output: &'static Expr) -> Self {
        Self {
            output,
            neg: false,
        }
    }
}

static SIN_TABLE: Lazy<HashMap<&Expr, TrigOut>> = Lazy::new(|| HashMap::from([
    // sin(0) = 0
    (&*ZERO, TrigOut::from(&*ZERO)),

    // sin(1/12 = pi/6 rad = 30 deg) = 1/2
    (&*ONE_TWELFTH, TrigOut::from(&*ONE_HALF)),

    // sin(1/8 = pi/4 rad = 45 deg) = 1/sqrt(2) = sqrt(2)/2
    (&*ONE_EIGHTH, TrigOut::from(&*SQRT_TWO_HALF)),

    // sin(1/6 = pi/3 rad = 60 deg) = sqrt(3)/2
    (&*ONE_SIXTH, TrigOut::from(&*SQRT_THREE_HALF)),

    // sin(1/4 = pi/2 rad = 90 deg) = 1
    (&*ONE_FOURTH, TrigOut::from(&*ONE)),

    // sin(1/3 = 2pi/3 rad = 120 deg) = sqrt(3)/2
    (&*ONE_THIRD, TrigOut::from(&*SQRT_THREE_HALF)),

    // sin(3/8 = 3pi/4 rad = 135 deg) = 1/sqrt(2) = sqrt(2)/2
    (&*THREE_EIGHTS, TrigOut::from(&*SQRT_TWO_HALF)),

    // sin(5/12 = 5pi/6 rad = 150 deg) = 1/2
    (&*FIVE_TWELFTHS, TrigOut::from(&*ONE_HALF)),

    // sin(1/2 = pi rad = 180 deg) = 0
    (&*ONE_HALF, TrigOut::from(&*ZERO)),

    // sin(7/12 = ...) = -1/2
    (&*SEVEN_TWELFTHS, TrigOut { output: &ONE_HALF, neg: true }),

    // sin(5/8 = ...) = -1/sqrt(2) = -sqrt(2)/2
    (&*FIVE_EIGHTHS, TrigOut { output: &SQRT_TWO_HALF, neg: true }),

    // sin(2/3 = ...) = -sqrt(3)/2
    (&*TWO_THIRDS, TrigOut { output: &SQRT_THREE_HALF, neg: true }),

    // sin(3/4 = ...) = -1
    (&*THREE_FOURTHS, TrigOut { output: &ONE, neg: true }),

    // sin(5/6 = ...) = -sqrt(3)/2
    (&*FIVE_SIXTHS, TrigOut { output: &SQRT_THREE_HALF, neg: true }),

    // sin(7/8 = ...) = -1/sqrt(2) = -sqrt(2)/2
    (&*SEVEN_EIGHTHS, TrigOut { output: &SQRT_TWO_HALF, neg: true }),

    // sin(11/12 = ...) = -1/2
    (&*ELEVEN_TWELFTHS, TrigOut { output: &ONE_HALF, neg: true }),

    // sin(1) = 0
    (&*ONE, TrigOut::from(&*ZERO)),
]));

/// `sin(x)`
pub fn sin(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_call(expr, "sin", |args| {
        // example: compute sin(pi/6)
        // compute (pi/6) / (2pi) = 1/12
        let mut expr = {
            let arg = args.get(0).cloned()?;
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

        // use the SIN_TABLE to get the output
        SIN_TABLE.get(&fraction)
            .map(|out| {
                if out.neg {
                    -out.output.clone()
                } else {
                    out.output.clone()
                }
            })
    })?;

    // keep the step collection logic outside of the closure to make it implement `Fn`
    step_collector.push(Step::Sin);
    Some(opt)
}

/// Applies all trigonometric rules.
///
/// All trigonometric rules will reduce the complexity of the expression.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    sin(expr, step_collector)
}
