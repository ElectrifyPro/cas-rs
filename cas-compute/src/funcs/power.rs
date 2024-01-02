//! Functions related to powers, exponentiation, and roots.

use cas_attrs::builtin;
use crate::consts::{I, TAU, TEN};
use crate::primitive::{complex, float};
use rug::{ops::Pow as _, Complex, Float};

/// Builds a function whose `rug` implementation has the same name as the given function.
macro_rules! simple {
    ($($name:ident $upname:ident; $doc:literal),* $(,)?) => {
        $(
            #[doc = $doc]
            #[derive(Debug)]
            pub struct $upname;

            #[cfg_attr(feature = "numerical", builtin)]
            impl $upname {
                pub fn eval_static(n: Complex) -> Complex {
                    n.$name()
                }
            }
        )*
    };
}

/// Returns `arg[0] * 10 ^ arg[1]`. This provides a convenient way to express numbers in scientific
/// notation.
#[derive(Debug)]
pub struct Scientific;

#[cfg_attr(feature = "numerical", builtin)]
impl Scientific {
    pub fn eval_static(a: Complex, b: Complex) -> Complex {
        a * complex(&*TEN).pow(b)
    }
}

/// The logarithm function to an arbitrary base, `log(x, base = 10)`.
#[derive(Debug)]
pub struct Log;

#[cfg_attr(feature = "numerical", builtin)]
impl Log {
    pub fn eval_static(n: Complex, base: Option<Complex>) -> Complex {
        let base = base.unwrap_or(complex(&*TEN));
        n.ln() / base.ln()
    }
}

/// Basic power function, `pow(x, y)`.
#[derive(Debug)]
pub struct Pow;

#[cfg_attr(feature = "numerical", builtin)]
impl Pow {
    pub fn eval_static(x: Complex, y: Complex) -> Complex {
        x.pow(y)
    }
}

/// The cube root function, `cbrt(x)`.
///
/// This function returns the principal cube root of `x`.
#[derive(Debug)]
pub struct Cbrt;

#[cfg_attr(feature = "numerical", builtin)]
impl Cbrt {
    pub fn eval_static(n: Complex) -> Complex {
        let one_third = float(3.0).recip();
        if n.real().is_sign_positive() {
            n.pow(one_third)
        } else {
            // alternate form for negative numbers which chooses the branch closest to the real axis
            let (abs, arg) = (
                complex(n.abs_ref()).into_real_imag().0,
                n.arg().into_real_imag().0,
            );
            let lhs = abs.cbrt();
            let rhs = complex(one_third * (arg + &*TAU) * &*I).exp();
            lhs * rhs
        }
    }
}

/// Returns the `n`th root of `x`.
#[derive(Debug)]
pub struct Root;

#[cfg_attr(feature = "numerical", builtin)]
impl Root {
    pub fn eval_static(x: Complex, n: Complex) -> Complex {
        x.pow(n.recip())
    }
}

/// Returns the hypothenuse of a right triangle with sides `a` and `b`.
#[derive(Debug)]
pub struct Hypot;

#[cfg_attr(feature = "numerical", builtin)]
impl Hypot {
    pub fn eval_static(a: Float, b: Float) -> Float {
        a.hypot(&b)
    }
}

simple! {
    exp Exp; "The exponential function, `e ^ x`.",
    ln Ln; "The natural logarithm, `ln(x)`.",
    sqrt Sqrt; "The square root function, `sqrt(x)`.",
}
