//! Rounding functions.

use cas_attrs::builtin;
use crate::primitive::{complex, float};
use rug::{ops::Pow, Complex, Float, Integer};

/// Build a rounding function.
macro_rules! build_rounding {
    ($($name:ident $upname:ident; $doc:literal),* $(,)?) => {
        $(
            #[doc = $doc]
            #[derive(Debug)]
            pub struct $upname;

            #[cfg_attr(feature = "numerical", builtin)]
            impl $upname {
                pub fn eval_static(n: Complex, s: Option<Float>) -> Complex {
                    fn inner(n: Float, s: &Float) -> Float {
                        (n / s).$name() * s
                    }

                    let (real, imag) = n.into_real_imag();
                    let s = s.unwrap_or_else(|| float(1));
                    complex((inner(real, &s), inner(imag, &s)))
                }
            }
        )*
    };
}

/// Rounds a number to a given number of significant digits.
#[derive(Debug)]
pub struct Siground;

#[cfg_attr(feature = "numerical", builtin)]
impl Siground {
    pub fn eval_static(n: Complex, d: Integer) -> Complex {
        fn inner(n: Float, d: &Integer) -> Float {
            if n.is_zero() {
                return n;
            }

            let num_digits = float(n.abs_ref()).log10().ceil().to_integer().unwrap();
            let power = d - num_digits;

            let magnitude = float(10).pow(&power);
            let shifted = (n * &magnitude).round();
            shifted / magnitude
        }

        let (real, imag) = n.into_real_imag();
        complex((inner(real, &d), inner(imag, &d)))
    }
}

build_rounding! {
    round Round; "Round a number to the nearest integer.",
    ceil Ceil; "Round a number up to the nearest integer.",
    floor Floor; "Round a number down to the nearest integer.",
    trunc Trunc; "Rounds a number towards zero.",
}
