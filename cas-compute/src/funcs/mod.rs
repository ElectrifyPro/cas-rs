//! All built-in functions provided by the numerical and symbolic libraries.
//!
//! Each function is implemented as a unit `struct` with an associated `eval_static` method. This
//! method can be used to evaluate the function in Rust code if the types of the arguments are
//! known at compile time.
//!
//! If the `numerical` feature is enabled, the [`Builtin`] trait (provided by the feature) is also
//! implemented for each function, enabling the function to be evaluated with arbitrary arguments
//! at runtime after type checking. See the `cas-vm` crate for more information on runtime
//! evaluation.
//!
//! # Example
//!
//! ```
//! use cas_compute::consts::PI;
//! use cas_compute::funcs::trigonometry::Sin;
//! use cas_compute::primitive::complex;
//!
//! // evaluate sin(pi / 2) using `eval_static`
//! let result = Sin::eval_static(complex(&*PI) / 2.0);
//! println!("sin(pi / 2) = {}", result);
//!
//! // see `cas-vm` for runtime evaluation of this example
//! ```

pub mod angle;
pub mod complex;
pub mod combinatoric;
pub mod miscellaneous;
pub mod power;
pub mod print;
pub mod probability; // TODO: add poison distribution
pub mod round;
pub mod sequence;
pub mod trigonometry;

#[cfg(feature = "numerical")]
use crate::numerical::builtin::Builtin;

#[cfg(feature = "numerical")]
use std::collections::HashMap;

use std::sync::OnceLock;

/// Returns a list of all builtin functions that can be numerically evaluated.
#[cfg(feature = "numerical")]
pub fn all() -> &'static HashMap<&'static str, Box<dyn Builtin>> {
    use angle::*;
    use complex::*;
    use combinatoric::*;
    use miscellaneous::*;
    use power::*;
    use print::*;
    use probability::*;
    use round::*;
    use sequence::*;
    use trigonometry::*;

    static FUNCTIONS: OnceLock<HashMap<&'static str, Box<dyn Builtin>>> = OnceLock::new();

    macro_rules! build {
        ($($name:literal $upname:ident),* $(,)?) => {
            [
                $(
                    ($name, Box::new($upname) as Box<dyn Builtin>),
                )*
            ]
                .into_iter()
                .collect()
        };
    }

    FUNCTIONS.get_or_init(||
        build! {
            "print" Print,
            "sin" Sin,
            "cos" Cos,
            "tan" Tan,
            "csc" Csc,
            "sec" Sec,
            "cot" Cot,
            "asin" Asin,
            "acos" Acos,
            "atan" Atan,
            "atan2" Atan2,
            "acsc" Acsc,
            "asec" Asec,
            "acot" Acot,
            "sinh" Sinh,
            "cosh" Cosh,
            "tanh" Tanh,
            "csch" Csch,
            "sech" Sech,
            "coth" Coth,
            "asinh" Asinh,
            "acosh" Acosh,
            "atanh" Atanh,
            "acsch" Acsch,
            "asech" Asech,
            "acoth" Acoth,
            "dtr" Dtr,
            "rad" Dtr, // intentional alias for dtr
            "rtd" Rtd,
            "deg" Rtd, // intentional alias for rtd
            "circle" Circle,
            "exp" Exp,
            "ln" Ln,
            "log" Log,
            "scientific" Scientific,
            "pow" Pow,
            "sqrt" Sqrt,
            "cbrt" Cbrt,
            "root" Root,
            "hypot" Hypot,
            "re" Re,
            "im" Im,
            "arg" Arg,
            "conj" Conj,
            "fib" Fib,
            "ncr" Ncr,
            "npr" Npr,
            "erf" Erf,
            "erfc" Erfc,
            "inverf" Inverf,
            "normpdf" Normpdf,
            "normcdf" Normcdf,
            "invnorm" Invnorm,
            "geompdf" Geompdf,
            "geomcdf" Geomcdf,
            "binompdf" Binompdf,
            "binomcdf" Binomcdf,
            "poisspdf" Poisspdf,
            "poisscdf" Poisscdf,
            "siground" Siground,
            "round" Round,
            "ceil" Ceil,
            "floor" Floor,
            "trunc" Trunc,
            "abs" Abs,
            "bool" Bool,
            "rand" Rand,
            "factorial" Factorial,
            "gamma" Gamma,
            "lerp" Lerp,
            "invlerp" Invlerp,
            "min" Min,
            "max" Max,
            "clamp" Clamp,
            "gcf" Gcf,
            "lcm" Lcm,
            "sign" Sign,
            "size" Size,
        }
    )
}
