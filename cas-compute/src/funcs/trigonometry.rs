//! General trigonometric and hyperbolic trigonometric functions.

use cas_attrs::builtin;
use rug::{Complex, Float};

/// Build a trigonometric function that takes in a single radian input argument.
macro_rules! build_radian_input {
    ($($name:ident $upname:ident; $func:expr),* $(,)?) => {
        $(
            #[derive(Debug)]
            pub struct $upname;

            #[cfg_attr(feature = "numerical", builtin(radian = input))]
            impl $upname {
                pub fn eval_static(n: Complex) -> Complex {
                    // NOTE: the closure call is contained within the macro, so we allow
                    // the clippy::redundant_closure_call lint
                    #[allow(clippy::redundant_closure_call)]
                    ($func)(n)
                }
            }
        )*
    };
}

/// Build a trigonometric function that takes in a single input argument and returns a radian
/// value.
macro_rules! build_radian_output {
    ($($name:ident $upname:ident; $func:expr),* $(,)?) => {
        $(
            #[derive(Debug)]
            pub struct $upname;

            #[cfg_attr(feature = "numerical", builtin(radian = output))]
            impl $upname {
                pub fn eval_static(n: Complex) -> Complex {
                    #[allow(clippy::redundant_closure_call)]
                    ($func)(n)
                }
            }
        )*
    };
}

/// Build a hyperbolic trigonometric function.
macro_rules! build_hyperbolic {
    ($($name:ident $upname:ident; $func:expr),* $(,)?) => {
        $(
            #[derive(Debug)]
            pub struct $upname;

            #[cfg_attr(feature = "numerical", builtin)]
            impl $upname {
                pub fn eval_static(n: Complex) -> Complex {
                    #[allow(clippy::redundant_closure_call)]
                    ($func)(n)
                }
            }
        )*
    };
}

#[cfg(feature = "numerical")]
build_radian_input! {
    sin Sin; Complex::sin,
    cos Cos; Complex::cos,
    tan Tan; Complex::tan,
    csc Csc; |n: Complex| n.sin().recip(), // csc is implemented for Float but not Complex
    sec Sec; |n: Complex| n.cos().recip(),
    cot Cot; |n: Complex| n.tan().recip(),
}

#[cfg(feature = "numerical")]
build_radian_output! {
    asin Asin; Complex::asin,
    acos Acos; Complex::acos,
    atan Atan; Complex::atan,
    acsc Acsc; |n: Complex| n.recip().asin(), // similar to csc, etc.
    asec Asec; |n: Complex| n.recip().acos(),
    acot Acot; |n: Complex| n.recip().atan(),
}

#[cfg(feature = "numerical")]
build_hyperbolic! {
    sinh Sinh; Complex::sinh,
    cosh Cosh; Complex::cosh,
    tanh Tanh; Complex::tanh,
    csch Csch; |n: Complex| n.sinh().recip(),
    sech Sech; |n: Complex| n.cosh().recip(),
    coth Coth; |n: Complex| n.tanh().recip(),
    asinh Asinh; Complex::asinh,
    acosh Acosh; Complex::acosh,
    atanh Atanh; Complex::atanh,
    acsch Acsch; |n: Complex| n.recip().asinh(),
    asech Asech; |n: Complex| n.recip().acosh(),
    acoth Acoth; |n: Complex| n.recip().atanh(),
}

#[derive(Debug)]
pub struct Atan2;

#[cfg_attr(feature = "numerical", builtin(radian = output))]
impl Atan2 {
    pub fn eval_static(y: Float, x: &Float) -> Float {
        y.atan2(x)
    }
}
