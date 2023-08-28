//! Built-in functions for CalcScript.

pub mod error;
pub mod func_specific;

use cas_attrs::{args, out};
use error::BuiltinError;
use rand::Rng;
use rug::{integer::Order, ops::Pow, rand::RandState, Float, Integer};
use super::{
    builtins::func_specific::{NcprError, NcprErrorKind},
    consts::{ONE, PHI, PI, TAU, TEN, complex, float, int, int_from_float},
    ctxt::{Ctxt, TrigMode},
    error::kind::{MissingArgument, TooManyArguments, TypeMismatch},
    fmt::trim_trailing_zeroes,
    funcs::{factorial as rs_factorial, gamma as rs_gamma, partial_factorial},
    value::Value::{self, *},
};

/// Generates builtin implementations for simple one-argument functions that take a complex number.
macro_rules! generate_complex_builtin {
    ($($name:ident)+) => {
        $(
            #[args(n: Complex)]
            pub fn $name(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
                Ok(Complex(n.$name()))
            }
        )*
    };
}

generate_complex_builtin!(
    // trigonometric functions
    sinh cosh tanh // csch sech coth
    asinh acosh atanh // acsch asech acoth

    // exponential and logarithmic functions
    exp ln

    // root / power functions
    sqrt // cbrt

    abs
);

// trigonometric functions

#[args(n: Complex radians)]
pub fn sin(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.sin()))
}

#[args(n: Complex radians)]
pub fn cos(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.cos()))
}

#[args(n: Complex radians)]
pub fn tan(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.tan()))
}

#[args(n: Complex radians)]
pub fn csc(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.sin().recip()))
}

#[args(n: Complex radians)]
pub fn sec(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.cos().recip()))
}

#[args(n: Complex radians)]
pub fn cot(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.tan().recip()))
}

#[args(n: Complex)]
#[out(radians)]
pub fn asin(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.asin()))
}

#[args(n: Complex)]
#[out(radians)]
pub fn acos(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.acos()))
}

#[args(n: Complex)]
#[out(radians)]
pub fn atan(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.atan()))
}

#[args(n: Complex)]
#[out(radians)]
pub fn acsc(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().asin()))
}

#[args(n: Complex)]
#[out(radians)]
pub fn asec(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().acos()))
}

#[args(n: Complex)]
#[out(radians)]
pub fn acot(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().atan()))
}

#[args(y: Number, x: Number)]
#[out(radians)]
pub fn atan2(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(y.atan2(&x)))
}

#[args(n: Complex)]
pub fn csch(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.sinh().recip()))
}

#[args(n: Complex)]
pub fn sech(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.cosh().recip()))
}

#[args(n: Complex)]
pub fn coth(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.tanh().recip()))
}

#[args(n: Complex)]
pub fn acsch(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().asinh()))
}

#[args(n: Complex)]
pub fn asech(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().acosh()))
}

#[args(n: Complex)]
pub fn acoth(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().atanh()))
}

#[args(n: Number)]
pub fn dtr(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n * &*PI / 180.0))
}

/// Alias for `dtr`.
#[args(n: Number)]
pub fn rad(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n * &*PI / 180.0))
}

#[args(n: Number)]
pub fn rtd(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n * 180.0 / &*PI))
}

/// Alias for `rtd`.
#[args(n: Number)]
pub fn deg(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n * 180.0 / &*PI))
}

#[args(n: Number)]
pub fn circle(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    match ctxt.trig_mode {
        TrigMode::Radians => Ok(Number(n * &*TAU)),
        TrigMode::Degrees => Ok(Number(n * 360.0)),
    }
}

// exponential and logarithmic functions

#[args(a: Complex, b: Complex)]
pub fn scientific(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(a * complex(&*TEN).pow(b)))
}

#[args(x: Complex, y: Complex = complex(&*TEN))]
pub fn log(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(x.ln() / y.ln()))
}

// root / power functions

#[args(a: Number, b: Number)]
pub fn hypot(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(a.hypot(&b)))
}

#[args(n: Complex)]
pub fn cbrt(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.pow(1.0 / 3.0)))
}

#[args(n: Complex, i: Complex)]
pub fn root(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.pow(&i.recip())))
}

#[args(n: Complex, p: Complex)]
pub fn pow(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.pow(&p)))
}

// complex numbers

#[args(z: Complex)]
pub fn re(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(z.into_real_imag().0))
}

#[args(z: Complex)]
pub fn im(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(z.into_real_imag().1))
}

#[args(z: Complex)]
pub fn arg(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(z.arg().into_real_imag().0))
}

#[args(z: Complex)]
pub fn conj(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(z.conj()))
}

// statistics

#[args(n: Number, k: Number)]
pub fn ncr(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    if &n < &k {
        return Err(NcprError::new("ncr", NcprErrorKind::NLessThanK).into());
    } else if k.is_sign_negative() {
        // if k is positive, then n is also positive
        return Err(NcprError::new("ncr", NcprErrorKind::NegativeArgs).into());
    }

    let (n, k) = (n.to_integer().unwrap(), k.to_integer().unwrap());
    let sub = int(&n - &k);

    if &k > &sub {
        Ok(Number(float(partial_factorial(n, k) / rs_factorial(sub))))
    } else {
        Ok(Number(float(partial_factorial(n, sub) / rs_factorial(k))))
    }
}

#[args(n: Number, k: Number)]
pub fn npr(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    if &n < &k {
        return Err(NcprError::new("npr", NcprErrorKind::NLessThanK).into());
    } else if k.is_sign_negative() {
        // if k is positive, then n is also positive
        return Err(NcprError::new("npr", NcprErrorKind::NegativeArgs).into());
    }

    let (n, k) = (n.to_integer().unwrap(), k.to_integer().unwrap());
    let sub = int(&n - &k);
    Ok(Number(float(partial_factorial(n, sub))))
}

// sequences

/// Returns the `n`th term of the Fibonacci sequence, using Binet's formula.
#[args(n: Number)]
pub fn fib(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    let result_negative = if n.is_sign_negative() {
        // TODO
        n.to_integer().unwrap().is_even()
    } else {
        false
    };

    let one_minus_phi = float(&*ONE - &*PHI);
    let five_sqrt = float(5.0).sqrt();
    let raw = ((float((&*PHI).pow(&n)) - one_minus_phi.pow(&n)) / five_sqrt).round();

    Ok(Number(if result_negative { -raw } else { raw }))
}

// miscellaneous functions

#[args(n: Number)]
pub fn erf(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n.erf()))
}

#[args(n: Number)]
pub fn erfc(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n.erfc()))
}

#[args()]
pub fn rand(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    let mut seed = Integer::new();
    let mut digits = [0u128; 2];
    rand::thread_rng().fill(&mut digits);
    seed.assign_digits(&digits, Order::Lsf);

    let mut rand = RandState::new();
    rand.seed(&seed);
    Ok(Number(float(Float::random_bits(&mut rand))))
}

#[args(n: Complex)]
pub fn factorial(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    if !n.imag().is_zero() || n.real().is_sign_negative() {
        Ok(Complex(rs_gamma(n + 1)))
    } else {
        Ok(Number(float(rs_factorial(int_from_float(n.into_real_imag().0)))))
    }
}

#[args(n: Complex)]
pub fn gamma(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(rs_gamma(n)))
}

#[args(v1: Complex, v2: Complex, t: Number)]
pub fn lerp(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(&v1 + complex(&v2 - &v1) * t))
}

#[args(v1: Number, v2: Number, v: Number)]
pub fn invlerp(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(float(&v - &v1) / float(&v2 - &v1)))
}

#[args(n: Number, d: Number)]
pub fn siground(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    if n.is_zero() {
        Ok(n.into())
    } else {
        let num_digits = float(n.abs_ref()).log10().ceil().to_integer().unwrap();
        let power = d.to_integer().unwrap() - num_digits;

        let magnitude = float(10.0).pow(&power);
        let shifted = (float(&n) * &magnitude).round();
        Ok(Number(shifted / magnitude))
    }
}

macro_rules! generate_rounding_builtin {
    ($($name:ident)+) => {
        $(
            #[args(n: Complex, s: Number = float(&*ONE))]
            pub fn $name(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
                let recip = float(s.recip_ref());
                let (real, imag) = n.into_real_imag();
                Ok(Complex(complex((
                    float(real * &recip).$name() * &s,
                    float(imag * &recip).$name() * &s,
                ))))
            }
        )*
    };
}

generate_rounding_builtin!(round ceil floor trunc);

#[args(a: Number, b: Number)]
pub fn min(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(a.min(&b)))
}

#[args(a: Number, b: Number)]
pub fn max(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(a.max(&b)))
}

#[args(n: Number, l: Number, r: Number)]
pub fn clamp(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n.clamp(&l, &r)))
}

#[args(a: Number, b: Number)]
pub fn gcf(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(float(int_from_float(a).gcd(&int_from_float(b)))))
}

#[args(a: Number, b: Number)]
pub fn lcm(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(float(int_from_float(a).lcm(&int_from_float(b)))))
}

#[args(n: Number)]
pub fn sign(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    if n.is_zero() {
        Ok(n.into())
    } else {
        Ok(Number(n.signum()))
    }
}

#[args(n: Number)]
pub fn size(_: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
    let s = n.to_string_radix(2, None);
    let mut bits = trim_trailing_zeroes(&s).len();
    if s.contains('.') {
        bits -= 1;
    }
    Ok(Number(float(bits)))
}

/// Returns the builtin function with the given name.
pub fn get_builtin(name: &str) -> Option<fn(&Ctxt, &[Value]) -> Result<Value, BuiltinError>> {
    macro_rules! match_builtin {
        ($($name:ident)+) => {
            match name {
                $(
                    stringify!($name) => Some($name),
                )*
                _ => None,
            }
        };
    }

    match_builtin!(
        // trigonometric functions
        sin cos tan csc sec cot
        asin acos atan atan2 acsc asec acot
        sinh cosh tanh csch sech coth
        asinh acosh atanh acsch asech acoth

        // conversion functions
        dtr rad rtd deg circle

        // exponential and logarithmic functions
        exp scientific log ln

        // root / power functions
        hypot sqrt cbrt root pow

        // complex numbers
        re im arg conj

        // sequences
        fib

        // statistics
        ncr npr

        // miscellaneous functions
        erf erfc rand factorial gamma
        abs lerp invlerp
        siground round ceil floor trunc
        min max clamp gcf lcm sign size
    )
}
