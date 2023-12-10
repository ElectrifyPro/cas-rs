//! Built-in functions for CalcScript.

pub mod error;
pub mod func_specific;

use cas_attrs::args;
use error::BuiltinError;
use rug::{integer::Order, ops::Pow, rand::RandState, Float, Integer};
use super::{
    builtins::func_specific::{NcprError, NcprErrorKind},
    consts::{ONE, PHI, PI, TAU, TEN, TWO, ZERO, complex, float, int, int_from_float},
    ctxt::{Ctxt, TrigMode},
    error::kind::{MissingArgument, TooManyArguments, TypeMismatch},
    fmt::trim_trailing_zeroes,
    funcs::{
        binompdf as rs_binompdf,
        choose,
        factorial as rs_factorial,
        fill_random,
        gamma as rs_gamma,
        inverse_error,
        partial_factorial,
    },
    value::Value::{self, *},
};

type Result = std::result::Result<Value, BuiltinError>;

/// A trait implemented by all builtin functions.
pub trait Builtin {
    /// Number of args the function takes.
    // NOTE: this is a `&self` method and not an associated constant to make the trait object-safe
    fn num_args(&self) -> usize;

    /// Evaluates the function.
    fn eval(&self, ctxt: &Ctxt, args: &[Value]) -> Result;
}

/// Prints the given value to stdout with a trailing newline.
#[args(s: Any)]
pub fn print(_: &Ctxt, args: &[Value]) -> Result {
    println!("{}", s);
    Ok(Unit)
}

/// Generates builtin implementations for simple one-argument functions that take a complex number.
macro_rules! generate_complex_builtin {
    ($($name:ident)+) => {
        $(
            #[args(n: Complex)]
            pub fn $name(_: &Ctxt, args: &[Value]) -> Result {
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
pub fn sin(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.sin()))
}

#[args(n: Complex radians)]
pub fn cos(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.cos()))
}

#[args(n: Complex radians)]
pub fn tan(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.tan()))
}

#[args(n: Complex radians)]
pub fn csc(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.sin().recip()))
}

#[args(n: Complex radians)]
pub fn sec(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.cos().recip()))
}

#[args(n: Complex radians)]
pub fn cot(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.tan().recip()))
}

#[args(n: Complex -> radians)]
pub fn asin(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.asin()))
}

#[args(n: Complex -> radians)]
pub fn acos(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.acos()))
}

#[args(n: Complex -> radians)]
pub fn atan(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.atan()))
}

#[args(n: Complex -> radians)]
pub fn acsc(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.recip().asin()))
}

#[args(n: Complex -> radians)]
pub fn asec(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.recip().acos()))
}

#[args(n: Complex -> radians)]
pub fn acot(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.recip().atan()))
}

#[args(y: Number, x: Number -> radians)]
pub fn atan2(ctxt: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(y.atan2(&x)))
}

#[args(n: Complex)]
pub fn csch(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.sinh().recip()))
}

#[args(n: Complex)]
pub fn sech(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.cosh().recip()))
}

#[args(n: Complex)]
pub fn coth(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.tanh().recip()))
}

#[args(n: Complex)]
pub fn acsch(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.recip().asinh()))
}

#[args(n: Complex)]
pub fn asech(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.recip().acosh()))
}

#[args(n: Complex)]
pub fn acoth(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.recip().atanh()))
}

#[args(n: Number)]
pub fn dtr(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(n * &*PI / 180.0))
}

/// Alias for `dtr`.
#[args(n: Number)]
pub fn rad(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(n * &*PI / 180.0))
}

#[args(n: Number)]
pub fn rtd(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(n * 180.0 / &*PI))
}

/// Alias for `rtd`.
#[args(n: Number)]
pub fn deg(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(n * 180.0 / &*PI))
}

#[args(n: Number)]
pub fn circle(ctxt: &Ctxt, args: &[Value]) -> Result {
    match ctxt.trig_mode {
        TrigMode::Radians => Ok(Number(n * &*TAU)),
        TrigMode::Degrees => Ok(Number(n * 360.0)),
    }
}

// exponential and logarithmic functions

#[args(a: Complex, b: Complex)]
pub fn scientific(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(a * complex(&*TEN).pow(b)))
}

#[args(x: Complex, y: Complex = complex(&*TEN))]
pub fn log(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(x.ln() / y.ln()))
}

// root / power functions

#[args(a: Number, b: Number)]
pub fn hypot(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(a.hypot(&b)))
}

#[args(n: Complex)]
pub fn cbrt(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.pow(1.0 / 3.0)))
}

#[args(n: Complex, i: Complex)]
pub fn root(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.pow(&i.recip())))
}

#[args(n: Complex, p: Complex)]
pub fn pow(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(n.pow(&p)))
}

// complex numbers

#[args(z: Complex)]
pub fn re(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(z.into_real_imag().0))
}

#[args(z: Complex)]
pub fn im(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(z.into_real_imag().1))
}

#[args(z: Complex)]
pub fn arg(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(z.arg().into_real_imag().0))
}

#[args(z: Complex)]
pub fn conj(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(z.conj()))
}

// statistics

#[args(n: Number, k: Number)]
pub fn ncr(_: &Ctxt, args: &[Value]) -> Result {
    if n < k {
        return Err(NcprError::new("ncr", NcprErrorKind::NLessThanK).into());
    } else if k.is_sign_negative() {
        // if k is positive, then n is also positive
        return Err(NcprError::new("ncr", NcprErrorKind::NegativeArgs).into());
    }

    let result = choose(n.to_integer().unwrap(), k.to_integer().unwrap());
    Ok(Number(float(result)))
}

#[args(n: Number, k: Number)]
pub fn npr(_: &Ctxt, args: &[Value]) -> Result {
    if n < k {
        return Err(NcprError::new("npr", NcprErrorKind::NLessThanK).into());
    } else if k.is_sign_negative() {
        // if k is positive, then n is also positive
        return Err(NcprError::new("npr", NcprErrorKind::NegativeArgs).into());
    }

    let (n, k) = (n.to_integer().unwrap(), k.to_integer().unwrap());
    let sub = int(&n - &k);
    Ok(Number(float(partial_factorial(n, sub))))
}

// probability

/// Normal probability density function.
///
/// This function does not actually represent any probability. To get the probability of a normally
/// distributed random variable being between a value `a` and `b`, use [`normcdf`].
#[args(x: Number, m: Number = float(&*ZERO), s: Number = float(&*ONE))]
pub fn normpdf(_: &Ctxt, args: &[Value]) -> Result {
    let exp_arg = Float::exp(float(x - m).square() / (-2 * float(s.square_ref())));
    let bot = s * float(TAU.sqrt_ref());
    Ok(Number(exp_arg / bot))
}

/// Normal cumulative distribution function.
///
/// Returns the probability that a random variable, chosen from a normal distribution with mean `m`
/// and standard deviation `s`, will be between `a` and `b`, inclusive.
#[args(a: Number, b: Number, m: Number = float(&*ZERO), s: Number = float(&*ONE))]
pub fn normcdf(_: &Ctxt, args: &[Value]) -> Result {
    let sqrt_two = float(TWO.sqrt_ref());
    let z_a = (a - &m) / float(&s * &sqrt_two);
    let z_b = (b - &m) / float(&s * &sqrt_two);
    Ok(Number((z_b.erf() - z_a.erf()) / &*TWO))
}

/// Inverse normal cumulative distribution function.
///
/// Returns the value `x` such that `normcdf(x, m, s) = p`.
#[args(p: Number, m: Number = float(&*ZERO), s: Number = float(&*ONE))]
pub fn invnorm(_: &Ctxt, args: &[Value]) -> Result {
    let sqrt_two = float(TWO.sqrt_ref());
    let z = float(sqrt_two * inverse_error(2 * p - 1));
    Ok(Number(m + s * z))
}

/// Geometric probability function.
///
/// Returns the probability of the first success of an event occurring on the `n`th trial, where the
/// probability of success on a single trial is `p`.
#[args(p: Number, n: Number)]
pub fn geompdf(_: &Ctxt, args: &[Value]) -> Result {
    if n <= *ZERO {
        return Ok(Number(float(&*ZERO)));
    }

    let q = float(&*ONE - &p);
    Ok(Number(p * q.pow(n - &*ONE)))
}

/// Cummulative geometric probability function.
///
/// Returns the probability of the first success of an event occurring on or before the `n`th trial,
/// where the probability of success on a single trial is `p`.
#[args(p: Number, n: Number)]
pub fn geomcdf(_: &Ctxt, args: &[Value]) -> Result {
    if n <= *ZERO {
        return Ok(Number(float(&*ZERO)));
    }

    let q = float(&*ONE - &p);
    Ok(Number(&*ONE - q.pow(n)))
}

/// Binomial probability function.
///
/// Returns the probability of exactly `x` successes occurring in `n` trials, where the probability
/// of success on a single trial is `p`.
#[args(n: Number, p: Number, x: Number)]
pub fn binompdf(_: &Ctxt, args: &[Value]) -> Result {
    if x < *ZERO || x > n {
        return Ok(Number(float(&*ZERO)));
    }

    Ok(Number(rs_binompdf(n.to_integer().unwrap(), p, x.to_integer().unwrap())))
}

/// Cummulative binomial probability function.
///
/// Returns the probability of `x` or fewer successes occurring in `n` trials, where the probability
/// of success on a single trial is `p`.
#[args(n: Number, p: Number, x: Number)]
pub fn binomcdf(_: &Ctxt, args: &[Value]) -> Result {
    if x < *ZERO {
        return Ok(Number(float(&*ZERO)));
    } else if x >= n {
        return Ok(Number(float(&*ONE)));
    }

    let (n, mut x) = (n.to_integer().unwrap(), x.to_integer().unwrap());
    let mut sum = float(&*ZERO);
    while x >= *ZERO {
        sum += rs_binompdf(n.clone(), p.clone(), x.clone());
        x -= 1;
    }
    Ok(Number(sum))
}

// sequences

/// Returns the `n`th term of the Fibonacci sequence, using Binet's formula.
#[args(n: Number)]
pub fn fib(_: &Ctxt, args: &[Value]) -> Result {
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

#[args(v: Any)]
pub fn bool(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Boolean(v.is_truthy()))
}

#[args(n: Number)]
pub fn erf(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(n.erf()))
}

#[args(n: Number)]
pub fn erfc(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(n.erfc()))
}

#[args(n: Number)]
pub fn inverf(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(inverse_error(n)))
}

#[args()]
pub fn rand(_: &Ctxt, args: &[Value]) -> Result {
    let mut seed = Integer::new();
    let mut digits = [0u128; 2]; // 256 bits
    fill_random(&mut digits);
    seed.assign_digits(&digits, Order::Lsf);

    let mut rand_state = RandState::new();
    rand_state.seed(&seed);
    Ok(Number(float(Float::random_bits(&mut rand_state))))
}

#[args(n: Number)]
pub fn factorial(_: &Ctxt, args: &[Value]) -> Result {
    if !n.is_integer() || n.is_sign_negative() {
        Ok(Complex(rs_gamma(complex(n) + 1)))
    } else {
        Ok(Number(rs_factorial(n)))
    }
}

#[args(n: Complex)]
pub fn gamma(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(rs_gamma(n)))
}

#[args(v1: Complex, v2: Complex, t: Number)]
pub fn lerp(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Complex(&v1 + complex(&v2 - &v1) * t))
}

#[args(v1: Number, v2: Number, v: Number)]
pub fn invlerp(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(float(&v - &v1) / float(&v2 - &v1)))
}

#[args(n: Number, d: Number)]
pub fn siground(_: &Ctxt, args: &[Value]) -> Result {
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
            pub fn $name(_: &Ctxt, args: &[Value]) -> Result {
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
pub fn min(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(a.min(&b)))
}

#[args(a: Number, b: Number)]
pub fn max(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(a.max(&b)))
}

#[args(n: Number, l: Number, r: Number)]
pub fn clamp(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(n.clamp(&l, &r)))
}

#[args(a: Number, b: Number)]
pub fn gcf(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(float(int_from_float(a).gcd(&int_from_float(b)))))
}

#[args(a: Number, b: Number)]
pub fn lcm(_: &Ctxt, args: &[Value]) -> Result {
    Ok(Number(float(int_from_float(a).lcm(&int_from_float(b)))))
}

#[args(n: Number)]
pub fn sign(_: &Ctxt, args: &[Value]) -> Result {
    if n.is_zero() {
        Ok(n.into())
    } else {
        Ok(Number(n.signum()))
    }
}

#[args(n: Number)]
pub fn size(_: &Ctxt, args: &[Value]) -> Result {
    let s = n.to_string_radix(2, None);
    let mut bits = trim_trailing_zeroes(&s).len();
    if s.contains('.') {
        bits -= 1;
    }
    Ok(Number(float(bits)))
}

/// Returns the builtin function with the given name.
pub fn get_builtin(name: &str) -> Option<Box<dyn Builtin>> {
    macro_rules! match_builtin {
        ($($name:ident)+) => {
            match name {
                $(
                    stringify!($name) => Some(Box::new($name) as Box<dyn Builtin>),
                )*
                _ => None,
            }
        };
    }

    match_builtin!(
        print

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

        // probability
        normpdf normcdf invnorm
        geompdf geomcdf
        binompdf binomcdf

        // miscellaneous functions
        bool erf erfc inverf rand factorial gamma
        abs lerp invlerp
        siground round ceil floor trunc
        min max clamp gcf lcm sign size
    )
}
