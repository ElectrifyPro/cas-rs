use rug::{Complex, Float};
use std::fmt::{Display, Formatter};
use super::{consts::{PI, complex, float}, fmt::{FormatOptions, ValueFormatter}};

/// Represents any value that can be stored in a variable.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A number value.
    Number(Float),

    /// A complex number value.
    Complex(Complex),

    /// A boolean.
    Boolean(bool),

    /// The unit type, analogous to `()` in Rust.
    Unit,
}

#[cfg(test)]
impl Value {
    /// Returns true if two values are numbers, and they are approximately equal.
    pub fn approx_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => float(a - b) / float(a) < float(1e-3),
            _ => false,
        }
    }
}

impl Value {
    /// Returns the typename of this value.
    pub fn typename(&self) -> &'static str {
        match self {
            Value::Number(_) => "Number",
            Value::Complex(_) => "Complex",
            Value::Boolean(_) => "Boolean",
            Value::Unit => "Unit",
        }
    }

    /// If this value is a complex number, attempt to coerce it to a real number, returning the
    /// initial value. This is a no-op if the value is not a complex number.
    ///
    /// This is useful for when evaluation of an expression results in a `Value::Complex` with a
    /// zero value for the imaginary part. Using this value for certain operators, such as the
    /// bitwise operators, will result in an error, so we will need to coerce those values to
    /// `Value::Number` instead.
    pub fn coerce_real(self) -> Self {
        match self {
            Value::Complex(c) if c.imag().is_zero() => Value::Number(c.into_real_imag().0),
            _ => self,
        }
    }

    /// If this value is a real number, coerce it to a complex number, returning the initial value.
    /// This is a no-op if the value is not a real number.
    pub fn coerce_complex(self) -> Self {
        match self {
            Value::Number(n) => Value::Complex(complex(n)),
            _ => self,
        }
    }

    /// Converts this value from radians to degrees. If it is a real number, it is converted as
    /// usual. If it is a complex number, the real and imaginary parts are converted separately.
    pub fn to_degrees(self) -> Self {
        let convert = |n: Float| n * 180.0 / &*PI;
        match self {
            Value::Number(n) => Value::Number(convert(n)),
            Value::Complex(c) => Value::Complex({
                let (real, imag) = c.into_real_imag();
                complex((convert(real), convert(imag)))
            }),
            _ => self,
        }
    }

    /// Converts this value from degrees to radians. If it is a real number, it is converted as
    /// usual. If it is a complex number, the real and imaginary parts are converted separately.
    pub fn to_radians(self) -> Self {
        let convert = |n: Float| n * &*PI / 180.0;
        match self {
            Value::Number(n) => Value::Number(convert(n)),
            Value::Complex(c) => Value::Complex({
                let (real, imag) = c.into_real_imag();
                complex((convert(real), convert(imag)))
            }),
            _ => self,
        }
    }

    /// Returns true if this value is a real number, or can be coerced to one.
    pub fn is_real(&self) -> bool {
        match self {
            Value::Number(_) => true,
            Value::Complex(c) => c.imag().is_zero(),
            _ => false,
        }
    }

    /// Returns true if this value is a complex number, or can be coerced to one.
    pub fn is_complex(&self) -> bool {
        match self {
            Value::Complex(_) | Value::Number(_) => true,
            _ => false,
        }
    }

    /// Returns a formatter for the value with the given options.
    pub fn fmt(&self, options: FormatOptions) -> ValueFormatter {
        ValueFormatter {
            value: self,
            options,
        }
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Number(float(n))
    }
}

impl From<Float> for Value {
    fn from(n: Float) -> Self {
        Value::Number(n)
    }
}

impl From<Complex> for Value {
    fn from(c: Complex) -> Self {
        Value::Complex(c)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt(Default::default()).fmt(f)
    }
}
