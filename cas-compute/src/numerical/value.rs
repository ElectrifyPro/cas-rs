use rug::{Complex, Float, Integer};
use std::fmt::{Display, Formatter};
use super::{consts::{PI, complex, float}, fmt::{FormatOptions, ValueFormatter}};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Represents any value that can be stored in a variable.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Value {
    /// A floating-point value.
    Float(Float),

    /// An integer value.
    Integer(Integer),

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
            (Value::Float(a), Value::Float(b)) => float(a - b) / float(a) < float(1e-3),
            _ => false,
        }
    }
}

impl Value {
    /// Returns the typename of this value.
    pub fn typename(&self) -> &'static str {
        match self {
            Value::Float(_) => "Float",
            Value::Integer(_) => "Integer",
            Value::Complex(_) => "Complex",
            Value::Boolean(_) => "Boolean",
            Value::Unit => "Unit",
        }
    }

    /// Consumes and attempts to coerce the value to a real number. This coercion is lossless, and
    /// only occurs if one of the following is true:
    ///
    /// - The value is an integer.
    /// - The value is a complex number with a zero imaginary part.
    ///
    /// This is useful for when evaluation of an expression results in a [`Value::Complex`] with a
    /// zero value for the imaginary part. Using a complex number for certain operators, such as
    /// the bitwise operators, will result in an error, so we will need to coerce those values to
    /// [`Value::Float`] instead.
    pub fn coerce_float(self) -> Self {
        match self {
            Value::Integer(n) => Value::Float(float(n)),
            Value::Complex(c) if c.imag().is_zero() => Value::Float(c.into_real_imag().0),
            _ => self,
        }
    }

    /// Consumes and attempts to coerce the value to an integer. This coercion is lossless, and
    /// only occurs if one of the following is true:
    ///
    /// - The value is a float with a zero fractional part.
    /// - The value is a complex number with a zero imaginary part, and a real part with a zero
    ///  fractional part.
    pub fn coerce_integer(self) -> Self {
        match self {
            Value::Float(n) if n.is_integer() => Value::Integer(n.to_integer().unwrap()),
            Value::Complex(c) if c.imag().is_zero() && c.real().is_integer() => {
                Value::Integer(c.into_real_imag().0.to_integer().unwrap())
            }
            _ => self,
        }
    }

    /// Consumes and attempts to coerce the value to a complex number. This coercion is lossless.
    pub fn coerce_complex(self) -> Self {
        match self {
            Value::Float(n) => Value::Complex(complex(n)),
            Value::Integer(n) => Value::Complex(complex(n)),
            _ => self,
        }
    }

    /// Converts this value from radians to degrees. If it is a real number, it is converted as
    /// usual. If it is a complex number, the real and imaginary parts are converted separately.
    pub fn to_degrees(self) -> Self {
        let convert = |n: Float| n * 180.0 / &*PI;
        match self {
            Value::Float(n) => Value::Float(convert(n)),
            Value::Integer(n) => Value::Float(convert(float(n))),
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
            Value::Float(n) => Value::Float(convert(n)),
            Value::Integer(n) => Value::Float(convert(float(n))),
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
            Value::Float(_) => true,
            Value::Integer(_) => true,
            Value::Complex(c) => c.imag().is_zero(),
            _ => false,
        }
    }

    /// Returns true if this value is a complex number, or can be coerced to one.
    pub fn is_complex(&self) -> bool {
        matches!(self, Value::Complex(_) | Value::Float(_) | Value::Integer(_))
    }

    /// Returns true if this value is a boolean.
    pub fn is_boolean(&self) -> bool {
        matches!(self, Value::Boolean(_))
    }

    /// Returns true if this value is a unit type.
    pub fn is_unit(&self) -> bool {
        matches!(self, Value::Unit)
    }

    /// Returns true if this value is truthy.
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Float(n) => !n.is_zero(),
            Value::Integer(n) => !n.is_zero(),
            Value::Complex(c) => !c.is_zero(),
            Value::Boolean(b) => *b,
            Value::Unit => false,
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
        Value::Float(float(n))
    }
}

impl From<Float> for Value {
    fn from(n: Float) -> Self {
        Value::Float(n)
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
