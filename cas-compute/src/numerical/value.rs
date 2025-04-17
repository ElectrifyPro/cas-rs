use cas_parser::parser::ast::range::RangeKind;
use crate::consts::PI;
use crate::primitive::{complex, float};
use rug::{Complex, Float, Integer};
use std::{cell::RefCell, fmt::{Display, Formatter}, rc::Rc};
use super::{fmt::{FormatOptions, ValueFormatter}, func::Function};

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

    /// A list of values.
    ///
    /// In `cas-rs`, a list is a reference to a vector of values. This is done to allow efficient
    /// cloning of lists, as well as mutation of lists in-place. References are passed around
    /// by default, which can result in somewhat confusing behavior, for example:
    ///
    /// ```text
    /// a = [1, 2, 3]
    /// b = a
    /// b[0] = 4
    /// print(a) // prints [4, 2, 3]
    /// ```
    ///
    /// TODO: In the future, a `clone` method may be added to `cas-rs` to allow the user to
    /// explicitly clone the list instead of copying the reference.
    List(Rc<RefCell<Vec<Value>>>),

    /// A range representing a sequence of values, either half-open or closed.
    Range(Box<Value>, RangeKind, Box<Value>),

    /// A function.
    ///
    /// Functions are treated as values just like any other value in `cas-rs`; they can be stored
    /// in variables, passed as arguments to other functions, and returned from functions.
    Function(Function),
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
            Value::List(_) => "List",
            Value::Range(_, _, _) => "Range",
            Value::Function(_) => "Function",
        }
    }

    /// Consumes and attempts to coerce the value to a real number. **Note that this coercion can
    /// be lossy** if converting an arbitrary-precision integer to a fixed-width float. To preserve
    /// precision, see [`Value::coerce_integer`] and [`Value::coerce_number`].
    ///
    /// This conversion only occurs if one of the following is true:
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

    /// Consumes and attempts to coerce the value to an integer. **This coercion is lossless**.
    ///
    /// This conversion only occurs if one of the following is true:
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

    /// Consumes and attempts to coerce the value to a real number or an integer, preferring
    /// integers if possible. **This coercion is lossless**.
    ///
    /// This conversion follows these rules:
    ///
    /// - If the value is an integer, it is returned as-is.
    /// - If the value is a float with a zero fractional part, it is converted to an integer.
    /// Otherwise, it is returned as-is.
    /// - If the value is a complex number with a zero imaginary part, either an integer or float
    /// is returned if the real part is an integer or float, respectively. Otherwise, it is
    /// returned as-is.
    pub fn coerce_number(self) -> Self {
        match self {
            Value::Float(n) if n.is_integer() => Value::Integer(n.to_integer().unwrap()),
            Value::Complex(c) if c.imag().is_zero() => {
                let (real, _) = c.into_real_imag();
                if real.is_integer() {
                    Value::Integer(real.to_integer().unwrap())
                } else {
                    Value::Float(real)
                }
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
    pub fn into_degrees(self) -> Self {
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
    pub fn into_radians(self) -> Self {
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
    pub fn is_float(&self) -> bool {
        match self {
            Value::Float(_) => true,
            Value::Integer(_) => true,
            Value::Complex(c) => c.imag().is_zero(),
            _ => false,
        }
    }

    /// Returns true if this value is an integer, or can be coerced to one.
    pub fn is_integer(&self) -> bool {
        match self {
            Value::Float(n) => n.is_integer(),
            Value::Integer(_) => true,
            Value::Complex(c) => c.imag().is_zero() && c.real().is_integer(),
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

    /// Returns true if this value is a list.
    pub fn is_list(&self) -> bool {
        matches!(self, Value::List(_))
    }

    /// Returns true if this value is truthy.
    ///
    /// For each type, the following values are considered "truthy":
    ///
    /// - `Float`: any value except `0.0` and `NaN`
    /// - `Integer`: any value except `0`
    /// - `Complex`: any value except `0.0 + 0.0i` and `NaN + NaNi`
    /// - `Bool`: `true`
    /// - `Unit`: never true; always false
    /// - `List`: lists with at least one element; element(s) does not have to be truthy
    /// - `Range`: ranges with at least one element; element(s) does not have to be truthy
    /// - `Function`: always true
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Float(n) => !n.is_zero(),
            Value::Integer(n) => !n.is_zero(),
            Value::Complex(c) => !c.is_zero(),
            Value::Boolean(b) => *b,
            Value::Unit => false,
            Value::List(l) => !l.borrow().is_empty(),
            Value::Range(lhs, kind, rhs) => {
                match kind {
                    RangeKind::HalfOpen => lhs != rhs,
                    RangeKind::Closed => true,
                }
            },
            Value::Function(_) => true,
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

impl From<i64> for Value {
    fn from(n: i64) -> Self {
        Value::Integer(Integer::from(n))
    }
}

impl From<Integer> for Value {
    fn from(n: Integer) -> Self {
        Value::Integer(n)
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

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Unit
    }
}

impl From<Vec<Value>> for Value {
    fn from(values: Vec<Value>) -> Self {
        Value::List(Rc::new(RefCell::new(values)))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt(Default::default()).fmt(f)
    }
}
