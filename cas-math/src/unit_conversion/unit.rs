use std::fmt::{self, Display, Formatter};
use super::convert::Convert;

/// A unit of measurement.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Unit {
    /// The type of quantity measured by this unit.
    quantity: Quantity,

    /// The power of the base unit.
    power: u8,
}

impl Display for Unit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.quantity)?;
        if self.power > 1 {
            write!(f, "^{}", self.power)?;
        }
        Ok(())
    }
}

impl Unit {
    /// Creates a new unit with the given quantity, with power 1.
    pub fn new(quantity: impl Into<Quantity>) -> Self {
        Self { quantity: quantity.into(), power: 1 }
    }

    /// Creates a new unit with the given quantity and power.
    pub fn with_power(quantity: impl Into<Quantity>, power: u8) -> Self {
        Self { quantity: quantity.into(), power }
    }

    /// Returns [`Ok`] if this unit can be converted to the target unit, or [`Err`] otherwise.
    pub fn can_convert(&self, target: Unit) -> Result<(), ConversionError> {
        if self.power == target.power {
            (self.quantity.matches_kind(target.quantity))
                .then_some(())
                .ok_or(ConversionError { unit: *self, target })
        } else {
            // TODO
            Err(ConversionError { unit: *self, target })
        }
    }

    /// If this unit can be converted to the target unit, returns the conversion factor between
    /// them.
    pub fn conversion_factor(&self, target: Unit) -> Result<f64, ConversionError> {
        match (self.quantity, target.quantity) {
            (Quantity::Length(l1), Quantity::Length(l2)) => {
                Ok(l1.conversion_factor().powi(self.power as i32)
                    / l2.conversion_factor().powi(target.power as i32))
            }
            (Quantity::Time(t1), Quantity::Time(t2)) => {
                Ok(t1.conversion_factor().powi(self.power as i32)
                    / t2.conversion_factor().powi(target.power as i32))
            }
            _ => Err(ConversionError { unit: *self, target }),
        }
    }
}

/// Error returned if a unit cannot be converted to another.
#[derive(Debug)]
pub struct ConversionError {
    /// The unit that could not be converted.
    unit: Unit,

    /// The target unit.
    target: Unit,
}

impl Display for ConversionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f,
            "cannot convert from {} to {}",
            self.unit, self.target
        )
    }
}

/// A type of quantity, such as length, mass, volume, etc.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Quantity {
    Length(Length),
    Time(Time),
}

impl Display for Quantity {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Quantity::Length(l) => write!(f, "{}", l),
            Quantity::Time(t) => write!(f, "{}", t),
        }
    }
}

impl From<Quantity> for Unit {
    fn from(q: Quantity) -> Self {
        Self::new(q)
    }
}

impl Quantity {
    /// Returns true if this quantity is the same kind as the target quantity.
    pub fn matches_kind(&self, target: Quantity) -> bool {
        match (self, target) {
            (Quantity::Length(_), Quantity::Length(_)) => true,
            (Quantity::Time(_), Quantity::Time(_)) => true,
            _ => false,
        }
    }
}

/// A unit of length.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Length {
    Parsec,
    LightYear,
    AstronomicalUnit,
    NauticalMile,
    Kilometer,
    Meter,
    Decimeter,
    Centimeter,
    Millimeter,
    Micrometer,
    Nanometer,
    Angstrom,
    Picometer,

    Mile,
    Yard,
    Foot,
    Inch,
}

impl From<Length> for Unit {
    fn from(l: Length) -> Self {
        Self::new(Quantity::Length(l))
    }
}

impl From<Length> for Quantity {
    fn from(l: Length) -> Self {
        Self::Length(l)
    }
}

impl Display for Length {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Length::Parsec => write!(f, "pc"),
            Length::LightYear => write!(f, "ly"),
            Length::AstronomicalUnit => write!(f, "au"),
            Length::NauticalMile => write!(f, "nmi"),
            Length::Kilometer => write!(f, "km"),
            Length::Meter => write!(f, "m"),
            Length::Decimeter => write!(f, "dm"),
            Length::Centimeter => write!(f, "cm"),
            Length::Millimeter => write!(f, "mm"),
            Length::Micrometer => write!(f, "µm"),
            Length::Nanometer => write!(f, "nm"),
            Length::Angstrom => write!(f, "Å"),
            Length::Picometer => write!(f, "pm"),

            Length::Mile => write!(f, "mi"),
            Length::Yard => write!(f, "yd"),
            Length::Foot => write!(f, "ft"),
            Length::Inch => write!(f, "in"),
        }
    }
}

impl Convert for Length {
    const BASE: Self = Length::Meter;

    fn conversion_factor(&self) -> f64 {
        match self {
            Length::Parsec => 3.085677581e16,
            Length::LightYear => 9.4607304725808e15,
            Length::AstronomicalUnit => 1.495978707e11,
            Length::NauticalMile => 1852.0,
            Length::Kilometer => 1000.0,
            Length::Meter => 1.0,
            Length::Decimeter => 0.1,
            Length::Centimeter => 0.01,
            Length::Millimeter => 0.001,
            Length::Micrometer => 1e-6,
            Length::Nanometer => 1e-9,
            Length::Angstrom => 1e-10,
            Length::Picometer => 1e-12,

            Length::Mile => 1609.344,
            Length::Yard => 0.9144,
            Length::Foot => 0.3048,
            Length::Inch => 0.0254,
        }
    }
}

/// A unit of time.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Time {
    Century,
    Decade,

    /// A year of 365.25 days. Decades and centuries are defined in terms of this unit.
    Year,

    Week,
    Day,
    Hour,
    Minute,
    Second,
    Decisecond,
    Centisecond,
    Millisecond,
    Microsecond,
    Nanosecond,
    Picosecond,
}

impl From<Time> for Unit {
    fn from(t: Time) -> Self {
        Self::new(Quantity::Time(t))
    }
}

impl From<Time> for Quantity {
    fn from(t: Time) -> Self {
        Self::Time(t)
    }
}

impl Display for Time {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Time::Century => write!(f, "cen"),
            Time::Decade => write!(f, "dec"),
            Time::Year => write!(f, "yr"),
            Time::Week => write!(f, "wk"),
            Time::Day => write!(f, "day"),
            Time::Hour => write!(f, "hr"),
            Time::Minute => write!(f, "min"),
            Time::Second => write!(f, "sec"),
            Time::Decisecond => write!(f, "ds"),
            Time::Centisecond => write!(f, "cs"),
            Time::Millisecond => write!(f, "ms"),
            Time::Microsecond => write!(f, "µs"),
            Time::Nanosecond => write!(f, "ns"),
            Time::Picosecond => write!(f, "ps"),
        }
    }
}

impl Convert for Time {
    const BASE: Self = Time::Second;

    fn conversion_factor(&self) -> f64 {
        match self {
            Time::Century => 3.15576e9,
            Time::Decade => 3.15576e8,
            Time::Year => 3.15576e7,
            Time::Week => 604800.0,
            Time::Day => 86400.0,
            Time::Hour => 3600.0,
            Time::Minute => 60.0,
            Time::Second => 1.0,
            Time::Decisecond => 0.1,
            Time::Centisecond => 0.01,
            Time::Millisecond => 0.001,
            Time::Microsecond => 1e-6,
            Time::Nanosecond => 1e-9,
            Time::Picosecond => 1e-12,
        }
    }
}
