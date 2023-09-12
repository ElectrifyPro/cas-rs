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
    pub fn new(quantity: Quantity) -> Self {
        Self { quantity, power: 1 }
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
        self.can_convert(target)?;
        Ok(self.quantity.conversion_factor(target.quantity))
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
}

impl Display for Quantity {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Quantity::Length(l) => write!(f, "{}", l),
        }
    }
}

impl Quantity {
    /// Returns the conversion factor from this quantity to the target quantity.
    pub fn conversion_factor(&self, target: Quantity) -> f64 {
        match (self, target) {
            (Quantity::Length(l1), Quantity::Length(l2)) => l1.conversion_factor() / l2.conversion_factor(),
        }
    }

    /// Returns true if this quantity is the same kind as the target quantity.
    pub fn matches_kind(&self, target: Quantity) -> bool {
        match (self, target) {
            (Quantity::Length(_), Quantity::Length(_)) => true,
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
