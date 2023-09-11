use std::fmt::{self, Display, Formatter};

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

/// A type of quantity.
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
