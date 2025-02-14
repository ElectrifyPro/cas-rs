use cas_unit_convert::{unit::InvalidUnit, ConversionError};
use std::num::ParseFloatError;

/// Utility enum to package any error that can occur during unit conversion.
pub enum Error {
    /// Incorrect number of arguments.
    Args,

    /// Invalid float.
    Float(ParseFloatError),

    /// Invalid unit.
    Unit(InvalidUnit),

    /// No conversion between the two units.
    Conversion(ConversionError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Args => write!(f, "Format: <value> <from> <to>"),
            Self::Float(err) => write!(f, "{}", err),
            Self::Unit(err) => write!(f, "{}", err),
            Self::Conversion(err) => write!(f, "{}", err),
        }
    }
}

impl From<Vec<&str>> for Error {
    fn from(_: Vec<&str>) -> Self {
        Self::Args
    }
}

impl From<ParseFloatError> for Error {
    fn from(error: ParseFloatError) -> Self {
        Self::Float(error)
    }
}

impl From<InvalidUnit> for Error {
    fn from(error: InvalidUnit) -> Self {
        Self::Unit(error)
    }
}

impl From<ConversionError> for Error {
    fn from(error: ConversionError) -> Self {
        Self::Conversion(error)
    }
}
