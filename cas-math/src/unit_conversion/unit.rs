use std::{error::Error, fmt::{self, Display, Formatter}, str::FromStr};
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

/// Try a to convert a string to a [`Unit`].
///
/// The string should be in the format `<quantity>[^<power>]`, where `<quantity>` is the
/// abbreviation of the quantity (see [`Quantity`]), and `<power>` is the power of the unit. If
/// `<power>` is not specified, it is assumed to be 1.
impl FromStr for Unit {
    type Err = InvalidUnit;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let mut iter = value.split('^');
        let (a, b) = (iter.next(), iter.next());
        if iter.next().is_some() {
            return Err(InvalidUnit { unit: value.to_owned() });
        }

        match (a, b) {
            (Some(a), Some(b)) => {
                let quantity = Quantity::try_from(a)?;
                let power = b.parse().map_err(|_| InvalidUnit { unit: value.to_owned() })?;
                Ok(Unit::with_power(quantity, power))
            },
            (Some(a), None) => Ok(Quantity::try_from(a)?.into()),
            _ => Err(InvalidUnit { unit: value.to_owned() }),
        }
    }
}

/// Try a to convert a string to a [`Unit`].
///
/// The string should be in the format `<quantity>[^<power>]`, where `<quantity>` is the
/// abbreviation of the quantity (see [`Quantity`]), and `<power>` is the power of the unit. If
/// `<power>` is not specified, it is assumed to be 1.
impl TryFrom<&str> for Unit {
    type Error = InvalidUnit;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
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

    /// If this unit can be converted to the target unit, returns the conversion factor between
    /// them.
    pub fn conversion_factor(&self, target: Unit) -> Result<f64, ConversionError> {
        if self.power != target.power {
            return self.quantity.conversion_factor_to(target)
                .or_else(|| target.quantity.conversion_factor_to(*self).map(|f| 1.0 / f))
                .ok_or(ConversionError { unit: *self, target });
        }

        let power = self.power as i32;
        match (self.quantity, target.quantity) {
            (Quantity::Length(l1), Quantity::Length(l2)) => {
                Ok(l1.conversion_factor().powi(power)
                    / l2.conversion_factor().powi(power))
            },
            (Quantity::Area(a1), Quantity::Area(a2)) => {
                Ok(a1.conversion_factor().powi(power)
                    / a2.conversion_factor().powi(power))
            },
            (Quantity::Time(t1), Quantity::Time(t2)) => {
                Ok(t1.conversion_factor().powi(power)
                    / t2.conversion_factor().powi(power))
            },
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
            "cannot convert from `{}` to `{}`",
            self.unit, self.target
        )
    }
}

impl Error for ConversionError {}

/// Error returned if the given unit abbreviation is invalid.
#[derive(Debug)]
pub struct InvalidUnit {
    /// The invalid unit abbreviation.
    unit: String,
}

impl Display for InvalidUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "not a valid unit: `{}`", self.unit)
    }
}

impl Error for InvalidUnit {}

/// A type of quantity, such as length, mass, volume, etc.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Quantity {
    Length(Length),
    Area(Area),
    Time(Time),
}

impl Display for Quantity {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Quantity::Length(l) => write!(f, "{}", l),
            Quantity::Area(a) => write!(f, "{}", a),
            Quantity::Time(t) => write!(f, "{}", t),
        }
    }
}

/// Try a to convert a string to a quantity.
///
/// Note that [`Quantity`] does not encode the power of the unit, so this function will return an
/// error if the string contains a power. Use [`Unit::try_from`] to convert a string to a unit.
impl FromStr for Quantity {
    type Err = InvalidUnit;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Length::try_from(value).map(Quantity::Length)
            .or_else(|_| Area::try_from(value).map(Quantity::Area))
            .or_else(|_| Time::try_from(value).map(Quantity::Time))
    }
}

/// Try a to convert a string to a quantity.
///
/// Note that [`Quantity`] does not encode the power of the unit, so this function will return an
/// error if the string contains a power. Use [`Unit::try_from`] to convert a string to a unit.
impl TryFrom<&str> for Quantity {
    type Error = InvalidUnit;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<Quantity> for Unit {
    fn from(q: Quantity) -> Self {
        Self::new(q)
    }
}

impl Quantity {
    fn conversion_factor_to(&self, target: impl Into<Unit>) -> Option<f64> {
        let target = target.into();
        match self {
            Quantity::Length(l) => l.conversion_factor_to(target),
            Quantity::Area(a) => a.conversion_factor_to(target),
            Quantity::Time(t) => t.conversion_factor_to(target),
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

impl FromStr for Length {
    type Err = InvalidUnit;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "pc" => Ok(Length::Parsec),
            "ly" => Ok(Length::LightYear),
            "au" => Ok(Length::AstronomicalUnit),
            "nmi" => Ok(Length::NauticalMile),
            "km" => Ok(Length::Kilometer),
            "m" => Ok(Length::Meter),
            "dm" => Ok(Length::Decimeter),
            "cm" => Ok(Length::Centimeter),
            "mm" => Ok(Length::Millimeter),
            "µm" | "um" => Ok(Length::Micrometer),
            "nm" => Ok(Length::Nanometer),
            "Å" | "A" => Ok(Length::Angstrom),
            "pm" => Ok(Length::Picometer),

            "mi" => Ok(Length::Mile),
            "yd" => Ok(Length::Yard),
            "ft" => Ok(Length::Foot),
            "in" => Ok(Length::Inch),
            _ => Err(InvalidUnit { unit: value.to_owned() }),
        }
    }
}

impl TryFrom<&str> for Length {
    type Error = InvalidUnit;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
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

/// A unit of area.
///
/// Measurements of area are of the same kind as measurements of length with power 2. Thus, and
/// measurement created with an area unit can be converted to a a length unit squared, and vice
/// versa.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Area {
    Hectare,
    Decare,
    Are,
    Deciare,
    Centiare,
    Barn,

    Acre,
}

impl From<Area> for Unit {
    fn from(a: Area) -> Self {
        Self::new(Quantity::Area(a))
    }
}

impl From<Area> for Quantity {
    fn from(a: Area) -> Self {
        Self::Area(a)
    }
}

impl FromStr for Area {
    type Err = InvalidUnit;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "ha" => Ok(Area::Hectare),
            "daa" => Ok(Area::Decare),
            "a" => Ok(Area::Are),
            "da" => Ok(Area::Deciare),
            "ca" => Ok(Area::Centiare),
            "b" => Ok(Area::Barn),

            "ac" => Ok(Area::Acre),
            _ => Err(InvalidUnit { unit: value.to_owned() }),
        }
    }
}

impl TryFrom<&str> for Area {
    type Error = InvalidUnit;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl Display for Area {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Area::Hectare => write!(f, "ha"),
            Area::Decare => write!(f, "daa"),
            Area::Are => write!(f, "a"),
            Area::Deciare => write!(f, "da"),
            Area::Centiare => write!(f, "ca"),
            Area::Barn => write!(f, "b"),

            Area::Acre => write!(f, "ac"),
        }
    }
}

impl Convert for Area {
    const BASE: Self = Area::Are;

    fn conversion_factor(&self) -> f64 {
        match self {
            Area::Hectare => 100.0,
            Area::Decare => 10.0,
            Area::Are => 1.0,
            Area::Deciare => 0.1,
            Area::Centiare => 0.01,
            Area::Barn => 1e-30,

            Area::Acre => 40.468564224,
        }
    }

    fn conversion_factor_to(&self, target: impl Into<Unit>) -> Option<f64> {
        let target = target.into();
        if matches!(target.quantity, Quantity::Length(_)) && target.power == 2 {
            Some(
                // convert from self to base length unit squared
                self.conversion_factor() * 100.0

                // convert from base length unit squared to target
                * Unit::with_power(Length::BASE, 2).conversion_factor(target).unwrap()
            )
        } else {
            None
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

impl FromStr for Time {
    type Err = InvalidUnit;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "cen" => Ok(Time::Century),
            "dec" => Ok(Time::Decade),
            "yr" => Ok(Time::Year),
            "wk" => Ok(Time::Week),
            "day" => Ok(Time::Day),
            "hr" => Ok(Time::Hour),
            "min" => Ok(Time::Minute),
            "sec" => Ok(Time::Second),
            "ds" => Ok(Time::Decisecond),
            "cs" => Ok(Time::Centisecond),
            "ms" => Ok(Time::Millisecond),
            "µs" | "us" => Ok(Time::Microsecond),
            "ns" => Ok(Time::Nanosecond),
            "ps" => Ok(Time::Picosecond),
            _ => Err(InvalidUnit { unit: value.to_owned() }),
        }
    }
}

impl TryFrom<&str> for Time {
    type Error = InvalidUnit;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
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
