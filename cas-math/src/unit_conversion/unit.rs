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
            (Quantity::Mass(m1), Quantity::Mass(m2)) => {
                Ok(m1.conversion_factor().powi(power)
                    / m2.conversion_factor().powi(power))
            },
            (Quantity::Area(a1), Quantity::Area(a2)) => {
                Ok(a1.conversion_factor().powi(power)
                    / a2.conversion_factor().powi(power))
            },
            (Quantity::Volume(v1), Quantity::Volume(v2)) => {
                Ok(v1.conversion_factor().powi(power)
                    / v2.conversion_factor().powi(power))
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
///
/// See the corresponding enum variants for the available units and their abbreviations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Quantity {
    Length(Length),
    Mass(Mass),
    Area(Area),
    Volume(Volume),
    Time(Time),
}

impl Display for Quantity {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Quantity::Length(l) => write!(f, "{}", l),
            Quantity::Mass(m) => write!(f, "{}", m),
            Quantity::Area(a) => write!(f, "{}", a),
            Quantity::Volume(v) => write!(f, "{}", v),
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
            .or_else(|_| Mass::try_from(value).map(Quantity::Mass))
            .or_else(|_| Area::try_from(value).map(Quantity::Area))
            .or_else(|_| Volume::try_from(value).map(Quantity::Volume))
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
            Quantity::Mass(m) => m.conversion_factor_to(target),
            Quantity::Area(a) => a.conversion_factor_to(target),
            Quantity::Volume(v) => v.conversion_factor_to(target),
            Quantity::Time(t) => t.conversion_factor_to(target),
        }
    }
}

/// A unit of length.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Length {
    /// - Abbreviation: `pc`
    /// - `1 pc = 3.085677581e16 m`
    Parsec,

    /// - Abbreviation: `ly`
    /// - `1 ly = 9.4607304725808e15 m`
    LightYear,

    /// - Abbreviation: `au`
    /// - `1 au = 1.495978707e11 m`
    AstronomicalUnit,

    /// - Abbreviation: `nmi`
    /// - `1 nmi = 1852 m`
    NauticalMile,

    /// - Abbreviation: `km`
    /// - `1 km = 1000 m`
    Kilometer,

    /// - Abbreviation: `m`
    /// - `1 m = 1 m`
    Meter,

    /// - Abbreviation: `dm`
    /// - `1 dm = 0.1 m`
    Decimeter,

    /// - Abbreviation: `cm`
    /// - `1 cm = 0.01 m`
    Centimeter,

    /// - Abbreviation: `mm`
    /// - `1 mm = 0.001 m`
    Millimeter,

    /// - Abbreviation: `µm`, `um`
    /// - `1 µm = 1e-6 m`
    Micrometer,

    /// - Abbreviation: `nm`
    /// - `1 nm = 1e-9 m`
    Nanometer,

    /// - Abbreviation: `Å`, `A`
    /// - `1 Å = 1e-10 m`
    Angstrom,

    /// - Abbreviation: `pm`
    /// - `1 pm = 1e-12 m`
    Picometer,

    /// - Abbreviation: `mi`
    /// - `1 mi = 1609.344 m`
    Mile,

    /// - Abbreviation: `yd`
    /// - `1 yd = 0.9144 m`
    Yard,

    /// - Abbreviation: `ft`
    /// - `1 ft = 0.3048 m`
    Foot,

    /// - Abbreviation: `in`
    /// - `1 in = 0.0254 m`
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

/// A unit of mass.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Mass {
    /// A US (short) ton (2000 pounds).
    ///
    /// - Abbreviation: `tn`
    /// - `1 tn = 907.18474 kg`
    ShortTon,

    /// An imperial (long) ton (2240 pounds).
    ///
    /// - Abbreviation: `lt`
    /// - `1 lt = 1016.0469088 kg`
    LongTon,

    /// - Abbreviation: `lb`
    /// - `1 lb = 0.45359237 kg`
    Pound,

    /// - Abbreviation: `oz`
    /// - `1 oz = 0.028349523125 kg`
    Ounce,

    /// A metric tonne (1000 kilograms).
    ///
    /// - Abbreviation: `t`
    /// - `1 t = 1000 kg`
    Tonne,

    /// - Abbreviation: `kg`
    /// - `1 kg = 1 kg`
    Kilogram,

    /// - Abbreviation: `g`
    /// - `1 g = 0.001 kg`
    Gram,

    /// - Abbreviation: `cg`
    /// - `1 cg = 1e-5 kg`
    Centigram,

    /// - Abbreviation: `mg`
    /// - `1 mg = 1e-6 kg`
    Milligram,

    /// - Abbreviation: `µg`, `ug`
    /// - `1 µg = 1e-9 kg`
    Microgram,

    /// An atomic mass unit (1/12 of the mass of a carbon 12 atom).
    ///
    /// - Abbreviation: `amu`
    /// - `1 amu = 1.66053906892e-27 kg`
    AtomicMassUnit,
}

impl From<Mass> for Unit {
    fn from(m: Mass) -> Self {
        Self::new(Quantity::Mass(m))
    }
}

impl From<Mass> for Quantity {
    fn from(m: Mass) -> Self {
        Self::Mass(m)
    }
}

impl FromStr for Mass {
    type Err = InvalidUnit;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "tn" => Ok(Mass::ShortTon),
            "lt" => Ok(Mass::LongTon),
            "lb" => Ok(Mass::Pound),
            "oz" => Ok(Mass::Ounce),
            "t" => Ok(Mass::Tonne),
            "kg" => Ok(Mass::Kilogram),
            "g" => Ok(Mass::Gram),
            "cg" => Ok(Mass::Centigram),
            "mg" => Ok(Mass::Milligram),
            "µg" | "ug" => Ok(Mass::Microgram),
            "amu" => Ok(Mass::AtomicMassUnit),
            _ => Err(InvalidUnit { unit: value.to_owned() }),
        }
    }
}

impl TryFrom<&str> for Mass {
    type Error = InvalidUnit;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl Display for Mass {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Mass::ShortTon => write!(f, "tn"),
            Mass::LongTon => write!(f, "lt"),
            Mass::Pound => write!(f, "lb"),
            Mass::Ounce => write!(f, "oz"),
            Mass::Tonne => write!(f, "t"),
            Mass::Kilogram => write!(f, "kg"),
            Mass::Gram => write!(f, "g"),
            Mass::Centigram => write!(f, "cg"),
            Mass::Milligram => write!(f, "mg"),
            Mass::Microgram => write!(f, "µg"),
            Mass::AtomicMassUnit => write!(f, "u"),
        }
    }
}

impl Convert for Mass {
    const BASE: Self = Mass::Kilogram;

    fn conversion_factor(&self) -> f64 {
        match self {
            Mass::ShortTon => 907.18474,
            Mass::LongTon => 1016.0469088,
            Mass::Pound => 0.45359237,
            Mass::Ounce => 0.028349523125,
            Mass::Tonne => 1000.0,
            Mass::Kilogram => 1.0,
            Mass::Gram => 0.001,
            Mass::Centigram => 1e-5,
            Mass::Milligram => 1e-6,
            Mass::Microgram => 1e-9,
            Mass::AtomicMassUnit => 1.66053906892e-27,
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
    /// - Abbreviation: `ha`
    /// - `1 ha = 100 a`
    Hectare,

    /// - Abbreviation: `daa`
    /// - `1 daa = 10 a`
    Decare,

    /// - Abbreviation: `a`
    /// - `1 a = 1 a`
    Are,

    /// - Abbreviation: `da`
    /// - `1 da = 0.1 a`
    Deciare,

    /// - Abbreviation: `ca`
    /// - `1 ca = 0.01 a`
    Centiare,

    /// - Abbreviation: `b`
    /// - `1 b = 1e-30 a`
    Barn,

    /// - Abbreviation: `ac`
    /// - `1 ac = 40.468564224 a`
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

/// A unit of volume.
///
/// Measurements of volume are of the same kind as measurements of length with power 3. Thus, and
/// measurement created with a volume unit can be converted to a a length unit cubed, and vice
/// versa.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Volume {
    /// A US bushel (2150.42 cubic inches).
    ///
    /// - Abbreviation: `bu`
    /// - `1 bu = 35.2390704 L`
    Bushel,

    /// A US gallon (231 cubic inches).
    ///
    /// - Abbreviation: `gal`
    /// - `1 gal = 3.785411784 L`
    Gallon,

    /// A US quart (57.75 cubic inches).
    ///
    /// - Abbreviation: `qt`
    /// - `1 qt = 0.946352946 L`
    Quart,

    /// A US pint (28.875 cubic inches).
    ///
    /// - Abbreviation: `pt`
    /// - `1 pt = 0.473176473 L`
    Pint,

    /// - Abbreviation: `c`
    /// - `1 c = 0.2365882365 L`
    Cup,

    /// A US fluid ounce (1.8046875 cubic inches).
    ///
    /// - Abbreviation: `floz`
    /// - `1 floz = 0.0295735295625 L`
    FluidOunce,

    /// A US tablespoon (0.90234375 cubic inches).
    ///
    /// - Abbreviation: `tbsp`
    /// - `1 tbsp = 0.01478676478125 L`
    Tablespoon,

    /// A US teaspoon (0.30078125 cubic inches).
    ///
    /// - Abbreviation: `tsp`
    /// - `1 tsp = 0.00492892159375 L`
    Teaspoon,

    /// - Abbreviation: `kL`
    /// - `1 kL = 1000 L`
    Kiloliter,

    /// - Abbreviation: `L`
    /// - `1 L = 1 L`
    Liter,

    /// - Abbreviation: `cL`
    /// - `1 cL = 0.01 L`
    Centiliter,

    /// - Abbreviation: `mL`
    /// - `1 mL = 0.001 L`
    Milliliter,

    /// - Abbreviation: `µL`, `uL`
    /// - `1 µL = 1e-6 L`
    Microliter,
}

impl From<Volume> for Unit {
    fn from(v: Volume) -> Self {
        Self::new(Quantity::Volume(v))
    }
}

impl From<Volume> for Quantity {
    fn from(v: Volume) -> Self {
        Self::Volume(v)
    }
}

impl FromStr for Volume {
    type Err = InvalidUnit;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "bu" => Ok(Volume::Bushel),
            "gal" => Ok(Volume::Gallon),
            "qt" => Ok(Volume::Quart),
            "pt" => Ok(Volume::Pint),
            "c" => Ok(Volume::Cup),
            "floz" => Ok(Volume::FluidOunce),
            "tbsp" => Ok(Volume::Tablespoon),
            "tsp" => Ok(Volume::Teaspoon),
            "kL" => Ok(Volume::Kiloliter),
            "L" => Ok(Volume::Liter),
            "cL" => Ok(Volume::Centiliter),
            "mL" => Ok(Volume::Milliliter),
            "µL" | "uL" => Ok(Volume::Microliter),
            _ => Err(InvalidUnit { unit: value.to_owned() }),
        }
    }
}

impl TryFrom<&str> for Volume {
    type Error = InvalidUnit;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl Display for Volume {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Volume::Bushel => write!(f, "bu"),
            Volume::Gallon => write!(f, "gal"),
            Volume::Quart => write!(f, "qt"),
            Volume::Pint => write!(f, "pt"),
            Volume::Cup => write!(f, "c"),
            Volume::FluidOunce => write!(f, "floz"),
            Volume::Tablespoon => write!(f, "tbsp"),
            Volume::Teaspoon => write!(f, "tsp"),
            Volume::Kiloliter => write!(f, "kL"),
            Volume::Liter => write!(f, "L"),
            Volume::Centiliter => write!(f, "cL"),
            Volume::Milliliter => write!(f, "mL"),
            Volume::Microliter => write!(f, "µL"),
        }
    }
}

impl Convert for Volume {
    const BASE: Self = Volume::Liter;

    fn conversion_factor(&self) -> f64 {
        match self {
            Volume::Bushel => 35.2390704,
            Volume::Gallon => 3.785411784,
            Volume::Quart => 0.946352946,
            Volume::Pint => 0.473176473,
            Volume::Cup => 0.2365882365,
            Volume::FluidOunce => 0.0295735295625,
            Volume::Tablespoon => 0.01478676478125,
            Volume::Teaspoon => 0.00492892159375,
            Volume::Kiloliter => 1000.0,
            Volume::Liter => 1.0,
            Volume::Centiliter => 0.01,
            Volume::Milliliter => 0.001,
            Volume::Microliter => 1e-6,
        }
    }

    fn conversion_factor_to(&self, target: impl Into<Unit>) -> Option<f64> {
        let target = target.into();
        if matches!(target.quantity, Quantity::Length(_)) && target.power == 3 {
            Some(
                // convert from self to base length unit cubed
                self.conversion_factor() * 0.001

                // convert from base length unit cubed to target
                * Unit::with_power(Length::BASE, 3).conversion_factor(target).unwrap()
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
    /// 100 years, each of 365.25 days.
    ///
    /// - Abbreviation: `cen`
    /// - `1 cen = 3.15576e9 s`
    Century,

    /// 10 years, each of 365.25 days.
    ///
    /// - Abbreviation: `dec`
    /// - `1 dec = 3.15576e8 s`
    Decade,

    /// A year of 365.25 days. Decades and centuries are defined in terms of this unit.
    ///
    /// - Abbreviation: `yr`
    /// - `1 yr = 3.15576e7 s`
    Year,

    /// - Abbreviation: `wk`
    /// - `1 wk = 604800 s`
    Week,

    /// - Abbreviation: `day`
    /// - `1 day = 86400 s`
    Day,

    /// - Abbreviation: `hr`
    /// - `1 hr = 3600 s`
    Hour,

    /// - Abbreviation: `min`
    /// - `1 min = 60 s`
    Minute,

    /// - Abbreviation: `sec`
    /// - `1 sec = 1 s`
    Second,

    /// - Abbreviation: `ds`
    /// - `1 ds = 0.1 s`
    Decisecond,

    /// - Abbreviation: `cs`
    /// - `1 cs = 0.01 s`
    Centisecond,

    /// - Abbreviation: `ms`
    /// - `1 ms = 0.001 s`
    Millisecond,

    /// - Abbreviation: `µs`, `us`
    /// - `1 µs = 1e-6 s`
    Microsecond,

    /// - Abbreviation: `ns`
    /// - `1 ns = 1e-9 s`
    Nanosecond,

    /// - Abbreviation: `ps`
    /// - `1 ps = 1e-12 s`
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
