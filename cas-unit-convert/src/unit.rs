use std::{error::Error, fmt::{self, Display, Formatter}, str::FromStr};
use super::convert::Convert;

/// A compound unit, created by combining multiple [`Unit`]s.
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct CompoundUnit {
    /// The units that make up this compound unit.
    units: Vec<Unit>,
}

impl Display for CompoundUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Some((first, rest)) = self.units.split_first() else {
            return Ok(());
        };

        // there is no `/` or `*` preceding the first unit, so we format it separately
        write!(f, "{}", first)?;

        for unit in rest {
            if unit.power > 0 {
                write!(f, "*{}", unit.base)?;
            } else {
                write!(f, "/{}", unit.base)?;
            }
            if unit.power.abs() > 1 {
                write!(f, "^{}", unit.power.abs())?;
            }
        }

        Ok(())
    }
}

impl FromStr for CompoundUnit {
    type Err = InvalidUnit;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let mut units = Vec::new();

        /// Look back in the given string and return the index of the first `*` or `/` character
        /// found, along with the character itself.
        fn look_back(slice: &str) -> Option<(usize, char)> {
            let next_star = slice.rfind('*');
            let next_div = slice.rfind('/');
            match (next_star, next_div) {
                // choose the later of the two characters
                (Some(s), Some(d)) => Some((s.max(d), if s > d { '*' } else { '/' })),
                (Some(s), None) => Some((s, '*')),
                (None, Some(d)) => Some((d, '/')),
                _ => None,
            }
        }

        // value.split(&['*', '/']) is nice, but it doesn't tell us which character was used to
        // split. we need to know to determine the sign of the power
        //
        // we could use value.split(&['*', '/']).enumerate() and use the index to index into the
        // string, but that requires converting the &str to Vec<char>
        //
        // here is a manual approach
        let mut start = value.len();
        while let Some((next, c)) = look_back(&value[..start]) {
            let mut unit: Unit = value[next + 1..start].parse()?;
            unit.power *= if c == '*' { 1 } else { -1 };
            units.push(unit);

            start = next;
        }

        // unit at the start
        units.push(value[..start].parse()?);

        // match order of units in the string
        units.reverse();

        Ok(Self { units })
    }
}

impl TryFrom<&str> for CompoundUnit {
    type Error = InvalidUnit;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl CompoundUnit {
    /// If this derived unit can be converted to the target unit, returns the conversion factor
    /// between them.
    pub fn conversion_factor(&self, target: &CompoundUnit) -> Result<f64, ConversionError> {
        let mut factor = 1.0;
        for unit in &self.units {
            // this is simple and elegant... and it's wrong lol
            // let target_unit = target.units.iter()
            //     .find(|u| std::mem::discriminant(&u.quantity) == std::mem::discriminant(&unit.quantity))
            //     .ok_or(ConversionError { unit: *unit, target: target.units[0] })?;
            let target_unit_factor = target.units.iter()
                .flat_map(|u| unit.conversion_factor(*u))
                .next()
                .ok_or(ConversionError { unit: *unit, target: target.units[0] })?;
            factor *= target_unit_factor;
        }
        Ok(factor)
    }
}

/// Provides copy-pasteable impls for [`Unit`]s.
macro_rules! unit_impl {
    (
        $doc:literal,
        $enum_name:ident, $base_variant:ident: $base_abbr:literal $(=> $to_base_factor:literal $base_quantity:ident^$base_power:literal)?,
        $(
            $($variant_doc:literal,)? $variant:ident: $main_abbr:literal $(, $alt_abbr:literal)* => $factor:literal
        ),*
        $(,)?
    ) => {
        #[doc = $doc]
        ///
        /// The listed abbreviations are the abbreviations used to parse the unit with
        /// [`FromStr`]. The main abbreviation comes first, followed by any alternate
        /// abbreviations that can be used.
        ///
        /// The conversions are listed in terms of the [base unit]. The base unit for
        #[doc = concat!("[`", stringify!($enum_name), "`] is [`", stringify!($enum_name), "::", stringify!($base_variant), "`].")]
        ///
        /// [base unit]: Convert::BASE
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[non_exhaustive]
        pub enum $enum_name {
            $(
                $(
                    #[doc = $variant_doc]
                    ///
                )?
                #[doc = concat!("- Abbreviation: `", $main_abbr, "`", $(", `", $alt_abbr, "`")*)]
                ///
                #[doc = concat!("- `1 ", $main_abbr, " = ", $factor, " ", $base_abbr, "`")]
                $variant,
            )*
        }

        impl FromStr for $enum_name {
            type Err = InvalidUnit;

            fn from_str(value: &str) -> Result<Self, Self::Err> {
                match value {
                    $(
                        $main_abbr $(| $alt_abbr)* => Ok($enum_name::$variant),
                    )*
                    _ => Err(InvalidUnit { unit: value.to_owned() }),
                }
            }
        }

        impl Display for $enum_name {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        $enum_name::$variant => write!(f, $main_abbr),
                    )*
                }
            }
        }

        impl Convert for $enum_name {
            const BASE: Self = $enum_name::$base_variant;

            fn conversion_factor(&self) -> f64 {
                match self {
                    $(
                        $enum_name::$variant => $factor,
                    )*
                }
            }

            $(
                fn conversion_factor_to(&self, target: impl Into<Unit>) -> Option<f64> {
                    let target = target.into();
                    if matches!(target.base, Base::$base_quantity(_)) && target.power == $base_power {
                        Some(
                            // convert from self to base unit^base power
                            self.conversion_factor() * $to_base_factor

                            // convert from base unit^base power to target
                            * Unit::with_power($base_quantity::BASE, $base_power).conversion_factor(target).unwrap()
                        )
                    } else {
                        None
                    }
                }
            )?
        }

        impl From<$enum_name> for CompoundUnit {
            fn from(u: $enum_name) -> Self {
                Self { units: vec![Unit::from(u)] }
            }
        }

        impl From<$enum_name> for Unit {
            fn from(u: $enum_name) -> Self {
                Self::new(Base::$enum_name(u))
            }
        }

        impl From<$enum_name> for Base {
            fn from(u: $enum_name) -> Self {
                Self::$enum_name(u)
            }
        }

        impl TryFrom<&str> for $enum_name {
            type Error = InvalidUnit;

            fn try_from(value: &str) -> Result<Self, Self::Error> {
                value.parse()
            }
        }

        impl $enum_name {
            /// Creates a [`Unit`] with this quantity type and specified power.
            pub fn pow(&self, power: i8) -> Unit {
                Unit::with_power(Base::$enum_name(*self), power)
            }

            /// Creates a [`Unit`] with this quantity type and power 2.
            pub fn squared(&self) -> Unit {
                self.pow(2)
            }

            /// Creates a [`Unit`] with this quantity type and power 3.
            pub fn cubed(&self) -> Unit {
                self.pow(3)
            }
        }
    }
}

/// A unit of measurement that includes a power.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Unit {
    /// The base unit.
    base: Base,

    /// The power of the base unit.
    power: i8,
}

impl Display for Unit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.base)?;
        if self.power != 1 {
            write!(f, "^{}", self.power)?;
        }
        Ok(())
    }
}

/// Try a to convert a string to a [`Unit`].
///
/// The string should be in the format `<base>[^<power>]`, where `<quantity>` is the
/// abbreviation of the quantity (see [`Base`]), and `<power>` is a signed integer representing the
/// power of the unit. If `<power>` is not specified, it is assumed to be 1.
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
                let quantity = Base::try_from(a)?;
                let power = b.parse().map_err(|_| InvalidUnit { unit: value.to_owned() })?;
                Ok(Unit::with_power(quantity, power))
            },
            (Some(a), None) => Ok(Base::try_from(a)?.into()),
            _ => Err(InvalidUnit { unit: value.to_owned() }),
        }
    }
}

/// Try a to convert a string to a [`Unit`].
///
/// The string should be in the format `<quantity>[^<power>]`, where `<quantity>` is the
/// abbreviation of the quantity (see [`Base`]), and `<power>` is the power of the unit. If
/// `<power>` is not specified, it is assumed to be 1.
impl TryFrom<&str> for Unit {
    type Error = InvalidUnit;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<Unit> for CompoundUnit {
    fn from(u: Unit) -> Self {
        Self { units: vec![u] }
    }
}

impl Unit {
    /// Creates a new unit with the given quantity, with power 1.
    pub fn new(quantity: impl Into<Base>) -> Self {
        Self { base: quantity.into(), power: 1 }
    }

    /// Creates a new unit with the given quantity and power.
    pub fn with_power(quantity: impl Into<Base>, power: i8) -> Self {
        Self { base: quantity.into(), power }
    }

    /// If this unit can be converted to the target unit, returns the conversion factor between
    /// them.
    pub fn conversion_factor(&self, target: Unit) -> Result<f64, ConversionError> {
        if self.power != target.power {
            return self.base.conversion_factor_to(target)
                .or_else(|| target.base.conversion_factor_to(*self).map(|f| 1.0 / f))
                .ok_or(ConversionError { unit: *self, target });
        }

        let power = self.power as i32;
        match (self.base, target.base) {
            (Base::Length(l1), Base::Length(l2)) => {
                Ok(l1.conversion_factor().powi(power)
                    / l2.conversion_factor().powi(power))
            },
            (Base::Mass(m1), Base::Mass(m2)) => {
                Ok(m1.conversion_factor().powi(power)
                    / m2.conversion_factor().powi(power))
            },
            (Base::Area(a1), Base::Area(a2)) => {
                Ok(a1.conversion_factor().powi(power)
                    / a2.conversion_factor().powi(power))
            },
            (Base::Volume(v1), Base::Volume(v2)) => {
                Ok(v1.conversion_factor().powi(power)
                    / v2.conversion_factor().powi(power))
            },
            (Base::Time(t1), Base::Time(t2)) => {
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

/// A base unit with no power.
///
/// See the corresponding enum variants for the available units and their abbreviations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Base {
    Length(Length),
    Mass(Mass),
    Area(Area),
    Volume(Volume),
    Time(Time),
}

impl Display for Base {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Base::Length(l) => write!(f, "{}", l),
            Base::Mass(m) => write!(f, "{}", m),
            Base::Area(a) => write!(f, "{}", a),
            Base::Volume(v) => write!(f, "{}", v),
            Base::Time(t) => write!(f, "{}", t),
        }
    }
}

/// Try a to convert a string to a [`Base`].
///
/// Note that [`Base`] does not encode the power of the unit, so this function will return an
/// error if the string contains a power. Use [`Unit::try_from`] to convert a string to a unit,
/// while accounting for the power.
impl FromStr for Base {
    type Err = InvalidUnit;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Length::try_from(value).map(Base::Length)
            .or_else(|_| Mass::try_from(value).map(Base::Mass))
            .or_else(|_| Area::try_from(value).map(Base::Area))
            .or_else(|_| Volume::try_from(value).map(Base::Volume))
            .or_else(|_| Time::try_from(value).map(Base::Time))
    }
}

/// Try a to convert a string to a [`Base`].
///
/// Note that [`Base`] does not encode the power of the unit, so this function will return an
/// error if the string contains a power. Use [`Unit::try_from`] to convert a string to a unit,
impl TryFrom<&str> for Base {
    type Error = InvalidUnit;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<Base> for Unit {
    fn from(q: Base) -> Self {
        Self::new(q)
    }
}

impl Base {
    fn conversion_factor_to(&self, target: impl Into<Unit>) -> Option<f64> {
        let target = target.into();
        match self {
            Base::Length(l) => l.conversion_factor_to(target),
            Base::Mass(m) => m.conversion_factor_to(target),
            Base::Area(a) => a.conversion_factor_to(target),
            Base::Volume(v) => v.conversion_factor_to(target),
            Base::Time(t) => t.conversion_factor_to(target),
        }
    }

    /// Creates a [`Unit`] with this quantity type and specified power.
    pub fn pow(&self, power: i8) -> Unit {
        Unit::with_power(*self, power)
    }

    /// Creates a [`Unit`] with this quantity type and power 2.
    pub fn squared(&self) -> Unit {
        self.pow(2)
    }

    /// Creates a [`Unit`] with this quantity type and power 3.
    pub fn cubed(&self) -> Unit {
        self.pow(3)
    }
}

unit_impl!("A unit of length.",
    Length, Meter: "m",
    Parsec: "pc" => 3.085677581e16,
    LightYear: "ly" => 9.4607304725808e15,
    AstronomicalUnit: "au" => 1.495978707e11,
    NauticalMile: "nmi" => 1852.0,
    Kilometer: "km" => 1000.0,
    Meter: "m" => 1.0,
    Decimeter: "dm" => 0.1,
    Centimeter: "cm" => 0.01,
    Millimeter: "mm" => 0.001,
    Micrometer: "µm", "um" => 1e-6,
    Nanometer: "nm" => 1e-9,
    Angstrom: "Å", "A" => 1e-10,
    Picometer: "pm" => 1e-12,
    Mile: "mi" => 1609.344,
    Yard: "yd" => 0.9144,
    Foot: "ft" => 0.3048,
    Inch: "in" => 0.0254,
);

unit_impl!("A unit of mass.",
    Mass, Kilogram: "kg",
    "A US (short) ton (2000 pounds).", ShortTon: "tn" => 907.18474,
    "An imperial (long) ton (2240 pounds).", LongTon: "lt" => 1016.0469088,
    Pound: "lb" => 0.45359237,
    Ounce: "oz" => 0.028349523125,
    "A metric tonne (1000 kilograms).", Tonne: "t" => 1000.0,
    Kilogram: "kg" => 1.0,
    Gram: "g" => 0.001,
    Centigram: "cg" => 1e-5,
    Milligram: "mg" => 1e-6,
    Microgram: "µg", "ug" => 1e-9,
    "An atomic mass unit (1/12 of the mass of a carbon 12 atom).", AtomicMassUnit: "amu" => 1.66053906892e-27,
);

unit_impl!("A unit of area.\n\nMeasurements of area are of the same kind as measurements of length with power 2. Thus, any measurement created with an area unit can be converted to a a length unit squared, and vice versa.",
    Area, Are: "a" => 100.0 Length^2, // 1 are = 100 m^2
    Hectare: "ha" => 100.0,
    Decare: "daa" => 10.0,
    Are: "a" => 1.0,
    Deciare: "da" => 0.1,
    Centiare: "ca" => 0.01,
    Barn: "b" => 1e-30,
    Acre: "ac" => 40.468564224,
);

unit_impl!("A unit of volume.\n\nMeasurements of volume are of the same kind as measurements of length with power 3. Thus, any measurement created with a volume unit can be converted to a a length unit cubed, and vice versa.",
    Volume, Liter: "L" => 0.001 Length^3, // 1 L = 0.001 m^3
    "A US bushel (2150.42 cubic inches).", Bushel: "bu" => 35.2390704,
    "A US gallon (231 cubic inches).", Gallon: "gal" => 3.785411784,
    "A US quart (57.75 cubic inches).", Quart: "qt" => 0.946352946,
    "A US pint (28.875 cubic inches).", Pint: "pt" => 0.473176473,
    "A US cup (8 fluid ounces).", Cup: "c" => 0.2365882365,
    "A US fluid ounce (1.8046875 cubic inches).", FluidOunce: "floz" => 0.0295735295625,
    "A US tablespoon (0.90234375 cubic inches).", Tablespoon: "tbsp" => 0.01478676478125,
    "A US teaspoon (0.30078125 cubic inches).", Teaspoon: "tsp" => 0.00492892159375,
    Kiloliter: "kL" => 1000.0,
    Liter: "L" => 1.0,
    Centiliter: "cL" => 0.01,
    Milliliter: "mL" => 0.001,
    Microliter: "µL", "uL" => 1e-6,
);

unit_impl!("A unit of time.",
    Time, Second: "s",
    "100 years, each of 365.25 days.", Century: "cen" => 3.15576e9,
    "10 years, each of 365.25 days.", Decade: "dec" => 3.15576e8,
    "A year of 365.25 days. Decades and centuries are defined in terms of this unit.", Year: "yr" => 3.15576e7,
    Week: "wk" => 604800.0,
    Day: "day" => 86400.0,
    Hour: "hr" => 3600.0,
    Minute: "min" => 60.0,
    Second: "s" => 1.0,
    Decisecond: "ds" => 0.1,
    Centisecond: "cs" => 0.01,
    Millisecond: "ms" => 0.001,
    Microsecond: "µs", "us" => 1e-6,
    Nanosecond: "ns" => 1e-9,
    Picosecond: "ps" => 1e-12,
);
