#![doc = include_str!("../README.md")]
//!
//! [`assert_float_eq`]: https://crates.io/crates/assert_float_eq
//! [`approx`]: https://crates.io/crates/approx

pub mod convert;
pub mod unit;

use std::ops::Mul;
pub use unit::{Area, ConversionError, CompoundUnit, Length, Mass, Base, Time, Unit, Volume};

/// A value and the unit it represents.
pub struct Measurement<T> {
    value: T,
    unit: CompoundUnit,
}

impl<T> Measurement<T> {
    /// Create a new measurement.
    pub fn new(value: T, unit: impl Into<CompoundUnit>) -> Self {
        Self { value, unit: unit.into() }
    }

    /// Get the value of this measurement.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Get the unit of this measurement.
    pub fn unit(&self) -> &CompoundUnit {
        &self.unit
    }

    /// Convert this measurement to another unit. Returns [`Err`] if the conversion is not
    /// possible.
    pub fn convert(&self, target: impl Into<CompoundUnit>) -> Result<Self, ConversionError>
        where T: Copy + Mul<f64, Output = T>,
    {
        let target = target.into();
        Ok(Self {
            value: self.value * self.unit.conversion_factor(&target)?,
            unit: target,
        })
    }
}

#[cfg(test)]
mod tests {
    use assert_float_eq::{
        afe_abs,
        afe_relative_error_msg,
        afe_is_relative_eq,
        assert_float_relative_eq,
    };
    use super::*;
    use unit::{Area, Length, Mass, Time, Volume};

    #[test]
    fn parse_unit() {
        let unit: Unit = "µm^3".parse().unwrap();
        assert_eq!(unit, Length::Micrometer.cubed());
    }

    #[test]
    fn complex() {
        let ml: Unit = "mL".parse().unwrap();
        let cm_cubed: Unit = "cm^3".parse().unwrap();
        let m: Measurement<f64> = Measurement::new(500.0, ml);
        let m2 = m.convert(cm_cubed).unwrap();
        assert_float_relative_eq!(*m2.value(), 500.0);
    }

    #[test]
    fn identity_length() {
        let m = Measurement::new(2.0, Length::Yard);
        let m2 = m.convert(Length::Yard).unwrap();
        assert_float_relative_eq!(*m2.value(), 2.0);
    }

    #[test]
    fn convert_length() {
        let m = Measurement::new(2.0, Length::Mile);
        let m2 = m.convert(Length::Decimeter).unwrap();
        assert_float_relative_eq!(*m2.value(), 32186.88);
    }

    #[test]
    fn identity_mass() {
        let m = Measurement::new(1690.0, Mass::Kilogram);
        let m2 = m.convert(Mass::Kilogram).unwrap();
        assert_float_relative_eq!(*m2.value(), 1690.0);
    }

    #[test]
    fn convert_mass() {
        let m = Measurement::new(37.0, Mass::Kilogram);
        let m2 = m.convert(Mass::Pound).unwrap();
        assert_float_relative_eq!(*m2.value(), 81.571);
    }

    #[test]
    fn convert_area_as_length() {
        let m = Measurement::new(2.0, Length::Meter.squared());
        let m2 = m.convert(Length::Yard.squared()).unwrap();
        assert_float_relative_eq!(*m2.value(), 2.39198);
    }

    #[test]
    fn identity_area() {
        let m = Measurement::new(11.45, Area::Acre);
        let m2 = m.convert(Area::Acre).unwrap();
        assert_float_relative_eq!(*m2.value(), 11.45);
    }

    #[test]
    fn convert_len_sq_to_area() {
        let m = Measurement::new(11.45, Length::Meter.squared());
        let m2 = m.convert(Area::Hectare).unwrap();
        assert_float_relative_eq!(*m2.value(), 1.145e-3);
    }

    #[test]
    fn convert_area_to_len_sq() {
        let m = Measurement::new(11.45, Area::Acre);
        let m2 = m.convert(Length::NauticalMile.squared()).unwrap();
        assert_float_relative_eq!(*m2.value(), 1.3509563543609384e-2);
    }

    #[test]
    fn identity_volume() {
        let m = Measurement::new(25.25, Volume::Bushel);
        let m2 = m.convert(Volume::Bushel).unwrap();
        assert_float_relative_eq!(*m2.value(), 25.25);
    }

    #[test]
    fn convert_volume() {
        let m = Measurement::new(25.25, Volume::Bushel);
        let m2 = m.convert(Volume::Liter).unwrap();
        assert_float_relative_eq!(*m2.value(), 889.7865276);
    }

    #[test]
    fn convert_len_cube_to_volume() {
        let m = Measurement::new(56.0, Length::Inch.cubed());
        let m2 = m.convert(Volume::Milliliter).unwrap();
        assert_float_relative_eq!(*m2.value(), 917.676);
    }

    #[test]
    fn convert_volume_to_len_cube() {
        let m = Measurement::new(505.0, Volume::Kiloliter);
        let m2 = m.convert(Length::Foot.cubed()).unwrap();
        assert_float_relative_eq!(*m2.value(), 17833.9);
    }

    #[test]
    fn identity_time() {
        let m = Measurement::new(38.66, Time::Decade);
        let m2 = m.convert(Time::Decade).unwrap();
        assert_float_relative_eq!(*m2.value(), 38.66);
    }

    #[test]
    fn convert_time() {
        let m = Measurement::new(38.66, Time::Decade);
        let m2 = m.convert(Time::Decisecond).unwrap();
        assert_float_relative_eq!(*m2.value(), 1.220016816e11);
    }
}
