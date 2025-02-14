//! Provides the [`Convert`] trait, which is implemented by all [`Unit`]s.

use super::unit::Unit;

/// A trait implemented by all [`Unit`], providing the information needed to convert between them.
pub trait Convert {
    /// The base unit of this unit.
    ///
    /// The base unit is the unit in which conversions to other units of the same quantity are
    /// defined. For example, the base unit of length is the meter, and the base unit of volume
    /// is the cubic meter. It isn't particularly important which unit is chosen as the base
    /// unit, as long as conversions between each unit and the base unit are correctly defined.
    ///
    /// In some cases, the choice of base unit may affect the precision of conversions. For
    /// example, metric and customary units often don't have exact conversions between them. If the
    /// base unit is chosen to be a metric unit, then conversions between customary units will
    /// be done by converting to the metric base unit, and then to the target customary unit, which
    /// is where the precision loss can occur.
    const BASE: Self;

    /// Returns the conversion factor from `&self` to [`Convert::BASE`], i.e. the value to multiply
    /// a quantity in this unit by, in order to get a quantity in [`Convert::BASE`]. If the `self`
    /// unit is the same as this unit, then this function should return `1.0`.
    ///
    /// For example, if [`Convert::BASE`] is the meter, the conversion factor for a centimeter
    /// would be `0.01`.
    fn conversion_factor(&self) -> f64;

    /// Defines the conversion factor from [`Convert::BASE`], to a base unit that [`Convert::BASE`]
    /// is derived from. Returns [`None`] if there is no conversion factor, meaning the two units
    /// are unrelated.
    ///
    /// For example, [`Volume`] is derived from [`Length`] units cubed. The base unit for
    /// [`Length`] is defined to be [`Length::Meter`]. So, this function should be manually
    /// implemented for [`Volume`], and it should return the value to multiply by to convert from
    /// `Volume::?` to cubic meters.
    ///
    /// [`Volume`]: super::Volume
    /// [`Length`]: super::Length
    /// [`Length::Meter`]: super::Length::Meter
    fn conversion_factor_to(&self, _: impl Into<Unit>) -> Option<f64> {
        None
    }
}
