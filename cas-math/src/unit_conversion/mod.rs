//! A module for generic conversion between units.

pub mod unit;

use unit::Unit;

/// A value and the unit it represents.
///
/// This value can be converted to other units within the same quantity kind.
pub struct Measurement<T> {
    value: T,
    unit: Unit,
}

impl<T> Measurement<T> {
    /// Create a new measurement.
    pub fn new(value: T, unit: Unit) -> Self {
        Self { value, unit }
    }

    /// Get the value of this measurement.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Get the unit of this measurement.
    pub fn unit(&self) -> &Unit {
        &self.unit
    }

    /// Convert this measurement to another unit.
    ///
    /// In general, target units must be the same kind as the source unit, and with the same
    /// power. However, some conversions are allowed between different kinds of units, such as
    /// between cubed length units and volume units.
    pub fn convert(&self, target: Unit) -> Self {
        todo!()
    }
}
