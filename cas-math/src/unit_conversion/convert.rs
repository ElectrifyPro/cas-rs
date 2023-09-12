/// A trait implemented by all units, providing the information needed to convert between them.
pub trait Convert {
    /// The base unit of this unit.
    ///
    /// The base unit is the unit in which conversions to other units of the same quantity are
    /// defined. For example, the base unit of length is the meter, and the base unit of volume is
    /// the cubic meter. It isn't particularly important which unit is chosen as the base unit, as
    /// long as conversions between each unit and the base unit are correctly defined.
    const BASE: Self;

    /// Returns the conversion factor from this unit to the base unit, i.e. the value to multiply a
    /// quantity in this unit by, in order to get a quantity in the base unit. If the base unit is
    /// the same as this unit, then this function should return `1.0`.
    ///
    /// For example, if the base unit is the meter, the conversion factor for a centimeter would be
    /// `0.01`.
    fn conversion_factor(&self) -> f64;
}
