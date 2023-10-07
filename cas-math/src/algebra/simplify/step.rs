/// Possible simplification steps.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Step {
    /// `0+a = a`
    /// `a+0 = a`
    AddZero,

    /// `0*a = 0`
    /// `a*0 = 0`
    MultiplyZero,

    /// `1*a = a`
    /// `a*1 = a`
    MultiplyOne,

    /// `a*a = a^2`
    /// `a*a*a = a^3`
    /// `a^2*a^3 = a^5`
    /// etc.
    CombineLikeFactors,

    /// `a^0 = 1`
    PowerZero,

    /// `0^a = 0`
    PowerZeroLeft,

    /// `1^a = 1`
    PowerOneLeft,

    /// `a^1 = a`
    PowerOne,
}
