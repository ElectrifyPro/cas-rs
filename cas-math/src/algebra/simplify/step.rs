/// Possible simplification steps.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Step {
    /// `a^0 = 1`
    PowerZero,

    /// `0^a = 0`
    PowerZeroLeft,

    /// `1^a = 1`
    PowerOneLeft,

    /// `a^1 = a`
    PowerOne,
}
