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

    /// `3/12 = 1/4`
    /// `12/3 = 4`
    ReduceFraction,

    /// `a+a = 2a`
    /// `a+a+a = 3a`
    /// `2a+3a = 5a`
    /// etc.
    CombineLikeTerms,

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

    /// `a^b^c = a^(b*c)`
    PowerPower,

    /// `a*(b+c) = a*b + a*c`
    DistributiveProperty,

    /// `(a*b)^c = a^c*b^c`
    DistributePower,

    /// `i^(4n) = 1`
    I0,

    /// `i^(4n+1) = i`
    I1,

    /// `i^(4n+2) = -1`
    I2,

    /// `i^(4n+3) = -i`
    I3,

    /// `sin(x)` identity
    Sin,

    /// `cos(x)` identity
    Cos,

    /// `tan(x)` identity
    Tan,
}
