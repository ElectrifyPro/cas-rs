//! Counting functions.

use cas_attrs::builtin;
use crate::{funcs::miscellaneous::partial_factorial, primitive::int};
use rug::Integer;

/// Combinations function.
///
/// The returned value can be interepeted in a number of ways:
///
/// - Returns the number of ways to choose `k` (`r`) items from `n` items, where the order of the
/// items does not matter.
/// - Returns the coefficient of the `x^k` term in the polynomial expansion of `(x + 1)^n`, or the
/// coefficient of the `x^k * y^(n - k)` term in the polynomial expansion of `(x + y)^n`.
/// - Returns the number in row `n` and column `k` of Pascal's triangle.
#[derive(Debug)]
pub struct Ncr;

#[cfg_attr(feature = "numerical", builtin)]
impl Ncr {
    pub fn eval_static(n: Integer, k: Integer) -> Integer {
        if k > n {
            // TODO: what if k > n, return an error
            return Integer::from(0);
        }

        let sub = int(&n - &k);
        if k > sub {
            partial_factorial(n, k) / partial_factorial(sub, int(1))
        } else {
            partial_factorial(n, sub) / partial_factorial(k, int(1))
        }
    }
}

/// Permutations function. Returns the number of ways to choose `k` (`r`) items from `n` items,
/// where the order of the items does matter.
#[derive(Debug)]
pub struct Npr;

#[cfg_attr(feature = "numerical", builtin)]
impl Npr {
    pub fn eval_static(n: Integer, k: Integer) -> Integer {
        // TODO: report error

        let sub = &n - k;
        partial_factorial(n, sub)
    }
}
