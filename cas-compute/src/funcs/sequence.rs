//! Functions to determine the specified term of particular sequences.

use cas_attrs::builtin;
use crate::primitive::int;
use rug::Integer;

/// Returns the `n`th term of the Fibonacci sequence, using a fast doubling algorithm with `O(log
/// n)` complexity, extended to support negative indices.
///
/// The implementation considers `fib(1) = fib(2) = 1`.
#[derive(Debug)]
pub struct Fib;

#[cfg_attr(feature = "numerical", builtin)]
impl Fib {
    pub fn eval_static(n: Integer) -> Integer {
        let is_negative = n < 0;
        let is_even = n.is_even();

        let mut stack = vec![n.abs()];
        while let Some(last) = stack.last() {
            if *last > 1 {
                stack.push(int(last) / 2);
            } else {
                break;
            }
        }

        let (mut a, mut b) = (int(0), int(1));
        while let Some(next) = stack.pop() {
            let c = (int(2) * &b - &a) * &a;
            let d = int(&a * &a) + int(&b * &b);
            if next.is_even() {
                (a, b) = (c, d);
            } else {
                (b, a) = (c + &d, d);
            }
        }

        if is_negative && is_even {
            -a
        } else {
            a
        }
    }
}
