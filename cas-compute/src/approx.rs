use rug::{Float, Integer, Rational};
use std::cmp::Ordering;
use super::primitive::float;

/// Computes the [`Rational`] from the continued fraction form of a float.
fn rational_from_continued_fraction(continued_fraction_form: &[Integer]) -> Rational {
    let mut rational = Rational::new();
    for (i, integer) in continued_fraction_form.iter().rev().enumerate() {
        if i == 0 {
            if integer.cmp0() == Ordering::Equal {
                continue;
            }
            rational += Rational::from(Rational::ONE / integer);
        } else {
            rational = (rational + integer).recip();
        }
    }

    if rational.cmp0() == Ordering::Equal {
        rational
    } else {
        rational.recip()
    }
}

/// Approximates the given float as a rational fraction.
///
/// This function applies the continued fraction algorithm to the given float until the error is
/// less than `1e-60`.
///
/// We don't use [`Float::to_rational`] because it can produce bad / useless results due to
/// floating point arithmetic errors. Rather, we use the continued fraction algorithm to compute
/// the rational approximation ourselves.
///
/// See
/// [Wikipedia](https://en.wikipedia.org/wiki/Continued_fraction#Calculating_continued_fraction_representations)
/// for more information.
pub fn approximate_rational(n: &Float) -> Rational {
    let orig = n;

    let mut continued_fraction_form = Vec::new();
    let mut n = n.clone();
    loop {
        let (integer, fractional) = n.trunc_fract(float(0));
        continued_fraction_form.push(integer.to_integer().unwrap());

        // check how close we are to the original number
        let rational = rational_from_continued_fraction(&continued_fraction_form);
        let error = float(orig - rational).abs();

        if fractional.is_zero() || error < 1e-60 {
            break;
        }

        n = fractional.recip();
    }

    rational_from_continued_fraction(&continued_fraction_form)
}
