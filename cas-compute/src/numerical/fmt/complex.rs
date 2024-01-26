//! Utility functions to format complex numbers.

use rug::{Complex, Float};
use std::fmt::Formatter;
use super::{float, FormatOptions, NumberFormat};

/// Helper function to format the imaginary part of the complex number, with or without
/// parentheses.
fn fmt_helper(f: &mut Formatter<'_>, n: &Float, options: FormatOptions) -> std::fmt::Result {
    if n == &1 {
        return write!(f, "i");
    } else if n == &-1 {
        return write!(f, "-i");
    }

    // if we need to format this part in scientific notation, we need to add parentheses
    // around it to avoid ambiguity
    // we need parentheses if the user specifies scientific notation, or if
    // `NumberFormat::Auto` decides it's time
    if options.number == NumberFormat::Scientific
        || options.number == NumberFormat::Auto && float::should_use_scientific(n)
    {
        write!(f, "(")?;
        float::fmt_scientific(f, n, options.scientific)?;
        write!(f, ")")?;
    } else {
        float::fmt(f, n, options)?;
    }

    write!(f, "i")
}

/// Formats a complex number.
pub fn fmt(f: &mut Formatter<'_>, c: &Complex, options: FormatOptions) -> std::fmt::Result {
    let (re, im) = (c.real(), c.imag());

    // for the standard notation, real part comes first, then imaginary
    // four possible combinations:
    // 1. real and imaginary exist (i.e. are non-zero)
    // 2. only real exists
    // 3. only imaginary exists
    // 4. neither real nor imaginary exist (i.e. zero)
    match (re.is_zero(), im.is_zero()) {
        (false, false) => {
            // write real part
            float::fmt(f, re, options)?;

            // write imaginary part
            if im.is_sign_positive() {
                write!(f, " + ")?;
                fmt_helper(f, im, options)?;
            } else {
                write!(f, " - ")?;
                fmt_helper(f, &im.as_neg(), options)?;
            }

            Ok(())
        },
        (false, true) => float::fmt(f, re, options),
        (true, false) => fmt_helper(f, im, options),
        (true, true) => write!(f, "0"),
    }
}
