//! Utility functions to format floating-point numbers.

use crate::approx::approximate_rational;
use rug::{float::Round, Float};
use std::{cmp::Ordering, fmt::Formatter};
use super::{integer, FormatOptions, NumberFormat, Scientific, Separator};

/// Returns true if the given float is small or large enough that it should be formatted in
/// scientific notation.
pub fn should_use_scientific(n: &Float) -> bool {
    let abs = n.as_abs();
    *abs <= 1e-6 || *abs >= 1e+12
}

/// Trims trailing parts from a string assumed to represent a single number in decimal notation,
/// and attempts to remove minor errors introduced by floating point arithmetic.
fn trim_trailing(s: &str) -> &str {
    // floating point arithmetic can introduce results like this:
    //
    // input: 19829.98
    // output: 19829.98000000000000000000000000000000000000000000000000000000000000000000000001
    //
    // this is a slightly hacky way to fix this, by using only the first several fractional digits,
    // and trimming trailing zeros
    if let Some(index) = s.find('.') {
        // find out how long the fractional part is
        let fractional_part = s[index + 1..].len();

        // only use the first 145 digits of the fractional part, then trim trailing zeros
        s[..index + 1 + fractional_part.min(145)].trim_end_matches('0').trim_end_matches('.')
    } else {
        s
    }
}

/// Formats a float as a standard number.
fn fmt_decimal<F: std::fmt::Write>(f: &mut F, n: &Float, separators: Separator) -> std::fmt::Result {
    if !n.is_normal() {
        if n.is_nan() {
            return write!(f, "NaN");
        } else if n.is_infinite() {
            return write!(f, "{}∞", if n.is_sign_negative() { "-" } else { "" });
        } else if n.is_zero() {
            return write!(f, "0");
        }
    }

    let (sign, mut s, exponent) = n.to_sign_string_exp_round(10, Some(145), Round::Nearest);
    let exponent = exponent.unwrap(); // exponent is Some() if the number is normal

    // add decimal point
    match exponent.cmp(&0) {
        Ordering::Less => s.insert_str(0, &format!("0.{}", "0".repeat(-exponent as usize))),
        Ordering::Equal => s.insert_str(0, "0."),
        Ordering::Greater => {
            let exponent = exponent as usize;
            match s.len().cmp(&exponent) {
                // if there are not enough digits before the decimal point, add zeros
                Ordering::Less => s.push_str(&"0".repeat(exponent - s.len())),

                // place the decimal point in the correct place
                Ordering::Greater => s.insert(exponent , '.'),

                // if len == exponent, the decimal point would be at the end of the string, so we
                // don't add anything
                Ordering::Equal => {},
            }
        },
    }

    if separators == Separator::Always {
        integer::insert_separators(&mut s);
    }
    write!(f, "{}{}", if sign { "-" } else { "" }, trim_trailing(&s))
}

/// Formats a float in scientific notation.
pub fn fmt_scientific(f: &mut Formatter<'_>, n: &Float, scientific_suffix: Scientific) -> std::fmt::Result {
    if !n.is_normal() {
        // separator doesn't matter here because the number is not normal
        return fmt_decimal(f, n, Separator::Never);
    }

    let (sign, mut s, exponent) = n.to_sign_string_exp_round(10, Some(145), Round::Nearest);
    let mut exponent = exponent.unwrap(); // exponent is Some() if the number is normal

    // add decimal point
    s.insert(1, '.');

    // subtract 1 from the exponent because we inserted a decimal point after the first digit
    exponent -= 1;

    // there should be no need to add separators here, because the number is already in scientific
    // notation

    write!(f, "{}{}{}{}",
        if sign { "-" } else { "" },
        trim_trailing(&s),
        match scientific_suffix {
            Scientific::Times => " × 10 ^ ",
            Scientific::E => "E",
        },
        exponent,
    )
}

/// Formats a float as a rational fraction.
fn fmt_fraction(f: &mut Formatter<'_>, n: &Float, options: FormatOptions) -> std::fmt::Result {
    if !n.is_normal() {
        return fmt_decimal(f, n, options.separators);
    } else if n.is_integer() {
        return fmt(f, n, FormatOptions {
            number: NumberFormat::Auto,
            ..options
        });
    }

    let (numerator, denominator) = approximate_rational(n).into_numer_denom();

    // write numerator
    integer::fmt(f, &numerator, FormatOptions {
        number: NumberFormat::Auto,
        ..options
    })?;

    // write fraction bar
    write!(f, " / ")?;

    // write denominator
    let expected_format = if integer::should_use_scientific(&denominator) {
        NumberFormat::Scientific
    } else {
        NumberFormat::Decimal
    };

    if expected_format == NumberFormat::Scientific {
        // put the denominator in parentheses to avoid ambiguity
        write!(f, "(")?;
        integer::fmt_scientific(f, &denominator, options.scientific)?;
        write!(f, ")")?;
    } else {
        integer::fmt_decimal(f, &denominator, options.separators)?;
    }

    Ok(())
}

const DEC_NUM_NAMES: [&str; 30] = [
    "tenth",
    "hundredth",
    "thousandth",
    "ten-thousandth",
    "hundred-thousandth",
    "millionth",
    "ten-millionth",
    "hundred-millionth",
    "billionth",
    "ten-billionth",
    "hundred-billionth",
    "trillionth",
    "ten-trillionth",
    "hundred-trillionth",
    "quintillionth",
    "ten-quintillionth",
    "hundred-quintillionth",
    "sextillionth",
    "ten-sextillionth",
    "hundred-sextillionth",
    "septillionth",
    "ten-septillionth",
    "hundred-septillionth",
    "octillionth",
    "ten-octillionth",
    "hundred-octillionth",
    "nonillionth",
    "ten-nonillionth",
    "hundred-nonillionth",
    "decillionth",
];

/// Formats a float in word form.
///
/// TODO: does anyone actually use this? not even tested
fn fmt_word(f: &mut Formatter<'_>, n: &Float, options: FormatOptions) -> std::fmt::Result {
    if !n.is_normal() {
        return fmt_decimal(f, n, options.separators);
    }

    let mut s = String::new();
    fmt_decimal(&mut s, n, Separator::Never)?;

    let mut parts = s.split('.');
    if let Some(integer) = parts.next() {
        integer::fmt_word_str(f, integer)?;
    }

    if let Some(decimal) = parts.next() {
        write!(f, " and ")?;

        // same packing case as above
        if decimal.len() > DEC_NUM_NAMES.len() {
            integer::fmt_word_str(f, decimal)?;
            write!(f, " 0.{} {}", &decimal[DEC_NUM_NAMES.len()..], DEC_NUM_NAMES.last().unwrap())?;
        } else {
            integer::fmt_word_str(f, decimal)?;
            write!(f, " {}", DEC_NUM_NAMES[decimal.len() - 1])?;
        }

        if decimal != "1" {
            write!(f, "s")?;
        }
    }

    Ok(())
}

/// Format a floating-point number using the given formatting options.
pub fn fmt(f: &mut Formatter<'_>, n: &Float, options: FormatOptions) -> std::fmt::Result {
    match options.number {
        NumberFormat::Auto => {
            if should_use_scientific(n) {
                fmt_scientific(f, n, options.scientific)
            } else {
                fmt_decimal(f, n, options.separators)
            }
        }
        NumberFormat::Decimal => fmt_decimal(f, n, options.separators),
        NumberFormat::Scientific => fmt_scientific(f, n, options.scientific),
        NumberFormat::Fraction => fmt_fraction(f, n, options),
        NumberFormat::Word => fmt_word(f, n, options),
    }
}
