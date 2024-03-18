//! Utility functions to format integers.

use rug::Integer;
use std::fmt::{Formatter, Write};
use super::{FormatOptions, NumberFormat, Scientific, Separator};

/// Returns true if the given integer is large enough that it should be formatted in scientific
/// notation.
pub fn should_use_scientific(n: &Integer) -> bool {
    *n.as_abs() >= 1_000_000_000_000_i64
}

/// Rounds an integer in a given [`String`] with the maximum number of precision.
///
/// # Panics
///
/// Panics if the string contains any characters other than the ASCII digits (`'0'` - `'9'`).
fn round(mut s: String, max_digits: usize) -> String {
    // ensure all characters are ASCII digits
    // '0' - '9' (digits)
    for (i, byte) in s.as_bytes().iter().copied().enumerate() {
        if !byte.is_ascii_digit() {
            panic!(
                "invalid character in numeric string: '{}' ({}) found at byte position {}",
                byte as char,
                byte,
                i,
            );
        }
    }

    // SAFETY: we ensure the result is valid UTF-8 by only operating on a subset of ASCII
    let bytes = unsafe { s.as_bytes_mut() };

    // `max_digits` is greater than the number of digits in the string, so no rounding is
    // necessary
    if max_digits >= bytes.len() {
        return s;
    }

    let start_idx = max_digits;

    // round the number
    // begin at the "end" of the string (`start_idx`) and round like a human would
    let mut carry = None;

    // for each digit after `start_idx`, add 1 to the digit if the carry bit is set
    // if the digit is 9, set it to 0 and continue to the next digit
    // repeat until the carry bit is unset
    for byte in bytes.iter_mut().take(start_idx + 1).rev() {
        match carry {
            // continue adding 1 to this digit and moving to the next digit if necessary
            Some(true) => match *byte {
                b'9' => *byte = b'0',
                _ => {
                    *byte += 1;
                    carry = Some(false);
                }
            },

            // no more carrying to do
            Some(false) => break,

            // first (rightmost) digit, which is one past the set of digits we keep
            // only use it to determine if we round up or down
            // later, it will be set to '0' below
            None => carry = Some(*byte >= b'5'),
        }
    }

    // write zeroes after `start_idx` to the end of the slice
    // SAFETY: `start_idx` is checked to be a valid index within `bytes`, and we are writing valid
    // UTF-8
    unsafe {
        bytes.as_mut_ptr()
            .add(start_idx)
            .write_bytes(b'0', bytes.len() - start_idx);
    }

    s
}

/// Inserts separators in a string represeting an integer / float.
pub fn insert_separators(s: &mut String) {
    let decimal = s.find('.').unwrap_or(s.len());
    s.reserve(s.len() / 3); // reserve space for the commas

    // go backwards from the decimal point (backwards to avoid having to deal with the string
    // growing in index computation), inserting commas every 3 digits
    let mut i = decimal.saturating_sub(3);
    while i > 0 {
        s.insert(i, ',');
        i = i.saturating_sub(3);
    }
}

/// Formats an integer in decimal notation.
pub fn fmt_decimal(f: &mut Formatter<'_>, n: &Integer, options: FormatOptions) -> std::fmt::Result {
    let mut s = n.to_string_radix(10);
    if let Some(max_digits) = options.precision {
        if max_digits < s.len() {
            // use scientific notation to reduce the number of significant figures
            return fmt_scientific(f, n, options);
        }

        // integer has fewer digits than the maximum allowed precision, nothing else to do
    }
    if options.separators == Separator::Always {
        insert_separators(&mut s);
    }
    write!(f, "{}", s)
}

/// Formats an integer in scientific notation.
pub fn fmt_scientific(f: &mut Formatter<'_>, n: &Integer, options: FormatOptions) -> std::fmt::Result {
    let mut s = n.to_string_radix(10);

    if let Some(max_digits) = options.precision {
        s = round(s, max_digits);
    }

    // locate first non-zero digit, which will be either at index 0 or 1 (if there is a sign)
    let first_non_zero = s.find(|c: char| c.is_ascii_digit() && c != '0').unwrap();

    // count the number of digits after this point, representing the exponent
    let exponent = s.len() - first_non_zero - 1;

    if exponent == 0 {
        // if there are no digits after the first non-zero digit, this is a one-digit number
        // there is nothing to do
        write!(f, "{}", s)
    } else {
        // if there are digits after the first non-zero digit, insert a decimal point after it
        s.insert(first_non_zero + 1, '.');

        // remove trailing zeroes, since there is now a decimal point, making them redundant as
        // they are now part of the fractional part
        // (they could still be significant figures)
        let s = s.trim_end_matches('0');

        // if the user wants to use scientific notation, we need to add an exponent
        match options.scientific {
            Scientific::Times => write!(f, "{} × 10 ^ {}", s, exponent),
            Scientific::E => write!(f, "{}E{}", s, exponent),
        }
    }
}

const INT_NUM_NAMES: [&str; 21] = [
    "thousand",
    "million",
    "billion",
    "trillion",
    "quadrillion",
    "quintillion",
    "sextillion",
    "septillion",
    "octillion",
    "nonillion",
    "decillion",
    "undecillion",
    "duodecillion",
    "tredecillion",
    "quattuordecillion",
    "quindecillion",
    "sexdecillion",
    "septendecillion",
    "octodecillion",
    "novemdecillion",
    "vigintillion",
];

/// Formats a string slice containing an integer, in word form.
pub fn fmt_word_str(f: &mut Formatter<'_>, input: &str) -> std::fmt::Result {
    let chars = input.chars().collect::<Vec<_>>();
    let mut chunks = chars.rchunks(3).rev().enumerate();
    let num_chunks = input.len() / 3 + if input.len() % 3 == 0 { 0 } else { 1 };

    let mut parts = Vec::with_capacity(num_chunks);

    // check if the number is so large that we need to pack the start of the number into a
    // "centillion" (or whatever the largest unit in `INT_NUM_NAMES` is)
    if num_chunks > INT_NUM_NAMES.len() + 1 {
        let mut packed = String::new();
        for packed_chunk in chunks.by_ref().take(num_chunks - INT_NUM_NAMES.len()) {
            packed_chunk.1.iter().for_each(|&c| packed.push(c));
        }
        write!(packed, " {}", INT_NUM_NAMES.last().unwrap())?;
        parts.push(packed);
    }

    // break the integer part into chunks of 3 digits
    // for each chunk, write the chunk, followed by the appropriate name representing its place
    // value
    for (chunk_index, chunk) in chunks {
        let num = chunk.iter().collect::<String>().parse::<u16>().unwrap();
        if num == 0 {
            continue;
        }

        let mut part = String::new();
        write!(part, "{}", num)?;

        // write the name of the place value
        let place_index = (num_chunks - chunk_index).checked_sub(2);
        if let Some(place_index) = place_index {
            if place_index < INT_NUM_NAMES.len() {
                write!(part, " {}", INT_NUM_NAMES[place_index])?;
            }
        }

        parts.push(part);
    }

    write!(f, "{}", parts.join(" "))?;

    Ok(())
}

fn fmt_word(f: &mut Formatter<'_>, n: &Integer, options: FormatOptions) -> std::fmt::Result {
    let mut s = n.to_string_radix(10);
    if let Some(max_digits) = options.precision {
        s = round(s, max_digits);
    }
    fmt_word_str(f, &s)
}

/// Format an integer using the given formatting options.
pub fn fmt(f: &mut Formatter<'_>, n: &Integer, options: FormatOptions) -> std::fmt::Result {
    match options.number {
        NumberFormat::Auto => {
            if should_use_scientific(n) {
                fmt_scientific(f, n, options)
            } else {
                fmt_decimal(f, n, options)
            }
        }
        NumberFormat::Decimal | NumberFormat::Fraction => fmt_decimal(f, n, options),
        NumberFormat::Scientific => fmt_scientific(f, n, options),
        NumberFormat::Word => fmt_word(f, n, options),
    }
}
