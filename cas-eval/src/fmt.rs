use rug::{Assign, Complex, Float, Integer, Rational, float::Round};
use std::{cmp::Ordering, fmt::{Display, Formatter, Write}};
use super::{consts::float, value::Value};

/// Formatting options for values.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct FormatOptions {
    /// How to format a number.
    pub number: NumberFormat,

    /// Which suffix notation to use for scientific notation.
    ///
    /// This option is ignored if [`number`] is not [`NumberFormat::Scientific`].
    ///
    /// [`number`]: FormatOptions::number
    pub scientific: Scientific,

    /// Whether to display separators for large numbers.
    pub separators: Separator,
}

/// The different ways to format a number.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum NumberFormat {
    /// Chooses between decimal and scientific notation based on the magnitude of the number.
    ///
    /// Numbers that are in the ranges `[-1e-6, 1e-6]` and `[-inf, -1e+12] U [1e+12, inf]` are
    /// scientific notation, while all other numbers are formatted in decimal notation.
    #[default]
    Auto,

    /// Formats the number as a decimal.
    ///
    /// NOTE: This option is not available in CalcBot, as it can easily result in errors or cause
    /// the output to be too long to be displayed. The `Auto` option should be used instead.
    Decimal,

    /// Formats the number in scientific notation.
    ///
    /// The formatting of this option can be further customized using the [`scientific`] option in
    /// the [`FormatOptions`] struct.
    ///
    /// [`scientific`]: FormatOptions::scientific
    Scientific,

    /// Formats the number as a fraction.
    ///
    /// When using this option, formatting of the numerator and denominator is delegated to
    /// [`format_float`] with [`NumberFormat::Auto`] set.
    Fraction,

    /// Formats the number in word form (e.g. "one", "two", "three").
    Word,
}

/// The different ways to format the suffix of scientific notation.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum Scientific {
    /// Formats the number in scientific notation, using the suffix `× 10^` to denote the exponent.
    ///
    /// This formatting option includes a non-ASCII `×` character. Use the [`Scientific::E`] option
    /// to format the number in the popular `E` notation, which can be more easily parseable.
    ///
    /// This is the default option.
    #[default]
    Times,

    /// Formats the number in scientific notation, using `E` notation to denote the exponent.
    ///
    /// `E` is used instead of `e` to avoid ambiguity with Euler's number.
    E,
}

/// Whether to display separators for large numbers.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum Separator {
    /// Always display separators.
    Always,

    /// Never display separators.
    #[default]
    Never,
}

/// Formatter for a value.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ValueFormatter<'a> {
    /// The value to format.
    pub value: &'a Value,

    /// The options to use when formatting.
    pub options: FormatOptions,
}

impl Display for ValueFormatter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::Number(n) => format_float(f, n, self.options),
            Value::Complex(c) => format_complex(f, c, self.options),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
        }
    }
}

/// Returns true if the given value is small or large enough that it should be formatted in
/// scientific notation.
fn should_use_scientific<T>(n: T) -> bool
where
    Float: Assign<T>,
{
    let n = float(n);
    n <= 1e-6 || n >= 1e+12
}

/// Inserts separators in a string assumed to represent a single number in decimal / scientific
/// notation.
fn insert_separators(s: &mut String) {
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

/// Trims trailing zeroes from a string assumed to represent a single number in binary notation.
pub(crate) fn trim_trailing_zeroes(s: &str) -> &str {
    s.rfind(|c| c != '0')
        .map(|i| &s[..=i])
        .unwrap_or(s)
        .trim_end_matches('.')
}

/// Formats a float as a standard number.
fn format_decimal<F: std::fmt::Write>(f: &mut F, n: &Float, separators: Separator) -> std::fmt::Result {
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
            if s.len() < exponent {
                // if there are not enough digits before the decimal point, add zeros
                s.push_str(&format!("{}", "0".repeat(exponent - s.len())));
            } else if s.len() > exponent {
                // place the decimal point in the correct place
                s.insert(exponent, '.');
            }

            // if len == exponent, the decimal point would be at the end of the string, so we don't
            // add anything
        },
    }

    if separators == Separator::Always {
        insert_separators(&mut s);
    }
    write!(f, "{}{}", if sign { "-" } else { "" }, trim_trailing(&s))
}

/// Formats a float in scientific notation.
fn format_scientific(f: &mut Formatter<'_>, n: &Float, scientific_suffix: Scientific) -> std::fmt::Result {
    if !n.is_normal() {
        // separator doesn't matter here because the number is not normal
        return format_decimal(f, n, Separator::Never);
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

        if fractional.is_zero() || error < float(1e-60) {
            break;
        }

        n = fractional.recip();
    }

    rational_from_continued_fraction(&continued_fraction_form)
}

/// Formats a float as a rational fraction.
fn format_fraction(f: &mut Formatter<'_>, n: &Float, options: FormatOptions) -> std::fmt::Result {
    if !n.is_normal() {
        return format_decimal(f, n, options.separators);
    } else if n.is_integer() {
        return format_float(f, n, FormatOptions {
            number: NumberFormat::Auto,
            ..options
        });
    }

    let (numerator, denominator) = approximate_rational(n).into_numer_denom();

    // write numerator
    format_float(f, &float(numerator), FormatOptions {
        number: NumberFormat::Auto,
        ..options
    })?;

    // write fraction bar
    write!(f, " / ")?;

    // write denominator
    let expected_format = if should_use_scientific(&denominator) {
        NumberFormat::Scientific
    } else {
        NumberFormat::Decimal
    };

    if expected_format == NumberFormat::Scientific {
        // put the denominator in parentheses to avoid ambiguity
        write!(f, "(")?;
        format_scientific(f, &float(denominator), options.scientific)?;
        write!(f, ")")?;
    } else {
        format_decimal(f, &float(denominator), options.separators)?;
    }

    Ok(())
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
fn format_word(f: &mut Formatter<'_>, n: &Float, options: FormatOptions) -> std::fmt::Result {
    if !n.is_normal() {
        return format_decimal(f, n, options.separators);
    }

    let mut s = String::new();
    format_decimal(&mut s, n, Separator::Never)?;

    /// Formats an integer given as a string in word form.
    fn format_word_inner(f: &mut Formatter<'_>, input: &str) -> std::fmt::Result {
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

    let mut parts = s.split('.');
    if let Some(integer) = parts.next() {
        format_word_inner(f, integer)?;
    }

    if let Some(decimal) = parts.next() {
        write!(f, " and ")?;

        // same packing case as above
        if decimal.len() > DEC_NUM_NAMES.len() {
            format_word_inner(f, decimal)?;
            write!(f, " 0.{} {}", &decimal[DEC_NUM_NAMES.len()..], DEC_NUM_NAMES.last().unwrap())?;
        } else {
            format_word_inner(f, decimal)?;
            write!(f, " {}", DEC_NUM_NAMES[decimal.len() - 1])?;
        }

        if decimal != "1" {
            write!(f, "s")?;
        }
    }

    Ok(())
}

/// Format a float.
fn format_float(f: &mut Formatter<'_>, n: &Float, options: FormatOptions) -> std::fmt::Result {
    match options.number {
        NumberFormat::Auto => {
            if should_use_scientific(n.abs_ref()) {
                format_scientific(f, n, options.scientific)
            } else {
                format_decimal(f, n, options.separators)
            }
        }
        NumberFormat::Decimal => format_decimal(f, n, options.separators),
        NumberFormat::Scientific => format_scientific(f, n, options.scientific),
        NumberFormat::Fraction => format_fraction(f, n, options),
        NumberFormat::Word => format_word(f, n, options),
    }
}

/// Formats a complex number.
fn format_complex(f: &mut Formatter<'_>, c: &Complex, options: FormatOptions) -> std::fmt::Result {
    if c.is_zero() {
        return write!(f, "0");
    }

    let (re, im) = (c.real(), c.imag());

    // write the imaginary part first
    if im.is_zero() {
        return format_float(f, re, options);
    } else if im.eq(&1.0) {
        write!(f, "i")?;
    } else if im.eq(&-1.0) {
        write!(f, "-i")?;
    } else {
        // if we need to format this part in scientific notation, we need to add parentheses
        // around it to avoid ambiguity
        if options.number == NumberFormat::Scientific
            || options.number == NumberFormat::Auto && should_use_scientific(im.abs_ref())
        {
            write!(f, "(")?;
            format_scientific(f, im, options.scientific)?;
            write!(f, ")")?;
        } else {
            format_float(f, im, options)?;
        }
        write!(f, "i")?;
    }

    // write the real part
    if re.is_zero() {
        // the imaginary part was the only part and has already been written
        return Ok(());
    } else {
        // even if scientific notation is used here, it will be unambiguous, as the real part is not
        // multiplied by an imaginary unit
        if re.is_sign_positive() {
            write!(f, " + ")?;
            format_float(f, re, options)?;
        } else {
            write!(f, " - ")?;
            format_float(f, &re.as_neg(), options)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use cas_parser::parser::{ast::Expr, Parser};
    use crate::eval::Eval;

    use super::*;

    /// Evaluate the given expression and return the result.
    fn eval(expr: &str) -> Value {
        let expr = Parser::new(expr).try_parse_full::<Expr>().unwrap();
        expr.eval(&mut Default::default()).unwrap()
    }

    #[test]
    fn highly_precise_decimal() {
        let float = eval("2.1 ^ 100");
        let formatted = format!("{}", float.fmt(FormatOptions {
            number: NumberFormat::Decimal,
            ..Default::default()
        }));

        // this is the exact value
        assert_eq!(
            formatted,
            "166697648439633735919597210805076.6529167300667828951014331365469362133029070327866633033064632426906380900918045096212631206355582001",
        );
    }

    #[test]
    fn highly_precise_decimal_2() {
        let float = eval("2^457 / 10^50");
        let formatted = format!("{}", float.fmt(FormatOptions {
            number: NumberFormat::Decimal,
            ..Default::default()
        }));

        // this is the exact value
        assert_eq!(
            formatted,
            "3721414268393507279612537896386583215890643766719068468641229819804873155140597367430098.17965446945567110411062408283101969716033850703872",
        );
    }

    #[test]
    fn scientific_e() {
        let float = eval("1/256!");
        let formatted = format!("{}", float.fmt(FormatOptions {
            number: NumberFormat::Scientific,
            scientific: Scientific::E,
            ..Default::default()
        }));

        assert_eq!(
            formatted,
            "1.165748750776738805916790773964369524917922798218268987729711508111370170944086870143930638517645160936489928830921910872892159328905556849837005E-507",
        );
    }

    #[test]
    fn highly_precise_scientific() {
        let float = eval("124!");
        let formatted = format!("{}", float.fmt(FormatOptions {
            number: NumberFormat::Scientific,
            ..Default::default()
        }));

        assert_eq!(
            formatted,
            "1.506141741511140879795014161993280686076322918971939407100785852066825250652908790935063463115967385069171243567440461925041295354731044782551068 × 10 ^ 207",
        );
    }

    #[test]
    fn highly_precise_scientific_2() {
        let float = eval("3^1100 / 12^740");
        let formatted = format!("{}", float.fmt(FormatOptions {
            number: NumberFormat::Scientific,
            ..Default::default()
        }));

        assert_eq!(
            formatted,
            "1.734834764334917269540871863578823394365606310443812968931288568335237781473370327926955739134428816797925333326360742727045058167413970732732131 × 10 ^ -274",
        );
    }
}
