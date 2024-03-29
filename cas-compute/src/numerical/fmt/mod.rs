mod complex;
mod float;
mod integer;

use crate::primitive::float;
use std::fmt::{Display, Formatter};
use super::value::Value;

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
            Value::Float(n) => float::fmt(f, n, self.options),
            Value::Integer(n) => integer::fmt(f, n, self.options),
            Value::Complex(c) => complex::fmt(f, c, self.options),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
        }
    }
}

#[cfg(test)]
mod tests {
    use cas_parser::parser::{ast::Expr, Parser};
    use crate::numerical::eval::Eval;

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
    fn highly_precise_decimal_3() {
        let float = eval("1/sqrt(2)");
        let formatted = format!("{}", float.fmt(FormatOptions {
            number: NumberFormat::Decimal,
            ..Default::default()
        }));

        assert_eq!(
            formatted,
            "0.7071067811865475244008443621048490392848359376884740365883398689953662392310535194251937671638207863675069231154561485124624180279253686063220607",
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
            "1.506141741511140879795014161993280686076322918971939407100785852066825250652908790935063463115967385069171243567440461925041295354731044782551067660468376444194611004520057054167040000000000000000000000000000 × 10 ^ 207",
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

    #[test]
    fn highly_precise_complex() {
        let complex = eval("1/128! - 1/256! * i");
        let formatted = format!("{}", complex.fmt(FormatOptions {
            number: NumberFormat::Scientific,
            ..Default::default()
        }));

        assert_eq!(
            formatted,
            "2.593223248602619662891504891873020665912946504368101020675336527439552653035301608523337205118122116061736472259581293840191752984828642088111493 × 10 ^ -216 - (1.165748750776738805916790773964369524917922798218268987729711508111370170944086870143930638517645160936489928830921910872892159328905556849837005 × 10 ^ -507)i",
        );
    }

    #[test]
    fn complex_imaginary_part() {
        let complex = eval("sqrt(-4)");
        let formatted = format!("{}", complex.fmt(FormatOptions {
            number: NumberFormat::Decimal,
            ..Default::default()
        }));

        assert_eq!(formatted, "2i");
    }

    #[test]
    fn complex_imaginary_edge() {
        let complexes = [
            eval("1 + 0i"),
            eval("3 - i"),
            eval("1 - 2i"),
            eval("i + i - i + 6"),
            eval("i"),
            eval("-i"),
        ];
        let outputs = [
            "1",
            "3 - i",
            "1 - 2i",
            "6 + i",
            "i",
            "-i",
        ];

        for (complex, output) in complexes.iter().zip(outputs.iter()) {
            let formatted = format!("{}", complex.fmt(FormatOptions {
                number: NumberFormat::Decimal,
                ..Default::default()
            }));

            assert_eq!(formatted, *output);
        }
    }
}
