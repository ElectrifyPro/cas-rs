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

    /// The maximum number of digits to show. If [`None`], the number is formatted with full
    /// precision.
    ///
    /// This option might be useful when rendering floating-point numbers. Floating-point numbers
    /// in `cas-rs` have significantly higher precision than 32-bit / 64-bit floats; however, they
    /// are still subject to the same issues that can occur when performing floating-point
    /// arithmetic. The common solution to this is to trim off a small number of digits from the
    /// result.
    ///
    /// Also, integers have arbitrary precision and are usually formatted completely. If this
    /// option is set and the integer has too many digits, it will be formatted in scientific
    /// notation as if it were a float.
    ///
    /// This option **does not** control the precision of the number during calculation, only the
    /// number of digits to display during formatting.
    pub precision: Option<usize>,

    /// Whether to display separators for large numbers.
    pub separators: Separator,
}

impl FormatOptions {
    /// Wraps the given [`FormatOptions`] into a builder for further customization.
    pub fn into_builder(self) -> FormatOptionsBuilder {
        FormatOptionsBuilder(self)
    }
}

/// The different ways to format a number.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum NumberFormat {
    /// Chooses between decimal and scientific notation based on the magnitude of the number.
    ///
    /// Numbers that are in the ranges `[-1e-6, 1e-6]` U `[-inf, -1e+12] U [1e+12, inf]` will be
    /// represented in scientific notation, while all other numbers are formatted in decimal
    /// notation.
    ///
    /// This is the default option.
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

impl NumberFormat {
    /// Utility function to create a new [`FormatOptions`] with the same formating options as the
    /// given [`FormatOptions`], but with the number format set to this value.
    pub fn inside(self, options: FormatOptions) -> FormatOptions {
        FormatOptions {
            number: self,
            ..options
        }
    }
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

impl Scientific {
    /// Utility function to create a new [`FormatOptions`] with the same formating options as the
    /// given [`FormatOptions`], but with the scientific notation format set to this value.
    pub fn inside(self, options: FormatOptions) -> FormatOptions {
        FormatOptions {
            scientific: self,
            ..options
        }
    }
}

/// Whether to display separators for large numbers.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum Separator {
    /// Always display separators. For example, the number _one million_ is rendered with commas as
    /// `1,000,000`.
    Always,

    /// Never display separators.
    ///
    /// This is the default option.
    #[default]
    Never,
}

impl Separator {
    /// Utility function to create a new [`FormatOptions`] with the same formating options as the
    /// given [`FormatOptions`], but with the separator set to this value.
    pub fn inside(self, options: FormatOptions) -> FormatOptions {
        FormatOptions {
            separators: self,
            ..options
        }
    }
}

/// Helper struct to build a [`FormatOptions`] struct.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct FormatOptionsBuilder(FormatOptions);

impl FormatOptionsBuilder {
    /// Creates a new builder with the default options.
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the number format. See [`NumberFormat`] for more information.
    pub fn number(mut self, number: NumberFormat) -> Self {
        self.0.number = number;
        self
    }

    /// Sets the scientific notation suffix. See [`Scientific`] for more information.
    pub fn scientific(mut self, scientific: Scientific) -> Self {
        self.0.scientific = scientific;
        self
    }

    /// Sets the maximum number of digits to show. If [`None`], the number is formatted with full
    /// precision. See [`FormatOptions::precision`] for more information.
    pub fn precision(mut self, precision: Option<usize>) -> Self {
        self.0.precision = precision;
        self
    }

    /// Sets whether to display separators for large numbers. See [`Separator`] for more
    /// information.
    pub fn separators(mut self, separators: Separator) -> Self {
        self.0.separators = separators;
        self
    }

    /// Builds the [`FormatOptions`] struct.
    pub fn build(self) -> FormatOptions {
        self.0
    }
}

/// Formatter for a [`Value`].
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
            Value::List(l) => {
                write!(f, "[")?;
                for (i, item) in l.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ValueFormatter {
                        value: item,
                        options: self.options,
                    })?;
                }
                write!(f, "]")
            },
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
        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Decimal)
            .precision(Some(150))
            .build();
        let formatted = format!("{}", float.fmt(opts));

        // this is the exact value
        assert_eq!(
            formatted,
            "166697648439633735919597210805076.6529167300667828951014331365469362133029070327866633033064632426906380900918045096212631206355582001",
        );
    }

    #[test]
    fn highly_precise_decimal_2() {
        let float = eval("2^457 / 10^50");
        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Decimal)
            .precision(Some(150))
            .build();
        let formatted = format!("{}", float.fmt(opts));

        // this is the exact value
        assert_eq!(
            formatted,
            "3721414268393507279612537896386583215890643766719068468641229819804873155140597367430098.17965446945567110411062408283101969716033850703872",
        );
    }

    #[test]
    fn highly_precise_decimal_3() {
        let float = eval("1/sqrt(2)");
        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Decimal)
            .build();
        let formatted = format!("{}", float.fmt(opts));

        assert_eq!(
            formatted,
            "0.707106781186547524400844362104849039284835937688474036588339868995366239231053519425193767163820786367506923115456148512462418027925368606322060748549967929",
        );
    }

    #[test]
    fn scientific_e() {
        let float = eval("1/256!");
        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Scientific)
            .scientific(Scientific::E)
            .build();
        let formatted = format!("{}", float.fmt(opts));

        assert_eq!(
            formatted,
            "1.16574875077673880591679077396436952491792279821826898772971150811137017094408687014393063851764516093648992883092191087289215932890555684983700537703343422E-507",
        );
    }

    #[test]
    fn highly_precise_scientific() {
        let float = eval("124!");
        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Scientific)
            .build();
        let formatted = format!("{}", float.fmt(opts));

        assert_eq!(
            formatted,
            "1.50614174151114087979501416199328068607632291897193940710078585206682525065290879093506346311596738506917124356744046192504129535473104478255106766046837644419461100452005705416704 × 10 ^ 207",
        );
    }

    #[test]
    fn highly_precise_scientific_2() {
        let float = eval("3^1100 / 12^740");
        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Scientific)
            .build();
        let formatted = format!("{}", float.fmt(opts));

        assert_eq!(
            formatted,
            "1.73483476433491726954087186357882339436560631044381296893128856833523778147337032792695573913442881679792533332636074272704505816741397073273213142735271195 × 10 ^ -274",
        );
    }

    #[test]
    fn highly_precise_complex() {
        let complex = eval("1/128! - 1/256! * i");
        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Scientific)
            .build();
        let formatted = format!("{}", complex.fmt(opts));

        assert_eq!(
            formatted,
            "2.59322324860261966289150489187302066591294650436810102067533652743955265303530160852333720511812211606173647225958129384019175298482864208811149268313933175 × 10 ^ -216 - (1.16574875077673880591679077396436952491792279821826898772971150811137017094408687014393063851764516093648992883092191087289215932890555684983700537703343422 × 10 ^ -507)i"
        );
    }

    #[test]
    fn complex_imaginary_part() {
        let complex = eval("sqrt(-4)");
        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Decimal)
            .build();
        let formatted = format!("{}", complex.fmt(opts));

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

        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Decimal)
            .build();

        for (complex, output) in complexes.iter().zip(outputs.iter()) {
            let formatted = format!("{}", complex.fmt(opts));
            assert_eq!(formatted, *output);
        }
    }

    #[test]
    fn trailing_zeroes() {
        let float = eval("37000000.");
        let opts = FormatOptionsBuilder::new()
            .separators(Separator::Always)
            .build();
        let formatted = format!("{}", float.fmt(opts));

        assert_eq!(formatted, "37,000,000");
    }

    #[test]
    fn trailing_zeroes_2() {
        let float = eval("1400.0010");
        let opts = FormatOptionsBuilder::new()
            .precision(Some(10))
            .separators(Separator::Always)
            .build();
        let formatted = format!("{}", float.fmt(opts));

        assert_eq!(formatted, "1,400.001");
    }
}
