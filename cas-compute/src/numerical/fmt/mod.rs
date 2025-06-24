mod complex;
mod float;
mod integer;

use cas_parser::parser::ast::range::RangeKind;
use crate::primitive::float;
use std::{collections::{HashMap, HashSet}, fmt::{Display, Formatter}};
use super::{func::Function, value::Value};

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

    /// Whether to print addresses of reference types, such as lists.
    ///
    /// This is useful for debugging, but can be confusing to an uninitiated user.
    ///
    /// Regardless of this option's configuration, circular references will always be caught and
    /// displayed as `<circular> [...]` (or `<circular ref XX [...]`) to avoid infinite recursion.
    /// This behavior requires an allocation in the [`Display`] implementation of
    /// [`ValueFormatter`] in order to track reference cycles.
    pub show_refs: ShowRefs,
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
    /// The number is expressed in decimal notation with no regard to the magnitude of the number.
    //
    // NOTE: This option is not available in CalcBot, as it can easily result in errors or cause
    // the output to be too long to be displayed. The `Auto` option should be used instead.
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
    /// When using this option, the numerator and denominator are each formatted individually as if
    /// [`NumberFormat::Auto`] was used.
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

/// Whether to print reference IDs of reference types, such as lists.
///
/// Regardless of this option's configuration, circular references will always be caught and
/// displayed as `<circular> [...]` (or `<circular ref XX [...]`) to avoid infinite recursion. This
/// behavior requires an allocation in the [`Display`] implementation of [`ValueFormatter`] in
/// order to track reference cycles.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum ShowRefs {
    /// Always print reference IDs of reference types. This is useful for debugging, but can be
    /// confusing to an uninitiated user.
    ///
    /// This is the default option.
    ///
    /// Note: This option requires an allocation in the [`Display`] implementation of
    /// [`ValueFormatter`] in order to track reference cycles
    #[default]
    Always,

    /// Never print reference IDs of reference types.
    Never,
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

    /// Sets whether to print reference IDs of reference types, such as lists. See [`ShowRefs`] for
    /// more information.
    pub fn show_refs(mut self, show_refs: ShowRefs) -> Self {
        self.0.show_refs = show_refs;
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

impl<'a> ValueFormatter<'a> {
    /// Formats the value, keeping track of reference cycles. Does not allocate if there are no
    /// reference types contained within the value.
    fn fmt_inner(
        &self,
        f: &mut Formatter<'_>,
        // we can get around `&Vec<Value>` requiring `Eq` and `Hash` to be used in a hashmap by
        // using `*const` ptrs instead
        cycles: &mut HashMap<*const Vec<Value>, usize>,
        stack: &mut HashSet<*const Vec<Value>>,
    ) -> std::fmt::Result {
        match self.value {
            Value::Float(n) => float::fmt(f, n, self.options),
            Value::Integer(n) => integer::fmt(f, n, self.options),
            Value::Complex(c) => complex::fmt(f, c, self.options),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
            Value::List(l) => {
                if stack.contains(&(l.as_ptr() as *const _)) {
                    let ref_id = cycles.get(&(l.as_ptr() as *const _)).unwrap();
                    if self.options.show_refs == ShowRefs::Always {
                        write!(f, "<circular ref {}> [...]", ref_id)?;
                    } else {
                        write!(f, "<circular> [...]")?;
                    }
                    return Ok(());
                }

                let prev_len = cycles.len();
                let ref_id = cycles.entry(l.as_ptr())
                    .or_insert(prev_len + 1);
                stack.insert(l.as_ptr());

                if self.options.show_refs == ShowRefs::Always {
                    write!(f, "<ref {}> [", ref_id)?;
                } else {
                    write!(f, "[")?;
                }
                for (i, item) in l.borrow().iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    ValueFormatter {
                        value: item,
                        options: self.options,
                    }.fmt_inner(f, cycles, stack)?;
                }

                write!(f, "]")?;
                stack.remove(&(l.as_ptr() as *const _));

                Ok(())
            },
            Value::Range(lhs, kind, rhs) => {
                ValueFormatter {
                    value: lhs,
                    options: self.options,
                }.fmt_inner(f, cycles, stack)?;
                match kind {
                    RangeKind::HalfOpen => write!(f, " .. ")?,
                    RangeKind::Closed => write!(f, " ..= ")?,
                }
                ValueFormatter {
                    value: rhs,
                    options: self.options,
                }.fmt_inner(f, cycles, stack)
            },
            Value::Function(kind) => match kind {
                Function::User(_) => write!(f, "<function>"),
                Function::Builtin(builtin) => write!(f, "<builtin function: {}>", builtin.name()),
            },
        }
    }
}

impl Display for ValueFormatter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.fmt_inner(
            f,
            &mut HashMap::new(),
            &mut HashSet::new(),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::primitive::{complex, float_from_str, int};
    use rug::{ops::Pow, Integer};

    use super::*;

    #[test]
    fn highly_precise_decimal() {
        let float = Value::Float(float_from_str("2.1").pow(100u16));
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
        let float = Value::Float(float(2).pow(457u16) / float(10).pow(50u16));
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
        let float = Value::Float(float(2).sqrt().recip());
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
        let float = Value::Float(float(int(Integer::factorial(256))).recip());
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
        let int = Value::Integer(int(Integer::factorial(124)));
        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Scientific)
            .build();
        let formatted = format!("{}", int.fmt(opts));

        assert_eq!(
            formatted,
            "1.50614174151114087979501416199328068607632291897193940710078585206682525065290879093506346311596738506917124356744046192504129535473104478255106766046837644419461100452005705416704 × 10 ^ 207",
        );
    }

    #[test]
    fn highly_precise_scientific_2() {
        let float = Value::Float(float(3).pow(1100u16) / float(12).pow(740u16));
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
        let complex = Value::Complex(complex((
            float(int(Integer::factorial(128))).recip(),
            -float(int(Integer::factorial(256))).recip(),
        )));
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
        let complex = Value::Complex(complex(-4).sqrt());
        let opts = FormatOptionsBuilder::new()
            .number(NumberFormat::Decimal)
            .build();
        let formatted = format!("{}", complex.fmt(opts));

        assert_eq!(formatted, "2i");
    }

    #[test]
    fn complex_imaginary_edge() {
        let complexes = [
            Value::Complex(complex((1, 0))),
            Value::Complex(complex((3, -1))),
            Value::Complex(complex((1, -2))),
            Value::Complex(complex((6, 1))),
            Value::Complex(complex((0, 1))),
            Value::Complex(complex((0, -1))),
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
        let float = Value::Float(float(37000000.));
        let opts = FormatOptionsBuilder::new()
            .separators(Separator::Always)
            .build();
        let formatted = format!("{}", float.fmt(opts));

        assert_eq!(formatted, "37,000,000");
    }

    #[test]
    fn trailing_zeroes_2() {
        let float = Value::Float(float_from_str("1400.0010"));
        let opts = FormatOptionsBuilder::new()
            .precision(Some(10))
            .separators(Separator::Always)
            .build();
        let formatted = format!("{}", float.fmt(opts));

        assert_eq!(formatted, "1,400.001");
    }
}
