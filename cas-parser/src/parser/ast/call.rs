use crate::parser::{
    ast::{expr::Expr, helper::ParenDelimited, literal::LitSym},
    error::{kind::TooManyDerivatives, Error},
    fmt::{Latex, fmt_pow},
    token::Quote,
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A function call, such as `func(x, -40)`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Call {
    /// The name of the function to call.
    pub name: LitSym,

    /// The number of derivatives to take before calling the function.
    pub derivatives: u8,

    /// The arguments to the function.
    pub args: Vec<Expr>,

    /// The region of the source code that this function call was parsed from.
    pub span: Range<usize>,

    /// The span of the parentheses that surround the arguments.
    pub paren_span: Range<usize>,
}

impl Call {
    /// Returns the span of the function call.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    /// Returns a set of two spans, where the first is the span of the function name (with the
    /// opening parenthesis) and the second is the span of the closing parenthesis.
    pub fn outer_span(&self) -> [Range<usize>; 2] {
        [
            self.name.span.start..self.paren_span.start + 1,
            self.paren_span.end - 1..self.paren_span.end,
        ]
    }
}

impl<'source> Parse<'source> for Call {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let name = input.try_parse::<LitSym>().forward_errors(recoverable_errors)?;
        let mut derivatives = 0usize;
        let mut quote_span: Option<Range<_>> = None;
        let mut too_many_derivatives = false;

        while let Ok(quote) = input.try_parse::<Quote>().forward_errors(recoverable_errors) {
            if derivatives == u8::MAX.into() {
                too_many_derivatives = true;
            }

            derivatives += 1;
            quote_span = quote_span
                .or_else(|| Some(quote.span.clone()))
                .map(|span| span.start..quote.span.end);
        }

        if too_many_derivatives {
            recoverable_errors.push(Error::new(
                vec![quote_span.unwrap()],
                TooManyDerivatives { derivatives }
            ));
        }

        let surrounded = input.try_parse::<ParenDelimited<_>>().forward_errors(recoverable_errors)?;

        // use `name` here before it is moved into the struct
        let span = name.span.start..surrounded.end.span.end;
        Ok(Self {
            name,
            derivatives: derivatives as u8,
            args: surrounded.value.values,
            span,
            paren_span: surrounded.start.span.start..surrounded.end.span.end,
        })
    }
}

impl std::fmt::Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.name.fmt(f)?;
        for _ in 0..self.derivatives {
            write!(f, "'")?;
        }
        write!(f, "(")?;
        if let Some((last, args)) = self.args.split_last() {
            for arg in args {
                arg.fmt(f)?;
                write!(f, ", ")?;
            }
            last.fmt(f)?;
        }
        write!(f, ")")
    }
}

impl Latex for Call {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        enum SpecialFunc {
            Pow,
            Root,
            Cbrt,
            Sqrt,
            Abs,
            Other,
        }

        impl SpecialFunc {
            /// Write the name of the function.
            fn name(&self, f: &mut fmt::Formatter, call: &Call) -> fmt::Result {
                match self {
                    Self::Pow => Ok(()),
                    Self::Root => write!(f, "\\sqrt"),
                    Self::Cbrt => write!(f, "\\sqrt[3]"),
                    Self::Sqrt => write!(f, "\\sqrt"),
                    Self::Abs => Ok(()),
                    Self::Other => write!(f, "\\mathrm{{ {} }}", call.name.as_display()),
                }
            }

            /// Write the tokens surrounding the arguments, and delegate the arguments to `inner_args`.
            fn outer_args(&self, f: &mut fmt::Formatter, call: &Call) -> fmt::Result {
                match self {
                    Self::Pow => self.inner_args(f, call),
                    Self::Root => {
                        self.inner_args(f, call)?;
                        write!(f, "}}")
                    },
                    Self::Cbrt | Self::Sqrt => {
                        write!(f, "{{")?;
                        self.inner_args(f, call)?;
                        write!(f, "}}")
                    },
                    Self::Abs => {
                        write!(f, "\\left|")?;
                        self.inner_args(f, call)?;
                        write!(f, "\\right|")
                    },
                    Self::Other => {
                        write!(f, "\\left(")?;
                        self.inner_args(f, call)?;
                        write!(f, "\\right)")
                    },
                }
            }

            fn inner_args(&self, f: &mut fmt::Formatter, call: &Call) -> fmt::Result {
                match self {
                    Self::Pow => fmt_pow(f, call.args.first(), call.args.get(1))?,
                    Self::Root => {
                        if let Some(arg1) = call.args.get(1) {
                            write!(f, "[{}]", arg1.as_display())?;
                        }
                        write!(f, "{{")?;
                        if let Some(arg0) = call.args.first() {
                            arg0.fmt_latex(f)?;
                        }
                    },
                    Self::Cbrt | Self::Sqrt | Self::Abs | Self::Other => {
                        if let Some((last, args)) = call.args.split_last() {
                            for arg in args {
                                arg.fmt_latex(f)?;
                                write!(f, ", ")?;
                            }
                            last.fmt_latex(f)?;
                        }
                    },
                }

                Ok(())
            }
        }

        let func = match self.name.name.as_str() {
            "pow" => SpecialFunc::Pow,
            "root" => SpecialFunc::Root,
            "cbrt" => SpecialFunc::Cbrt,
            "sqrt" => SpecialFunc::Sqrt,
            "abs" => SpecialFunc::Abs,
            _ => SpecialFunc::Other,
        };

        func.name(f, self)?;
        match self.derivatives {
            0 => {},
            1 => write!(f, "'")?,
            2 => write!(f, "''")?,
            n => write!(f, "^{{ ({}) }}", n)?,
        }

        func.outer_args(f, self)
    }
}
