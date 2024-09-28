use cas_error::Error;
use crate::parser::{
    ast::{
        expr::Expr,
        literal::LitSym,
        range::Range as RangeExpr,
    },
    fmt::Latex,
    keyword::{In as InToken, Product as ProductToken},
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

/// A product expression, such as `product n in 1..10 of n`.
///
/// A product expression is a shortcut for a loop that represents an accumulative product. The
/// final expression is multiplied over the specified range, with a specific variable name taking
/// on each value in the range. The above example is equivalent to the following code:
///
/// ```calcscript
/// out = 1
/// for n in 1..10 {
///     out *= n
/// }
/// out
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Product {
    /// The variable name representing each value in the range.
    pub variable: LitSym,

    /// The range of values that the variable will take on.
    pub range: RangeExpr,

    /// The body of the product.
    pub body: Box<Expr>,

    /// The region of the source code that this `product` expression was parsed from.
    pub span: Range<usize>,
}

impl Product {
    /// Returns the span of the `product` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Product {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let product_token = input.try_parse::<ProductToken>().forward_errors(recoverable_errors)?;
        let variable = input.try_parse::<LitSym>().forward_errors(recoverable_errors)?;
        input.try_parse::<InToken>().forward_errors(recoverable_errors)?;
        let range = input.try_parse::<RangeExpr>().forward_errors(recoverable_errors)?;
        let body = input.try_parse_with_state::<_, Expr>(|state| {
            state.allow_of = true;
        }).forward_errors(recoverable_errors)?;
        let span = product_token.span.start..body.span().end;

        Ok(Self {
            variable,
            range,
            body: Box::new(body),
            span,
        })
    }
}

impl std::fmt::Display for Product {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "product {} in {} {}", self.variable, self.range, self.body)
    }
}

impl Latex for Product {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\\prod_{{{}={}}}^{{{}}} {}",
            self.variable,
            self.range.start,
            self.range.end,
            self.body,
        )
    }
}
