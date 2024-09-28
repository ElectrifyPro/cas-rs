use cas_error::Error;
use crate::parser::{
    ast::expr::Expr,
    error::{OfOutsideSumProduct, ThenOutsideIfWhile},
    fmt::Latex,
    keyword::{Of as OfToken, Then as ThenToken},
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A `then` expression, such as `then x += 1`.
///
/// A `then` expression simply evaluates the expression that follows the `then` keyword, and
/// returns its result. It is only valid in the context of `if` and `while` expressions, and is
/// used to concisely write one-line `if` and `while` expressions, such as `if x < 10 then x += 1`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Then {
    /// The expression to evaluate.
    pub expr: Box<Expr>,

    /// The region of the source code that this expression was parsed from.
    pub span: Range<usize>,

    /// The span of the `then` keyword.
    pub then_span: Range<usize>,
}

impl Then {
    /// Returns the span of the `then` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Then {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let then_token = input.try_parse::<ThenToken>().forward_errors(recoverable_errors)?;

        if !input.state.allow_then {
            recoverable_errors.push(Error::new(
                vec![then_token.span.clone()],
                ThenOutsideIfWhile,
            ));
        }

        // this is to catch stuff like `if true then then then then then 1`
        let body = input.try_parse_with_state::<_, Expr>(|input| {
            input.allow_then = false;
        }).forward_errors(recoverable_errors)?;
        let span = then_token.span.start..body.span().end;

        Ok(Self {
            expr: Box::new(body),
            span,
            then_span: then_token.span,
        })
    }
}

impl std::fmt::Display for Then {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "then {}", self.expr)
    }
}

impl Latex for Then {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\text{{ then }}")?;
        self.expr.fmt_latex(f)?;
        Ok(())
    }
}

/// An `of` expression, such as `of x`.
///
/// An `of` expression's body is repeatedly evaluated during summation or product expressions,
/// with a specific variable in the `of` expression taking on each value in the range of the
/// summation or product. It is only valid in the context of `sum` and `product` expressions, and
/// is used to concisely write one-line `sum` and `product` expressions, such as `sum x in 1..10 of
/// x`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Of {
    /// The expression to evaluate.
    pub expr: Box<Expr>,

    /// The region of the source code that this expression was parsed from.
    pub span: Range<usize>,

    /// The span of the `of` keyword.
    pub of_span: Range<usize>,
}

impl Of {
    /// Returns the span of the `of` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Of {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let of_token = input.try_parse::<OfToken>().forward_errors(recoverable_errors)?;

        if !input.state.allow_of {
            recoverable_errors.push(Error::new(
                vec![of_token.span.clone()],
                OfOutsideSumProduct,
            ));
        }

        // this is to catch stuff like `sum x in 1..10 of of of of of 1`
        let body = input.try_parse_with_state::<_, Expr>(|input| {
            input.allow_of = false;
        }).forward_errors(recoverable_errors)?;
        let span = of_token.span.start..body.span().end;

        Ok(Self {
            expr: Box::new(body),
            span,
            of_span: of_token.span,
        })
    }
}

impl std::fmt::Display for Of {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "of {}", self.expr)
    }
}

impl Latex for Of {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\text{{ of }}")?;
        self.expr.fmt_latex(f)?;
        Ok(())
    }
}
