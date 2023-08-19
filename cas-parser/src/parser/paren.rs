use std::ops::Range;
use super::{
    error::{kind, Error},
    expr::Expr,
    token::{CloseParen, OpenParen},
    Parse,
    Parser
};

/// A parenthesized expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Paren {
    /// The inner expression.
    pub expr: Box<Expr>,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl Paren {
    /// Returns the span of the parenthesized expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl Parse for Paren {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let open_paren = input.try_parse::<OpenParen>()?;
        let expr = match input.try_parse::<Expr>() {
            Ok(expr) => expr,
            Err(err) => {
                if let Ok(close_paren) = input.try_parse::<CloseParen>() {
                    return Err(Error::new_fatal(
                        vec![open_paren.span.start..close_paren.span.end],
                        kind::EmptyParenthesis,
                    ));
                } else {
                    return Err(err);
                }
            },
        };
        let close_paren = input.try_parse::<CloseParen>().map_err(|_| {
            Error::new_fatal(
                vec![open_paren.span.clone()],
                kind::UnclosedParenthesis { opening: true },
            )
        })?;
        Ok(Self {
            expr: Box::new(expr),
            span: open_paren.span.start..close_paren.span.end,
        })
    }
}
