use std::ops::Range;
use super::{
    error::Error,
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
        let expr = input.try_parse::<Expr>()?;
        let close_paren = input.try_parse::<CloseParen>()?;
        Ok(Self {
            expr: Box::new(expr),
            span: open_paren.span.start..close_paren.span.end,
        })
    }
}
