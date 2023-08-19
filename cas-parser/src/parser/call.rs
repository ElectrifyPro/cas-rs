use std::ops::Range;
use crate::{
    parser::{
        error::Error,
        expr::Expr,
        literal::LitSym,
        token::{CloseParen, OpenParen},
        Parse,
        Parser,
    },
    tokenizer::TokenKind,
};

/// A function call, such as `func(x, -40)`.
#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    /// The name of the function to call.
    pub name: LitSym,

    /// The arguments to the function.
    pub args: Vec<Expr>,

    /// The region of the source code that this function call was parsed from.
    pub span: Range<usize>,
}

impl Call {
    /// Returns the span of the function call.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl Parse for Call {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let name = input.try_parse::<LitSym>()?;
        input.try_parse::<OpenParen>()?;
        let args = input.try_parse_delimited::<Expr>(TokenKind::Comma)?;
        let close_paren = input.try_parse::<CloseParen>()?;

        let span = name.span.start..close_paren.span.end;
        Ok(Self { name, args, span })
    }
}
