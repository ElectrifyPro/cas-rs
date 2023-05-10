use std::ops::Range;
use super::{
    error::Error,
    token::{Float, Name, Int},
    Parse,
    Parser,
};

/// A number literal. Integers and floating-point numbers are both supported and represented here
/// as `f64`.
#[derive(Debug, Clone, PartialEq)]
pub struct LitNum {
    /// The value of the number literal.
    pub value: f64,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl Parse for LitNum {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let (lexeme, span) = input
            .try_parse::<Int>()
            .map(|num| (num.lexeme, num.span))
            .or_else(|_| input.try_parse::<Float>().map(|num| (num.lexeme, num.span)))?;
        Ok(Self {
            value: lexeme.parse().unwrap(),
            span,
        })
    }
}

/// A symbol / identifier literal. Symbols are used to represent variables and functions.
#[derive(Debug, Clone, PartialEq)]
pub struct LitSym {
    /// The name of the symbol.
    pub name: String,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl Parse for LitSym {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let token = input.try_parse::<Name>()?;
        Ok(Self {
            name: token.lexeme,
            span: token.span,
        })
    }
}

/// Represents a literal value in CalcScript.
///
/// A literal is any value that can is written directly into the source code. For example, the
/// number `1` is a literal (it is currently the only literal type supported by CalcScript).
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// A number literal. Integers and floating-point numbers are both supported and represented
    /// here as `f64`.
    Number(LitNum),

    /// A symbol / identifier literal. Symbols are used to represent variables and functions.
    Symbol(LitSym),
}

impl Literal {
    /// Returns the span of the literal.
    pub fn span(&self) -> Range<usize> {
        match self {
            Literal::Number(num) => num.span.clone(),
            Literal::Symbol(name) => name.span.clone(),
        }
    }
}

impl Parse for Literal {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        input.try_parse::<LitNum>().map(Literal::Number)
            .or_else(|_| input.try_parse::<LitSym>().map(Literal::Symbol))
    }
}
