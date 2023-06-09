use std::ops::Range;
use crate::{
    parser::{
        error::Error,
        expr::Expr,
        literal::LitSym,
        token::{Assign as AssignOp, CloseParen, OpenParen},
        Parse,
        Parser,
    },
    tokenizer::TokenKind,
};

/// A parameter of a function declaration, such as `x` or `y = 1` in the declaration `f(x, y = 1) =
/// x^y`.
#[derive(Debug, Clone, PartialEq)]
pub enum Param {
    /// A parameter with no default value, such as `x` in `f(x) = x^2`.
    Symbol(LitSym),

    /// A parameter with a default value, such as `y = 1` in `f(x, y = 1) = x^y`.
    Default(LitSym, Expr),
}

impl Parse for Param {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let symbol = input.try_parse::<LitSym>()?;

        if input.try_parse::<AssignOp>().is_ok() {
            let default = input.try_parse::<Expr>()?;
            Ok(Param::Default(symbol, default))
        } else {
            Ok(Param::Symbol(symbol))
        }
    }
}

/// A function header, **not including the body**. Functions can have multiple parameters with
/// optional default values, like in `f(x, y = 1)`. When a function with this header is called, the
/// default values are used (i.e. `y = 1`), unless the caller provides their own values (`f(2,
/// 3)`).
#[derive(Debug, Clone, PartialEq)]
pub struct FuncHeader {
    /// The name of the function.
    pub name: LitSym,

    /// The parameters of the function.
    pub params: Vec<Param>,

    /// The region of the source code that this function header was parsed from.
    pub span: Range<usize>,
}

impl FuncHeader {
    /// Returns the span of the function header.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl Parse for FuncHeader {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let name = input.try_parse::<LitSym>()?;
        input.try_parse::<OpenParen>()?;
        let params = input.try_parse_delimited::<Param>(TokenKind::Comma)?;
        let close_paren = input.try_parse::<CloseParen>()?;

        let span = name.span.start..close_paren.span.end;
        Ok(Self { name, params, span })
    }
}

/// An assignment target, such as `x` or `f(x)`.
#[derive(Debug, Clone, PartialEq)]
pub enum AssignTarget {
    /// A symbol, such as `x`.
    Symbol(LitSym),

    /// A function, such as `f(x)`.
    Func(FuncHeader),
}

impl AssignTarget {
    /// Returns the span of the assignment target.
    pub fn span(&self) -> Range<usize> {
        match self {
            AssignTarget::Symbol(symbol) => symbol.span.clone(),
            AssignTarget::Func(func) => func.span(),
        }
    }
}

impl Parse for AssignTarget {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        input.try_parse::<FuncHeader>().map(AssignTarget::Func)
            .or_else(|_| input.try_parse::<LitSym>().map(AssignTarget::Symbol))
    }
}

/// An assignment of a variable or function, such as `x = 1` or `f(x) = x^2`.
#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    /// The target to assign to.
    pub target: AssignTarget,

    /// The expression to assign to the target.
    pub value: Box<Expr>,

    /// The region of the source code that this assignment expression was parsed from.
    pub span: Range<usize>,
}

impl Assign {
    /// Returns the span of the assignment expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl Parse for Assign {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let target = input.try_parse::<AssignTarget>()?;
        input.try_parse::<AssignOp>()?;
        let value = input.try_parse::<Expr>()?;

        let span = target.span().start..value.span().end;
        Ok(Self {
            target,
            value: Box::new(value),
            span,
        })
    }
}
