use std::ops::Range;
use crate::{
    parser::{
        assign::Assign,
        binary::Binary,
        block::Block,
        call::Call,
        error::{kind, Error},
        if_expr::If,
        iter::ExprIter,
        literal::Literal,
        paren::Paren,
        token::{op::Precedence, CloseParen},
        unary::Unary,
        Parse,
        Parser,
    },
    return_if_ok,
};

/// Represents a general expression in CalcScript.
///
/// An expression is any valid piece of code that can be evaluated to produce a value. Expressions
/// can be used as the right-hand side of an assignment, or as the argument to a function call.
///
/// In CalcBot, the `c-calculate` command accepts an expression as its argument.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A literal value.
    Literal(Literal),

    /// A parenthesized expression, such as `(1 + 2)`.
    Paren(Paren),

    /// A blocked expression, such as `{1 + 2}`.
    Block(Block),

    /// An if expression, such as `if x > 0 then x else -x`.
    If(If),

    /// A function call, such as `abs(-1)`.
    Call(Call),

    /// A unary operation, such as `-1` or `!true`.
    Unary(Unary),

    /// A binary operation, such as `1 + 2`.
    Binary(Binary),

    /// An assignment of a variable or function, such as `x = 1` or `f(x) = x^2`.
    Assign(Assign),
}

impl Expr {
    /// Returns the span of the expression.
    pub fn span(&self) -> Range<usize> {
        match self {
            Expr::Literal(literal) => literal.span(),
            Expr::Paren(paren) => paren.span(),
            Expr::Block(block) => block.span(),
            Expr::If(if_expr) => if_expr.span(),
            Expr::Call(call) => call.span(),
            Expr::Unary(unary) => unary.span(),
            Expr::Binary(binary) => binary.span(),
            Expr::Assign(assign) => assign.span(),
        }
    }

    /// Returns an iterator that traverses the tree of expressions in left-to-right post-order
    /// (i.e. depth-first).
    pub fn post_order_iter(&self) -> ExprIter {
        ExprIter::new(self)
    }
}

impl<'source> Parse<'source> for Expr {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        if input.clone().try_parse::<CloseParen>().is_ok() {
            return Err(vec![input.error(kind::UnclosedParenthesis { opening: false })]);
        }

        let _ = return_if_ok!(input.try_parse::<Assign>().map(Self::Assign).forward_errors(recoverable_errors));
        let lhs = Unary::parse_or_lower(input, recoverable_errors)?;
        Binary::parse_expr(input, recoverable_errors, lhs, Precedence::Any)
    }
}

/// Represents a primary expression in CalcScript.
///
/// Primary expressions are the simplest expressions, and are the building blocks of more complex
/// expressions. Primary expressions are also self-contained. This means that primary expressions
/// within a larger expression can be replaced with equivalent, but different kinds of primary
/// expressions, and the larger expression will still be valid.
#[derive(Debug, Clone, PartialEq)]
pub enum Primary {
    /// A literal value.
    Literal(Literal),

    /// A parenthesized expression, such as `(1 + 2)`.
    Paren(Paren),

    /// A blocked expression, such as `{1 + 2}`.
    Block(Block),

    /// An if expression, such as `if x > 0 then x else -x`.
    If(If),

    /// A function call, such as `abs(-1)`.
    Call(Call),
}

impl Primary {
    /// Returns the span of the primary expression.
    pub fn span(&self) -> Range<usize> {
        match self {
            Primary::Literal(literal) => literal.span(),
            Primary::Paren(paren) => paren.span(),
            Primary::Block(block) => block.span(),
            Primary::If(if_expr) => if_expr.span(),
            Primary::Call(call) => call.span(),
        }
    }
}

impl<'source> Parse<'source> for Primary {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        // function calls can overlap with literals, so we need to try parsing a function call
        // first
        let _ = return_if_ok!(input.try_parse::<If>().map(Self::If).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse::<Call>().map(Self::Call).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse::<Literal>().map(Self::Literal).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse::<Paren>().map(Self::Paren).forward_errors(recoverable_errors));
        input.try_parse::<Block>().map(Self::Block).forward_errors(recoverable_errors)
    }
}

impl From<Primary> for Expr {
    fn from(primary: Primary) -> Self {
        match primary {
            Primary::Literal(literal) => Self::Literal(literal),
            Primary::Paren(paren) => Self::Paren(paren),
            Primary::Block(block) => Self::Block(block),
            Primary::If(if_expr) => Self::If(if_expr),
            Primary::Call(call) => Self::Call(call),
        }
    }
}
