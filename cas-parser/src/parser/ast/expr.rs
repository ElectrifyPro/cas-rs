use crate::{
    parser::{
        ast::{
            assign::Assign,
            binary::Binary,
            block::Block,
            call::Call,
            if_expr::If,
            index::Index,
            literal::Literal,
            loop_expr::{Break, Continue, Loop},
            paren::Paren,
            return_expr::Return,
            unary::Unary,
            while_expr::While,
        },
        error::{kind, Error},
        fmt::Latex,
        iter::ExprIter,
        token::{op::Precedence, CloseParen},
        Parse,
        Parser,
    },
    return_if_ok,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Represents a general expression in CalcScript.
///
/// An expression is any valid piece of code that can be evaluated to produce a value. Expressions
/// can be used as the right-hand side of an assignment, or as the argument to a function call.
///
/// In CalcBot, the `c-calculate` command accepts an expression as its argument.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Expr {
    /// A literal value.
    Literal(Literal),

    /// A parenthesized expression, such as `(1 + 2)`.
    Paren(Paren),

    /// A blocked expression, such as `{1 + 2}`.
    Block(Block),

    /// An if expression, such as `if x > 0 then x else -x`.
    If(If),

    /// A loop expression, as in `loop { ... }`.
    Loop(Loop),

    /// A while loop expression, as in `while x > 0 then { ... }`.
    While(While),

    /// A break expression, used to exit a loop, optionally with a value.
    Break(Break),

    /// A continue expression, used to skip the rest of a loop iteration.
    Continue(Continue),

    /// A return expression, as in `return x`, used to return a value from a function.
    Return(Return),

    /// A function call, such as `abs(-1)`.
    Call(Call),

    /// List indexing, such as `list[0]`.
    Index(Index),

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
            Expr::Loop(loop_expr) => loop_expr.span(),
            Expr::While(while_expr) => while_expr.span(),
            Expr::Break(break_expr) => break_expr.span(),
            Expr::Continue(continue_expr) => continue_expr.span(),
            Expr::Return(return_expr) => return_expr.span(),
            Expr::Call(call) => call.span(),
            Expr::Index(index) => index.span(),
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

    /// If this expression is a [`Expr::Paren`], returns the innermost expression in the
    /// parenthesized expression. Otherwise, returns `self`.
    pub fn innermost(&self) -> &Expr {
        let mut inner = self;
        while let Expr::Paren(paren) = inner {
            inner = &paren.expr;
        }
        inner
    }

    /// Returns true if the given expression can be used as a target for implicit multiplication.
    pub fn is_implicit_mul_target(&self) -> bool {
        // TODO: there may be more reasonable targets
        matches!(self,
            Expr::Literal(Literal::Integer(_))
                | Expr::Literal(Literal::Float(_))
                | Expr::Literal(Literal::Radix(_))
                | Expr::Literal(Literal::Symbol(_))
                | Expr::Literal(Literal::Unit(_))
                | Expr::Paren(_)
                | Expr::Call(_)
                | Expr::Unary(_)
        )
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

        let _ = return_if_ok!(input.try_parse().map(Self::Assign).forward_errors(recoverable_errors));
        let lhs = Unary::parse_or_lower(input, recoverable_errors)?;
        Ok(Binary::parse_expr(input, recoverable_errors, lhs, Precedence::Any)?.0)
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(literal) => literal.fmt(f),
            Expr::Paren(paren) => paren.fmt(f),
            Expr::Block(block) => block.fmt(f),
            Expr::If(if_expr) => if_expr.fmt(f),
            Expr::Loop(loop_expr) => loop_expr.fmt(f),
            Expr::While(while_expr) => while_expr.fmt(f),
            Expr::Break(break_expr) => break_expr.fmt(f),
            Expr::Continue(continue_expr) => continue_expr.fmt(f),
            Expr::Return(return_expr) => return_expr.fmt(f),
            Expr::Call(call) => call.fmt(f),
            Expr::Index(index) => index.fmt(f),
            Expr::Unary(unary) => unary.fmt(f),
            Expr::Binary(binary) => binary.fmt(f),
            Expr::Assign(assign) => assign.fmt(f),
        }
    }
}

impl Latex for Expr {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(literal) => literal.fmt_latex(f),
            Expr::Paren(paren) => paren.fmt_latex(f),
            Expr::Block(block) => block.fmt_latex(f),
            Expr::If(if_expr) => if_expr.fmt_latex(f),
            Expr::Loop(loop_expr) => loop_expr.fmt_latex(f),
            Expr::While(while_expr) => while_expr.fmt_latex(f),
            Expr::Break(break_expr) => break_expr.fmt_latex(f),
            Expr::Continue(continue_expr) => continue_expr.fmt_latex(f),
            Expr::Return(return_expr) => return_expr.fmt_latex(f),
            Expr::Call(call) => call.fmt_latex(f),
            Expr::Index(index) => index.fmt_latex(f),
            Expr::Unary(unary) => unary.fmt_latex(f),
            Expr::Binary(binary) => binary.fmt_latex(f),
            Expr::Assign(assign) => assign.fmt_latex(f),
        }
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

    /// A loop expression, as in `loop { ... }`.
    Loop(Loop),

    /// A while loop expression, as in `while x > 0 then { ... }`.
    While(While),

    /// A break expression, used to exit a loop, optionally with a value.
    Break(Break),

    /// A continue expression, used to skip the rest of a loop iteration.
    Continue(Continue),

    /// A return expression, as in `return x`, used to return a value from a function.
    Return(Return),

    /// A function call, such as `abs(-1)`.
    Call(Call),

    /// List indexing, such as `list[0]`.
    Index(Index),
}

impl Primary {
    /// Returns the span of the primary expression.
    pub fn span(&self) -> Range<usize> {
        match self {
            Primary::Literal(literal) => literal.span(),
            Primary::Paren(paren) => paren.span(),
            Primary::Block(block) => block.span(),
            Primary::If(if_expr) => if_expr.span(),
            Primary::Loop(loop_expr) => loop_expr.span(),
            Primary::While(while_expr) => while_expr.span(),
            Primary::Break(break_expr) => break_expr.span(),
            Primary::Continue(continue_expr) => continue_expr.span(),
            Primary::Return(return_expr) => return_expr.span(),
            Primary::Call(call) => call.span(),
            Primary::Index(index) => index.span(),
        }
    }
}

impl<'source> Parse<'source> for Primary {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let _ = return_if_ok!(input.try_parse().map(Self::If).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Self::Loop).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Self::While).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Self::Break).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Self::Continue).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Self::Return).forward_errors(recoverable_errors));
        let _ = return_if_ok!(Index::parse_or_lower(input, recoverable_errors));
        // function calls can overlap with literals, so we need to try parsing a function call
        // first
        let _ = return_if_ok!(input.try_parse().map(Self::Call).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Self::Literal).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Self::Paren).forward_errors(recoverable_errors));
        input.try_parse().map(Self::Block).forward_errors(recoverable_errors)
    }
}

impl From<Primary> for Expr {
    fn from(primary: Primary) -> Self {
        match primary {
            Primary::Literal(literal) => Self::Literal(literal),
            Primary::Paren(paren) => Self::Paren(paren),
            Primary::Block(block) => Self::Block(block),
            Primary::If(if_expr) => Self::If(if_expr),
            Primary::Loop(loop_expr) => Self::Loop(loop_expr),
            Primary::While(while_expr) => Self::While(while_expr),
            Primary::Break(break_expr) => Self::Break(break_expr),
            Primary::Continue(continue_expr) => Self::Continue(continue_expr),
            Primary::Return(return_expr) => Self::Return(return_expr),
            Primary::Call(call) => Self::Call(call),
            Primary::Index(index) => Self::Index(index),
        }
    }
}
