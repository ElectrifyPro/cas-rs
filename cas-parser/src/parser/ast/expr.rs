use cas_error::Error;
use crate::{
    parser::{
        ast::{
            assign::Assign,
            binary::Binary,
            block::Block,
            branch::{Of, Then},
            call::Call,
            for_expr::For,
            if_expr::If,
            index::Index,
            literal::Literal,
            loop_expr::{Break, Continue, Loop},
            paren::Paren,
            product::Product,
            range::Range,
            return_expr::Return,
            sum::Sum,
            unary::Unary,
            while_expr::While,
        },
        error::{ExpectedExpr, UnclosedParenthesis},
        fmt::Latex,
        iter::ExprIter,
        token::{op::Precedence, CloseParen},
        Parse,
        ParseResult,
        Parser,
    },
    tokenizer::TokenKind,
    return_if_ok,
};
use std::fmt;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Represents any kind of expression in CalcScript.
///
/// An expression is any valid piece of code that can be evaluated to produce a value. Expressions
/// can be used as the right-hand side of an assignment, or as the argument to a function call.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Expr {
    /// A literal value.
    Literal(Literal),

    /// A parenthesized expression, such as `(1 + 2)`.
    Paren(Paren),

    /// A blocked expression, such as `{1 + 2}`.
    Block(Block),

    /// A sum expression, such as `sum n in 1..10 of n`.
    Sum(Sum),

    /// A product expression, such as `product n in 1..10 of n`.
    Product(Product),

    /// An if expression, such as `if x > 0 then x else -x`.
    If(If),

    /// A loop expression, as in `loop { ... }`.
    Loop(Loop),

    /// A while loop expression, as in `while x > 0 then { ... }`.
    While(While),

    /// A for loop expression, as in `for i in 0..10 then print(i)`.
    For(For),

    /// A then expression, as in `then x += 1`.
    Then(Then),

    /// An of expression, as in `of x`.
    Of(Of),

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

    /// A range expression, such as `1..10`.
    Range(Range),
}

impl Expr {
    /// Returns the span of the expression.
    pub fn span(&self) -> std::ops::Range<usize> {
        match self {
            Expr::Literal(literal) => literal.span(),
            Expr::Paren(paren) => paren.span(),
            Expr::Block(block) => block.span(),
            Expr::Sum(sum) => sum.span(),
            Expr::Product(product) => product.span(),
            Expr::If(if_expr) => if_expr.span(),
            Expr::Loop(loop_expr) => loop_expr.span(),
            Expr::While(while_expr) => while_expr.span(),
            Expr::For(for_expr) => for_expr.span(),
            Expr::Then(then) => then.span(),
            Expr::Of(of) => of.span(),
            Expr::Break(break_expr) => break_expr.span(),
            Expr::Continue(continue_expr) => continue_expr.span(),
            Expr::Return(return_expr) => return_expr.span(),
            Expr::Call(call) => call.span(),
            Expr::Index(index) => index.span(),
            Expr::Unary(unary) => unary.span(),
            Expr::Binary(binary) => binary.span(),
            Expr::Assign(assign) => assign.span(),
            Expr::Range(range) => range.span(),
        }
    }

    /// Returns an iterator that traverses the tree of expressions in left-to-right post-order
    /// (i.e. depth-first).
    pub fn post_order_iter(&self) -> ExprIter {
        ExprIter::new(self)
    }

    /// If this expression is an [`Expr::Paren`], returns the innermost expression in the
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
            return Err(vec![input.error(UnclosedParenthesis { opening: false })]);
        }

        let _ = return_if_ok!(input.try_parse().map(Self::Assign).forward_errors(recoverable_errors));
        let lhs = Unary::parse_or_lower(input, recoverable_errors)?;
        Ok(Binary::parse_expr(input, recoverable_errors, lhs, Precedence::Any)?.0)
    }
}

/// Implements [`Parse`] for an [`Expr`] variant by parsing an [`Expr`] and then converting it to the
/// variant.
///
/// This is done for completeness.
macro_rules! impl_by_parsing_expr {
    ($( $variant:ident $expected:literal ),* $(,)?) => {
        $(
            impl<'source> Parse<'source> for $variant {
                fn std_parse(
                    input: &mut Parser<'source>,
                    recoverable_errors: &mut Vec<Error>
                ) -> Result<Self, Vec<Error>> {
                    match input.try_parse::<Expr>().forward_errors(recoverable_errors) {
                        Ok(Expr::$variant(expr)) => Ok(expr),
                        _ => Err(vec![input.error(ExpectedExpr { expected: $expected })]),
                    }
                }
            }
        )*
    };
}

impl_by_parsing_expr!(
    Call "a function call",
    Index "a list indexing expression",
    Unary "a unary operation",
    Range "a range expression"
);

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(literal) => literal.fmt(f),
            Expr::Paren(paren) => paren.fmt(f),
            Expr::Block(block) => block.fmt(f),
            Expr::Sum(sum) => sum.fmt(f),
            Expr::Product(product) => product.fmt(f),
            Expr::If(if_expr) => if_expr.fmt(f),
            Expr::Loop(loop_expr) => loop_expr.fmt(f),
            Expr::While(while_expr) => while_expr.fmt(f),
            Expr::For(for_expr) => for_expr.fmt(f),
            Expr::Then(then) => then.fmt(f),
            Expr::Of(of) => of.fmt(f),
            Expr::Break(break_expr) => break_expr.fmt(f),
            Expr::Continue(continue_expr) => continue_expr.fmt(f),
            Expr::Return(return_expr) => return_expr.fmt(f),
            Expr::Call(call) => call.fmt(f),
            Expr::Index(index) => index.fmt(f),
            Expr::Unary(unary) => unary.fmt(f),
            Expr::Binary(binary) => binary.fmt(f),
            Expr::Assign(assign) => assign.fmt(f),
            Expr::Range(range) => range.fmt(f),
        }
    }
}

impl Latex for Expr {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(literal) => literal.fmt_latex(f),
            Expr::Paren(paren) => paren.fmt_latex(f),
            Expr::Block(block) => block.fmt_latex(f),
            Expr::Sum(sum) => sum.fmt_latex(f),
            Expr::Product(product) => product.fmt_latex(f),
            Expr::If(if_expr) => if_expr.fmt_latex(f),
            Expr::Loop(loop_expr) => loop_expr.fmt_latex(f),
            Expr::While(while_expr) => while_expr.fmt_latex(f),
            Expr::For(for_expr) => for_expr.fmt_latex(f),
            Expr::Then(then) => then.fmt_latex(f),
            Expr::Of(of) => of.fmt_latex(f),
            Expr::Break(break_expr) => break_expr.fmt_latex(f),
            Expr::Continue(continue_expr) => continue_expr.fmt_latex(f),
            Expr::Return(return_expr) => return_expr.fmt_latex(f),
            Expr::Call(call) => call.fmt_latex(f),
            Expr::Index(index) => index.fmt_latex(f),
            Expr::Unary(unary) => unary.fmt_latex(f),
            Expr::Binary(binary) => binary.fmt_latex(f),
            Expr::Assign(assign) => assign.fmt_latex(f),
            Expr::Range(range) => range.fmt_latex(f),
        }
    }
}

/// Represents a primary expression in CalcScript.
///
/// Primary expressions extend the concept of [`Atom`] by allowing for more complex and ambiguous
/// expressions that are still self-contained. These extensions include function calls and list
/// indexing expressions, which can be ambiguous when encountered in isolation.
///
/// For example, when trying to parse a [`Primary`], a literal value like `abc` cannot
/// automatically be declared a [`Primary::Literal`]. Instead, we must parse forward a little more
/// to see if this is actually calling a function named `abc`, or indexing into a list named `abc`,
/// or neither.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primary {
    /// A literal value.
    Literal(Literal),

    /// A parenthesized expression, such as `(1 + 2)`.
    Paren(Paren),

    /// A blocked expression, such as `{1 + 2}`.
    Block(Block),

    /// A sum expression, such as `sum n in 1..10 of n`.
    Sum(Sum),

    /// A product expression, such as `product n in 1..10 of n`.
    Product(Product),

    /// An if expression, such as `if x > 0 then x else -x`.
    If(If),

    /// A loop expression, as in `loop { ... }`.
    Loop(Loop),

    /// A while loop expression, as in `while x > 0 then { ... }`.
    While(While),

    /// A for loop expression, as in `for i in 0..10 then print(i)`.
    For(For),

    /// A then expression, as in `then x += 1`.
    Then(Then),

    /// An of expression, as in `of x`.
    Of(Of),

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
    pub fn span(&self) -> std::ops::Range<usize> {
        match self {
            Primary::Literal(literal) => literal.span(),
            Primary::Paren(paren) => paren.span(),
            Primary::Block(block) => block.span(),
            Primary::Sum(sum) => sum.span(),
            Primary::Product(product) => product.span(),
            Primary::If(if_expr) => if_expr.span(),
            Primary::Loop(loop_expr) => loop_expr.span(),
            Primary::While(while_expr) => while_expr.span(),
            Primary::For(for_expr) => for_expr.span(),
            Primary::Then(then) => then.span(),
            Primary::Of(of) => of.span(),
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
        let atom = input.try_parse::<Atom>().forward_errors(recoverable_errors)?;
        let mut primary = Primary::from(atom);

        loop {
            let mut fork = input.clone();
            match fork.next_token() {
                Ok(next) if next.kind == TokenKind::OpenParen || next.kind == TokenKind::Quote => {
                    match Call::parse_or_lower(input, recoverable_errors, primary)? {
                        (new_primary, true) => primary = new_primary,
                        // call was not parsed; is this implicit multiplication?
                        (unchanged_primary, false) => break Ok(unchanged_primary),
                    }
                },
                Ok(next) if next.kind == TokenKind::OpenSquare => {
                    match Index::parse_or_lower(input, recoverable_errors, primary) {
                        (new_primary, true) => primary = new_primary,
                        (unchanged_primary, false) => break Ok(unchanged_primary),
                    }
                },
                _ => break Ok(primary),
            }
        }
    }
}

impl From<Primary> for Expr {
    fn from(primary: Primary) -> Self {
        match primary {
            Primary::Literal(literal) => Self::Literal(literal),
            Primary::Paren(paren) => Self::Paren(paren),
            Primary::Block(block) => Self::Block(block),
            Primary::Sum(sum) => Self::Sum(sum),
            Primary::Product(product) => Self::Product(product),
            Primary::If(if_expr) => Self::If(if_expr),
            Primary::Loop(loop_expr) => Self::Loop(loop_expr),
            Primary::While(while_expr) => Self::While(while_expr),
            Primary::For(for_expr) => Self::For(for_expr),
            Primary::Then(then) => Self::Then(then),
            Primary::Of(of) => Self::Of(of),
            Primary::Break(break_expr) => Self::Break(break_expr),
            Primary::Continue(continue_expr) => Self::Continue(continue_expr),
            Primary::Return(return_expr) => Self::Return(return_expr),
            Primary::Call(call) => Self::Call(call),
            Primary::Index(index) => Self::Index(index),
        }
    }
}

/// Represents an atom expression in CalcScript.
///
/// Atom expressions are the simplest kind of expression, and are entirely unambiguous to parse,
/// meaning that they can be parsed without needing any context.
///
/// For example, a literal value like `1` or `true` has no ambiguity; when we encounter a numeric
/// or boolean token, we know that it must be a literal value.
///
/// Some expressions, like `if` expressions or `loop` expressions, are also atom expressions,
/// because they have a unique keyword that identifies them; when we encounter the `if` keyword, we
/// automatically know there is only one correct way to parse the expression.
///
/// In addition, all atom expressions are self-contained. This means that atom expressions within a
/// larger [`Expr`] can be replaced with semantically equivalent, but different variants of atom
/// expressions, and the larger expression will still be valid.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Atom {
    /// A literal value.
    Literal(Literal),

    /// A parenthesized expression, such as `(1 + 2)`.
    Paren(Paren),

    /// A blocked expression, such as `{1 + 2}`.
    Block(Block),

    /// A sum expression, such as `sum n in 1..10 of n`.
    Sum(Sum),

    /// A product expression, such as `product n in 1..10 of n`.
    Product(Product),

    /// An if expression, such as `if x > 0 then x else -x`.
    If(If),

    /// A loop expression, as in `loop { ... }`.
    Loop(Loop),

    /// A while loop expression, as in `while x > 0 then { ... }`.
    While(While),

    /// A for loop expression, as in `for i in 0..10 then print(i)`.
    For(For),

    /// A then expression, as in `then x += 1`.
    Then(Then),

    /// An of expression, as in `of x`.
    Of(Of),

    /// A break expression, used to exit a loop, optionally with a value.
    Break(Break),

    /// A continue expression, used to skip the rest of a loop iteration.
    Continue(Continue),

    /// A return expression, as in `return x`, used to return a value from a function.
    Return(Return),
}

impl<'source> Parse<'source> for Atom {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        #[inline]
        fn parse_no_branch<'source, T: Parse<'source>>(input: &mut Parser<'source>) -> ParseResult<T> {
            input.try_parse_with_state::<_, T>(|state| {
                state.allow_then = false;
                state.allow_of = false;
            })
        }

        // definitely not every `parse_no_then` is needed, but it's safest to just try them all
        // all this is to catch funny business like `if x > 0 { then x }`, where `then` is part
        // of the body, not directly after the condition; that's invalid
        let _ = return_if_ok!(parse_no_branch(input).map(Self::Literal).forward_errors(recoverable_errors));
        let _ = return_if_ok!(parse_no_branch(input).map(Self::Paren).forward_errors(recoverable_errors));
        let _ = return_if_ok!(parse_no_branch(input).map(Self::Block).forward_errors(recoverable_errors));
        let _ = return_if_ok!(parse_no_branch(input).map(Self::Sum).forward_errors(recoverable_errors));
        let _ = return_if_ok!(parse_no_branch(input).map(Self::Product).forward_errors(recoverable_errors));
        let _ = return_if_ok!(parse_no_branch(input).map(Self::If).forward_errors(recoverable_errors));
        let _ = return_if_ok!(parse_no_branch(input).map(Self::Loop).forward_errors(recoverable_errors));
        let _ = return_if_ok!(parse_no_branch(input).map(Self::While).forward_errors(recoverable_errors));
        let _ = return_if_ok!(parse_no_branch(input).map(Self::For).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse_with_state::<_, _>(|state| {
            state.allow_of = false;
        }).map(Self::Then).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse_with_state::<_, _>(|state| {
            state.allow_then = false;
        }).map(Self::Of).forward_errors(recoverable_errors));
        let _ = return_if_ok!(parse_no_branch(input).map(Self::Break).forward_errors(recoverable_errors));
        let _ = return_if_ok!(parse_no_branch(input).map(Self::Continue).forward_errors(recoverable_errors));
        parse_no_branch(input).map(Self::Return).forward_errors(recoverable_errors)
    }
}

impl From<Atom> for Primary {
    fn from(atom: Atom) -> Self {
        match atom {
            Atom::Literal(literal) => Self::Literal(literal),
            Atom::Paren(paren) => Self::Paren(paren),
            Atom::Block(block) => Self::Block(block),
            Atom::Sum(sum) => Self::Sum(sum),
            Atom::Product(product) => Self::Product(product),
            Atom::If(if_expr) => Self::If(if_expr),
            Atom::Loop(loop_expr) => Self::Loop(loop_expr),
            Atom::While(while_expr) => Self::While(while_expr),
            Atom::For(for_expr) => Self::For(for_expr),
            Atom::Then(then) => Self::Then(then),
            Atom::Of(of) => Self::Of(of),
            Atom::Break(break_expr) => Self::Break(break_expr),
            Atom::Continue(continue_expr) => Self::Continue(continue_expr),
            Atom::Return(return_expr) => Self::Return(return_expr),
        }
    }
}

impl From<Atom> for Expr {
    fn from(atom: Atom) -> Self {
        match atom {
            Atom::Literal(literal) => Self::Literal(literal),
            Atom::Paren(paren) => Self::Paren(paren),
            Atom::Block(block) => Self::Block(block),
            Atom::Sum(sum) => Self::Sum(sum),
            Atom::Product(product) => Self::Product(product),
            Atom::If(if_expr) => Self::If(if_expr),
            Atom::Loop(loop_expr) => Self::Loop(loop_expr),
            Atom::While(while_expr) => Self::While(while_expr),
            Atom::For(for_expr) => Self::For(for_expr),
            Atom::Then(then) => Self::Then(then),
            Atom::Of(of) => Self::Of(of),
            Atom::Break(break_expr) => Self::Break(break_expr),
            Atom::Continue(continue_expr) => Self::Continue(continue_expr),
            Atom::Return(return_expr) => Self::Return(return_expr),
        }
    }
}
