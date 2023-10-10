//! A representation of mathematical expressions that is easier to manipulate than an AST.
//!
//! The [`Expr`](cas_parser::parser::ast::expr::Expr) type from `cas_parser` is a recursive `enum`
//! that represents the AST of a mathematical expression. It's convenient for parsing, but not so
//! much for algebraic manipulation.
//!
//! This module defines a separate [`Expr`](crate::algebra::expr::Expr), a type that stores
//! additional information about the expression, such as the terms and factors that make it up. It
//! simplifies the AST by recursively flattening it into a list of terms or factors, depending on
//! the operation, and normalizing the expression into a sum of products.
//!
//! All algebra submodules in this crate that deal with algebraic manipulation will use
//! [`Expr`](crate::algebra::expr::Expr), and any occurrences of the word `expression` will refer
//! to this type.
//!
//! # Strict equality
//!
//! A common problem that arises in symbolic computation is determining if two expressions are
//! semantically / mathematically equal, in order to determine if terms / factors are similar
//! enough to be combined, for example. However, this is extremely difficult to do, because there
//! are an infinite number of ways to represent the same expression.
//!
//! Consider pairs of expressions such as `x^2 + 2x + 1` and `(x + 1)^2`, or `cos(2x)` and
//! `cos(x)^2 - sin(x)^2`. Both pairs are semantically equal, but it is impossible to check this
//! without first applying expansion / factoring / simplification. As you can see, this is a bit of
//! a chicken-and-egg problem. To simplify, we need to check semantic equality, but to check
//! semantic equality, we need to simplify!
//!
//! To alleviate these issues, we define a subset of semantic equality for expressions, called
//! **strict equality**. We define two expressions to be strictly equal if:
//!
//! - They are the same type of expression (i.e. both [`Expr::Primary`], both [`Expr::Add`], etc.).
//! - If both are [`Expr::Primary`], both expressions must have strictly equal values.
//! - If both are [`Expr::Add`] or [`Expr::Mul`], both expressions must have strictly equal terms /
//! factors, in any order.
//! - If both are [`Expr::Exp`], both expressions must have strictly equal base and exponent.
//!
//! Strict equality is not the same as semantic / mathematical equality. For the pairs of
//! expressions listed above, `x^2 + 2x + 1` and `(x + 1)^2`, and `cos(2x)` and `cos(x)^2 - sin(x)^2`
//! would **not** be considered strictly equal.
//!
//! However, because strict equality is a subset of semantic equality, strict equality can
//! **never** report false positives. If two expressions are strictly equal, then they must be
//! semantically equal. More importantly, strict equality is intended to be simple and fast to
//! compute, and it does not depend on any simplification to work. This means that strict equality
//! can be used **in conjunction** with simplification to determine if two expressions are similar
//! enough to be combined.
//!
//! The [`PartialEq`] and [`Eq`] implementation for [`Expr`](crate::algebra::expr::Expr) implements
//! strict equality, not semantic equality.

mod iter;

use cas_eval::{funcs::from_str_radix, consts::{float, float_from_str}};
use cas_parser::parser::{
    ast::{expr::Expr as AstExpr, literal::Literal},
    token::op::{BinOpKind, UnaryOpKind},
};
use iter::ExprIter;
use rug::Float;
use std::ops::{Add, AddAssign, Mul, MulAssign};

/// A single term / factor, such as a number, variable, or function call.
#[derive(Debug, Clone, PartialEq)]
pub enum Primary {
    /// A numerical literal, such as `2` or `3.14`.
    Number(Float),

    /// A variable, such as `x` or `y`.
    Symbol(String),

    /// A function call, such as `sin(x)` or `f(x, y)`.
    Call(String, Vec<Expr>),
}

/// [`Eq`] is implemented manually to allow comparing [`Primary::Number`]s. This module **must
/// never** produce non-normal [`Float`]s (such as `NaN` or `Infinity`)! Report any bugs that cause
/// this to happen.
impl Eq for Primary {}

/// Adds two [`Primary`]s together. If both are [`Primary::Number`]s, the numbers are added
/// together. Otherwise, the two [`Primary`]s are wrapped in an [`Expr::Add`].
impl Add<Primary> for Primary {
    type Output = Expr;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primary::Number(lhs), Primary::Number(rhs)) => {
                Expr::Primary(Primary::Number(lhs + rhs))
            },
            (lhs, rhs) => Expr::Add(vec![
                Expr::Primary(lhs),
                Expr::Primary(rhs),
            ]),
        }
    }
}

/// Multiplies two [`Primary`]s together. If both are [`Primary::Number`]s, the numbers are
/// multiplied together. Otherwise, the two [`Primary`]s are wrapped in an [`Expr::Mul`].
impl Mul<Primary> for Primary {
    type Output = Expr;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primary::Number(lhs), Primary::Number(rhs)) => {
                Expr::Primary(Primary::Number(lhs * rhs))
            },
            (lhs, rhs) => Expr::Mul(vec![
                Expr::Primary(lhs),
                Expr::Primary(rhs),
            ]),
        }
    }
}

/// A mathematical expression with information about its terms and factors.
///
/// This type should be distinguished from the [`cas_parser::parser::ast::Expr`] type, which is
/// produced by [`cas_parser`]. The main difference is that this type **flattens** out the tree
/// structure. For example, the expression `x + (y + z)` would be represented internally as a
/// single [`Expr::Add`] node with _three_ children, `x`, `y`, and `z`.
///
/// For more information about this type, see the [module-level documentation](crate::algebra).
#[derive(Debug, Clone, Eq)]
pub enum Expr {
    /// A single term or factor.
    Primary(Primary),

    /// Multiple terms added together.
    Add(Vec<Expr>),

    /// Multiple factors multiplied together.
    Mul(Vec<Expr>),

    /// An expression raised to a power.
    Exp(Box<Expr>, Box<Expr>),
}

impl Expr {
    /// Returns true if the expression only contains [`Primary::Number`]s (no other [`Primary`]s).
    pub fn is_constant(&self) -> bool {
        self.post_order_iter()
            .all(|expr| {
                matches!(expr,
                    Self::Primary(Primary::Number(_))
                    | Self::Add(_)
                    | Self::Mul(_)
                    | Self::Exp(_, _)
                )
            })
    }

    /// If the expression is a [`Primary::Number`], returns a reference to the contained number.
    pub fn as_number(&self) -> Option<&Float> {
        match self {
            Self::Primary(Primary::Number(num)) => Some(num),
            _ => None,
        }
    }

    /// If the expression is a [`Primary::Number`], returns the contained number.
    pub fn into_number(self) -> Option<Float> {
        match self {
            Self::Primary(Primary::Number(num)) => Some(num),
            _ => None,
        }
    }

    /// Returns true if the expression is a [`Primary::Number`].
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Primary(Primary::Number(_)))
    }

    /// Returns true if the expression is a [`Primary::Number`] raised to the power of -1.
    pub fn is_number_recip(&self) -> bool {
        if let Self::Exp(base, exp) = self {
            if matches!(&**base, Self::Primary(Primary::Number(_))) {
                if let Self::Primary(Primary::Number(exp)) = &**exp {
                    return exp == &-1;
                }
            }
        }

        false
    }

    /// If the expression is a [`Primary::Number`] raised to the power of -1, returns a reference to
    /// the contained number (the left-hand side of the fraction).
    pub fn as_number_recip(&self) -> Option<&Float> {
        if let Self::Exp(base, exp) = self {
            if matches!(&**base, Self::Primary(Primary::Number(_))) {
                if let Self::Primary(Primary::Number(exp)) = &**exp {
                    if exp == &-1 {
                        return base.as_number();
                    }
                }
            }
        }

        None
    }

    /// If the expression is a [`Primary::Symbol`], returns a reference to the contained symbol.
    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Self::Primary(Primary::Symbol(sym)) => Some(sym),
            _ => None,
        }
    }

    /// Trivially downgrades the expression into a simpler form.
    ///
    /// Some operations may result in an [`Expr::Add`] with zero / one term, or an [`Expr::Mul`]
    /// with zero / one factor. This function checks for these cases and simplifies the expression
    /// into the single term / factor, or an [`Expr::Primary`] containing the number 0 or 1.
    pub(crate) fn downgrade(self) -> Self {
        match self {
            Self::Add(mut terms) => {
                if terms.is_empty() {
                    Self::Primary(Primary::Number(float(0)))
                } else if terms.len() == 1 {
                    terms.remove(0)
                } else {
                    Self::Add(terms)
                }
            },
            Self::Mul(mut factors) => {
                if factors.is_empty() {
                    Self::Primary(Primary::Number(float(1)))
                } else if factors.len() == 1 {
                    factors.remove(0)
                } else {
                    Self::Mul(factors)
                }
            },
            _ => self,
        }
    }

    /// Returns an iterator that traverses the tree of expressions in left-to-right post-order
    /// (i.e. depth-first).
    pub fn post_order_iter(&self) -> ExprIter {
        ExprIter::new(self)
    }
}

/// Checks if two expressions are **strictly** equal.
///
/// Two expressions are strictly equal if:
/// - They are the same type of expression (i.e. both [`Expr::Primary`], both [`Expr::Add`], etc.).
/// - If both are [`Expr::Primary`], both expressions must have strictly equal values.
/// - If both are [`Expr::Add`] or [`Expr::Mul`], both expressions must have strictly equal terms /
/// factors, in any order.
/// - If both are [`Expr::Exp`], both expressions must have strictly equal base and exponent.
///
/// For more information about strict equality, see the [module-level documentation](crate::algebra::expr).
impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Primary(lhs), Self::Primary(rhs)) => lhs == rhs,
            (Self::Add(lhs), Self::Add(rhs)) | (Self::Mul(lhs), Self::Mul(rhs)) => {
                lhs.len() == rhs.len()
                    && lhs.iter().all(|lhs| rhs.contains(lhs))
            },
            (Self::Exp(lhs_base, lhs_exp), Self::Exp(rhs_base, rhs_exp)) => {
                lhs_base == rhs_base && lhs_exp == rhs_exp
            },
            _ => false,
        }
    }
}

impl From<AstExpr> for Expr {
    fn from(expr: AstExpr) -> Self {
        match expr {
            AstExpr::Literal(literal) => match literal {
                Literal::Number(num) => Self::Primary(Primary::Number(float_from_str(&num.value))),
                Literal::Radix(radix) => Self::Primary(Primary::Number(float(from_str_radix(&radix.value, radix.base)))),
                Literal::Symbol(sym) => Self::Primary(Primary::Symbol(sym.name)),
            },
            AstExpr::Paren(paren) => Self::from(paren.into_innermost()),
            AstExpr::Block(_) => todo!(),
            AstExpr::If(_) => todo!(),
            AstExpr::Loop(_) => todo!(),
            AstExpr::While(_) => todo!(),
            AstExpr::Break(_) => todo!(),
            AstExpr::Continue(_) => todo!(),
            AstExpr::Call(call) => {
                let args = call.args.into_iter().map(Self::from).collect();
                Self::Primary(Primary::Call(call.name.name, args))
            },
            AstExpr::Unary(unary) => {
                match unary.op.kind {
                    UnaryOpKind::Neg => {
                        // treat this as -1 * rhs
                        Self::Primary(Primary::Number(float(-1)))
                            * Self::from(*unary.operand)
                    },
                    _ => todo!(),
                }
            },
            AstExpr::Binary(bin) => {
                match bin.op.kind {
                    BinOpKind::Exp => {
                        Self::Exp(Box::new(Self::from(*bin.lhs)), Box::new(Self::from(*bin.rhs)))
                    },
                    BinOpKind::Mul => {
                        // iteratively flatten binary expressions into factors
                        // because the AST obviously exists, `factors` will never end up as a
                        // `Expr::Mul` with zero factors
                        let mut factors = Self::Mul(Vec::new());
                        let mut stack = vec![AstExpr::Binary(bin)];
                        while let Some(bin) = stack.pop() {
                            match bin {
                                AstExpr::Binary(bin) => {
                                    if bin.op.kind == BinOpKind::Mul {
                                        stack.push(*bin.lhs);
                                        stack.push(*bin.rhs);
                                    } else {
                                        // if the generated `MathExpr` is another `MathExpr::Mul`,
                                        // add its factors to the current list of factors instead
                                        // we call this "flattening" the expression
                                        factors *= Self::from(AstExpr::Binary(bin));
                                    }
                                },
                                expr => {
                                    // same as above
                                    factors *= Self::from(expr);
                                },
                            }
                        }
                        factors
                    },
                    BinOpKind::Div => {
                        // treat this as lhs*rhs^-1
                        // add lhs factors, flattening `MathExpr::Mul`s if necessary
                        Self::from(*bin.lhs) *
                            Self::Exp(
                                Box::new(Expr::from(*bin.rhs)),
                                Box::new(Self::Primary(Primary::Number(float(-1))))
                            )
                    },
                    BinOpKind::Mod => todo!(),
                    BinOpKind::Add => {
                        // iteratively flatten binary expressions into terms
                        // because the AST obviously exists, `terms` will never end up as a
                        // `Expr::Add` with zero terms
                        let mut terms = Self::Add(Vec::new());
                        let mut stack = vec![AstExpr::Binary(bin)];
                        while let Some(bin) = stack.pop() {
                            match bin {
                                AstExpr::Binary(bin) => {
                                    if bin.op.kind == BinOpKind::Add {
                                        stack.push(*bin.lhs);
                                        stack.push(*bin.rhs);
                                    } else {
                                        // if the generated `MathExpr` is another `MathExpr::Add`,
                                        // add its terms to the current list of terms instead
                                        // we call this "flattening" the expression
                                        terms += Self::from(AstExpr::Binary(bin));
                                    }
                                },
                                _ => {
                                    // same as above
                                    terms += Self::from(bin);
                                },
                            }
                        }
                        terms
                    },
                    BinOpKind::Sub => {
                        // treat this as lhs + -1 * rhs
                        // add lhs and rhs terms, flattening `MathExpr::Add`s if necessary
                        Self::from(*bin.lhs) +
                            Self::Primary(Primary::Number(float(-1))) * Self::from(*bin.rhs)
                    },
                    BinOpKind::BitRight => todo!(),
                    BinOpKind::BitLeft => todo!(),
                    BinOpKind::BitAnd => todo!(),
                    BinOpKind::BitOr => todo!(),
                    BinOpKind::Greater => todo!(),
                    BinOpKind::GreaterEq => todo!(),
                    BinOpKind::Less => todo!(),
                    BinOpKind::LessEq => todo!(),
                    BinOpKind::Eq => todo!(),
                    BinOpKind::NotEq => todo!(),
                    BinOpKind::ApproxEq => todo!(),
                    BinOpKind::ApproxNotEq => todo!(),
                    BinOpKind::And => todo!(),
                    BinOpKind::Or => todo!(),
                }
            },
            AstExpr::Assign(_) => todo!(),
        }
    }
}

/// Adds two [`Expr`]s together. No simplification is done, except for the case where the operands
/// are a mix of [`Primary`] and / or [`Expr::Add`], in which case both are combined in one list
/// of terms (flattening).
impl Add for Expr {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Primary(lhs), Self::Primary(rhs)) => lhs + rhs,
            (Self::Add(mut terms), Self::Add(rhs_terms)) => {
                terms.extend(rhs_terms);
                Self::Add(terms)
            },
            (Self::Add(mut terms), other) | (other, Self::Add(mut terms)) => {
                terms.push(other);
                Self::Add(terms)
            },
            (lhs, rhs) => Self::Add(vec![lhs, rhs]),
        }
    }
}

impl AddAssign for Expr {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.clone() + rhs;
    }
}

/// Multiplies two [`Expr`]s together. No simplification is done, except for the case where the
/// operands are a mix of [`Primary`] and / or [`Expr::Mul`], in which case both are combined in
/// one list of factors (flattening).
impl Mul for Expr {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Primary(lhs), Self::Primary(rhs)) => lhs * rhs,
            (Self::Mul(mut factors), Self::Mul(other)) => {
                factors.extend(other);
                Self::Mul(factors)
            },
            (Self::Mul(mut factors), other) | (other, Self::Mul(mut factors)) => {
                factors.push(other);
                Self::Mul(factors)
            },
            (lhs, rhs) => Self::Mul(vec![lhs, rhs]),
        }
    }
}

impl MulAssign for Expr {
    fn mul_assign(&mut self, rhs: Self) {
        *self = self.clone() * rhs;
    }
}

/// NOTE: The output of `pretty_assertions` for failing tests is horrifically ugly, mainly because
/// we're utilizing strict equality here. Strict equality allows different orderings of terms and
/// factors, but `pretty_assertions` doesn't care about order. If a test fails and we put the
/// expected terms and factors in a different order than the generated terms and factors, the
/// output will be a mess. Just a forewarning!
#[cfg(test)]
mod tests {
    use cas_parser::parser::{ast::expr::Expr as AstExpr, Parser};
    use pretty_assertions::assert_eq;
    use super::*;

    #[test]
    fn strict_equality() {
        let a = String::from("2(x + (y - 5))");
        let b = String::from("(y - 5 + x) * 2");
        let a_expr = Parser::new(&a).try_parse_full::<AstExpr>().unwrap();
        let b_expr = Parser::new(&b).try_parse_full::<AstExpr>().unwrap();
        let a_math_expr = Expr::from(a_expr);
        let b_math_expr = Expr::from(b_expr);
        assert_eq!(a_math_expr, b_math_expr);
    }

    #[test]
    fn strict_equality_2() {
        // these is NOT strictly equal (but are semantically equal)
        // `b` is a simplified version of `a`
        let a = String::from("2(x + (y - 5))");
        let b = String::from("2x + 2y - 10");
        let a_expr = Parser::new(&a).try_parse_full::<AstExpr>().unwrap();
        let b_expr = Parser::new(&b).try_parse_full::<AstExpr>().unwrap();
        let a_math_expr = Expr::from(a_expr);
        let b_math_expr = Expr::from(b_expr);
        assert_ne!(a_math_expr, b_math_expr);
    }

    #[test]
    fn simple_expr() {
        let input = String::from("x^2 + 5x + 6");
        let expr = Parser::new(&input).try_parse_full::<AstExpr>().unwrap();
        let math_expr = Expr::from(expr);

        // NOTE: the order of the terms and factors is not guaranteed, but the output is still
        // semantically correct
        assert_eq!(math_expr, Expr::Add(vec![
            // 6
            Expr::Primary(Primary::Number(float(6))),
            // + 5x
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("x"))),
                Expr::Primary(Primary::Number(float(5))),
            ]),
            // + x^2
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol(String::from("x")))),
                Box::new(Expr::Primary(Primary::Number(float(2)))),
            ),
        ]));
    }

    #[test]
    fn factors_only() {
        let input = String::from("-2x^2y^-3/5");
        let expr = Parser::new(&input).try_parse_full::<AstExpr>().unwrap();
        let math_expr = Expr::from(expr);

        assert_eq!(math_expr, Expr::Mul(vec![
            // y^-3
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol(String::from("y")))),
                Box::new(Expr::Primary(Primary::Number(float(-3)))),
            ),
            // * x^2
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol(String::from("x")))),
                Box::new(Expr::Primary(Primary::Number(float(2)))),
            ),
            // * -2
            Expr::Primary(Primary::Number(float(-2))),
            // / 5
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Number(float(5)))),
                Box::new(Expr::Primary(Primary::Number(float(-1)))),
            ),
        ]));
    }

    #[test]
    fn complicated_expr() {
        let input = String::from("3x - (x+t)y - z*a^(1/5/6)*b");
        let expr = Parser::new(&input).try_parse_full::<AstExpr>().unwrap();
        let math_expr = Expr::from(expr);

        assert_eq!(math_expr, Expr::Add(vec![
            // 3 * x
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("x"))),
                Expr::Primary(Primary::Number(float(3))),
            ]),
            // + -1 * (x + t) * y
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("y"))),
                Expr::Add(vec![
                    Expr::Primary(Primary::Symbol(String::from("t"))),
                    Expr::Primary(Primary::Symbol(String::from("x"))),
                ]),
                Expr::Primary(Primary::Number(float(-1))),
            ]),
            // + -1 * z * a^(1/5/6) * b
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("b"))),
                Expr::Exp(
                    Box::new(Expr::Primary(Primary::Symbol(String::from("a")))),
                    Box::new(Expr::Mul(vec![
                        Expr::Primary(Primary::Number(float(1))),
                        Expr::Exp(
                            Box::new(Expr::Primary(Primary::Number(float(5)))),
                            Box::new(Expr::Primary(Primary::Number(float(-1)))),
                        ),
                        Expr::Exp(
                            Box::new(Expr::Primary(Primary::Number(float(6)))),
                            Box::new(Expr::Primary(Primary::Number(float(-1)))),
                        ),
                    ])),
                ),
                Expr::Primary(Primary::Symbol(String::from("z"))),
                Expr::Primary(Primary::Number(float(-1))),
            ]),
        ]));
    }

    #[test]
    fn complicated_expr_2() {
        let input = String::from("3x^2y - 16x y + 2x^2y - 13x y + 4x y^2 - 11x y^2");
        let expr = Parser::new(&input).try_parse_full::<AstExpr>().unwrap();
        let math_expr = Expr::from(expr);

        assert_eq!(math_expr, Expr::Add(vec![
            // 4 * x * y^2
            Expr::Mul(vec![
                Expr::Exp(
                    Box::new(Expr::Primary(Primary::Symbol(String::from("y")))),
                    Box::new(Expr::Primary(Primary::Number(float(2)))),
                ),
                Expr::Primary(Primary::Symbol(String::from("x"))),
                Expr::Primary(Primary::Number(float(4))),
            ]),
            // + 2 * x^2 * y
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("y"))),
                Expr::Exp(
                    Box::new(Expr::Primary(Primary::Symbol(String::from("x")))),
                    Box::new(Expr::Primary(Primary::Number(float(2)))),
                ),
                Expr::Primary(Primary::Number(float(2))),
            ]),
            // + 3 * x^2 * y
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("y"))),
                Expr::Exp(
                    Box::new(Expr::Primary(Primary::Symbol(String::from("x")))),
                    Box::new(Expr::Primary(Primary::Number(float(2)))),
                ),
                Expr::Primary(Primary::Number(float(3))),
            ]),
            // + -1 * 16 * x * y
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("y"))),
                Expr::Primary(Primary::Symbol(String::from("x"))),
                Expr::Primary(Primary::Number(float(16))),
                Expr::Primary(Primary::Number(float(-1))),
            ]),
            // + -1 * 13 * x * y
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("y"))),
                Expr::Primary(Primary::Symbol(String::from("x"))),
                Expr::Primary(Primary::Number(float(13))),
                Expr::Primary(Primary::Number(float(-1))),
            ]),
            // + -1 * 11 * x * y^2
            Expr::Mul(vec![
                Expr::Exp(
                    Box::new(Expr::Primary(Primary::Symbol(String::from("y")))),
                    Box::new(Expr::Primary(Primary::Number(float(2)))),
                ),
                Expr::Primary(Primary::Symbol(String::from("x"))),
                Expr::Primary(Primary::Number(float(11))),
                Expr::Primary(Primary::Number(float(-1))),
            ]),
        ]));
    }
}
