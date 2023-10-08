//! A representation of mathematical expressions that is easier to manipulate than an AST.
//!
//! The [`Expr`](cas_parser::parser::ast::expr::Expr) type from `cas_parser` is a recursive `enum`
//! that represents the AST of a mathematical expression. It's convenient for parsing, but not so
//! much for algebraic manipulation.
//!
//! This module defines a separate [`Expr`](crate::algebra::expr::Expr), a type that stores
//! additional information about the expression, such as the terms and factors that make it up. It
//! simplifies the situation by recursively flattening the AST into a vector of terms or factors,
//! depending on the operation, and normalizing the expression into a sum of products.
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
use std::ops::{Add, AddAssign, Mul};

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
    /// If the expression is an [`Primary::Number`], returns a reference to the contained number.
    pub fn as_number(&self) -> Option<&Float> {
        match self {
            Self::Primary(Primary::Number(num)) => Some(num),
            _ => None,
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
                        Self::Mul(vec![
                            Self::Primary(Primary::Number(float(-1))),
                            Self::from(*unary.operand),
                        ])
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
                        let mut factors = Vec::new();
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
                                        let math_expr = Self::from(AstExpr::Binary(bin));
                                        if let Self::Mul(mul) = math_expr {
                                            factors.extend(mul);
                                        } else {
                                            factors.push(math_expr);
                                        }
                                    }
                                },
                                _ => {
                                    // same as above
                                    let math_expr = Self::from(bin);
                                    if let Self::Mul(mul) = math_expr {
                                        factors.extend(mul);
                                    } else {
                                        factors.push(math_expr);
                                    }
                                },
                            }
                        }
                        Self::Mul(factors)
                    },
                    BinOpKind::Div => {
                        // treat this as lhs*rhs^-1
                        let mut factors = Vec::new();

                        // add lhs factors, flattening `MathExpr::Mul`s if necessary
                        let lhs_factors = Self::from(*bin.lhs);
                        if let Self::Mul(mul) = lhs_factors {
                            factors.extend(mul);
                        } else {
                            factors.push(lhs_factors);
                        }

                        // don't do the same for rhs? TODO
                        let rhs = Self::Exp(
                            Box::new(Expr::from(*bin.rhs)),
                            Box::new(Self::Primary(Primary::Number(float(-1)))),
                        );
                        factors.push(rhs);

                        Self::Mul(factors)
                    },
                    BinOpKind::Mod => todo!(),
                    BinOpKind::Add => {
                        // iteratively flatten binary expressions into terms
                        let mut terms = Vec::new();
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
                                        let math_expr = Self::from(AstExpr::Binary(bin));
                                        if let Self::Add(add) = math_expr {
                                            terms.extend(add);
                                        } else {
                                            terms.push(math_expr);
                                        }
                                    }
                                },
                                _ => {
                                    // same as above
                                    let math_expr = Self::from(bin);
                                    if let Self::Add(add) = math_expr {
                                        terms.extend(add);
                                    } else {
                                        terms.push(math_expr);
                                    }
                                },
                            }
                        }
                        Self::Add(terms)
                    },
                    BinOpKind::Sub => {
                        // treat this as lhs + -1 * rhs
                        let mut terms = Vec::new();

                        // add lhs terms, flattening `MathExpr::Add`s if necessary
                        let lhs_terms = Self::from(*bin.lhs);
                        if let Self::Add(add) = lhs_terms {
                            terms.extend(add);
                        } else {
                            terms.push(lhs_terms);
                        }

                        // don't do the same for rhs? TODO
                        let rhs = Self::Primary(Primary::Number(float(-1)))
                            * Self::from(*bin.rhs);
                        terms.push(rhs);

                        Self::Add(terms)
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
            (Self::Primary(primary), Self::Mul(mut other))
                | (Self::Mul(mut other), Self::Primary(primary)) => {
                other.push(Self::Primary(primary));
                Self::Mul(other)
            },
            (Self::Mul(mut factors), Self::Mul(other)) => {
                factors.extend(other);
                Self::Mul(factors)
            },
            (lhs, rhs) => Self::Mul(vec![lhs, rhs]),
        }
    }
}

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
                Box::new(Expr::Mul(vec![
                    Expr::Primary(Primary::Number(float(-1))),
                    Expr::Primary(Primary::Number(float(3))),
                ])),
            ),
            // * x^2
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol(String::from("x")))),
                Box::new(Expr::Primary(Primary::Number(float(2)))),
            ),
            // * -1
            Expr::Primary(Primary::Number(float(-1))),
            // * 2
            Expr::Primary(Primary::Number(float(2))),
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
}
