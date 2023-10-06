/// A representation of mathematical expressions that is easier to manipulate than an AST.
///
/// The [`Expr`] type from `cas_parser` is a recursive `enum` that represents the AST of a
/// mathematical expression. It's convenient for parsing, but not so much for algebraic
/// manipulation.
///
/// This module defines [`MathExpr`], a type that stores additional information about the
/// expression, such as the terms and factors that make it up. It simplifies the situation by
/// recursively flattening the AST into a vector of terms or factors, depending on the operation,
/// and normalizing the expression into a sum of products.

use cas_parser::parser::{ast::{expr::Expr as AstExpr, literal::Literal}, token::op::{BinOpKind, UnaryOpKind}};
use std::ops::Mul;

/// A single term / factor, such as a number, variable, or function call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primary {
    /// A numerical literal, such as `2`, `3.14`, or `4/7`.
    Number(String),

    /// A variable, such as `x` or `y`.
    Symbol(String),

    /// A function call, such as `sin(x)` or `f(x, y)`.
    Call(String, Vec<Expr>),
}

/// A mathematical expression with information about its terms and factors.
#[derive(Debug, Clone, PartialEq, Eq)]
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

impl From<AstExpr> for Expr {
    fn from(expr: AstExpr) -> Self {
        match expr {
            AstExpr::Literal(literal) => match literal {
                Literal::Number(num) => Self::Primary(Primary::Number(num.value)),
                Literal::Radix(_) => todo!(),
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
                        let mut factors = Vec::new();
                        factors.push(Self::Primary(Primary::Number(String::from("-1"))));
                        factors.push(Self::from(*unary.operand));
                        Self::Mul(factors)
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
                            Box::new(Self::Primary(Primary::Number(String::from("-1")))),
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
                        let rhs = Self::Primary(Primary::Number(String::from("-1")))
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

/// Multiplies two [`MathExpr`]s together. No simplification is done, except for the case where the
/// operands are a [`MathExpr::Number`] and a [`MathExpr::Mul`], in which case the number is added
/// to the list of factors (flattening).
impl Mul for Expr {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Primary(Primary::Number(num)), Self::Mul(mut other))
                | (Self::Mul(mut other), Self::Primary(Primary::Number(num))) => {
                other.push(Self::Primary(Primary::Number(num)));
                Self::Mul(other)
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
    fn complicated_expr() {
        let input = String::from("3x - (x+t)y - z*a^(1/5/6)*b");
        let expr = Parser::new(&input).try_parse_full::<AstExpr>().unwrap();
        let math_expr = Expr::from(expr);

        // NOTE: the order of the terms and factors is not guaranteed, but the output is still
        // semantically correct
        assert_eq!(math_expr, Expr::Add(vec![
            // 3 * x
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("x"))),
                Expr::Primary(Primary::Number(String::from("3"))),
            ]),
            // + -1 * (x + t) * y
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("y"))),
                Expr::Add(vec![
                    Expr::Primary(Primary::Symbol(String::from("t"))),
                    Expr::Primary(Primary::Symbol(String::from("x"))),
                ]),
                Expr::Primary(Primary::Number(String::from("-1"))),
            ]),
            // + -1 * z * a^(1/5/6) * b
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("b"))),
                Expr::Exp(
                    Box::new(Expr::Primary(Primary::Symbol(String::from("a")))),
                    Box::new(Expr::Mul(vec![
                        Expr::Primary(Primary::Number(String::from("1"))),
                        Expr::Exp(
                            Box::new(Expr::Primary(Primary::Number(String::from("5")))),
                            Box::new(Expr::Primary(Primary::Number(String::from("-1")))),
                        ),
                        Expr::Exp(
                            Box::new(Expr::Primary(Primary::Number(String::from("6")))),
                            Box::new(Expr::Primary(Primary::Number(String::from("-1")))),
                        ),
                    ])),
                ),
                Expr::Primary(Primary::Symbol(String::from("z"))),
                Expr::Primary(Primary::Number(String::from("-1"))),
            ]),
        ]));
    }
}
