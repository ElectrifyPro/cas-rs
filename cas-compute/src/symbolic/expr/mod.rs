//! A representation of mathematical expressions that is easier to manipulate than an AST.
//!
//! The [`Expr`](cas_parser::parser::ast::expr::Expr) type from `cas_parser` is a recursive `enum`
//! that represents the AST of a mathematical expression. It's convenient for parsing, but not so
//! much for algebraic manipulation.
//!
//! This module defines a similar but separate [`SymExpr`], a type that stores additional
//! information about the expression that comes in handy during symbolic computation, such as the
//! terms and factors that make it up. It simplifies the AST by recursively flattening it into a
//! list of terms or factors (depending on the operator) and normalizing the expression into a sum
//! of products.
//!
//! All submodules in this crate that deal with symbolic manipulation will use [`SymExpr`], and any
//! occurrences of the word `expression` will refer to this type.
//!
//! # Strict equality
//!
//! A common problem that arises in symbolic computation is determining if two expressions are
//! semantically / mathematically equal, in order to determine if terms / factors are similar
//! enough to be combined, for example. However, this is extremely difficult (i.e. impossible) to
//! do, because there are an infinite number of ways to represent the same expression.
//!
//! Consider pairs of expressions such as `x^2 + 2x + 1` and `(x + 1)^2`, or `cos(2x)` and
//! `cos(x)^2 - sin(x)^2`. Both pairs are semantically equal, but this is not immediately obvious
//! without first applying expansion / factoring / simplification. As you can see, this is a bit of
//! a chicken-and-egg problem. To simplify, we need to check semantic equality, but to check
//! semantic equality, we need to simplify!
//!
//! To alleviate these issues, we define a subset of semantic equality for expressions, called
//! **strict equality**. We define two expressions to be strictly equal if:
//!
//! - They are the same type of expression (i.e. both [`SymExpr::Primary`], both [`SymExpr::Add`],
//!   etc.).
//! - If both are [`SymExpr::Primary`], both expressions must have strictly equal values.
//! - If both are [`SymExpr::Add`] or [`SymExpr::Mul`], both expressions must have strictly equal
//!   terms / factors, **_in any order_**.
//! - If both are [`SymExpr::Exp`], both expressions must have strictly equal base and exponent.
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
//! enough to be combined, resolving the chicken-and-egg problem.
//!
//! ## Strict equality, [`PartialEq`] and [`Hash`]
//!
//! The [`PartialEq`] and [`Eq`] implementations for [`SymExpr`] implement **strict equality**, not
//! semantic equality.
//!
//! The [`Hash`] implementation for [`SymExpr`] also implements **strict equality**, and runs in
//! `O(n)` time, where `n` is the number of sub-expressions contained within the expression.

mod iter;

use crate::primitive::{float_from_str, from_str_radix, int, int_from_str};
use cas_parser::parser::{
    ast::{expr::Expr as AstExpr, literal::Literal},
    token::op::{BinOpKind, Precedence, UnaryOpKind},
};
use iter::ExprIter;
use rug::{Float, Integer};
use std::{
    cmp::Ordering,
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    ops::{Add, AddAssign, Mul, MulAssign, Neg},
};
use super::simplify::fraction::make_fraction;

/// A single term / factor, such as a number, variable, or function call.
#[derive(Debug, Clone, PartialEq)]
pub enum Primary {
    /// An integer, such as `2` or `144`.
    Integer(Integer),

    /// A floating-point number, such as `3.14` or `0.5`.
    Float(Float),

    /// A variable, such as `x` or `y`.
    Symbol(String),

    /// A function call, such as `sin(x)` or `f(x, y)`.
    Call(String, Vec<SymExpr>),
}

/// [`Hash`] is implemented manually to allow hashing [`Primary::Float`]s. This module **must
/// never** produce non-normal [`Float`]s (such as `NaN` or `Infinity`)! Report any bugs that cause
/// this to happen.
impl Hash for Primary {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Integer(int) => int.hash(state),
            // this must be safe for the `Hash` impl to be valid
            Self::Float(float) => float.get_significand().unwrap().hash(state),
            Self::Symbol(sym) => sym.hash(state),
            Self::Call(name, args) => {
                name.hash(state);
                args.hash(state);
            }
        }
    }
}

impl std::fmt::Display for Primary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(num) => write!(f, "{}", num.to_f64()),
            Self::Float(num) => write!(f, "{}", num.to_f64()),
            Self::Symbol(sym) => write!(f, "{}", sym),
            Self::Call(name, args) => {
                write!(f, "{}(", name)?;
                let mut iter = args.iter();
                if let Some(arg) = iter.next() {
                    write!(f, "{}", arg)?;
                    for arg in iter {
                        write!(f, ", {}", arg)?;
                    }
                }
                write!(f, ")")
            },
        }
    }
}

/// [`Eq`] is implemented manually to allow comparing [`Primary::Integer`] and [`Primary::Float`]s.
/// This module **must never** produce non-normal [`Float`]s (such as `NaN` or `Infinity`)! Report
/// any bugs that cause this to happen.
impl Eq for Primary {}

/// Adds two [`Primary`]s together. If both are the **same numeric type**, the numbers are added
/// together. Otherwise, the two [`Primary`]s are wrapped in a [`SymExpr::Add`].
///
/// Note this means that adding an [`Integer`] and a [`Float`] will result in a
/// **[`SymExpr::Add`]**.
impl Add<Primary> for Primary {
    type Output = SymExpr;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primary::Integer(lhs), Primary::Integer(rhs)) => {
                SymExpr::Primary(Primary::Integer(lhs + rhs))
            },
            (Primary::Float(lhs), Primary::Float(rhs)) => {
                SymExpr::Primary(Primary::Float(lhs + rhs))
            },
            (lhs, rhs) => SymExpr::Add(vec![
                SymExpr::Primary(lhs),
                SymExpr::Primary(rhs),
            ]),
        }
    }
}

/// Multiplies two [`Primary`]s together. If both are the **same numeric type**, the numbers are
/// multiplied together. Otherwise, the two [`Primary`]s are wrapped in a [`SymExpr::Mul`].
///
/// Note this means that multiplying an [`Integer`] and a [`Float`] will result in a
/// **[`SymExpr::Mul`]**.
impl Mul<Primary> for Primary {
    type Output = SymExpr;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primary::Integer(lhs), Primary::Integer(rhs)) => {
                SymExpr::Primary(Primary::Integer(lhs * rhs))
            },
            (Primary::Float(lhs), Primary::Float(rhs)) => {
                SymExpr::Primary(Primary::Float(lhs * rhs))
            },
            (lhs, rhs) => SymExpr::Mul(vec![
                SymExpr::Primary(lhs),
                SymExpr::Primary(rhs),
            ]),
        }
    }
}

/// A mathematical expression with information about its terms and factors.
///
/// This type should be distinguished from the [`cas_parser::parser::ast::Expr`] type, which is
/// produced by [`cas_parser`]. The main difference is that this type **flattens** out the tree
/// structure. For example, the expression `x + (y + z)` would be represented internally as a
/// single [`SymExpr::Add`] node with _three_ children, `x`, `y`, and `z`.
///
/// For more information about this type, see the [module-level documentation](self).
#[derive(Debug, Clone, Eq)]
pub enum SymExpr {
    /// A single term or factor.
    Primary(Primary),

    /// Multiple terms added together.
    Add(Vec<SymExpr>),

    /// Multiple factors multiplied together.
    Mul(Vec<SymExpr>),

    /// An expression raised to a power.
    Exp(Box<SymExpr>, Box<SymExpr>),
}

impl std::fmt::Display for SymExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primary(primary) => write!(f, "{}", primary),
            Self::Add(terms) => {
                let mut iter = terms.iter();
                if let Some(term) = iter.next() {
                    write!(f, "{}", term)?;
                    for term in iter {
                        write!(f, " + {}", term)?;
                    }
                }
                Ok(())
            },
            Self::Mul(factors) => {
                let mut iter = factors.iter();
                if let Some(factor) = iter.next() {
                    if matches!(factor.cmp_precedence(self), Ordering::Less) {
                        write!(f, "({})", factor)?;
                    } else {
                        write!(f, "{}", factor)?;
                    }
                    for factor in iter {
                        if matches!(factor.cmp_precedence(self), Ordering::Less) {
                            write!(f, " * ({})", factor)?;
                        } else {
                            write!(f, " * {}", factor)?;
                        }
                    }
                }
                Ok(())
            },
            Self::Exp(base, exp) => {
                if matches!(base.cmp_precedence(self), Ordering::Less) {
                    write!(f, "({})", base)?;
                } else {
                    write!(f, "{}", base)?;
                }
                write!(f, "^")?;
                if matches!(exp.cmp_precedence(self), Ordering::Less) {
                    write!(f, "({})", exp)?;
                } else {
                    write!(f, "{}", exp)?;
                }
                Ok(())
            },
        }
    }
}

impl SymExpr {
    /// Returns the precedence of the expression.
    fn precedence(&self) -> Option<Precedence> {
        match self {
            Self::Primary(_) => None,
            Self::Add(_) => Some(BinOpKind::Add.precedence()),
            Self::Mul(_) => Some(BinOpKind::Mul.precedence()),
            Self::Exp(_, _) => Some(BinOpKind::Exp.precedence()),
        }
    }

    /// Returns true if the given expression has lower precedence than this expression.
    ///
    /// This is used to determine if parentheses are needed around the given expression when
    /// printing.
    pub fn cmp_precedence(&self, other: &Self) -> Ordering {
        #[derive(PartialEq, Eq)]
        enum PrecedenceExt {
            Primary,
            Op(Precedence),
        }

        impl PartialOrd for PrecedenceExt {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for PrecedenceExt {
            fn cmp(&self, other: &Self) -> Ordering {
                match (self, other) {
                    (Self::Primary, Self::Primary) => Ordering::Equal,
                    (Self::Primary, Self::Op(_)) => Ordering::Greater,
                    (Self::Op(_), Self::Primary) => Ordering::Less,
                    (Self::Op(lhs), Self::Op(rhs)) => lhs.cmp(rhs),
                }
            }
        }

        let lhs = self.precedence().map(PrecedenceExt::Op).unwrap_or(PrecedenceExt::Primary);
        let rhs = other.precedence().map(PrecedenceExt::Op).unwrap_or(PrecedenceExt::Primary);
        lhs.cmp(&rhs)
    }

    /// If the expression is a [`Primary::Integer`], returns a reference to the contained integer.
    pub fn as_integer(&self) -> Option<&Integer> {
        match self {
            Self::Primary(Primary::Integer(int)) => Some(int),
            _ => None,
        }
    }

    /// If the expression is a [`Primary::Integer`], returns the contained integer.
    pub fn into_integer(self) -> Option<Integer> {
        match self {
            Self::Primary(Primary::Integer(int)) => Some(int),
            _ => None,
        }
    }

    /// Returns true if the expression is a [`Primary::Integer`].
    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Primary(Primary::Integer(_)))
    }

    /// Returns true if the expression is a [`Primary::Integer`] raised to the power of -1.
    pub fn is_integer_recip(&self) -> bool {
        if let Self::Exp(base, exp) = self {
            if matches!(&**base, Self::Primary(Primary::Integer(_))) {
                if let Self::Primary(Primary::Integer(exp)) = &**exp {
                    return exp == &-1;
                }
            }
        }

        false
    }

    /// If the expression is a [`Primary::Integer`] raised to the power of -1, returns a reference to
    /// the contained integer (the denominator of the fraction).
    pub fn as_integer_recip(&self) -> Option<&Integer> {
        if let Self::Exp(base, exp) = self {
            if matches!(&**base, Self::Primary(Primary::Integer(_))) {
                if let Self::Primary(Primary::Integer(exp)) = &**exp {
                    if exp == &-1 {
                        return base.as_integer();
                    }
                }
            }
        }

        None
    }

    /// If the expression is a [`Primary::Integer`] raised to the power of -1, returns the contained
    /// integer (the denominator of the fraction).
    pub fn into_integer_recip(self) -> Option<Integer> {
        if let Self::Exp(base, exp) = self {
            if matches!(*base, Self::Primary(Primary::Integer(_))) {
                if let Self::Primary(Primary::Integer(exp)) = *exp {
                    if exp == -1 {
                        return base.into_integer();
                    }
                }
            }
        }

        None
    }

    /// Returns true if the expression is a [`Primary::Float`].
    pub fn is_float(&self) -> bool {
        matches!(self, Self::Primary(Primary::Float(_)))
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
    /// Some operations may result in a [`SymExpr::Add`] with zero / one term, or a
    /// [`SymExpr::Mul`] with zero / one factor. This function checks for these cases and
    /// simplifies the expression into the single term / factor, or a [`SymExpr::Primary`]
    /// containing the integer 0 or 1.
    pub(crate) fn downgrade(self) -> Self {
        match self {
            Self::Add(mut terms) => {
                if terms.is_empty() {
                    Self::Primary(Primary::Integer(int(0)))
                } else if terms.len() == 1 {
                    terms.remove(0)
                } else {
                    Self::Add(terms)
                }
            },
            Self::Mul(mut factors) => {
                if factors.is_empty() {
                    Self::Primary(Primary::Integer(int(1)))
                } else if factors.len() == 1 {
                    factors.remove(0)
                } else {
                    Self::Mul(factors)
                }
            },
            _ => self,
        }
    }

    /// Returns the square root of this expression. No simplification is done.
    pub fn sqrt(self) -> Self {
        Self::Exp(
            Box::new(self),
            Box::new(make_fraction(
                Self::Primary(Primary::Integer(int(1))),
                Self::Primary(Primary::Integer(int(2))),
            )),
        )
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
/// - They are the same type of expression (i.e. both [`SymExpr::Primary`], both [`SymExpr::Add`],
///   etc.).
/// - If both are [`SymExpr::Primary`], both expressions must have strictly equal values.
/// - If both are [`SymExpr::Add`] or [`SymExpr::Mul`], both expressions must have strictly equal
///   terms / factors, **_in any order_**.
/// - If both are [`SymExpr::Exp`], both expressions must have strictly equal base and exponent.
///
/// For more information about strict equality, see the [module-level documentation](self).
impl PartialEq for SymExpr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Primary(lhs), Self::Primary(rhs)) => lhs == rhs,
            (Self::Add(_), Self::Add(_)) | (Self::Mul(_), Self::Mul(_)) => {
                // it is only important that if lhs = rhs, then hash(lhs) = hash(rhs) holds; no
                // guarantees on hash inequality if lhs != rhs
                //
                // we uphold this by ensuring that hash(add | mul) implements strict equality
                // correctly, then having hash(lhs) == hash(rhs) => lhs == rhs
                //
                // in this case, it is not possible for lhs = rhs and hash(lhs) != hash(rhs); if
                // lhs = rhs, then the same sub-expressions are hashed and summed together
                let lhs_hash = {
                    let mut hasher = DefaultHasher::new();
                    // NOTE: use `self` and not `Self::Add(lhs)` to utilize commutative hashing,
                    // instead of native Vec<T> hashing
                    self.hash(&mut hasher);
                    hasher.finish()
                };

                let rhs_hash = {
                    let mut hasher = DefaultHasher::new();
                    // NOTE: use `other` and not `Self::Add(rhs)` to utilize commutative hashing,
                    // instead of native Vec<T> hashing
                    other.hash(&mut hasher);
                    hasher.finish()
                };

                lhs_hash == rhs_hash
            },
            (Self::Exp(lhs_base, lhs_exp), Self::Exp(rhs_base, rhs_exp)) => {
                lhs_base == rhs_base && lhs_exp == rhs_exp
            },
            _ => false,
        }
    }
}

/// [`Hash`] is manually implemented for [`SymExpr`] to ensure consistent behavior with
/// [`PartialEq`].
///
/// We also manage to hash [`SymExpr::Add`] and [`SymExpr::Mul`] in `O(n)` time by using a
/// commutative hashing function, unaffected by the order of the elements.
impl Hash for SymExpr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);

        match self {
            SymExpr::Primary(val) => val.hash(state),
            SymExpr::Add(val) | SymExpr::Mul(val) => {
                // for Add and Mul, the order of the elements must not affect the resulting hash
                // this is maintained by hashing each sub-expression individually and using a
                // commutative operation (addition) to combine the hashes
                //
                // as long as the inner hash function is well-distributed and random, adding those
                // hashes together should also be well-distributed and random

                let out: u64 = val
                    .iter()
                    .map(|expr| {
                        let mut hasher = DefaultHasher::new();
                        expr.hash(&mut hasher);
                        hasher.finish()
                    })
                    .fold(0, |acc, curr| acc.wrapping_add(curr));

                out.hash(state);
            }
            SymExpr::Exp(val_base, val_exp) => {
                val_base.hash(state);
                val_exp.hash(state);
            }
        }
    }
}

impl From<AstExpr> for SymExpr {
    fn from(expr: AstExpr) -> Self {
        match expr {
            AstExpr::Literal(literal) => match literal {
                Literal::Integer(int) => Self::Primary(Primary::Integer(int_from_str(&int.value))),
                Literal::Float(float) => Self::Primary(Primary::Float(float_from_str(&float.value))),
                Literal::Radix(radix) => Self::Primary(Primary::Integer(from_str_radix(&radix.value, radix.base))),
                Literal::Boolean(_) => todo!(),
                Literal::Symbol(sym) => Self::Primary(Primary::Symbol(sym.name)),
                Literal::Unit(_) => todo!(),
                Literal::List(_) => todo!(),
                Literal::ListRepeat(_) => todo!(),
            },
            AstExpr::Paren(paren) => Self::from(paren.into_innermost()),
            AstExpr::Block(_) => todo!(),
            AstExpr::Sum(_) => todo!(),
            AstExpr::Product(_) => todo!(),
            AstExpr::If(_) => todo!(),
            AstExpr::Loop(_) => todo!(),
            AstExpr::While(_) => todo!(),
            AstExpr::For(_) => todo!(),
            AstExpr::Then(_) => todo!(),
            AstExpr::Of(_) => todo!(),
            AstExpr::Break(_) => todo!(),
            AstExpr::Continue(_) => todo!(),
            AstExpr::Return(_) => todo!(),
            AstExpr::Call(call) => {
                let args = call.args.into_iter().map(Self::from).collect();
                Self::Primary(Primary::Call(call.name.name, args))
            },
            AstExpr::Index(_) => todo!(),
            AstExpr::Unary(unary) => {
                match unary.op.kind {
                    UnaryOpKind::Neg => {
                        // treat this as -1 * rhs
                        Self::from(*unary.operand).neg()
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
                        make_fraction(
                            Self::from(*bin.lhs),
                            Self::from(*bin.rhs),
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
                            Self::from(*bin.rhs).neg()
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
            AstExpr::Range(_) => todo!(),
        }
    }
}

impl From<SymExpr> for AstExpr {
    fn from(expr: SymExpr) -> Self {
        use cas_parser::parser::{
            ast::{Binary, Call, LitFloat, LitInt, LitSym},
            token::op::BinOp,
        };

        match expr {
            SymExpr::Primary(primary) => match primary {
                Primary::Integer(int) => AstExpr::Literal(Literal::Integer(LitInt {
                    value: int.to_string(),
                    span: 0..0, // TODO: what to do about this?
                })),
                Primary::Float(float) => AstExpr::Literal(Literal::Float(LitFloat {
                    value: float.to_string(),
                    span: 0..0,
                })),
                Primary::Symbol(sym) => AstExpr::Literal(Literal::Symbol(LitSym {
                    name: sym,
                    span: 0..0,
                })),
                Primary::Call(name, args) => AstExpr::Call(Call {
                    name: LitSym { name, span: 0..0 },
                    derivatives: 0,
                    args: args.into_iter().map(Self::from).collect(),
                    span: 0..0,
                    paren_span: 0..0,
                }),
            },
            SymExpr::Add(terms) => {
                let mut iter = terms.into_iter();
                let mut expr = Self::from(iter.next().unwrap());
                for term in iter {
                    expr = AstExpr::Binary(Binary {
                        lhs: Box::new(expr),
                        op: BinOp {
                            kind: BinOpKind::Add,
                            implicit: false,
                            span: 0..0,
                        },
                        rhs: Box::new(Self::from(term)),
                        span: 0..0,
                    });
                }
                expr
            },
            SymExpr::Mul(factors) => {
                let mut iter = factors.into_iter();
                let mut expr = Self::from(iter.next().unwrap());
                for factor in iter {
                    expr = AstExpr::Binary(Binary {
                        lhs: Box::new(expr),
                        op: BinOp {
                            kind: BinOpKind::Mul,
                            implicit: false,
                            span: 0..0,
                        },
                        rhs: Box::new(Self::from(factor)),
                        span: 0..0,
                    });
                }
                expr
            },
            SymExpr::Exp(lhs, rhs) => AstExpr::Binary(Binary {
                lhs: Box::new(Self::from(*lhs)),
                op: BinOp {
                    kind: BinOpKind::Exp,
                    implicit: false,
                    span: 0..0,
                },
                rhs: Box::new(Self::from(*rhs)),
                span: 0..0,
            }),
        }
    }
}

/// Adds two [`SymExpr`]s together. No simplification is done, except for the case where the
/// operands are a mix of [`Primary`] and / or [`SymExpr::Add`], in which case both are combined in
/// one list of terms (flattening).
impl Add for SymExpr {
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

/// Adds two [`SymExpr`]s together. The behavior is the same as [`Add`], except we can reuse the
/// allocated memory of `self` if possible.
impl AddAssign for SymExpr {
    fn add_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (Self::Primary(Primary::Integer(lhs)), Self::Primary(Primary::Integer(rhs))) => {
                *lhs += rhs;
            },
            (Self::Primary(Primary::Float(lhs)), Self::Primary(Primary::Float(rhs))) => {
                *lhs += rhs;
            },
            (Self::Add(terms), Self::Add(rhs_terms)) => {
                terms.extend(rhs_terms);
            },
            (Self::Add(terms), other) => {
                terms.push(other);
            },
            (other, Self::Add(mut terms)) => {
                // SAFETY: same as (lhs, rhs) branch
                unsafe {
                    let owned = std::ptr::read(other);
                    // TODO: doesn't this panic on oom? this could be unsound if it does
                    terms.push(owned);
                    std::ptr::write(other, Self::Add(terms));
                }
            },
            (lhs, rhs) => {
                // SAFETY: we're essentially moving `lhs` into a new `Self::Add` variant, and
                // reassigning the new `Self::Add` to `lhs`. nothing is dropped, since `owned` and
                // `rhs` are moved into `Self::Add`, and neither `write` nor `read` drop their
                // arguments
                unsafe {
                    let owned = std::ptr::read(lhs);
                    std::ptr::write(lhs, Self::Add(vec![owned, rhs]));
                }
            },
        }
    }
}

/// Multiplies two [`SymExpr`]s together. No simplification is done, except for the case where the
/// operands are a mix of [`Primary`] and / or [`SymExpr::Mul`], in which case both are combined in
/// one list of factors (flattening).
impl Mul for SymExpr {
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

impl MulAssign for SymExpr {
    fn mul_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (Self::Primary(Primary::Integer(lhs)), Self::Primary(Primary::Integer(rhs))) => {
                *lhs *= rhs;
            },
            (Self::Primary(Primary::Float(lhs)), Self::Primary(Primary::Float(rhs))) => {
                *lhs *= rhs;
            },
            (Self::Mul(factors), Self::Mul(rhs_factors)) => {
                factors.extend(rhs_factors);
            },
            (Self::Mul(factors), other) => {
                factors.push(other);
            },
            (other, Self::Mul(mut factors)) => {
                // SAFETY: see [`AddAssign`] implementation
                unsafe {
                    let owned = std::ptr::read(other);
                    factors.push(owned);
                    std::ptr::write(other, Self::Mul(factors));
                }
            },
            (lhs, rhs) => {
                // SAFETY: see [`AddAssign`] implementation
                unsafe {
                    let owned = std::ptr::read(lhs);
                    std::ptr::write(lhs, Self::Mul(vec![owned, rhs]));
                }
            },
        }
    }
}

/// Multiplies this expression by -1. No simplification is done, except for the case where the
/// expression is a numeric [`Primary`], in which case the number is negated.
impl Neg for SymExpr {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Primary(Primary::Integer(int)) => Self::Primary(Primary::Integer(-int)),
            Self::Primary(Primary::Float(float)) => Self::Primary(Primary::Float(-float)),
            expr => Self::Primary(Primary::Integer(int(-1))) * expr,
        }
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
    use crate::symbolic::simplify;

    use super::*;

    /// Parse the given expression and return the [`Expr`] representation.
    fn parse_expr(input: &str) -> SymExpr {
        let expr = Parser::new(input).try_parse_full::<AstExpr>().unwrap();
        SymExpr::from(expr)
    }

    /// Get the hash of the given [`Expr`].
    fn hash(expr: &SymExpr) -> u64 {
        let mut hasher = DefaultHasher::new();
        expr.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn strict_equality() {
        let a = parse_expr("2(x + (y - 5))");
        let b = parse_expr("(y - 5 + x) * 2");
        assert_eq!(a, b);
    }

    #[test]
    fn strict_equality_2() {
        // these are NOT strictly equal (but are semantically equal)
        // `b` is a simplified version of `a`
        let a = parse_expr("2(x + (y - 5))");
        let b = parse_expr("2x + 2y - 10");
        assert_ne!(a, b);
    }

    #[test]
    fn simple_expr() {
        let expr = parse_expr("x^2 + 5x + 6");

        // NOTE: the order of the terms and factors is not guaranteed, but the output is still
        // semantically correct
        assert_eq!(expr, SymExpr::Add(vec![
            // 6
            SymExpr::Primary(Primary::Integer(int(6))),
            // + 5x
            SymExpr::Mul(vec![
                SymExpr::Primary(Primary::Symbol(String::from("x"))),
                SymExpr::Primary(Primary::Integer(int(5))),
            ]),
            // + x^2
            SymExpr::Exp(
                Box::new(SymExpr::Primary(Primary::Symbol(String::from("x")))),
                Box::new(SymExpr::Primary(Primary::Integer(int(2)))),
            ),
        ]));
    }

    #[test]
    fn factors_only() {
        let expr = parse_expr("-2x^2y^-3/5");
        assert_eq!(expr, SymExpr::Mul(vec![
            // y^-3
            SymExpr::Exp(
                Box::new(SymExpr::Primary(Primary::Symbol(String::from("y")))),
                Box::new(SymExpr::Primary(Primary::Integer(int(-3)))),
            ),
            // * x^2
            SymExpr::Exp(
                Box::new(SymExpr::Primary(Primary::Symbol(String::from("x")))),
                Box::new(SymExpr::Primary(Primary::Integer(int(2)))),
            ),
            // * -2
            SymExpr::Primary(Primary::Integer(int(-2))),
            // / 5
            SymExpr::Exp(
                Box::new(SymExpr::Primary(Primary::Integer(int(5)))),
                Box::new(SymExpr::Primary(Primary::Integer(int(-1)))),
            ),
        ]));
    }

    #[test]
    fn complicated_expr() {
        let expr = parse_expr("3x - (x+t)y - z*a^(1/5/6)*b");
        assert_eq!(expr, SymExpr::Add(vec![
            // 3 * x
            SymExpr::Mul(vec![
                SymExpr::Primary(Primary::Symbol(String::from("x"))),
                SymExpr::Primary(Primary::Integer(int(3))),
            ]),
            // + -1 * (x + t) * y
            SymExpr::Mul(vec![
                SymExpr::Primary(Primary::Symbol(String::from("y"))),
                SymExpr::Add(vec![
                    SymExpr::Primary(Primary::Symbol(String::from("t"))),
                    SymExpr::Primary(Primary::Symbol(String::from("x"))),
                ]),
                SymExpr::Primary(Primary::Integer(int(-1))),
            ]),
            // + -1 * z * a^(1/5/6) * b
            SymExpr::Mul(vec![
                SymExpr::Primary(Primary::Symbol(String::from("b"))),
                SymExpr::Exp(
                    Box::new(SymExpr::Primary(Primary::Symbol(String::from("a")))),
                    Box::new(SymExpr::Mul(vec![
                        SymExpr::Primary(Primary::Integer(int(1))),
                        SymExpr::Exp(
                            Box::new(SymExpr::Primary(Primary::Integer(int(5)))),
                            Box::new(SymExpr::Primary(Primary::Integer(int(-1)))),
                        ),
                        SymExpr::Exp(
                            Box::new(SymExpr::Primary(Primary::Integer(int(6)))),
                            Box::new(SymExpr::Primary(Primary::Integer(int(-1)))),
                        ),
                    ])),
                ),
                SymExpr::Primary(Primary::Symbol(String::from("z"))),
                SymExpr::Primary(Primary::Integer(int(-1))),
            ]),
        ]));
    }

    #[test]
    fn complicated_expr_2() {
        let expr = parse_expr("3x^2y - 16x y + 2x^2y - 13x y + 4x y^2 - 11x y^2");
        assert_eq!(expr, SymExpr::Add(vec![
            // 4 * x * y^2
            SymExpr::Mul(vec![
                SymExpr::Exp(
                    Box::new(SymExpr::Primary(Primary::Symbol(String::from("y")))),
                    Box::new(SymExpr::Primary(Primary::Integer(int(2)))),
                ),
                SymExpr::Primary(Primary::Symbol(String::from("x"))),
                SymExpr::Primary(Primary::Integer(int(4))),
            ]),
            // + 2 * x^2 * y
            SymExpr::Mul(vec![
                SymExpr::Primary(Primary::Symbol(String::from("y"))),
                SymExpr::Exp(
                    Box::new(SymExpr::Primary(Primary::Symbol(String::from("x")))),
                    Box::new(SymExpr::Primary(Primary::Integer(int(2)))),
                ),
                SymExpr::Primary(Primary::Integer(int(2))),
            ]),
            // + 3 * x^2 * y
            SymExpr::Mul(vec![
                SymExpr::Primary(Primary::Symbol(String::from("y"))),
                SymExpr::Exp(
                    Box::new(SymExpr::Primary(Primary::Symbol(String::from("x")))),
                    Box::new(SymExpr::Primary(Primary::Integer(int(2)))),
                ),
                SymExpr::Primary(Primary::Integer(int(3))),
            ]),
            // + -1 * 16 * x * y
            SymExpr::Mul(vec![
                SymExpr::Primary(Primary::Symbol(String::from("y"))),
                SymExpr::Primary(Primary::Symbol(String::from("x"))),
                SymExpr::Primary(Primary::Integer(int(16))),
                SymExpr::Primary(Primary::Integer(int(-1))),
            ]),
            // + -1 * 13 * x * y
            SymExpr::Mul(vec![
                SymExpr::Primary(Primary::Symbol(String::from("y"))),
                SymExpr::Primary(Primary::Symbol(String::from("x"))),
                SymExpr::Primary(Primary::Integer(int(13))),
                SymExpr::Primary(Primary::Integer(int(-1))),
            ]),
            // + -1 * 11 * x * y^2
            SymExpr::Mul(vec![
                SymExpr::Exp(
                    Box::new(SymExpr::Primary(Primary::Symbol(String::from("y")))),
                    Box::new(SymExpr::Primary(Primary::Integer(int(2)))),
                ),
                SymExpr::Primary(Primary::Symbol(String::from("x"))),
                SymExpr::Primary(Primary::Integer(int(11))),
                SymExpr::Primary(Primary::Integer(int(-1))),
            ]),
        ]));
    }

    #[test]
    fn same_symbols_not_eq() {
        // these used to mistakenly compare equal due to a buggy partialeq impl
        let expr_a = parse_expr("x * x");
        let expr_b = parse_expr("-x");

        assert_ne!(expr_a, expr_b);
        assert_ne!(expr_b, expr_a);

        let expr_c = parse_expr("x * x");
        let expr_d = parse_expr("x");

        assert_ne!(expr_c, expr_d);
        assert_ne!(expr_d, expr_c);
    }

    #[test]
    fn fmt_expr() {
        let expr = parse_expr("8a^73b sqrt(2634*a*b)");

        // the order switches around a bit because of the way we're traversing the tree
        // but the output is still semantically correct
        assert_eq!(expr.to_string(), "sqrt(b * a * 2634) * b * a^73 * 8");
    }

    #[test]
    fn fmt_expr_2() {
        let expr = parse_expr("(((((((((a) b) c) d) e + f) g) h) i) j)");
        assert_eq!(expr.to_string(), "j * i * h * g * (f + e * d * c * b * a)");
    }

    #[test]
    fn commutative_hashing() {
        let exprs = vec![
            ("1 + 2", "2 + 1"),
            ("x + y", "y + x"),
            ("1 + x", "x + 1"),
            ("a + b + c", "c + b + a"),
            ("(x + y) + z", "z + (y + x)"),

            ("1 * 2", "2 * 1"),
            ("x * y", "y * x"),
            ("2 * x * y", "y * x * 2"),
            ("a * b * c", "c * b * a"),
            ("(x * y) * z", "z * (y * x)"),
            ("x * (y + z)", "(y + z) * x"),
            ("a * (b + c)", "(b + c) * a"),
            ("1 + (x * y)", "(y * x) + 1"),

            ("1 2", "2 1"),
            ("x y", "y x"),
            ("2 x y", "y x 2"),
            ("a b c", "c b a"),
            ("(x y) z", "z * (y x)"),
            ("1 + (x y)", "(y x) + 1"),

            // TODO: these should pass, but they don't as of now because of ambiguity
            // in the parser it gets parsed as a fucntion call instead of implicit
            // multiplication `x * (y + z)` and `a * (b + c)` respectively. Reintroduce
            // these tests once following issue is resolved:
            // https://github.com/ElectrifyPro/cas-rs/issues/3
            // ("x (y + z)", "(y + z) x"),
            // ("a (b + c)", "(b + c) a"),
        ];

        for (ea, eb) in exprs {
            let a = parse_expr(ea);
            let b = parse_expr(eb);
            assert_eq!(hash(&a), hash(&b), "a: {:?}, b: {:?}", a, b);
        }
    }

    #[test]
    fn non_commutative_hashing() {
        let exprs = vec![
            ("1 - 2", "2 - 1"),
            ("x / y", "y / x"),
            ("a - b + c", "c + b - a"),
            ("1 + 1", "2 + 2"),
            ("a * a", "b * b")
        ];

        for (nea, neb) in exprs {
            let a = parse_expr(nea);
            let b = parse_expr(neb);
            assert_ne!(hash(&a), hash(&b), "a: {:?}, b: {:?}", a, b);
        }
    }
}
