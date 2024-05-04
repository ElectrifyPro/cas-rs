use cas_error::Error;
use crate::parser::{
    ast::{
        assign::{Assign as AssignExpr, AssignTarget},
        expr::Expr,
        unary::Unary,
    },
    error::NonFatal,
    fmt::{Latex, fmt_pow},
    token::{op::{AssignOp, Associativity, BinOp, BinOpKind, Precedence}, Assign},
    Parse,
    Parser,
    ParseResult,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A binary operator, including assignment.
#[derive(Debug, Clone, PartialEq, Eq)]
enum BinOpExt {
    /// A binary operator, such as `+` or `*`.
    Op(BinOp),

    /// Implicit multiplication, such as `2x` or `x(x + 1)`.
    ///
    /// This is not a real operator, but it is treated as one for the purposes of parsing.
    ImplicitMultiplication,

    /// An assignment operator, such as `+=` or `/=`.
    Assign(AssignOp),
}

impl BinOpExt {
    /// Returns the precedence of the binary operator.
    fn precedence(&self) -> Precedence {
        match self {
            BinOpExt::Op(op) => op.precedence(),
            BinOpExt::ImplicitMultiplication => Precedence::Factor,
            BinOpExt::Assign(_) => Precedence::Assign,
        }
    }
}

impl From<BinOp> for BinOpExt {
    fn from(op: BinOp) -> Self {
        BinOpExt::Op(op)
    }
}

impl From<AssignOp> for BinOpExt {
    fn from(op: AssignOp) -> Self {
        BinOpExt::Assign(op)
    }
}

/// A binary expression, such as `1 + 2`. Binary expressions can include nested expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Binary {
    /// The left-hand side of the binary expression.
    pub lhs: Box<Expr>,

    /// The operator of the binary expression.
    pub op: BinOp,

    /// The right-hand side of the binary expression.
    pub rhs: Box<Expr>,

    /// The region of the source code that this binary expression was parsed from.
    pub span: Range<usize>,
}

impl Binary {
    /// Returns the span of the binary expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    /// After parsing the left-hand-side, the operator, and the right-hand-side of a potential
    /// binary expression, parse ahead to see if the right-hand-side is incomplete.
    ///
    /// If we are parsing the expression `1 + 2 * 3`, we will first parse the left-hand-side `1`,
    /// then the operator `+`, then the right-hand-side `2`. However, before we build the
    /// corresponding AST node, we should check if the operator after `2` has higher precedence
    /// than `+` (if it exists).
    ///
    /// If it does, we should parse the expression starting with `2` first, so that we get `2 * 3`
    /// as the right-hand-side to the `1 +` node. This works by calling into [`Self::parse_expr`]
    /// again, but with `rhs` (`2` in this case) as the `lhs` argument.
    ///
    /// If it does not (such as in the expression `3 * 2 + 1`), we build the AST node `3 * 2`
    /// first. Then, [`Self::parse_expr`] will pick up the `+ 1` part of the expression, and
    /// build the AST node `3 * 2 + 1`.
    ///
    /// Implicit multiplication is also handled here. In an expression such as `1 + 2x y`, the
    /// first call to [`Self::parse_expr`] will parse `1 + 2`. However, there is no operator after
    /// `2`, so instead, we assume an implicit multiplication operator, because multiplication has
    /// higher precedence than addition, then continue with the same procedure as if the operator
    /// did exist.
    ///
    /// There is one distinction we must make when parsing implicit multiplication. Since we're
    /// essentially creating multiplication out of thin air, [`Self::complete_rhs`] can get into an
    /// infinite loop if we're not careful!
    ///
    /// Consider the expression `1 + 2x`. The first call to [`Self::complete_rhs`] will contain the
    /// left-hand-side `1`, the operator `+`, and the right-hand-side `2`. Since there is no
    /// operator after `2`, we assume implicit multiplication (higher precedence than `+`), and
    /// successfully parse `2x`. At this point, we've returned to the first call to
    /// [`Self::complete_rhs`], where the right-hand-side is now `2x`. However, there is still no
    /// operator after `2x`, so we assume implicit multiplication again, but parse nothing;
    /// [`Self::parse_expr`] would simply return `2x` as-is, and we would return to the first call
    /// to [`Self::complete_rhs`] again.
    ///
    /// Thankfully, the solution is simple: just check if `2x` is returned as-is! If so, there is
    /// no expression after `2x`, so we should break out of the loop and return the AST node
    /// `1 + 2x`. This is the purpose of the `changed` boolean returned by [`Self::parse_expr`].
    fn complete_rhs(
        input: &mut Parser,
        recoverable_errors: &mut Vec<Error>,
        lhs: Expr,
        op: BinOpExt,
        mut rhs: Expr
    ) -> Result<Expr, Vec<Error>> {
        let precedence = op.precedence();

        loop {
            // before creating the `lhs op rhs` node, we should check the precedence of the
            // following operator, if any
            // this is because we can't parse an expression like `3 + 4 * 5`, as (3 + 4) * 5

            // clone the input stream to emulate peeking
            let mut input_ahead = input.clone();
            if let Ok(next_op) = input_ahead.try_parse::<BinOp>().forward_errors(recoverable_errors) {
                if next_op.precedence() > precedence || next_op.associativity() == Associativity::Right {
                    // this operator has a higher precedence or it is right associative, so we should
                    // parse its expression starting with `rhs` first
                    rhs = Self::parse_expr(input, recoverable_errors, rhs, next_op.precedence())?.0;
                } else {
                    // this operator has lower precedence, or equal precedence and
                    // left-associativity; this is in scenarios like:
                    // `1 * 2 + 3` or `1 * 2 * 3`
                    // prec(+) < prec(*), prec(*) == prec(*)
                    //
                    // so just break out of the loop and let `lhs` become `1 * 2`
                    // we will parse this operator on the next iteration of the outside loop
                    break;
                }
            } else if input_ahead.try_parse::<Assign>().is_ok() {
                // assignment is right-associative, so we should parse its expression starting with
                // `rhs` first
                if Precedence::Assign >= precedence {
                    rhs = Self::parse_expr(input, recoverable_errors, rhs, Precedence::Assign)?.0;
                } else {
                    break;
                }
            } else {
                // there is no operator; check if there is a valid expression after
                // if there is, this could be implicit multiplication
                //
                // first, check if the previous operator has higher or equal precedence; if so, we
                // cannot give priority to implicit multiplication
                if precedence >= BinOpKind::Mul.precedence() {
                    break;
                }

                // then check if there is significant whitespace after `rhs`; if there is, we cannot
                // parse implicit multiplication, as that would be confusing
                input_ahead.advance_past_non_significant_whitespace();
                if let Some(token) = input_ahead.current_token() {
                    if token.kind.is_significant_whitespace() {
                        break;
                    }
                }

                let (expr, changed) = Self::parse_expr(input, recoverable_errors, rhs, BinOpKind::Mul.precedence())?;

                // `rhs = expr;` must happen in all cases, even if `changed` is false, otherwise it
                // would've been moved into `Self::parse_expr` above
                rhs = expr;

                if !changed {
                    break;
                }
            }
        }

        // create the binary node representing `lhs op rhs`
        let (start_span, end_span) = (lhs.span().start, rhs.span().end);
        match op {
            BinOpExt::Op(op) => Ok(Expr::Binary(Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span: start_span..end_span,
            })),
            BinOpExt::ImplicitMultiplication => {
                let op_span = lhs.span().end..rhs.span().start;
                Ok(Expr::Binary(Binary {
                    lhs: Box::new(lhs),
                    op: BinOp {
                        kind: BinOpKind::Mul,
                        implicit: true,
                        span: op_span,
                    },
                    rhs: Box::new(rhs),
                    span: start_span..end_span,
                }))
            },
            BinOpExt::Assign(op) => Ok(Expr::Assign(AssignExpr {
                target: AssignTarget::try_from_with_op(lhs, &op).forward_errors(recoverable_errors)?,
                op,
                value: Box::new(rhs),
                span: start_span..end_span,
            })),
        }
    }

    /// After parsing the left-hand-side of a potential binary expression, parse ahead to see if
    /// there is a binary operator and a right-hand-side.
    ///
    /// See [`Self::complete_rhs`] for more information about the return value of this function.
    pub fn parse_expr(
        input: &mut Parser,
        recoverable_errors: &mut Vec<Error>,
        mut lhs: Expr,
        precedence: Precedence
    ) -> Result<(Expr, bool), Vec<Error>> {
        let mut changed = false;
        loop {
            let mut input_ahead = input.clone();
            if let Ok(op) = input_ahead.try_parse_then::<BinOp, _>(|bin_op, input| {
                if bin_op.precedence() >= precedence {
                    ParseResult::Ok(())
                } else {
                    ParseResult::Unrecoverable(vec![input.error(NonFatal)])
                }
            }).forward_errors(recoverable_errors) {
                input.set_cursor(&input_ahead);
                let rhs = Unary::parse_or_lower(input, recoverable_errors)?;
                lhs = Self::complete_rhs(input, recoverable_errors, lhs, op.into(), rhs)?;
            } else if let Ok(assign) = input_ahead.try_parse_then::<AssignOp, _>(|_, input| {
                if Precedence::Assign >= precedence {
                    ParseResult::Ok(())
                } else {
                    ParseResult::Unrecoverable(vec![input.error(NonFatal)])
                }
            }).forward_errors(recoverable_errors) {
                // assignment is also a binary expression, however it requires special handling
                // because not all expressions are valid as the left-hand side of an assignment
                // expression, and there is some syntax is only valid in the context of an
                // assignment expression (i.e. function headers)
                input.set_cursor(&input_ahead);
                let rhs = Unary::parse_or_lower(input, recoverable_errors)?;
                lhs = Self::complete_rhs(input, recoverable_errors, lhs, assign.into(), rhs)?;
            } else if BinOpKind::Mul.precedence() >= precedence {
                // implicit multiplication test

                // do not continue if there is significant whitespace after `lhs`
                input_ahead.advance_past_non_significant_whitespace();
                if let Some(token) = input_ahead.current_token() {
                    if token.kind.is_significant_whitespace() {
                        break;
                    }
                }

                // ensure that we get here because there is *no* operator, not because the operator
                // has lower precedence
                if input_ahead.try_parse_then::<BinOp, _>(|op, input| {
                    if op.precedence() > BinOpKind::Mul.precedence() {
                        ParseResult::Unrecoverable(vec![input.error(NonFatal)])
                    } else {
                        ParseResult::Ok(())
                    }
                }).is_ok() {
                    break;
                }

                // if there is no expression, there is no implicit multiplication and all our
                // attempts to parse a binary expression fail
                let mut inner_recoverable_errors = Vec::new();
                let Ok(rhs) = Unary::parse_or_lower(&mut input_ahead, &mut inner_recoverable_errors) else {
                    break;
                };

                if rhs.is_implicit_mul_target() {
                    // TODO: this can be refactored with input.try_parse_with_fn once Try trait is
                    // stabilized, which would make ParseResult so much nicer to work with

                    // add the recoverable errors from the inner parser to the outer parser, since
                    // we know now implicit multiplication is the correct branch to take
                    recoverable_errors.extend(inner_recoverable_errors);
                    input.set_cursor(&input_ahead);
                    lhs = Self::complete_rhs(input, recoverable_errors, lhs, BinOpExt::ImplicitMultiplication, rhs.into())?;
                } else {
                    break;
                }
            } else {
                break;
            }

            changed = true;
        }

        Ok((lhs, changed))
    }
}

impl<'source> Parse<'source> for Binary {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        match input.try_parse().forward_errors(recoverable_errors)? {
            Expr::Binary(binary) => Ok(binary),
            _ => todo!(),
        }
    }
}

impl std::fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.lhs.fmt(f)?;
        self.op.fmt(f)?;
        self.rhs.fmt(f)
    }
}

impl Latex for Binary {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.op.kind {
            BinOpKind::Exp => fmt_pow(f, Some(&*self.lhs), Some(&*self.rhs)),
            BinOpKind::Div => {
                write!(f, "\\frac{{")?;
                self.lhs.fmt_latex(f)?;
                write!(f, "}}{{")?;
                self.rhs.innermost().fmt_latex(f)?;
                write!(f, "}}")
            },
            _ => {
                self.lhs.fmt_latex(f)?;
                self.op.fmt_latex(f)?;
                self.rhs.fmt_latex(f)
            },
        }
    }
}
