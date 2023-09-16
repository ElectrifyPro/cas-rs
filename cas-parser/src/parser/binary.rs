use std::{fmt, ops::Range};
use super::{
    assign::{Assign as AssignExpr, AssignTarget},
    expr::{Expr, Primary},
    error::{kind, Error},
    fmt::{Latex, fmt_pow},
    token::{op::{AssignOp, Associativity, BinOp, BinOpKind, Precedence}, Assign},
    unary::Unary,
    Parse,
    Parser,
    ParseResult,
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A binary operator, including assignment.
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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
                    rhs = Self::parse_expr(input, recoverable_errors, rhs, next_op.precedence())?;
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
                    rhs = Self::parse_expr(input, recoverable_errors, rhs, Precedence::Assign)?;
                } else {
                    break;
                }
            } else {
                // there is no operator; check if there is a primary expression instead
                // if there is, this is implicit multiplication
                //
                // first, check if the previous operator has higher or equal precedence; if so, we
                // cannot give priority to implicit multiplication
                if precedence >= BinOpKind::Mul.precedence() {
                    break;
                }

                if let Ok(primary) = input.try_parse::<Primary>().forward_errors(recoverable_errors) {
                    let expr: Expr = primary.into();
                    let (start_span, end_span) = (rhs.span().start, expr.span().end);
                    let op_span = rhs.span().end..expr.span().start;
                    rhs = Expr::Binary(Binary {
                        lhs: Box::new(rhs),
                        op: BinOp {
                            kind: BinOpKind::Mul,
                            implicit: true,
                            span: op_span,
                        },
                        rhs: Box::new(expr),
                        span: start_span..end_span,
                    });
                } else {
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

    pub fn parse_expr(
        input: &mut Parser,
        recoverable_errors: &mut Vec<Error>,
        mut lhs: Expr,
        precedence: Precedence
    ) -> Result<Expr, Vec<Error>> {
        loop {
            let mut input_ahead = input.clone();
            if let Ok(op) = input_ahead.try_parse_then::<BinOp, _>(|bin_op, input| {
                if bin_op.precedence() >= precedence {
                    ParseResult::Ok(())
                } else {
                    ParseResult::Unrecoverable(vec![input.error(kind::NonFatal)])
                }
            }).forward_errors(recoverable_errors) {
                input.set_cursor(&input_ahead);
                let rhs = Unary::parse_or_lower(input, recoverable_errors)?;
                lhs = Self::complete_rhs(input, recoverable_errors, lhs, op.into(), rhs)?;
            } else if let Ok(assign) = input_ahead.try_parse_then::<AssignOp, _>(|_, input| {
                if Precedence::Assign >= precedence {
                    ParseResult::Ok(())
                } else {
                    ParseResult::Unrecoverable(vec![input.error(kind::NonFatal)])
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
                //
                // ensure that we get here because there is *no* operator, not because the operator
                // has lower precedence
                if input_ahead.try_parse_then::<BinOp, _>(|op, input| {
                    if op.precedence() > BinOpKind::Mul.precedence() {
                        ParseResult::Unrecoverable(vec![input.error(kind::NonFatal)])
                    } else {
                        ParseResult::Ok(())
                    }
                }).is_ok() {
                    break;
                }

                // if there is no expression, there is no implicit multiplication and all our
                // attempts to parse a binary expression fail
                let Ok(rhs) = Unary::parse_or_lower(input, recoverable_errors) else {
                    break;
                };
                lhs = Self::complete_rhs(input, recoverable_errors, lhs, BinOpExt::ImplicitMultiplication, rhs)?;
            } else {
                break;
            }
        }

        Ok(lhs)
    }
}

impl<'source> Parse<'source> for Binary {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        match input.try_parse::<Expr>().forward_errors(recoverable_errors)? {
            Expr::Binary(binary) => Ok(binary),
            _ => todo!(),
        }
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
