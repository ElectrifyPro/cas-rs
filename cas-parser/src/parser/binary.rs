use std::ops::Range;
use super::{
    assign::{Assign as AssignExpr, AssignTarget},
    expr::{Expr, Primary},
    error::{kind, Error},
    token::{op::{BinOp, BinOpKind}, Assign},
    unary::Unary,
    Associativity,
    Parse,
    Parser,
    Precedence,
};

/// A binary operator, including assignment.
#[derive(Debug, Clone, PartialEq)]
enum BinOpExt<'a> {
    /// A binary operator, such as `+` or `*`.
    Op(BinOp),

    /// Implicit multiplication, such as `2x` or `x(x + 1)`.
    ///
    /// This is not a real operator, but it is treated as one for the purposes of parsing.
    ImplicitMultiplication,

    /// An assignment operator, such as `+=` or `/=`.
    Assign(Assign<'a>),
}

impl BinOpExt<'_> {
    /// Returns the precedence of the binary operator.
    fn precedence(&self) -> Precedence {
        match self {
            BinOpExt::Op(op) => op.precedence(),
            BinOpExt::ImplicitMultiplication => Precedence::Factor,
            BinOpExt::Assign(_) => Precedence::Assign,
        }
    }
}

impl From<BinOp> for BinOpExt<'_> {
    fn from(op: BinOp) -> Self {
        BinOpExt::Op(op)
    }
}

impl<'a> From<Assign<'a>> for BinOpExt<'a> {
    fn from(op: Assign<'a>) -> Self {
        BinOpExt::Assign(op)
    }
}

/// A binary expression, such as `1 + 2`. Binary expressions can include nested expressions.
#[derive(Debug, Clone, PartialEq)]
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
    fn complete_rhs(input: &mut Parser, lhs: Expr, op: BinOpExt, mut rhs: Expr) -> Result<Expr, Error> {
        let precedence = op.precedence();

        loop {
            // before creating the `lhs op rhs` node, we should check the precedence of the
            // following operator, if any
            // this is because we can't parse an expression like `3 + 4 * 5`, as (3 + 4) * 5

            // clone the input stream to emulate peeking
            let mut input_ahead = input.clone();
            if let Ok(next_op) = input_ahead.try_parse::<BinOp>() {
                if next_op.precedence() > precedence || next_op.associativity() == Associativity::Right {
                    // this operator has a higher precedence or it is right associative, so we should
                    // parse its expression starting with `rhs` first
                    rhs = Self::parse_expr(input, rhs, next_op.precedence())?;
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
                    rhs = Self::parse_expr(input, rhs, Precedence::Assign)?;
                } else {
                    break;
                }
            } else {
                // there is no operator; check if there is a primary expression instead
                // if there is, this is implicit multiplication
                //
                // first, check if the previous operator has higher precedence; if so, we cannot
                // give priority to implicit multiplication
                if precedence > BinOpKind::Mul.precedence() {
                    break;
                }

                if let Ok(primary) = input.try_parse::<Primary>() {
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
                target: AssignTarget::try_from_with_op(lhs, &op)?, // TODO move this to the top
                value: Box::new(rhs),
                span: start_span..end_span,
            })),
        }
    }

    pub fn parse_expr(input: &mut Parser, mut lhs: Expr, precedence: Precedence) -> Result<Expr, Error> {
        loop {
            let mut input_ahead = input.clone();
            if let Ok(op) = input_ahead.try_parse_then::<BinOp, _>(|bin_op, input| {
                if bin_op.precedence() >= precedence {
                    Ok(())
                } else {
                    Err(input.error(kind::NonFatal))
                }
            }) {
                input.set_cursor(&input_ahead);
                let rhs = input.try_parse_with_fn(Unary::parse_or_lower)?;
                lhs = Self::complete_rhs(input, lhs, op.into(), rhs)?;
            } else if let Ok(assign) = input_ahead.try_parse_then::<Assign, _>(|_, input| {
                if Precedence::Assign >= precedence {
                    Ok(())
                } else {
                    Err(input.error(kind::NonFatal))
                }
            }) {
                // assignment is also a binary expression, however it requires special handling
                // because not all expressions are valid as the left-hand side of an assignment
                // expression, and there is some syntax is only valid in the context of an
                // assignment expression (i.e. function headers)
                input.set_cursor(&input_ahead);
                let rhs = input.try_parse_with_fn(Unary::parse_or_lower)?;
                lhs = Self::complete_rhs(input, lhs, assign.into(), rhs)?;
            } else if BinOpKind::Mul.precedence() >= precedence {
                // implicit multiplication test
                //
                // ensure that we get here because there is *no* operator, not because the operator
                // has lower precedence
                if input_ahead.try_parse_then::<BinOp, _>(|op, input| {
                    if op.precedence() > BinOpKind::Mul.precedence() {
                        Err(input.error(kind::NonFatal))
                    } else {
                        Ok(())
                    }
                }).is_ok() {
                    break;
                }

                // if there is no expression, there is no implicit multiplication and all our
                // attempts to parse a binary expression fail
                let Ok(rhs) = input.try_parse_with_fn(Unary::parse_or_lower) else {
                    break;
                };
                lhs = Self::complete_rhs(input, lhs, BinOpExt::ImplicitMultiplication, rhs)?;
            } else {
                break;
            }
        }

        Ok(lhs)
    }
}

impl<'source> Parse<'source> for Binary {
    fn parse(input: &mut Parser<'source>) -> Result<Self, Error> {
        match input.try_parse::<Expr>()? {
            Expr::Binary(binary) => Ok(binary),
            _ => todo!(),
        }
    }
}
