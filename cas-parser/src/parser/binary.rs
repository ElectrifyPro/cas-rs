use std::ops::Range;
use super::{
    expr::{Expr, Primary},
    error::{kind, Error},
    token::op::{BinOp, BinOpKind},
    unary::Unary,
    Associativity,
    Parse,
    Parser,
    Precedence,
};

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

    pub fn parse_expr(input: &mut Parser, mut lhs: Expr, precedence: Precedence) -> Result<Expr, Error> {
       let start_span = lhs.span().start;

       loop {
           // the left hand side has already been parsed, so parse an operator and right operand
           let (op, mut rhs) = match input.try_parse_with_fn(|input| {
               match input.try_parse::<BinOp>() {
                   Ok(op) => {
                       if op.precedence() >= precedence {
                           let rhs = input.try_parse_with_fn(Unary::parse_or_lower)?;
                           Ok((op, rhs))
                       } else {
                           Err(input.error(kind::NonFatal))
                       }
                   },
                   Err(_) => {
                       if BinOpKind::Mul.precedence() >= precedence {
                           let rhs = input.try_parse_with_fn(Unary::parse_or_lower)
                               .map_err(|_| input.error(kind::NonFatal))?;
                           Ok((
                               BinOp {
                                   kind: BinOpKind::Mul,
                                   implicit: true,
                                   span: lhs.span().end..rhs.span().start,
                               },
                               rhs,
                            ))
                       } else {
                           Err(input.error(kind::NonFatal))
                       }
                   },
               }
           }) {
               Ok(result) => result,
               Err(err) => {
                   if err.fatal {
                       return Err(err);
                   } else {
                       break;
                   }
               },
           };
           let precedence = op.precedence();

           loop {
               // before creating the `lhs op rhs` node, we should check the precedence of the
               // following operator, if any
               // this is because we can't parse an expression like `3 + 4 * 5`, as (3 + 4) * 5

               // clone the input stream to emulate peeking
               let mut input_ahead = input.clone();
               let next_op = input_ahead.try_parse::<BinOp>();

               if let Ok(next_op) = next_op {
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
           let end_span = rhs.span().end;
           lhs = Expr::Binary(Binary {
               lhs: Box::new(lhs),
               op,
               rhs: Box::new(rhs),
               span: start_span..end_span,
           });
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
