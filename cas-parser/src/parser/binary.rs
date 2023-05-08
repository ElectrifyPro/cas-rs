use std::ops::Range;
use super::{
    expr::Expr,
    error::Error,
    token::op::BinOp,
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
           // the left hand side has already been parsed, so parse an operator
           let Ok(op) = input.try_parse_then::<BinOp, _>(|op, input| {
               if op.precedence() >= precedence {
                   Ok(())
               } else {
                   Err(input.non_fatal())
               }
           }) else {
               break;
           };

           let precedence = op.precedence();

           // parse the right hand side
           let mut rhs = input.try_parse_with_fn(Unary::parse_or_lower)?;

           loop {
               // before creating the `lhs op rhs` node, we should check the precedence of the
               // following operator, if any
               // this is because we can't parse an expression like `3 + 4 * 5`, as (3 + 4) * 5

               // clone the input stream to emulate peeking
               let mut input_ahead = input.clone();
               let next_op = input_ahead.try_parse_then::<BinOp, _>(|next_op, input| {
                   if next_op.precedence() > precedence || next_op.associativity() == Associativity::Right {
                       Ok(())
                   } else {
                       Err(input.non_fatal())
                   }
               });

               // this operator has a higher precedence or it is right associative, so we should
               // parse its expression starting with `rhs` first
               if let Ok(next_op) = next_op {
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
           }

           // create the binary node representing `lhs op rhs`
           let end_span = rhs.span().end;
           lhs = Expr::Binary(Box::new(Binary {
               lhs: Box::new(lhs),
               op,
               rhs: Box::new(rhs),
               span: start_span..end_span,
           }));
       }

       Ok(lhs)
    }
}

impl Parse for Binary {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        match input.try_parse::<Expr>()? {
            Expr::Binary(binary) => Ok(*binary),
            _ => todo!(),
        }
    }
}
