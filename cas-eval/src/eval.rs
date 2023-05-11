use cas_parser::parser::{
    expr::{Expr, Primary},
    binary::Binary,
    literal::Literal,
    unary::Unary,
    token::op::{BinOp, UnaryOp},
};
use super::{ctxt::Ctxt, funcs::{factorial, from_str_radix}};

/// Any type that can be evaluated to produce a value.
pub trait Eval {
    /// Evaluate the expression to produce a value. The expression should return [`None`] if it
    /// cannot be evaluated.
    fn eval(&self) -> Option<f64>;

    /// Evaluate the expression to produce a value, using the given context. The expression should
    /// return [`None`] if it cannot be evaluated.
    fn eval_with(&self, ctxt: &Ctxt) -> Option<f64>;

    /// Evaluate the expression to produce a value, using the default context. The expression should
    /// return [`None`] if it cannot be evaluated.
    fn eval_default(&self) -> Option<f64> {
        self.eval_with(&Ctxt::with_defaults())
    }
}

impl Eval for Expr {
    fn eval(&self) -> Option<f64> {
        match self {
            Expr::Literal(literal) => literal.eval(),
            Expr::Paren(paren) => paren.expr.eval(),
            Expr::Unary(unary) => unary.eval(),
            Expr::Binary(binary) => binary.eval(),
        }
    }

    fn eval_with(&self, ctxt: &Ctxt) -> Option<f64> {
        match self {
            Expr::Literal(literal) => literal.eval_with(ctxt),
            Expr::Paren(paren) => paren.expr.eval_with(ctxt),
            Expr::Unary(unary) => unary.eval_with(ctxt),
            Expr::Binary(binary) => binary.eval_with(ctxt),
        }
    }
}

impl Eval for Primary {
    fn eval(&self) -> Option<f64> {
        match self {
            Primary::Literal(literal) => literal.eval(),
            Primary::Paren(paren) => paren.expr.eval(),
        }
    }

    fn eval_with(&self, ctxt: &Ctxt) -> Option<f64> {
        match self {
            Primary::Literal(literal) => literal.eval_with(ctxt),
            Primary::Paren(paren) => paren.expr.eval_with(ctxt),
        }
    }
}

impl Eval for Literal {
    fn eval(&self) -> Option<f64> {
        match self {
            Literal::Number(num) => Some(num.value),
            Literal::Radix(radix) => Some(from_str_radix(radix.value.as_str(), radix.base)),
            Literal::Symbol(_) => None, // context needed
        }
    }

    fn eval_with(&self, ctxt: &Ctxt) -> Option<f64> {
        match self {
            Literal::Number(num) => Some(num.value),
            Literal::Radix(radix) => Some(from_str_radix(radix.value.as_str(), radix.base)),
            Literal::Symbol(sym) => ctxt.get_var(sym.name.as_str()),
        }
    }
}

impl Eval for Unary {
    fn eval(&self) -> Option<f64> {
        let operand = self.operand.eval()?;
        Some(match self.op {
            UnaryOp::Not => if operand == 0.0 { 1.0 } else { 0.0 },
            UnaryOp::Factorial => factorial(operand),
            UnaryOp::Neg => -1.0 * operand,
        })
    }

    fn eval_with(&self, ctxt: &Ctxt) -> Option<f64> {
        let operand = self.operand.eval_with(ctxt)?;
        Some(match self.op {
            UnaryOp::Not => if operand == 0.0 { 1.0 } else { 0.0 },
            UnaryOp::Factorial => factorial(operand),
            UnaryOp::Neg => -1.0 * operand,
        })
    }
}

impl Eval for Binary {
    fn eval(&self) -> Option<f64> {
        let left = self.lhs.eval()?;
        let right = self.rhs.eval()?;
        Some(match self.op {
            BinOp::Exp => left.powf(right),
            BinOp::Mul => left * right,
            BinOp::Div => left / right,
            BinOp::Add => left + right,
            BinOp::Sub => left - right,
        })
    }

    fn eval_with(&self, ctxt: &Ctxt) -> Option<f64> {
        let left = self.lhs.eval_with(ctxt)?;
        let right = self.rhs.eval_with(ctxt)?;
        Some(match self.op {
            BinOp::Exp => left.powf(right),
            BinOp::Mul => left * right,
            BinOp::Div => left / right,
            BinOp::Add => left + right,
            BinOp::Sub => left - right,
        })
    }
}

/// Eval tests depend on the parser, so ensure that parser tests pass before running these.
#[cfg(test)]
mod tests {
    use super::*;

    use cas_parser::parser::Parser;
    use std::f64::consts;

    #[test]
    fn binary_expr() {
        let mut parser = Parser::new("1 + 2");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(), Some(3.0));
    }

    #[test]
    fn binary_expr_2() {
        let mut parser = Parser::new("1 + 2 * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(), Some(7.0));
    }

    #[test]
    fn binary_and_unary() {
        let mut parser = Parser::new("3 * -5 / 5! + 6");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(), Some(5.875));
    }

    #[test]
    fn parenthesized() {
        let mut parser = Parser::new("((1 + 9) / 5) * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(), Some(6.0));
    }

    #[test]
    fn degree_to_radian() {
        let mut parser = Parser::new("90 * 2 * pi / 360");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default(), Some(consts::PI / 2.0));
    }

    #[test]
    fn precision() {
        let mut parser = Parser::new("e^2 - tau");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default(), Some(consts::E.powf(2.0) - consts::TAU));
    }

    #[test]
    fn precision_2() {
        let mut parser = Parser::new("pi^2 * 17! / -4.9 + e");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(
            expr.eval_default(),
            Some(consts::PI.powf(2.0) * factorial(17.0) / -4.9 + consts::E)
        );
    }
}
