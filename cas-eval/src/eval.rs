use cas_parser::parser::{
    expr::{Expr, Primary},
    binary::Binary,
    literal::Literal,
    unary::Unary,
    token::op::{BinOp, UnaryOp},
};
use super::{ctxt::Ctxt, funcs::factorial};

/// Any type that can be evaluated to produce a value.
pub trait Eval {
    /// Evaluate the expression to produce a value.
    fn eval(&self) -> f64;

    /// Evaluate the expression to produce a value, using the given context.
    fn eval_with(&self, ctxt: &Ctxt) -> f64;
}

impl Eval for Expr {
    fn eval(&self) -> f64 {
        match self {
            Expr::Literal(literal) => literal.eval(),
            Expr::Paren(paren) => paren.expr.eval(),
            Expr::Unary(unary) => unary.eval(),
            Expr::Binary(binary) => binary.eval(),
        }
    }

    fn eval_with(&self, _: &Ctxt) -> f64 {
        // TODO: symbol literals not yet implemented
        self.eval()
    }
}

impl Eval for Primary {
    fn eval(&self) -> f64 {
        match self {
            Primary::Literal(literal) => literal.eval(),
            Primary::Paren(paren) => paren.expr.eval(),
        }
    }

    fn eval_with(&self, _: &Ctxt) -> f64 {
        // TODO: symbol literals not yet implemented
        self.eval()
    }
}

impl Eval for Literal {
    fn eval(&self) -> f64 {
        match self {
            Literal::Number(num) => num.value,
        }
    }

    fn eval_with(&self, _: &Ctxt) -> f64 {
        // TODO: symbol literals not yet implemented
        self.eval()
    }
}

impl Eval for Unary {
    fn eval(&self) -> f64 {
        match self.op {
            UnaryOp::Not => if self.operand.eval() == 0.0 { 1.0 } else { 0.0 },
            UnaryOp::Factorial => factorial(self.operand.eval()),
            UnaryOp::Neg => -1.0 * self.operand.eval(),
        }
    }

    fn eval_with(&self, _: &Ctxt) -> f64 {
        // TODO: symbol literals not yet implemented
        self.eval()
    }
}

impl Eval for Binary {
    fn eval(&self) -> f64 {
        let left = self.lhs.eval();
        let right = self.rhs.eval();
        match self.op {
            BinOp::Exp => left.powf(right),
            BinOp::Mul => left * right,
            BinOp::Div => left / right,
            BinOp::Add => left + right,
            BinOp::Sub => left - right,
        }
    }

    fn eval_with(&self, _: &Ctxt) -> f64 {
        // TODO: symbol literals not yet implemented
        self.eval()
    }
}

/// Eval tests depend on the parser, so ensure that parser tests pass before running these.
#[cfg(test)]
mod tests {
    use super::*;

    use cas_parser::parser::Parser;

    #[test]
    fn binary_expr() {
        let mut parser = Parser::new("1 + 2");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(), 3.0);
    }

    #[test]
    fn binary_expr_2() {
        let mut parser = Parser::new("1 + 2 * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(), 7.0);
    }

    #[test]
    fn binary_and_unary() {
        let mut parser = Parser::new("3 * -5 / 5! + 6");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(), 5.875);
    }

    #[test]
    fn parenthesized() {
        let mut parser = Parser::new("((1 + 9) / 5) * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(), 6.0);
    }
}
