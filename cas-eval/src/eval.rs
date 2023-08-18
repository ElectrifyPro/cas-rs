use cas_parser::parser::{
    assign::{AssignTarget, Param},
    call::Call,
    expr::{Expr, Primary},
    binary::Binary,
    literal::Literal,
    unary::Unary,
    token::op::{BinOp, UnaryOp},
};
use super::{ctxt::Ctxt, funcs::{factorial, from_str_radix}};

/// Any type that can be evaluated to produce a value.
pub trait Eval {
    /// Evaluate the expression to produce a value, without any available context. The expression
    /// should return [`None`] if it cannot be evaluated.
    fn eval(&self) -> Option<f64>;

    /// Evaluate the expression to produce a value, using the given context. The expression should
    /// return [`None`] if it cannot be evaluated.
    fn eval_with(&self, ctxt: &mut Ctxt) -> Option<f64>;

    /// Evaluate the expression to produce a value, using the default context. The expression should
    /// return [`None`] if it cannot be evaluated.
    fn eval_default(&self) -> Option<f64> {
        self.eval_with(&mut Default::default())
    }
}

impl Eval for Expr {
    fn eval(&self) -> Option<f64> {
        match self {
            Expr::Literal(literal) => literal.eval(),
            Expr::Paren(paren) => paren.expr.eval(),
            Expr::Call(call) => call.eval(),
            Expr::Unary(unary) => unary.eval(),
            Expr::Binary(binary) => binary.eval(),
            Expr::Assign(_) => None,
        }
    }

    fn eval_with(&self, ctxt: &mut Ctxt) -> Option<f64> {
        match self {
            Expr::Literal(literal) => literal.eval_with(ctxt),
            Expr::Paren(paren) => paren.expr.eval_with(ctxt),
            Expr::Call(call) => call.eval_with(ctxt),
            Expr::Unary(unary) => unary.eval_with(ctxt),
            Expr::Binary(binary) => binary.eval_with(ctxt),
            Expr::Assign(assign) => {
                match &assign.target {
                    AssignTarget::Symbol(symbol) => {
                        // variable assignment
                        let value = assign.value.eval_with(ctxt)?;
                        ctxt.add_var(&symbol.name, value);
                        Some(value)
                    },
                    AssignTarget::Func(header) => {
                        // function assignment
                        ctxt.add_func(header.clone(), *assign.value.clone());
                        None
                    },
                }
            },
        }
    }
}

impl Eval for Primary {
    fn eval(&self) -> Option<f64> {
        match self {
            Primary::Literal(literal) => literal.eval(),
            Primary::Paren(paren) => paren.expr.eval(),
            Primary::Call(call) => call.eval(),
        }
    }

    fn eval_with(&self, ctxt: &mut Ctxt) -> Option<f64> {
        match self {
            Primary::Literal(literal) => literal.eval_with(ctxt),
            Primary::Paren(paren) => paren.expr.eval_with(ctxt),
            Primary::Call(call) => call.eval_with(ctxt),
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

    fn eval_with(&self, ctxt: &mut Ctxt) -> Option<f64> {
        match self {
            Literal::Number(num) => Some(num.value),
            Literal::Radix(radix) => Some(from_str_radix(radix.value.as_str(), radix.base)),
            Literal::Symbol(sym) => ctxt.get_var(sym.name.as_str()),
        }
    }
}

impl Eval for Call {
    fn eval(&self) -> Option<f64> {
        // without a context, we can't evaluate a function call
        None
    }

    fn eval_with(&self, ctxt: &mut Ctxt) -> Option<f64> {
        let (header, body) = ctxt.get_func(&self.name.name)?;
        let mut ctxt = ctxt.clone();
        for (arg, param) in self.args.iter().zip(header.params.iter()) {
            // evaluate the argument, then add it to the context for use in the function body,
            // cloning the context to avoid mutating the original
            // if not provided, try the default value
            // if there is no default, return None
            let value = arg.eval_with(&mut ctxt)
                .or_else(|| match param {
                    Param::Symbol(_) => None,
                    Param::Default(_, expr) => expr.eval_with(&mut ctxt),
                })?;
            ctxt.add_var(&param.symbol().name, value);
        }
        body.eval_with(&mut ctxt)
    }
}

impl Eval for Unary {
    fn eval(&self) -> Option<f64> {
        let operand = self.operand.eval()?;
        Some(match self.op {
            UnaryOp::Not => if operand == 0.0 { 1.0 } else { 0.0 },
            UnaryOp::BitNot => !(operand as i64) as f64,
            UnaryOp::Factorial => factorial(operand),
            UnaryOp::Neg => -1.0 * operand,
        })
    }

    fn eval_with(&self, ctxt: &mut Ctxt) -> Option<f64> {
        let operand = self.operand.eval_with(ctxt)?;
        Some(match self.op {
            UnaryOp::Not => if operand == 0.0 { 1.0 } else { 0.0 },
            UnaryOp::BitNot => !(operand as i64) as f64,
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
            BinOp::BitRight => ((left as i64) >> (right as i64)) as f64,
            BinOp::BitLeft => ((left as i64) << (right as i64)) as f64,
            BinOp::BitAnd => ((left as i64) & (right as i64)) as f64,
            BinOp::BitOr => ((left as i64) | (right as i64)) as f64,
            BinOp::Greater => (left > right) as u8 as f64,
            BinOp::GreaterEq => (left >= right) as u8 as f64,
            BinOp::Less => (left < right) as u8 as f64,
            BinOp::LessEq => (left <= right) as u8 as f64,
            BinOp::Eq => (left == right) as u8 as f64,
            BinOp::NotEq => (left != right) as u8 as f64,
            BinOp::ApproxEq => ((left - right).abs() < 1e-6) as u8 as f64,
            BinOp::ApproxNotEq => ((left - right).abs() >= 1e-6) as u8 as f64,
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
        })
    }

    fn eval_with(&self, ctxt: &mut Ctxt) -> Option<f64> {
        let left = self.lhs.eval_with(ctxt)?;
        let right = self.rhs.eval_with(ctxt)?;
        Some(match self.op {
            BinOp::Exp => left.powf(right),
            BinOp::Mul => left * right,
            BinOp::Div => left / right,
            BinOp::Add => left + right,
            BinOp::Sub => left - right,
            BinOp::BitRight => ((left as i64) >> (right as i64)) as f64,
            BinOp::BitLeft => ((left as i64) << (right as i64)) as f64,
            BinOp::BitAnd => ((left as i64) & (right as i64)) as f64,
            BinOp::BitOr => ((left as i64) | (right as i64)) as f64,
            BinOp::Greater => (left > right) as u8 as f64,
            BinOp::GreaterEq => (left >= right) as u8 as f64,
            BinOp::Less => (left < right) as u8 as f64,
            BinOp::LessEq => (left <= right) as u8 as f64,
            BinOp::Eq => (left == right) as u8 as f64,
            BinOp::NotEq => (left != right) as u8 as f64,
            BinOp::ApproxEq => ((left - right).abs() < 1e-6) as u8 as f64,
            BinOp::ApproxNotEq => ((left - right).abs() >= 1e-6) as u8 as f64,
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
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

    #[test]
    fn func_call() {
        let mut ctxt = Ctxt::default();

        // assign function
        let mut parser = Parser::new("f(x) = x^2 + 5x + 6");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_with(&mut ctxt), None);

        // call function
        let mut parser = Parser::new("f(7)");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_with(&mut ctxt), Some(90.0));
    }
}
