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
    /// Evaluate the expression to produce a value, using the given context. The expression should
    /// return [`None`] if it cannot be evaluated.
    fn eval(&self, ctxt: &mut Ctxt) -> Option<f64>;

    /// Evaluate the expression to produce a value, using the default context. The expression should
    /// return [`None`] if it cannot be evaluated.
    fn eval_default(&self) -> Option<f64> {
        self.eval(&mut Default::default())
    }
}

impl Eval for Expr {
    fn eval(&self, ctxt: &mut Ctxt) -> Option<f64> {
        match self {
            Expr::Literal(literal) => literal.eval(ctxt),
            Expr::Paren(paren) => paren.expr.eval(ctxt),
            Expr::Call(call) => call.eval(ctxt),
            Expr::Unary(unary) => unary.eval(ctxt),
            Expr::Binary(binary) => binary.eval(ctxt),
            Expr::Assign(assign) => {
                match &assign.target {
                    AssignTarget::Symbol(symbol) => {
                        // variable assignment
                        let value = assign.value.eval(ctxt)?;
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
    fn eval(&self, ctxt: &mut Ctxt) -> Option<f64> {
        match self {
            Primary::Literal(literal) => literal.eval(ctxt),
            Primary::Paren(paren) => paren.expr.eval(ctxt),
            Primary::Call(call) => call.eval(ctxt),
        }
    }
}

impl Eval for Literal {
    fn eval(&self, ctxt: &mut Ctxt) -> Option<f64> {
        match self {
            Literal::Number(num) => Some(num.value),
            Literal::Radix(radix) => Some(from_str_radix(radix.value.as_str(), radix.base)),
            Literal::Symbol(sym) => ctxt.get_var(sym.name.as_str()),
        }
    }
}

impl Eval for Call {
    fn eval(&self, ctxt: &mut Ctxt) -> Option<f64> {
        let (header, body) = ctxt.get_func(&self.name.name)?;
        let mut ctxt = ctxt.clone();

        let mut args = self.args.iter();
        let mut params = header.params.iter();
        loop {
            match (args.next(), params.next()) {
                // a positional argument
                // evaluate it and add it to the context for use in the function body
                (Some(arg), Some(param)) => {
                    let value = arg.eval(&mut ctxt)?;
                    ctxt.add_var(&param.symbol().name, value);
                },

                // too many arguments were given
                (Some(_), None) => return None,

                // no argument was given for this parameter
                // use the default value if there is one
                (None, Some(param)) => {
                    // if there is no default, return None
                    let value = match param {
                        Param::Symbol(_) => return None,
                        Param::Default(_, expr) => expr.eval(&mut ctxt),
                    }?;
                    ctxt.add_var(&param.symbol().name, value);
                },

                // begin evaluation
                (None, None) => break,
            }
        }

        body.eval(&mut ctxt)
    }
}

impl Eval for Unary {
    fn eval(&self, ctxt: &mut Ctxt) -> Option<f64> {
        let operand = self.operand.eval(ctxt)?;
        Some(match self.op {
            UnaryOp::Not => if operand == 0.0 { 1.0 } else { 0.0 },
            UnaryOp::BitNot => !(operand as i64) as f64,
            UnaryOp::Factorial => factorial(operand),
            UnaryOp::Neg => -1.0 * operand,
        })
    }
}

impl Eval for Binary {
    fn eval(&self, ctxt: &mut Ctxt) -> Option<f64> {
        let left = self.lhs.eval(ctxt)?;
        let right = self.rhs.eval(ctxt)?;
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
        assert_eq!(expr.eval_default(), Some(3.0));
    }

    #[test]
    fn binary_expr_2() {
        let mut parser = Parser::new("1 + 2 * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default(), Some(7.0));
    }

    #[test]
    fn binary_and_unary() {
        let mut parser = Parser::new("3 * -5 / 5! + 6");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default(), Some(5.875));
    }

    #[test]
    fn parenthesized() {
        let mut parser = Parser::new("((1 + 9) / 5) * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default(), Some(6.0));
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
        assert_eq!(expr.eval(&mut ctxt), None);

        // call function
        let mut parser = Parser::new("f(7)");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(&mut ctxt), Some(90.0));
    }

    #[test]
    fn complicated_func_call() {
        let mut ctxt = Ctxt::default();

        // assign function
        let mut parser = Parser::new("f(n = 3, k = 6) = n * k");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(&mut ctxt), None);

        // call function
        let tries = [
            (None, None, 18.0),
            // (None, Some(4.0), 12.0), // TODO: there is currently no way to pass only the second argument
            (Some(9.0), None, 54.0),
            (Some(8.0), Some(14.0), 112.0),
        ];
        for (n, k, expected_result) in tries {
            let source = format!(
                "f({}{})",
                n.map_or("".to_string(), |n| n.to_string()),
                k.map_or("".to_string(), |k| format!(", {}", k)),
            );
            let mut parser = Parser::new(&source);
            let expr = parser.try_parse_full::<Expr>().unwrap();
            assert_eq!(
                expr.eval(&mut ctxt),
                Some(expected_result),
                "source: {}",
                source,
            );
        }
    }
}
