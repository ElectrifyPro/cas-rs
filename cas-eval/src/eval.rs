use cas_parser::parser::{
    assign::{AssignTarget, Param},
    call::Call,
    expr::{Expr, Primary},
    binary::Binary,
    literal::Literal,
    unary::Unary,
    token::op::{BinOpKind, UnaryOpKind},
};
use super::{
    builtins,
    ctxt::Ctxt,
    error::{
        kind::{
            InvalidBinaryOperation,
            InvalidUnaryOperation,
            MissingArgument,
            TooManyArguments,
            UndefinedFunction,
            UndefinedVariable,
        },
        Error,
    },
    funcs::{factorial, from_str_radix},
    value::Value,
};

/// Any type that can be evaluated to produce a value.
pub trait Eval {
    /// Evaluate the expression to produce a value, using the given context. The expression should
    /// return [`None`] if it cannot be evaluated.
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error>;

    /// Evaluate the expression to produce a value, using the default context. The expression should
    /// return [`None`] if it cannot be evaluated.
    fn eval_default(&self) -> Result<Value, Error> {
        self.eval(&mut Default::default())
    }
}

impl Eval for Expr {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
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
                        ctxt.add_var(&symbol.name, value.clone());
                        Ok(value)
                    },
                    AssignTarget::Func(header) => {
                        // function assignment
                        ctxt.add_func(header.clone(), *assign.value.clone());
                        Ok(Value::Unit)
                    },
                }
            },
        }
    }
}

impl Eval for Primary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match self {
            Primary::Literal(literal) => literal.eval(ctxt),
            Primary::Paren(paren) => paren.expr.eval(ctxt),
            Primary::Call(call) => call.eval(ctxt),
        }
    }
}

impl Eval for Literal {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match self {
            Literal::Number(num) => Ok(Value::Number(num.value)),
            Literal::Radix(radix) => Ok(Value::Number(from_str_radix(radix.value.as_str(), radix.base))),
            Literal::Symbol(sym) => ctxt.get_var(sym.name.as_str())
                .ok_or_else(|| Error::new(vec![sym.span.clone()], UndefinedVariable { name: sym.name.clone() })),
        }
    }
}

impl Eval for Call {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        // try using a builtin function
        if let Some(builtin) = builtins::get_builtin(&self.name.name) {
            let args = self.args.iter()
                .map(|arg| arg.eval(ctxt))
                .collect::<Result<Vec<_>, _>>()?;
            return builtin(&args)
                .map_err(|err| err.into_error(self));
        }

        let (header, body) = ctxt.get_func(&self.name.name)
            .ok_or_else(|| Error::new(vec![self.name.span.clone()], UndefinedFunction {
                name: self.name.name.clone(),
                suggestions: ctxt.get_similar_funcs(&self.name.name)
                    .into_iter()
                    .map(|(header, _)| header.name.name.clone())
                    .collect(),
            }))?;
        let mut ctxt = ctxt.clone();

        let mut index = 0;
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
                (Some(_), None) => return Err(Error::new(vec![self.span()], TooManyArguments {
                    name: self.name.name.clone(),
                    expected: header.params.len(),
                    given: self.args.len(),
                })),

                // no argument was given for this parameter
                // use the default value if there is one
                (None, Some(param)) => {
                    // if there is no default, that's an error
                    let value = match param {
                        Param::Symbol(_) => return Err(Error::new(vec![self.span()], MissingArgument {
                            name: self.name.name.clone(),
                            index,
                            expected: header.params.len(),
                            given: self.args.len(),
                        })),
                        Param::Default(_, expr) => expr.eval(&mut ctxt),
                    }?;
                    ctxt.add_var(&param.symbol().name, value);
                },

                // begin evaluation
                (None, None) => break,
            }

            index += 1;
        }

        body.eval(&mut ctxt)
    }
}

impl Eval for Unary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let operand = self.operand.eval(ctxt)?;
        match operand {
            Value::Number(num) => Ok(Value::Number(match self.op.kind {
                UnaryOpKind::Not => if num == 0.0 { 1.0 } else { 0.0 },
                UnaryOpKind::BitNot => !(num as i64) as f64,
                UnaryOpKind::Factorial => factorial(num),
                UnaryOpKind::Neg => -1.0 * num,
            })),
            Value::Unit => Err(Error::new(vec![self.operand.span(), self.op.span.clone()], InvalidUnaryOperation {
                op: self.op.kind,
                expr_type: format!("{:?}", operand),
            })),
        }
    }
}

impl Eval for Binary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let left = self.lhs.eval(ctxt)?;
        let right = self.rhs.eval(ctxt)?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Number(match self.op.kind {
                BinOpKind::Exp => left.powf(right),
                BinOpKind::Mul => left * right,
                BinOpKind::Div => left / right,
                BinOpKind::Add => left + right,
                BinOpKind::Sub => left - right,
                BinOpKind::BitRight => ((left as i64) >> (right as i64)) as f64,
                BinOpKind::BitLeft => ((left as i64) << (right as i64)) as f64,
                BinOpKind::BitAnd => ((left as i64) & (right as i64)) as f64,
                BinOpKind::BitOr => ((left as i64) | (right as i64)) as f64,
                BinOpKind::Greater => (left > right) as u8 as f64,
                BinOpKind::GreaterEq => (left >= right) as u8 as f64,
                BinOpKind::Less => (left < right) as u8 as f64,
                BinOpKind::LessEq => (left <= right) as u8 as f64,
                BinOpKind::Eq => (left == right) as u8 as f64,
                BinOpKind::NotEq => (left != right) as u8 as f64,
                BinOpKind::ApproxEq => ((left - right).abs() < 1e-6) as u8 as f64,
                BinOpKind::ApproxNotEq => ((left - right).abs() >= 1e-6) as u8 as f64,
                BinOpKind::And => if left != 0.0 && right != 0.0 { 1.0 } else { 0.0 },
                BinOpKind::Or => if left != 0.0 || right != 0.0 { 1.0 } else { 0.0 },
            })),
            (left, right) => Err(Error::new(
                vec![self.lhs.span(), self.op.span.clone(), self.rhs.span()],
                InvalidBinaryOperation {
                    op: self.op.kind,
                    implicit: self.op.implicit,
                    left: format!("{:?}", left),
                    right: format!("{:?}", right),
                },
            )),
        }
    }
}

/// Eval tests depend on the parser, so ensure that parser tests pass before running these.
#[cfg(test)]
mod tests {
    use crate::builtins;
    use super::*;

    use cas_parser::parser::Parser;
    use std::f64::consts;

    #[test]
    fn binary_expr() {
        let mut parser = Parser::new("1 + 2");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), Value::Number(3.0));
    }

    #[test]
    fn binary_expr_2() {
        let mut parser = Parser::new("1 + 2 * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), Value::Number(7.0));
    }

    #[test]
    fn binary_and_unary() {
        let mut parser = Parser::new("3 * -5 / 5! + 6");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), Value::Number(5.875));
    }

    #[test]
    fn parenthesized() {
        let mut parser = Parser::new("((1 + 9) / 5) * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), Value::Number(6.0));
    }

    #[test]
    fn degree_to_radian() {
        let mut parser = Parser::new("90 * 2 * pi / 360");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), Value::Number(consts::PI / 2.0));
    }

    #[test]
    fn precision() {
        let mut parser = Parser::new("e^2 - tau");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), Value::Number(consts::E.powf(2.0) - consts::TAU));
    }

    #[test]
    fn precision_2() {
        let mut parser = Parser::new("pi^2 * 17! / -4.9 + e");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(
            expr.eval_default().unwrap(),
            Value::Number(consts::PI.powf(2.0) * factorial(17.0) / -4.9 + consts::E)
        );
    }

    #[test]
    fn func_call() {
        let mut ctxt = Ctxt::default();

        // assign function
        let mut parser = Parser::new("f(x) = x^2 + 5x + 6");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(&mut ctxt).unwrap(), Value::Unit);

        // call function
        let mut parser = Parser::new("f(7)");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(&mut ctxt).unwrap(), Value::Number(90.0));
    }

    #[test]
    fn complicated_func_call() {
        let mut ctxt = Ctxt::default();

        // assign function
        let mut parser = Parser::new("f(n = 3, k = 6) = n * k");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(&mut ctxt).unwrap(), Value::Unit);

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
                expr.eval(&mut ctxt).unwrap(),
                Value::Number(expected_result),
                "source: {}",
                source,
            );
        }
    }

    #[test]
    fn builtin_func_arg_check() {
        assert_eq!(builtins::abs(&[Value::Number(-4.0)]).unwrap(), Value::Number(4.0));
        assert!(builtins::abs(&[Value::Unit]).is_err());
    }
}
