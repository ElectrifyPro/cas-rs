use cas_parser::parser::{unary::Unary, token::op::UnaryOpKind};
use crate::{
    consts::{int_from_float, float},
    ctxt::Ctxt,
    error::{kind::InvalidUnaryOperation, Error},
    eval::Eval,
    funcs::factorial,
    value::Value,
};

impl Eval for Unary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let operand = self.operand.eval(ctxt)?;
        match operand {
            Value::Number(num) => Ok(match self.op.kind {
                UnaryOpKind::Not => Value::Boolean(num.is_zero()),
                UnaryOpKind::BitNot => Value::Number(float(!int_from_float(num))),
                UnaryOpKind::Factorial => Value::Number(float(factorial(int_from_float(num)))),
                UnaryOpKind::Neg => Value::Number(-num),
            }),
            Value::Boolean(b) => {
                if self.op.kind == UnaryOpKind::Not {
                    Ok(Value::Boolean(!b))
                } else {
                    Err(Error::new(vec![self.op.span.clone()], InvalidUnaryOperation {
                        op: self.op.kind,
                        expr_type: format!("{:?}", operand),
                    }))
                }
            },
            Value::Unit => Err(Error::new(vec![self.operand.span(), self.op.span.clone()], InvalidUnaryOperation {
                op: self.op.kind,
                expr_type: format!("{:?}", operand),
            })),
        }
    }
}
