use cas_parser::parser::{unary::Unary, token::op::UnaryOpKind};
use crate::{
    consts::{complex, int_from_float, float},
    ctxt::Ctxt,
    error::{kind::InvalidUnaryOperation, Error},
    eval::Eval,
    funcs::factorial,
    value::Value,
    eval_break,
};

impl Eval for Unary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let operand = eval_break!(self.operand, ctxt).coerce_real();
        match operand {
            Value::Number(num) => Ok(match self.op.kind {
                UnaryOpKind::Not => Value::Boolean(num.is_zero()),
                UnaryOpKind::BitNot => Value::Number(float(!int_from_float(num))),
                UnaryOpKind::Factorial => Value::Number(factorial(num)),
                UnaryOpKind::Neg => Value::Number(-num),
            }),
            Value::Complex(ref comp) => Ok(match self.op.kind {
                UnaryOpKind::Not => Value::Boolean(comp.eq0()),
                UnaryOpKind::Neg => Value::Complex(complex(&*comp.as_neg())),
                _ => return Err(Error::new(vec![self.operand.span(), self.op.span.clone()], InvalidUnaryOperation {
                    op: self.op.kind,
                    expr_type: operand.typename(),
                })),
            }),
            Value::Boolean(b) => {
                if self.op.kind == UnaryOpKind::Not {
                    Ok(Value::Boolean(!b))
                } else {
                    Err(Error::new(vec![self.operand.span(), self.op.span.clone()], InvalidUnaryOperation {
                        op: self.op.kind,
                        expr_type: operand.typename(),
                    }))
                }
            },
            Value::Unit => Err(Error::new(vec![self.operand.span(), self.op.span.clone()], InvalidUnaryOperation {
                op: self.op.kind,
                expr_type: operand.typename(),
            })),
        }
    }
}
