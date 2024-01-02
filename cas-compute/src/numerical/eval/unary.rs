use cas_parser::parser::{ast::unary::Unary, token::op::UnaryOpKind};
use crate::eval_break;
use crate::funcs::miscellaneous::Factorial;
use crate::numerical::{
    ctxt::Ctxt,
    error::{kind::InvalidUnaryOperation, Error},
    eval::Eval,
    value::Value,
};
use crate::primitive::{complex, int_from_float, float};

impl Eval for Unary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let operand = eval_break!(self.operand, ctxt).coerce_float();
        match operand {
            Value::Float(num) => Ok(match self.op.kind {
                UnaryOpKind::Not => Value::Boolean(num.is_zero()),
                UnaryOpKind::BitNot => Value::Float(float(!int_from_float(num))),
                UnaryOpKind::Factorial => Value::Float(Factorial::eval_static(num)),
                UnaryOpKind::Neg => Value::Float(-num),
            }),
            Value::Integer(num) => Ok(match self.op.kind {
                UnaryOpKind::Not => Value::Boolean(num.is_zero()),
                UnaryOpKind::BitNot => Value::Integer(!num),
                UnaryOpKind::Factorial => Value::Float(Factorial::eval_static(float(num))),
                UnaryOpKind::Neg => Value::Integer(-num),
            }),
            Value::Complex(ref comp) => Ok(match self.op.kind {
                UnaryOpKind::Not => Value::Boolean(comp.is_zero()),
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
