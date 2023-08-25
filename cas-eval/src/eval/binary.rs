use cas_parser::parser::{binary::Binary, token::op::BinOpKind};
use rug::ops::Pow;
use crate::{
    consts::{int_from_float, float},
    ctxt::Ctxt,
    error::{kind::{BitshiftOverflow, InvalidBinaryOperation}, Error},
    eval::Eval,
    value::Value,
};

impl Eval for Binary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let left = self.lhs.eval(ctxt)?;
        let right = self.rhs.eval(ctxt)?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => Ok(match self.op.kind {
                BinOpKind::Exp => Value::Number(left.pow(right)),
                BinOpKind::Mul => Value::Number(left * right),
                BinOpKind::Div => Value::Number(left / right),
                BinOpKind::Mod => Value::Number(left % right),
                BinOpKind::Add => Value::Number(left + right),
                BinOpKind::Sub => Value::Number(left - right),
                BinOpKind::BitRight => Value::Number(float(int_from_float(left) >> int_from_float(right).to_usize().ok_or_else(|| Error::new(
                    vec![self.lhs.span(), self.op.span.clone(), self.rhs.span()],
                    BitshiftOverflow,
                ))?)),
                BinOpKind::BitLeft => Value::Number(float(int_from_float(left) << int_from_float(right).to_usize().ok_or_else(|| Error::new(
                    vec![self.lhs.span(), self.op.span.clone(), self.rhs.span()],
                    BitshiftOverflow,
                ))?)),
                BinOpKind::BitAnd => Value::Number(float(int_from_float(left) & int_from_float(right))),
                BinOpKind::BitOr => Value::Number(float(int_from_float(left) | int_from_float(right))),
                BinOpKind::Greater => Value::Boolean(left > right),
                BinOpKind::GreaterEq => Value::Boolean(left >= right),
                BinOpKind::Less => Value::Boolean(left < right),
                BinOpKind::LessEq => Value::Boolean(left <= right),
                BinOpKind::Eq => Value::Boolean(left == right),
                BinOpKind::NotEq => Value::Boolean(left != right),
                BinOpKind::ApproxEq => Value::Boolean((left - right).abs() < 1e-6),
                BinOpKind::ApproxNotEq => Value::Boolean((left - right).abs() >= 1e-6),
                BinOpKind::And => Value::Boolean(!left.is_zero() && !right.is_zero()),
                BinOpKind::Or => Value::Boolean(!left.is_zero() || !right.is_zero()),
            }),
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
