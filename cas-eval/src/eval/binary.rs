use cas_parser::parser::{binary::Binary, token::op::BinOpKind};
use rug::ops::Pow;
use crate::{
    consts::{int_from_float, float},
    ctxt::Ctxt,
    error::{kind::{BitshiftOverflow, InvalidBinaryOperation}, Error},
    eval::Eval,
    value::Value,
};

/// Evaluates a binary expression with two real operands.
fn eval_real_operands(
    binary: &Binary,
    left: Value,
    right: Value,
) -> Result<Value, Error> {
    let typename = left.typename();
    let (Value::Number(left), Value::Number(right)) = (left, right) else {
        unreachable!()
    };
    Ok(match binary.op.kind {
        BinOpKind::Exp => Value::Number(left.pow(right)),
        BinOpKind::Mul => Value::Number(left * right),
        BinOpKind::Div => Value::Number(left / right),
        BinOpKind::Mod => Value::Number(left % right),
        BinOpKind::Add => Value::Number(left + right),
        BinOpKind::Sub => Value::Number(left - right),
        BinOpKind::BitRight => Value::Number(float(int_from_float(left) >> int_from_float(right).to_usize().ok_or_else(|| Error::new(
            vec![binary.lhs.span(), binary.op.span.clone(), binary.rhs.span()],
            BitshiftOverflow,
        ))?)),
        BinOpKind::BitLeft => Value::Number(float(int_from_float(left) << int_from_float(right).to_usize().ok_or_else(|| Error::new(
            vec![binary.lhs.span(), binary.op.span.clone(), binary.rhs.span()],
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
        BinOpKind::And | BinOpKind::Or => return Err(Error::new(
            vec![binary.lhs.span(), binary.op.span.clone(), binary.rhs.span()],
            InvalidBinaryOperation {
                op: binary.op.kind,
                implicit: binary.op.implicit,
                left: typename,
                right: typename,
            },
        )),
    })
}

/// Evaluates a binary expression with two complex operands.
fn eval_complex_operands(
    binary: &Binary,
    left: Value,
    right: Value,
) -> Result<Value, Error> {
    let typename = left.typename();
    let (Value::Complex(left), Value::Complex(right)) = (left, right) else {
        unreachable!()
    };
    Ok(match binary.op.kind {
        BinOpKind::Exp => Value::Complex(left.pow(right)),
        BinOpKind::Mul => Value::Complex(left * right),
        BinOpKind::Div => Value::Complex(left / right),
        BinOpKind::Add => Value::Complex(left + right),
        BinOpKind::Sub => Value::Complex(left - right),
        BinOpKind::Eq => Value::Boolean(left == right),
        BinOpKind::NotEq => Value::Boolean(left != right),
        BinOpKind::ApproxEq => {
            let (real, float) = left.into_real_imag();
            let (real2, float2) = right.into_real_imag();
            Value::Boolean(
                (real - real2).abs() < 1e-6 && (float - float2).abs() < 1e-6,
            )
        },
        BinOpKind::ApproxNotEq => {
            let (real, float) = left.into_real_imag();
            let (real2, float2) = right.into_real_imag();
            Value::Boolean(
                (real - real2).abs() >= 1e-6 || (float - float2).abs() >= 1e-6,
            )
        },
        BinOpKind::And | BinOpKind::Or| BinOpKind::Mod
            | BinOpKind::BitRight | BinOpKind::BitLeft | BinOpKind::BitAnd | BinOpKind::BitOr
            | BinOpKind::Greater | BinOpKind::GreaterEq | BinOpKind::Less | BinOpKind::LessEq => return Err(Error::new(
                vec![binary.lhs.span(), binary.op.span.clone(), binary.rhs.span()],
                InvalidBinaryOperation {
                    op: binary.op.kind,
                    implicit: binary.op.implicit,
                    left: typename,
                    right: typename,
                },
            )),
    })
}

/// Evaluates a binary expression with two boolean operands.
fn eval_bool_operands(
    binary: &Binary,
    left: Value,
    right: Value,
) -> Result<Value, Error> {
    let typename = left.typename();
    let (Value::Boolean(left), Value::Boolean(right)) = (left, right) else {
        unreachable!()
    };
    Ok(match binary.op.kind {
        BinOpKind::And => Value::Boolean(left && right),
        BinOpKind::Or => Value::Boolean(left || right),
        BinOpKind::Eq => Value::Boolean(left == right),
        BinOpKind::NotEq => Value::Boolean(left != right),
        BinOpKind::ApproxEq => Value::Boolean(left == right),
        BinOpKind::ApproxNotEq => Value::Boolean(left != right),
        BinOpKind::Exp | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod | BinOpKind::Add
            | BinOpKind::Sub | BinOpKind::BitRight | BinOpKind::BitLeft | BinOpKind::BitAnd
            | BinOpKind::BitOr | BinOpKind::Greater | BinOpKind::GreaterEq | BinOpKind::Less
            | BinOpKind::LessEq => return Err(Error::new(
                vec![binary.lhs.span(), binary.op.span.clone(), binary.rhs.span()],
                InvalidBinaryOperation {
                    op: binary.op.kind,
                    implicit: binary.op.implicit,
                    left: typename,
                    right: typename,
                },
            )),
    })
}

impl Eval for Binary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let left = self.lhs.eval(ctxt)?;
        let right = self.rhs.eval(ctxt)?;
        if left.is_real() && right.is_real() {
            return eval_real_operands(self, left.coerce_real(), right.coerce_real());
        }

        if left.is_complex() && right.is_complex() {
            return eval_complex_operands(self, left.coerce_complex(), right.coerce_complex());
        }

        if left.is_boolean() && right.is_boolean() {
            return eval_bool_operands(self, left, right);
        }

        Err(Error::new(
            vec![self.lhs.span(), self.op.span.clone(), self.rhs.span()],
            InvalidBinaryOperation {
                op: self.op.kind,
                implicit: self.op.implicit,
                left: left.typename(),
                right: right.typename(),
            },
        ))
    }
}
