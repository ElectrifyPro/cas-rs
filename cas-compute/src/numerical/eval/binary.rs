use cas_parser::parser::{ast::binary::Binary, token::op::BinOpKind};
use rug::ops::Pow;
use crate::eval_break;
use crate::numerical::{
    consts::{int_from_float, float},
    ctxt::Ctxt,
    error::{kind::{BitshiftOverflow, InvalidBinaryOperation}, Error},
    eval::{error::EvalError, Eval},
    value::Value,
};

/// Evaluates a binary expression with two real operands.
fn eval_real_operands(
    op: BinOpKind,
    implicit: bool,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    let typename = left.typename();
    let (Value::Number(left), Value::Number(right)) = (left, right) else {
        unreachable!()
    };
    Ok(match op {
        BinOpKind::Exp => Value::Number(left.pow(right)),
        BinOpKind::Mul => Value::Number(left * right),
        BinOpKind::Div => Value::Number(left / right),
        BinOpKind::Mod => Value::Number(left % right),
        BinOpKind::Add => Value::Number(left + right),
        BinOpKind::Sub => Value::Number(left - right),
        BinOpKind::BitRight => Value::Number(float(int_from_float(left) >> int_from_float(right).to_usize().ok_or(BitshiftOverflow)?)),
        BinOpKind::BitLeft => Value::Number(float(int_from_float(left) << int_from_float(right).to_usize().ok_or(BitshiftOverflow)?)),
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
        BinOpKind::And | BinOpKind::Or => Err(InvalidBinaryOperation {
            op,
            implicit,
            left: typename,
            right: typename,
        })?
    })
}

/// Evaluates a binary expression with two complex operands.
fn eval_complex_operands(
    op: BinOpKind,
    implicit: bool,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    let typename = left.typename();
    let (Value::Complex(left), Value::Complex(right)) = (left, right) else {
        unreachable!()
    };
    Ok(match op {
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
        BinOpKind::And | BinOpKind::Or | BinOpKind::Mod
            | BinOpKind::BitRight | BinOpKind::BitLeft | BinOpKind::BitAnd | BinOpKind::BitOr
            | BinOpKind::Greater | BinOpKind::GreaterEq | BinOpKind::Less | BinOpKind::LessEq => Err(InvalidBinaryOperation {
                op,
                implicit,
                left: typename,
                right: typename,
            })?
    })
}

/// Evaluates a binary expression with two boolean operands.
fn eval_bool_operands(
    op: BinOpKind,
    implicit: bool,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    let typename = left.typename();
    let (Value::Boolean(left), Value::Boolean(right)) = (left, right) else {
        unreachable!()
    };
    Ok(match op {
        BinOpKind::And => Value::Boolean(left && right),
        BinOpKind::Or => Value::Boolean(left || right),
        BinOpKind::Eq => Value::Boolean(left == right),
        BinOpKind::NotEq => Value::Boolean(left != right),
        BinOpKind::ApproxEq => Value::Boolean(left == right),
        BinOpKind::ApproxNotEq => Value::Boolean(left != right),
        BinOpKind::Exp | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod | BinOpKind::Add
            | BinOpKind::Sub | BinOpKind::BitRight | BinOpKind::BitLeft | BinOpKind::BitAnd
            | BinOpKind::BitOr | BinOpKind::Greater | BinOpKind::GreaterEq | BinOpKind::Less
            | BinOpKind::LessEq => Err(InvalidBinaryOperation {
                op,
                implicit,
                left: typename,
                right: typename,
            })?,
    })
}

/// Evaluates a binary expression with two unit type operands.
fn eval_unit_operands(
    op: BinOpKind,
    implicit: bool,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    let typename = left.typename();
    let (Value::Unit, Value::Unit) = (left, right) else {
        unreachable!()
    };
    Ok(match op {
        BinOpKind::Eq | BinOpKind::ApproxEq => Value::Boolean(true),
        BinOpKind::NotEq | BinOpKind::ApproxNotEq | BinOpKind::Greater | BinOpKind::GreaterEq
            | BinOpKind::Less | BinOpKind::LessEq => Value::Boolean(false),
        BinOpKind::Exp | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod | BinOpKind::Add
            | BinOpKind::Sub | BinOpKind::BitRight | BinOpKind::BitLeft | BinOpKind::BitAnd
            | BinOpKind::BitOr | BinOpKind::And | BinOpKind::Or => Err(InvalidBinaryOperation {
                op,
                implicit,
                left: typename,
                right: typename,
            })?,
    })
}

/// Evaluates the binary expression given the operator, and the left and right operands.
pub(crate) fn eval_operands(
    op: BinOpKind,
    implicit: bool,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    if left.is_real() && right.is_real() {
        return eval_real_operands(op, implicit, left.coerce_real(), right.coerce_real());
    }

    if left.is_complex() && right.is_complex() {
        return eval_complex_operands(op, implicit, left.coerce_complex(), right.coerce_complex());
    }

    if left.is_boolean() && right.is_boolean() {
        return eval_bool_operands(op, implicit, left, right);
    }

    if left.is_unit() && right.is_unit() {
        return eval_unit_operands(op, implicit, left, right);
    }

    Err(InvalidBinaryOperation {
        op,
        implicit,
        left: left.typename(),
        right: right.typename(),
    }.into())
}

impl Eval for Binary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let left = eval_break!(self.lhs, ctxt);
        let right = eval_break!(self.rhs, ctxt);
        eval_operands(self.op.kind, self.op.implicit, left, right)
            .map_err(|e| e.into_error(self))
    }
}
