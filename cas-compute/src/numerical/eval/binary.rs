use cas_parser::parser::{ast::binary::Binary, token::op::BinOpKind};
use rug::ops::Pow;
use crate::eval_break;
use crate::numerical::{
    ctxt::Ctxt,
    error::{kind::{BitshiftOverflow, InvalidBinaryOperation}, Error},
    eval::{error::EvalError, Eval},
    value::Value,
};
use crate::primitive::{int_from_float, float};

/// Evaluates a binary expression with two integer operands.
fn eval_integer_operands(
    op: BinOpKind,
    implicit: bool,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    let typename = left.typename();
    let (Value::Integer(left), Value::Integer(right)) = (left, right) else {
        unreachable!()
    };
    println!("{}, {}", left, right);
    Ok(match op {
        BinOpKind::Exp => {
            // NOTE: there is no implementation of `pow` for `rug::Integer` with `rug::Integer`
            if let Some(right) = right.to_u16() {
                // so try the `u32` implementation if `right` fits in a `u16` (`u32::MAX` takes
                // unreasonably long to compute), ensuring we get full precision

                // `right` is a positive integer
                Value::Integer(left.pow(u32::from(right)))
            } else {
                // otherwise, use the `Float` implementation, which will be faster, but can lose
                // precision
                Value::Float(float(left).pow(right))
            }
        },
        BinOpKind::Mul => Value::Integer(left * right),
        BinOpKind::Div => Value::Float(float(left) / float(right)),
        BinOpKind::Mod => Value::Integer(left % right),
        BinOpKind::Add => Value::Integer(left + right),
        BinOpKind::Sub => Value::Integer(left - right),
        BinOpKind::BitRight => Value::Integer(left >> right.to_usize().ok_or(BitshiftOverflow)?),
        BinOpKind::BitLeft => Value::Integer(left << right.to_usize().ok_or(BitshiftOverflow)?),
        BinOpKind::BitAnd => Value::Integer(left & right),
        BinOpKind::BitOr => Value::Integer(left | right),
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

/// Evaluates a binary expression with two real operands.
fn eval_real_operands(
    op: BinOpKind,
    implicit: bool,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    let typename = left.typename();
    let (Value::Float(left), Value::Float(right)) = (left, right) else {
        unreachable!()
    };
    Ok(match op {
        BinOpKind::Exp => Value::Float(left.pow(right)),
        BinOpKind::Mul => Value::Float(left * right),
        BinOpKind::Div => Value::Float(left / right),
        BinOpKind::Mod => Value::Float(left % right),
        BinOpKind::Add => Value::Float(left + right),
        BinOpKind::Sub => Value::Float(left - right),
        BinOpKind::BitRight => Value::Float(float(int_from_float(left) >> int_from_float(right).to_usize().ok_or(BitshiftOverflow)?)),
        BinOpKind::BitLeft => Value::Float(float(int_from_float(left) << int_from_float(right).to_usize().ok_or(BitshiftOverflow)?)),
        BinOpKind::BitAnd => Value::Float(float(int_from_float(left) & int_from_float(right))),
        BinOpKind::BitOr => Value::Float(float(int_from_float(left) | int_from_float(right))),
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
        BinOpKind::Eq | BinOpKind::ApproxEq | BinOpKind::GreaterEq | BinOpKind::LessEq => Value::Boolean(true),
        BinOpKind::NotEq | BinOpKind::ApproxNotEq | BinOpKind::Greater | BinOpKind::Less => Value::Boolean(false),
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
    if left.is_integer() && right.is_integer() {
        return eval_integer_operands(op, implicit, left.coerce_integer(), right.coerce_integer());
    }

    if left.is_float() && right.is_float() {
        return eval_real_operands(op, implicit, left.coerce_float(), right.coerce_float());
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
