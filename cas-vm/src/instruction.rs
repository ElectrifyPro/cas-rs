use cas_compute::{
    funcs::miscellaneous::Factorial,
    numerical::{error::kind::InvalidBinaryOperation, value::Value},
    primitive::{complex, float, int_from_float},
};
use cas_parser::parser::token::op::{BinOpKind, UnaryOpKind};
use rug::ops::Pow;

/// Evaluates a binary expression with two integer operands.
fn eval_integer_operands(
    op: BinOpKind,
    left: Value,
    right: Value,
) -> Result<Value, ()> {
    let (Value::Integer(left), Value::Integer(right)) = (left, right) else {
        unreachable!()
    };
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
        BinOpKind::BitRight => Value::Integer(left >> right.to_usize().unwrap()),
        BinOpKind::BitLeft => Value::Integer(left << right.to_usize().unwrap()),
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
        BinOpKind::And | BinOpKind::Or => unreachable!(),
    })
}

/// Evaluates a binary expression with two real operands.
fn eval_real_operands(
    op: BinOpKind,
    left: Value,
    right: Value,
) -> Result<Value, ()> {
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
        BinOpKind::BitRight => Value::Float(float(int_from_float(left) >> int_from_float(right).to_usize().unwrap())),
        BinOpKind::BitLeft => Value::Float(float(int_from_float(left) << int_from_float(right).to_usize().unwrap())),
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
        BinOpKind::And | BinOpKind::Or => unreachable!(),
    })
}

/// Evaluates a binary expression with two complex operands.
fn eval_complex_operands(
    op: BinOpKind,
    left: Value,
    right: Value,
) -> Result<Value, ()> {
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
            | BinOpKind::Greater | BinOpKind::GreaterEq | BinOpKind::Less | BinOpKind::LessEq => unreachable!(),
    })
}

/// Evaluates a binary expression with two boolean operands.
fn eval_bool_operands(
    op: BinOpKind,
    left: Value,
    right: Value,
) -> Result<Value, ()> {
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
            | BinOpKind::LessEq => unreachable!(),
    })
}

/// Evaluates a binary expression with two unit type operands.
fn eval_unit_operands(
    op: BinOpKind,
    left: Value,
    right: Value,
) -> Result<Value, ()> {
    let (Value::Unit, Value::Unit) = (left, right) else {
        unreachable!()
    };
    Ok(match op {
        BinOpKind::Eq | BinOpKind::ApproxEq | BinOpKind::GreaterEq | BinOpKind::LessEq => Value::Boolean(true),
        BinOpKind::NotEq | BinOpKind::ApproxNotEq | BinOpKind::Greater | BinOpKind::Less => Value::Boolean(false),
        BinOpKind::Exp | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod | BinOpKind::Add
            | BinOpKind::Sub | BinOpKind::BitRight | BinOpKind::BitLeft | BinOpKind::BitAnd
            | BinOpKind::BitOr | BinOpKind::And | BinOpKind::Or => unreachable!(),
    })
}

/// Helper to execute binary evaluation instructions. The result is pushed onto the value stack.
pub fn exec_binary_instruction(op: BinOpKind, value_stack: &mut Vec<Value>) -> Result<(), InvalidBinaryOperation> {
    let right = value_stack.pop().unwrap();
    let left = value_stack.pop().unwrap();

    if left.is_integer() && right.is_integer() {
        return Ok(value_stack.push(eval_integer_operands(op, left.coerce_integer(), right.coerce_integer()).unwrap()));
    }

    if left.is_real() && right.is_real() {
        return Ok(value_stack.push(eval_real_operands(op, left.coerce_float(), right.coerce_float()).unwrap()));
    }

    if left.is_complex() && right.is_complex() {
        return Ok(value_stack.push(eval_complex_operands(op, left.coerce_complex(), right.coerce_complex()).unwrap()));
    }

    if left.is_boolean() && right.is_boolean() {
        return Ok(value_stack.push(eval_bool_operands(op, left, right).unwrap()));
    }

    if left.is_unit() && right.is_unit() {
        return Ok(value_stack.push(eval_unit_operands(op, left, right).unwrap()));
    }

    Err(InvalidBinaryOperation {
        op,
        implicit: false, // TODO
        left: left.typename(),
        right: right.typename(),
    })
}

/// Helper to execute unary evaluation instructions.
pub fn exec_unary_instruction(op: UnaryOpKind, value_stack: &mut Vec<Value>) {
    let operand = value_stack.pop().unwrap().coerce_number();
    let value = match operand {
        Value::Float(num) => match op {
            UnaryOpKind::Not => Value::Boolean(num.is_zero()),
            UnaryOpKind::BitNot => Value::Float(float(!int_from_float(num))),
            UnaryOpKind::Factorial => Factorial::eval_static(num),
            UnaryOpKind::Neg => Value::Float(-num),
        },
        Value::Integer(num) => match op {
            UnaryOpKind::Not => Value::Boolean(num.is_zero()),
            UnaryOpKind::BitNot => Value::Integer(!num),
            UnaryOpKind::Factorial => Factorial::eval_static(float(num)),
            UnaryOpKind::Neg => Value::Integer(-num),
        },
        Value::Complex(ref comp) => match op {
            UnaryOpKind::Not => Value::Boolean(comp.is_zero()),
            UnaryOpKind::Neg => Value::Complex(complex(&*comp.as_neg())),
            _ => unreachable!(),
        },
        Value::Boolean(b) => {
            if op == UnaryOpKind::Not {
                Value::Boolean(!b)
            } else {
                unreachable!()
            }
        },
        Value::Unit | Value::List(_) => unreachable!(),
    };
    value_stack.push(value);
}