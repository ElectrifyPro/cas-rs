use cas_compute::{
    funcs::{combinatoric::Ncr, miscellaneous::Factorial},
    numerical::{builtin::Builtin, value::Value},
    primitive::{complex, float, int_from_float},
};
use cas_error::{ErrorKind, Error};
use cas_parser::parser::token::op::{BinOpKind, UnaryOpKind};
use crate::error::{
    BitshiftOverflow,
    InvalidBinaryOperation,
    InvalidUnaryOperation,
    NonNumericDerivative,
};
use replace_with::replace_with_or_abort;
use rug::{ops::Pow, Float};
use std::ops::Range;

/// Represents an error that can occur while evaluating a binary or unary expression.
#[derive(Debug)]
pub(crate) enum EvalError {
    /// Attempted to apply a binary operator to incompatible operands.
    InvalidBinaryOperation(InvalidBinaryOperation),

    /// Attempted to apply a unary operator to an incompatible operand.
    InvalidUnaryOperation(InvalidUnaryOperation),

    /// Attempted to bitshift by a value that is too large.
    BitshiftOverflow(BitshiftOverflow),

    /// Encountered a non-numeric type while using prime notation to derivate a function call.
    NonNumericDerivative(NonNumericDerivative),
}

impl From<InvalidBinaryOperation> for EvalError {
    fn from(e: InvalidBinaryOperation) -> Self {
        EvalError::InvalidBinaryOperation(e)
    }
}

impl From<InvalidUnaryOperation> for EvalError {
    fn from(e: InvalidUnaryOperation) -> Self {
        EvalError::InvalidUnaryOperation(e)
    }
}

impl From<BitshiftOverflow> for EvalError {
    fn from(e: BitshiftOverflow) -> Self {
        EvalError::BitshiftOverflow(e)
    }
}

impl From<NonNumericDerivative> for EvalError {
    fn from(e: NonNumericDerivative) -> Self {
        EvalError::NonNumericDerivative(e)
    }
}

impl EvalError {
    /// Convert the [`EvalError`] into the general [`Error`] type, using the given syntax tree to
    /// provide spans.
    pub fn into_error(self, spans: Vec<Range<usize>>) -> Error {
        macro_rules! error {
            ($( $kind:ident ),* $(,)?) => {
                match self {
                    $( EvalError::$kind(e) => Error {
                        spans,
                        kind: Box::new(e) as Box<dyn ErrorKind>,
                    }, )*
                }
            };
        }

        error!(
            InvalidBinaryOperation,
            InvalidUnaryOperation,
            BitshiftOverflow,
            NonNumericDerivative,
        )
    }
}

/// Evaluates a binary expression with two integer operands.
fn eval_integer_operands(
    op: BinOpKind,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    let typename = left.typename();
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
        BinOpKind::And | BinOpKind::Or => return Err(InvalidBinaryOperation {
            op,
            implicit: false,
            left: typename,
            right: typename,
        })?,
    })
}

/// Evaluates a binary expression with two real operands.
fn eval_float_operands(
    op: BinOpKind,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    let typename = left.typename();
    let (Value::Float(left), Value::Float(right)) = (left, right) else {
        unreachable!()
    };
    Ok(match op {
        BinOpKind::Exp => complex(left).pow(right).into(),
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
        BinOpKind::And | BinOpKind::Or => return Err(InvalidBinaryOperation {
            op,
            implicit: false,
            left: typename,
            right: typename,
        })?,
    })
}

/// Evaluates a binary expression with two complex operands.
fn eval_complex_operands(
    op: BinOpKind,
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
            | BinOpKind::Greater | BinOpKind::GreaterEq | BinOpKind::Less | BinOpKind::LessEq => return Err(InvalidBinaryOperation {
            op,
            implicit: false,
            left: typename,
            right: typename,
        })?
    })
}

/// Evaluates a binary expression with two boolean operands.
fn eval_bool_operands(
    op: BinOpKind,
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
        BinOpKind::BitAnd => Value::Boolean(left & right),
        BinOpKind::BitOr => Value::Boolean(left | right),
        BinOpKind::Greater => Value::Boolean(left & !right),
        BinOpKind::GreaterEq => Value::Boolean(left >= right),
        BinOpKind::Less => Value::Boolean(!left & right),
        BinOpKind::LessEq => Value::Boolean(left <= right),
        BinOpKind::Exp | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod | BinOpKind::Add
            | BinOpKind::Sub | BinOpKind::BitRight | BinOpKind::BitLeft => return Err(InvalidBinaryOperation {
            op,
            implicit: false,
            left: typename,
            right: typename,
        })?
    })
}

/// Evaluates a binary expression with two unit type operands.
fn eval_unit_operands(
    op: BinOpKind,
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
            | BinOpKind::BitOr | BinOpKind::And | BinOpKind::Or => return Err(InvalidBinaryOperation {
            op,
            implicit: false,
            left: typename,
            right: typename,
        })?
    })
}

/// Evaluates a binary expression with two list operands.
fn eval_list_operands(
    op: BinOpKind,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    let typename = left.typename();
    let (Value::List(left), Value::List(right)) = (left, right) else {
        unreachable!()
    };
    Ok(match op {
        BinOpKind::Eq => Value::Boolean(left == right),
        BinOpKind::NotEq => Value::Boolean(left != right),
        BinOpKind::ApproxEq => Value::Boolean(left == right),
        BinOpKind::ApproxNotEq => Value::Boolean(left != right),
        BinOpKind::Exp | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod | BinOpKind::Add
            | BinOpKind::Sub | BinOpKind::BitRight | BinOpKind::BitLeft | BinOpKind::BitAnd
            | BinOpKind::BitOr | BinOpKind::And | BinOpKind::Or | BinOpKind::Greater
            | BinOpKind::GreaterEq | BinOpKind::Less | BinOpKind::LessEq => return Err(InvalidBinaryOperation {
            op,
            implicit: false,
            left: typename,
            right: typename,
        })?
    })
}

/// Evaluates a binary expression with exactly one list operand, and any other type.
///
/// This works by applying the operation element-wise to the list.
fn eval_list_operand(op: BinOpKind, left: Value, right: Value) -> Result<Value, EvalError> {
    match (left, right) {
        (Value::List(list), right) => {
            {
                let mut list = list.borrow_mut();
                for value in list.iter_mut() {
                    // replace_with_or_abort(value, |value| {
                    //     eval_binary(op, value, right.clone()).unwrap()
                    // });
                    unsafe {
                        let copy = std::ptr::read(value);
                        match eval_binary(op, copy, right.clone()) {
                            Ok(result) => {
                                std::ptr::write(value, result);
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    }
                }
            }
            Ok(Value::List(list))
        }
        (left, Value::List(list)) => {
            {
                let mut list = list.borrow_mut();
                for value in list.iter_mut() {
                    // replace_with_or_abort(value, |value| {
                    //     eval_binary(op, left.clone(), value).unwrap()
                    // });
                    unsafe {
                        let copy = std::ptr::read(value);
                        match eval_binary(op, left.clone(), copy) {
                            Ok(result) => {
                                std::ptr::write(value, result);
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    }
                }
            }
            Ok(Value::List(list))
        }
        _ => unreachable!(),
    }
}

/// Evaluates a binary expression with two operands.
fn eval_binary(
    op: BinOpKind,
    left: Value,
    right: Value,
) -> Result<Value, EvalError> {
    if left.is_integer() && right.is_integer() {
        return eval_integer_operands(op, left.coerce_integer(), right.coerce_integer());
    }

    if left.is_float() && right.is_float() {
        return eval_float_operands(op, left.coerce_float(), right.coerce_float());
    }

    if left.is_complex() && right.is_complex() {
        return eval_complex_operands(op, left.coerce_complex(), right.coerce_complex());
    }

    if left.is_boolean() && right.is_boolean() {
        return eval_bool_operands(op, left, right);
    }

    if left.is_unit() && right.is_unit() {
        return eval_unit_operands(op, left, right);
    }

    if left.is_list() && right.is_list() {
        return eval_list_operands(op, left, right);
    } else if left.is_list() || right.is_list() {
        return eval_list_operand(op, left, right);
    }

    Err(InvalidBinaryOperation {
        op,
        implicit: false, // TODO
        left: left.typename(),
        right: right.typename(),
    })?
}

/// Helper to execute binary evaluation instructions. The result is pushed onto the value stack.
pub fn exec_binary_instruction(
    op: BinOpKind,
    value_stack: &mut Vec<Value>,
) -> Result<(), EvalError> {
    let right = value_stack.pop().unwrap();
    let left = value_stack.pop().unwrap();
    value_stack.push(eval_binary(op, left, right)?);
    Ok(())
}

/// Helper to execute unary evaluation instructions.
pub fn exec_unary_instruction(op: UnaryOpKind, value_stack: &mut Vec<Value>) -> Result<(), EvalError> {
    fn eval(op: UnaryOpKind, value: Value) -> Result<Value, EvalError> {
        let typename = value.typename();
        Ok(match value {
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
                op => return Err(InvalidUnaryOperation {
                    op,
                    expr_type: typename,
                })?,
            },
            Value::Boolean(b) => {
                if op == UnaryOpKind::Not {
                    Value::Boolean(!b)
                } else {
                    return Err(InvalidUnaryOperation {
                        op,
                        expr_type: typename,
                    })?;
                }
            },
            Value::List(list) => {
                // apply unary operation element-wise
                {
                    let mut list = list.borrow_mut();
                    for value in list.iter_mut() {
                        // TODO: aborts if the operation is invalid
                        // replace_with_or_abort(value, |value| eval(op, value));
                        unsafe {
                            let copy = std::ptr::read(value);
                            match eval(op, copy) {
                                Ok(result) => {
                                    std::ptr::write(value, result);
                                }
                                Err(e) => {
                                    return Err(e);
                                }
                            }
                        }
                    }
                }
                Value::List(list)
            },
            Value::Unit
                | Value::Range(_, _, _)
                | Value::Function(_) => return Err(InvalidUnaryOperation {
                op,
                expr_type: typename,
            })?,
        })
    }

    let operand = value_stack.pop().unwrap().coerce_number();
    value_stack.push(eval(op, operand)?);
    Ok(())
}

/// Extracts the real number from a value, or returns an error if the value is not a real number.
fn get_real(value: Value) -> Result<Float, EvalError> {
    match value.coerce_float() {
        Value::Float(n) => Ok(n),
        value => Err(NonNumericDerivative {
            expr_type: value.typename(),
        })?,
    }
}

/// Stateful struct that progressively evaluates the numerical derviative of a function.
///
/// This is useful for evaluating user-defined functions, where evaluation typically requires
/// executing multiple instructions many times.
///
/// To compute a derivative, call [`Derivative::next_eval`], which provides the next value to
/// evaluate the function with. Use this value to evaluate the function
pub struct Derivative {
    /// The current order of the derivative.
    current_order: u8,

    /// The total number of derivatives to compute.
    total_order: u8,

    /// The initial value of the function.
    initial: Float,

    eval: (Option<Float>, Option<Float>),

    /// The sums used to compute the derivative.
    sum: (Float, Float),

    /// The step size used.
    step: Float,
}

impl Derivative {
    /// Create a new derivative evaluator at the given initial value and step size.
    pub fn new(order: u8, initial: Value) -> Result<Self, EvalError> {
        Ok(Self {
            current_order: 0,
            total_order: order,
            initial: get_real(initial)?,
            eval: (None, None),
            sum: (float(0), float(0)),
            step: float(1e-32),
        })
    }

    /// Get the next value to evaluate the function with. After evaluating the function, submit the
    /// value by calling [`Derivative::advance`] with it.
    pub fn next_eval(&self) -> Option<Value> {
        match self.eval {
            (None, _) => {
                Some(Value::Float(&self.initial + float(self.current_order * &self.step)))
            },
            (Some(_), None) => {
                Some(Value::Float(&self.initial - float(self.current_order * &self.step)))
            },
            _ => None, // state will be reset by `advance`
        }
    }

    /// Advance the evaluation of the derivative, using the higher-order differentiation method
    /// found [here](https://en.wikipedia.org/wiki/Numerical_differentiation#Higher_derivatives).
    ///
    /// If the derivative has not yet finished computing, the function will return [`None`].
    /// Continue calling the function until it returns [`Some`].
    pub fn advance(&mut self, value: Value) -> Result<Option<Value>, EvalError> {
        let (c, d) = match std::mem::take(&mut self.eval) {
            (None, _) => {
                self.eval = (Some(get_real(value)?), None);
                return Ok(None);
            },
            (Some(left), None) => {
                (left, get_real(value)?)
            },
            _ => unreachable!(),
        };

        // synonym for a = (-1)^(k + derivatives) to avoid overflow errors
        let a = if self.current_order % 2 == self.total_order % 2 {
            1
        } else {
            -1
        };
        let b = Ncr::eval_static(self.total_order.into(), self.current_order.into());

        self.sum.0 += c * &b * a;
        self.sum.1 += d * &b * a;

        if self.current_order == self.total_order {
            let result_left = &self.sum.0 / float(&self.step).pow(self.total_order);
            let result_right = &self.sum.1 / float(-&self.step).pow(self.total_order);
            let result = (result_left + result_right) / 2;
            Ok(Some(Value::Float(result)))
        } else {
            self.current_order += 1;
            Ok(None)
        }
    }

    /// Helper to completely evaluate the derivative for builtin functions.
    pub fn eval_builtin(&mut self, builtin: &dyn Builtin) -> Result<Value, EvalError> {
        loop {
            let eval = self.next_eval().unwrap();

            // TODO remove unwrap
            let value = builtin.eval(Default::default(), vec![eval]).unwrap();
            match self.advance(value) {
                Ok(Some(result)) => break Ok(result),
                Ok(None) => (),
                Err(e) => break Err(e),
            }
        }
    }
}
