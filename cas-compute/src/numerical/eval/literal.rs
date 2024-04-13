use cas_parser::parser::ast::literal::Literal;
use crate::numerical::{
    ctxt::Ctxt,
    error::{kind::UndefinedVariable, Error},
    eval::Eval,
    value::Value,
};
use crate::primitive::{from_str_radix, float_from_str, int_from_str};
use std::{cell::RefCell, rc::Rc};

impl Eval for Literal {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match self {
            Literal::Integer(int) => Ok(Value::Integer(int_from_str(&int.value))),
            Literal::Float(float) => Ok(Value::Float(float_from_str(&float.value))),
            Literal::Radix(radix) => Ok(Value::Integer(from_str_radix(radix.value.as_str(), radix.base))),
            Literal::Boolean(boolean) => Ok(Value::Boolean(boolean.value)),
            Literal::Symbol(sym) => ctxt.get_var(sym.name.as_str())
                .ok_or_else(|| Error::new(vec![sym.span.clone()], UndefinedVariable { name: sym.name.clone() })),
            Literal::Unit(_) => Ok(Value::Unit),
            Literal::List(list) => {
                let mut values = Vec::with_capacity(list.values.len());
                for value in &list.values {
                    values.push(value.eval(ctxt)?);
                }
                Ok(Value::List(Rc::new(RefCell::new(values))))
            },
            Literal::ListRepeat(_) => todo!(),
        }
    }
}
