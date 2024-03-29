use cas_parser::parser::ast::literal::Literal;
use crate::numerical::{
    ctxt::Ctxt,
    error::{kind::UndefinedVariable, Error},
    eval::Eval,
    value::Value,
};
use crate::primitive::{float, from_str_radix, float_from_str};

impl Eval for Literal {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match self {
            Literal::Integer(int) => Ok(Value::Float(float_from_str(&int.value))),
            Literal::Float(float) => Ok(Value::Float(float_from_str(&float.value))),
            Literal::Radix(radix) => Ok(Value::Float(float(from_str_radix(radix.value.as_str(), radix.base)))),
            Literal::Symbol(sym) => ctxt.get_var(sym.name.as_str())
                .ok_or_else(|| Error::new(vec![sym.span.clone()], UndefinedVariable { name: sym.name.clone() })),
            Literal::Unit(_) => Ok(Value::Unit),
        }
    }
}
