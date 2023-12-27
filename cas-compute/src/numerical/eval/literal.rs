use cas_parser::parser::ast::literal::Literal;
use crate::numerical::{
    consts::{float, float_from_str},
    ctxt::Ctxt,
    error::{kind::UndefinedVariable, Error},
    eval::Eval,
    funcs::from_str_radix,
    value::Value,
};

impl Eval for Literal {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match self {
            Literal::Integer(int) => Ok(Value::Number(float_from_str(&int.value))),
            Literal::Float(float) => Ok(Value::Number(float_from_str(&float.value))),
            Literal::Radix(radix) => Ok(Value::Number(float(from_str_radix(radix.value.as_str(), radix.base)))),
            Literal::Symbol(sym) => ctxt.get_var(sym.name.as_str())
                .ok_or_else(|| Error::new(vec![sym.span.clone()], UndefinedVariable { name: sym.name.clone() })),
            Literal::Unit(_) => Ok(Value::Unit),
        }
    }
}
