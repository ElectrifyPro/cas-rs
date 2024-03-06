use cas_parser::parser::ast::loop_expr::Continue;
use crate::numerical::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

impl Eval for Continue {
    fn eval(&self, _: &mut Ctxt) -> Result<Value, Error> {
        Ok(Value::Unit)
    }
}
