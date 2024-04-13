use cas_parser::parser::ast::return_expr::Return;
use crate::numerical::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

impl Eval for Return {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        todo!()
    }
}
