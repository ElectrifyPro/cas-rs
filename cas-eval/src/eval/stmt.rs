use cas_parser::parser::stmt::Stmt;
use crate::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

impl Eval for Stmt {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        if self.semicolon.is_some() {
            self.expr.eval(ctxt)?;
            Ok(Value::Unit)
        } else {
            self.expr.eval(ctxt)
        }
    }
}
