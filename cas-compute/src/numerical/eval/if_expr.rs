use cas_parser::parser::ast::if_expr::If;
use crate::eval_break;
use crate::numerical::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

impl Eval for If {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let condition = eval_break!(self.condition, ctxt);
        if condition.is_truthy() {
            self.then_expr.eval(ctxt)
        } else if let Some(else_expr) = &self.else_expr {
            else_expr.eval(ctxt)
        } else {
            Ok(Value::Unit)
        }
    }
}
