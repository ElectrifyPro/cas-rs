use cas_parser::parser::if_expr::If;
use crate::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

impl Eval for If {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let condition = self.condition.eval(ctxt)?;
        if condition.is_truthy() {
            self.then_expr.eval(ctxt)
        } else {
            self.else_expr.eval(ctxt)
        }
    }
}
