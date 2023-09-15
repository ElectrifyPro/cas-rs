use cas_parser::parser::loop_expr::Continue;
use crate::{ctxt::Ctxt, error::{kind::LoopControlOutsideLoop, Error}, eval::Eval, value::Value};

impl Eval for Continue {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        if ctxt.loop_depth == 0 {
            Err(Error::new(
                vec![self.span.clone()],
                LoopControlOutsideLoop,
            ))
        } else {
            Ok(Value::Unit)
        }
    }
}
