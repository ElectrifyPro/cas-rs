use cas_parser::parser::loop_expr::Break;
use crate::{ctxt::Ctxt, error::{kind::LoopControlOutsideLoop, Error}, eval::Eval, value::Value};

/// Helper macro to call [`Eval::eval`], then check if the loop should be broken. Errors will also
/// be propogated automatically with the `?` operator.
///
/// The return type of this macro is [`Value`]. It can also diverge if evaluation fails, or if
/// `break_loop` is true, with type [`Result<Value, Error>`].
#[macro_export]
macro_rules! eval_break {
    ($value:expr, $ctxt:expr) => {{
        let value = $value.eval($ctxt)?;
        if $ctxt.break_loop {
            return Ok(value);
        }
        value
    }};
}

impl Eval for Break {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        if ctxt.loop_depth == 0 {
            Err(Error::new(
                vec![self.span.clone()],
                LoopControlOutsideLoop,
            ))
        } else {
            if let Some(value) = &self.value {
                // evaluate the entire expression first, then begin breaking the loop
                // `loop` expression will then set `break_loop` back to false once we propogate up
                // the stack
                let result = value.eval(ctxt)?;
                ctxt.break_loop = true;
                Ok(result)
            } else {
                ctxt.break_loop = true;
                Ok(Value::Unit)
            }
        }
    }
}
