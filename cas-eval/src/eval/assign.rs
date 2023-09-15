use cas_parser::parser::assign::{AssignTarget, Assign};
use crate::{ctxt::Ctxt, error::Error, eval::Eval, value::Value, eval_break};

impl Eval for Assign {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match &self.target {
            AssignTarget::Symbol(symbol) => {
                // variable assignment
                let value = eval_break!(self.value, ctxt);
                ctxt.add_var(&symbol.name, value.clone());
                Ok(value)
            },
            AssignTarget::Func(header) => {
                // function assignment
                ctxt.add_func(
                    header.clone(),
                    *self.value.clone(),
                    self.is_recursive(),
                );
                Ok(Value::Unit)
            },
        }
    }
}
