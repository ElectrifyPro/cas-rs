use cas_parser::parser::{
    ast::{assign::{Assign, AssignTarget}, literal::LitSym},
    token::op::AssignOpKind,
};
use crate::{
    ctxt::Ctxt,
    error::{kind::UndefinedVariable, Error},
    eval::{binary::eval_operands, Eval},
    value::Value,
    eval_break,
};

/// Evaluate an assignment expression to a symbol.
fn assign_to_symbol(
    lit_sym: &LitSym,
    assign: &Assign,
    op: AssignOpKind,
    ctxt: &mut Ctxt,
) -> Result<Value, Error> {
    let rhs = eval_break!(assign.value, ctxt);
    match op {
        AssignOpKind::Assign => {
            ctxt.add_var(&lit_sym.name, rhs.clone());
            Ok(rhs)
        },
        compound => {
            if let Some(lhs) = ctxt.get_var(&lit_sym.name) {
                let new_lhs = eval_operands(compound.into(), false, lhs, rhs)
                    .map_err(|e| e.into_error(assign))?;
                ctxt.add_var(&lit_sym.name, new_lhs.clone());
                Ok(new_lhs)
            } else {
                Err(Error::new(
                    vec![lit_sym.span.clone()],
                    UndefinedVariable { name: lit_sym.name.clone() },
                ))
            }
        },
    }
}

impl Eval for Assign {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match &self.target {
            AssignTarget::Symbol(symbol) => {
                // variable assignment
                assign_to_symbol(symbol, self, self.op.kind, ctxt)
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
