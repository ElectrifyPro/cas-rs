use cas_error::Error;
use cas_parser::parser::ast::call::Call;
use crate::{item::{Item, Symbol}, Compile, Compiler, InstructionKind};

/// Given a call to a function of the given `symbol`, returns `true` if this call is a call to
/// itself (i.e. recursive).
fn is_calling_self(compiler: &Compiler, symbol: Symbol) -> bool {
    let Symbol::User(symbol_id) = symbol else {
        return false;
    };
    let Some(item) = compiler.sym_table.resolve_item_by_id(symbol_id) else {
        return false;
    };
    let Item::Func(func) = item else {
        // call to an item that is NOT known to be a function
        return false;
    };

    if let Some(current_scope_id) = compiler.state.fn_scope {
        if func.scope_id != current_scope_id {
            // found a function, but `symbol` is not in its scope
            return false;
        }
    } else {
        // can't recurse when in the global scope
        return false;
    }

    true
}

impl Compile for Call {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.args.iter().try_for_each(|arg| arg.compile(compiler))?;

        // TODO: allow calling any receiever when it is supported
        // load the function
        let symbol_id = compiler.resolve_symbol(&self.name)?;

        let spans = self.outer_span()
            .into_iter()
            .chain(self.args.iter().map(|arg| arg.span()))
            .collect();

        if is_calling_self(compiler, symbol_id) {
            if self.derivatives > 0 {
                // compute derivative of function
                compiler.add_instr_with_spans(
                    InstructionKind::CallSelfDerivative(self.derivatives, self.args.len()),
                    spans,
                );
            } else {
                // call the function
                compiler.add_instr_with_spans(InstructionKind::CallSelf(self.args.len()), spans);
            }
        } else {
            compiler.add_instr_with_spans(
                InstructionKind::LoadVar(symbol_id),
                vec![self.name.span.clone()],
            );

            if self.derivatives > 0 {
                // compute derivative of function
                compiler.add_instr_with_spans(
                    InstructionKind::CallDerivative(self.derivatives, self.args.len()),
                    spans,
                );
            } else {
                // call the function
                compiler.add_instr_with_spans(InstructionKind::Call(self.args.len()), spans);
            }
        }

        Ok(())
    }
}
