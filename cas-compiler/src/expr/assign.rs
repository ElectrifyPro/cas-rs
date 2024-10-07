use cas_compute::numerical::{func::{Function, User}, value::Value};
use cas_error::Error;
use cas_parser::parser::{
    ast::{assign::{Assign, AssignTarget}, LitSym},
    token::op::AssignOpKind,
};
use crate::{
    error::OverrideBuiltinConstant,
    item::Symbol,
    Compile,
    Compiler,
    InstructionKind,
    NewChunk,
};

/// Extracts the user symbol ID from a [`Symbol`], returning an error if the symbol is not a
/// user-defined symbol.
fn extract_user_symbol(lit: &LitSym, symbol: Symbol) -> Result<usize, Error> {
    symbol.index()
        .map_err(|name| Error::new(vec![lit.span.clone()], OverrideBuiltinConstant {
            name: name.to_string(),
        }))
}

impl Compile for Assign {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        match &self.target {
            AssignTarget::Symbol(symbol) => {
                // variable assignment
                match self.op.kind {
                    AssignOpKind::Assign => {
                        compiler.with_state(|state| {
                            state.top_level_assign = false;
                        }, |compiler| {
                            self.value.compile(compiler)
                        })?;

                        let symbol_id = compiler.resolve_user_symbol_or_insert(symbol)?;
                        compiler.add_instr(InstructionKind::StoreVar(symbol_id));
                    },
                    compound => {
                        let resolved = compiler.resolve_symbol(symbol)?;
                        compiler.add_instr_with_spans(
                            InstructionKind::LoadVar(resolved),
                            vec![symbol.span.clone()],
                        );

                        compiler.with_state(|state| {
                            state.top_level_assign = false;
                        }, |compiler| {
                            self.value.compile(compiler)
                        })?;

                        compiler.add_instr(InstructionKind::Binary(compound.into()));
                        let symbol_id = extract_user_symbol(&symbol, compiler.resolve_symbol(symbol)?)?;
                        compiler.add_instr(InstructionKind::StoreVar(symbol_id));
                    }
                }
            },
            AssignTarget::Index(index) => {
                // assignment to index of list
                match self.op.kind {
                    AssignOpKind::Assign => {
                        // load new value
                        self.value.compile(compiler)?;
                    },
                    compound => {
                        // load the (hopefully) list value
                        index.target.compile(compiler)?;

                        // load value to index by
                        index.index.compile(compiler)?;

                        // compute the new value to assign
                        compiler.add_instr_with_spans(
                            InstructionKind::LoadIndexed,
                            vec![index.target.span(), index.index.span()],
                        );
                        self.value.compile(compiler)?;
                        compiler.add_instr(InstructionKind::Binary(compound.into()));
                    }
                }

                // load the (hopefully) list value
                index.target.compile(compiler)?;

                // load value to index by
                index.index.compile(compiler)?;

                compiler.add_instr_with_spans(
                    InstructionKind::StoreIndexed,
                    vec![index.target.span(), index.index.span()],
                );
            },
            AssignTarget::Func(header) => {
                // for function assignment, create a new chunk for the function body
                let NewChunk { id, chunk, captures } = compiler.new_chunk(header, |compiler| {
                    // arguments to function are placed on the stack, so we need to go through the
                    // parameters in reverse order to store them in the correct order
                    for param in header.params.iter().rev() {
                        let symbol_id = compiler.add_symbol(param.symbol())?;
                        compiler.add_instr(InstructionKind::AssignVar(symbol_id));
                    }
                    self.value.compile(compiler)?;
                    compiler.add_instr(InstructionKind::Return);
                    Ok(())
                })?;

                let user_func = User::new(chunk, captures);
                compiler.add_instr(InstructionKind::LoadConst(Value::Function(Function::User(user_func))));
                compiler.add_instr(InstructionKind::StoreVar(id));
            },
        };

        Ok(())
    }
}
