use cas_compute::numerical::value::Value;
use cas_parser::parser::{
    ast::{assign::{Assign, AssignTarget}, LitSym},
    token::op::AssignOpKind,
};
use crate::{
    error::{kind, Error},
    item::Symbol,
    Compile,
    Compiler,
    Instruction,
};

/// Extracts the user symbol ID from a [`Symbol`], returning an error if the symbol is not a
/// user-defined symbol.
fn extract_user_symbol(lit: &LitSym, symbol: Symbol) -> Result<usize, Error> {
    symbol.index()
        .map_err(|name| Error::new(vec![lit.span.clone()], kind::OverrideBuiltinConstant {
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
                        compiler.add_instr(Instruction::StoreVar(symbol_id));
                    },
                    compound => {
                        compiler.add_instr(Instruction::LoadVar(compiler.resolve_symbol(symbol)?));

                        compiler.with_state(|state| {
                            state.top_level_assign = false;
                        }, |compiler| {
                            self.value.compile(compiler)
                        })?;

                        compiler.add_instr(Instruction::Binary(compound.into()));
                        let symbol_id = extract_user_symbol(&symbol, compiler.resolve_symbol(symbol)?)?;
                        compiler.add_instr(Instruction::StoreVar(symbol_id));
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
                        // compute the new value to assign
                        compiler.add_instr(Instruction::LoadIndexed);
                        self.value.compile(compiler)?;
                        compiler.add_instr(Instruction::Binary(compound.into()));
                    }
                }

                // load the (hopefully) list value
                index.target.compile(compiler)?;

                // load value to index by
                index.index.compile(compiler)?;

                compiler.add_instr(Instruction::StoreIndexed);
            },
            AssignTarget::Func(header) => {
                // for function assignment, create a new chunk for the function body
                compiler.new_chunk(header, |compiler| {
                    // arguments to function are placed on the stack, so we need to go through the
                    // parameters in reverse order to store them in the correct order
                    for param in header.params.iter().rev() {
                        let symbol_id = compiler.resolve_user_symbol_or_insert(param.symbol())?;
                        compiler.add_instr(Instruction::AssignVar(symbol_id));
                    }
                    self.value.compile(compiler)?;
                    compiler.add_instr(Instruction::Return);
                    Ok(())
                })?;

                // TODO: function assignment needs to return something so the automatically
                // generated `Drop` instruction drops this value instead of something else
                // important in the stack
                // also, in the future, we might want to make functions their own type
                compiler.add_instr(Instruction::LoadConst(Value::Unit));
            },
        };

        Ok(())
    }
}
