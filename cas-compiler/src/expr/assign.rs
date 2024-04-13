use cas_compute::numerical::value::Value;
use cas_parser::parser::{
    ast::assign::{Assign, AssignTarget},
    token::op::AssignOpKind,
};
use crate::{error::Error, Compile, Compiler, Instruction};

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

                        let symbol_id = compiler.resolve_symbol_or_insert(symbol);
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
                        compiler.add_instr(Instruction::StoreVar(compiler.resolve_symbol(symbol)?));
                    }
                }
            },
            AssignTarget::Func(header) => {
                // for function assignment, create a new chunk for the function body
                compiler.new_chunk(header, |compiler| {
                    // arguments to function are placed on the stack, so we need to go through the
                    // parameters in reverse order to store them in the correct order
                    for param in header.params.iter().rev() {
                        let symbol_id = compiler.resolve_symbol_or_insert(param.symbol());
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
