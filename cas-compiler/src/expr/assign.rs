use cas_parser::parser::{
    ast::assign::{Assign, AssignTarget},
    token::op::AssignOpKind,
};
use crate::{Compiler, Compile, Instruction};

impl Compile for Assign {
    fn compile(&self, compiler: &mut Compiler) {
        match &self.target {
            AssignTarget::Symbol(symbol) => {
                // variable assignment
                match self.op.kind {
                    AssignOpKind::Assign => {
                        compiler.with_state(|state| {
                            state.top_level_assign = false;
                        }, |compiler| {
                            self.value.compile(compiler);
                        });

                        compiler.add_instr(Instruction::StoreVar(symbol.name.clone()))
                    },
                    compound => {
                        compiler.add_instr(Instruction::LoadVar(symbol.name.clone()));

                        compiler.with_state(|state| {
                            state.top_level_assign = false;
                        }, |compiler| {
                            self.value.compile(compiler);
                        });

                        compiler.add_instr(Instruction::Binary(compound.into()));
                        compiler.add_instr(Instruction::StoreVar(symbol.name.clone()));
                    }
                }
            },
            AssignTarget::Func(_) => todo!(),
        }
    }
}
