use cas_parser::parser::ast::unary::Unary;
use crate::{error::Error, Compile, Compiler, Instruction};

impl Compile for Unary {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.operand.compile(compiler)?;
        compiler.add_instr(Instruction::Unary(self.op.kind));
        Ok(())
    }
}
