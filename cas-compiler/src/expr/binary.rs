use cas_parser::parser::ast::binary::Binary;
use crate::{error::Error, Compile, Compiler, Instruction};

impl Compile for Binary {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.lhs.compile(compiler)?;
        self.rhs.compile(compiler)?;
        compiler.add_instr(Instruction::Binary(self.op.kind));
        Ok(())
    }
}
